program bookkeeper
! example program that can parse arguments given to optional unit_test command
!  o writing a CSV file to be read into spread sheets, SQLite3, ....
!  o writing a HTML document
!  o writing a NAMELIST group file for post-processing into formats like a TAP (Test Anything Protocol) document
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT, stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT
use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
use M_framework, only : wrt, str
! put something here into namelist and it becomes an argument
character(len=:),allocatable :: type    ; namelist /args/ type,    /long/ type
character(len=:),allocatable :: name    ; namelist /args/ name,    /long/ name
character(len=:),allocatable :: passed  ; namelist /args/ passed,  /long/ passed
character(len=:),allocatable :: msg     ; namelist /args/ msg,     /long/ msg
character(len=25)            :: date    ; namelist /args/ date,    /long/ date
integer(kind=int64)          :: clicks  ; namelist /args/ clicks,  /long/ clicks
logical                      :: help    ; namelist                 /long/ help
logical                      :: version ; namelist                 /long/ version
logical                      :: verbose ; namelist                 /long/ verbose
!integer                      :: level = -1         ; namelist /args/ level
!integer,allocatable          :: flags(:)           ; namelist /args/ flags
character(len=:),allocatable :: html_header(:), html_footer(:)
character(len=*),parameter   :: g='(*(g0,1x))'
character(len=*),parameter   :: g0='(*(g0))'
character(len=1),parameter   :: comma=',', quote='"'
character(len=:),allocatable :: color
logical                      :: intable=.true.
integer :: i 
integer :: htmlfile, csvfile, clicksfile, nmlfile
interface exists        ! for backward compatibility, accdig(3f) preferred
   procedure fileexists
   procedure unitexists
end interface exists
!SETUP 
   ! This is an example program. A simple kludge is to pick scratch output or file output
   ! to work with just one output. Not something to do in production code.

   open(newunit=htmlfile,file='bookkeeper.html',position='append')
   !open(newunit=htmlfile,position='append',status='scratch')

   open(newunit=csvfile,file='bookkeeper.csv',position='append',DELIM='quote',form='formatted')
   !open(newunit=csvfile,position='append',DELIM='quote',form='formatted',status='scratch')

   open(newunit=clicksfile,file='bookkeeper_clicks.csv',position='append')
   !open(newunit=clicksfile,position='append',status='scratch')

   open(newunit=nmlfile,file='bookkeeper.nml',position='append')
   !open(newunit=nmlfile,position='append',status='scratch')


   call header()
   verbose=.false.
   type = repeat(' ',4096)
   name = repeat(' ',4096)
   passed = repeat(' ',4096)
   msg = repeat(' ',4096)
   !intable=.false.
   date=here_and_now()
   clicks=-1
!   flags = [(-1, i=1, 1000)]
!PARSE
   call cmdline_()
!TEARDOWN
   type = trim(type)
   name = trim(name)
   passed = trim(passed)
   msg = trim(msg)
!   flags = pack(flags, flags  /=  -1)
!USE
   write(nmlfile,nml=args) ! cannot do advance='no'
   select case(passed)
   case("untested");color='yellow'
   case("passed") ;color='#9F9'
   case("failed") ;color='red'
   case default   ;color='white'
   end select

   select case(type)

   case("start")
      !intable=.true.
      write(htmlfile,g0)'<table id="',name,'">'
      write(htmlfile,g0)'<caption class="caption" style="text-align:left">',name,str(' -',if=msg.ne.''),' ',msg,'</caption>'
      write(htmlfile,g0)'<tbody>'
      write(htmlfile,g0)'<tr class="header" class="odd"><!-- start -->'
      write(htmlfile,g0)' <th style="width:25%;text-align:center;"> name   </th>'
      write(htmlfile,g0)' <th style="width:10%;text-align:center;"> passed </th>'
      write(htmlfile,g0)' <th style="width:40%;text-align:center;"> msg    </th>'
      write(htmlfile,g0)' <th style="width:25%;text-align:center;"> date   </th>'
      write(htmlfile,g0)'</tr>'

   case("check")
      write(csvfile,g0),quote,name,quote,comma,quote,here_and_now(),quote,comma,quote,passed,quote,comma,quote,msg,quote

      write(htmlfile,g0)'<tr class="',passed,'" class="even"><!-- check -->'
      write(htmlfile,g0)' <td >', name    ,' </td>'
      write(htmlfile,g0)' <td style="text-align:center;" bgcolor="',color,'">', passed  ,' </td>'
      write(htmlfile,g0)' <td >', msg     ,' </td>'
      write(htmlfile,g0)' <td style="text-align:center;">', date    ,' </td>'
      write(htmlfile,g0)'</tr>'
   case("message")
      if(intable)then
         write(htmlfile,g0)'<tr class="message"><!-- message -->'
         write(htmlfile,g0)'<td colspan="4" bgcolor="#AAF" style="text-align:center;">',msg,'</td>'
         write(htmlfile,g0)'</tr>'
      else
         !write(htmlfile,g0)msg
      endif

   case("end")
      !intable=.false.
      if(passed.eq.'untested')then
         write(csvfile,g0),quote,name,quote,comma,quote,here_and_now(),quote,comma,quote,passed,quote,comma,quote,msg,quote

         write(htmlfile,g0)'<tr class="',passed,'" class="odd"><!-- end -->'
         write(htmlfile,g0)' <td >', name    ,' </td>'
         write(htmlfile,g0)' <td bgcolor="',color,'" style="text-align:center;">', passed  ,' </td>'
         write(htmlfile,g0)' <td >', msg     ,' </td>'
         write(htmlfile,g0)' <td style="text-align:center;">', date    ,' </td>'
         write(htmlfile,g0)'</tr>'
      endif
      if(clicks.ne.0)then
         write(htmlfile,g0)'<tr class="clicks" class="',passed,'"><!-- clicks -->'
         write(htmlfile,g0)'<td colspan="4" bgcolor="#AAF" style="text-align:center;"> clicks:',clicks,' for ',name,'</td>'
         write(htmlfile,g0)'</tr>'
      endif
      write(htmlfile,g0)'</table>'


      write(clicksfile,g0),quote,name,quote,comma,           &
                           quote,here_and_now(),quote,comma, &
                           quote,clicks,quote,comma,         &
                           quote,msg,quote


   case("stop")
      !intable=.false.
      write(htmlfile,g0)'<!-- STOP -->'

   end select
contains
subroutine cmdline_()                                ! read arguments from command line as NAMELIST group input
character(len=4096), save :: input(3) = [character(len=4096) :: '&long', '', ' /'], arg
character(len=256) :: message1, message2
integer :: i, j, k, ios, equal_pos, iend
   help = .false.
   version = .false.
   do i = 1, command_argument_count()
      call get_command_argument(i, arg)
      do j = 1, len_trim(arg)                   ! blank out leading - or / so "--name=value" or "/name=value" works
         if (index('/- ', arg (j:j)) == 0) exit
         arg (j:j) = ' '
      enddo
      arg = ' '//adjustl(arg)
      if (index(arg, '=') == 0) arg = trim(arg)//'=T' ! if no equal sign add =T
      iend=len_trim(arg)
      input(2)=arg
      if(arg(iend:iend).ne.',')input(2)=input(2)//' ,'
      read (input, nml=long, iostat=ios, iomsg=message1)
      if (ios /= 0) then                             ! assume first failure might be because of missing quotes
         equal_pos = index(arg, '=')            ! find position of '='
         if (equal_pos /= 0) then
            ! requote and try again
            arg = arg(:equal_pos)//'"'//arg(equal_pos + 1:len_trim(arg))//'"'
            iend=len_trim(arg)
            input(2)=arg
            if(arg(iend:iend).ne.',')input(2)=input(2)//' ,'
            read (input, nml=long, iostat=ios, iomsg=message2)
            if (ios /= 0) then
               write(stderr,g) 'BOOKKEEPER:ERROR UNQUOTED:',trim(message1),': when reading ',trim(input(2))
               if(message1.ne.message2) write(stderr,g) 'BOOKKEEPER:ERROR QUOTED  :',trim(message2),': when reading ',trim(input(2))
               type=trim(type)
               name=trim(name)
               passed=trim(passed)
               msg=trim(msg)
               write (*, nml=long, delim='quote')
               stop 2
            endif
         else
            write(stderr,g)'ERROR:',trim(message1),': when reading ',trim(input(2))
            type=trim(type)
            name=trim(name)
            passed=trim(passed)
            msg=trim(msg)
            write (stderr, nml=long, delim='quote')
            stop 4
         endif
      endif
   enddo
   if(help)call printhelp()
   if(version)call printversion()
end subroutine cmdline_
subroutine printhelp()
implicit none
character(len=*),parameter     :: ident="@(#)printhelp(3f): prints help information"
character(len=:),allocatable :: help_text(:)
integer                        :: i
help_text=[ CHARACTER(LEN=128) :: &
!12345678901234567890123456789012345678901234567890123456789012345678901234567890',&
'NAME                                                                            ',&
'    bookkeeper-(1f) - example filter program callable from the M_framework(3f)  ',&
'    unit testing harness.                                                       ',&
'    (LICENSE:PD)                                                                ',&
'SYNOPSIS                                                                        ',&
' commands:                                                                      ',&
'                                                                                ',&
'     bookkeeper [help| version]                                                 ',&
'     bookkeeper type="start" name="NAME" msg="MESSAGE TEXT" [opts]              ',&
'     bookkeeper type="check" name="NAME" msg="MESSAGE TEXT" ...                 ',&
'                passed="passed|failed"                                          ',&
'     bookkeeper type="end" name="NAME" msg="MESSAGE TEXT" clicks=N ...          ',&
'                [passed="failed|passed|untested"]                               ',&
'     bookkeeper type="stop" name="NAME" msg="MESSAGE TEXT"                      ',&
'                [passed="failed|passed|untested"] clicks=M                      ',&
'     bookkeeper type="message" name="NAME" msg="MESSAGE TEXT"                   ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
' This is an example program that shows how to create an external program that   ',&
' can be called by the testing harness in the M_framework__verify(3f) module     ',&
' for custom processing. Data is based in the form of NAMELIST group data        ',&
' for the NAMELIST group                                                         ',&
'                                                                                ',&
'     character(len=:),allocatable :: type                                       ',&
'     character(len=:),allocatable :: name                                       ',&
'     character(len=:),allocatable :: passed                                     ',&
'     character(len=:),allocatable :: msg                                        ',&
'     character(len=25)            :: date                                       ',&
'     integer(kind=int64)          :: clicks                                     ',&
'     logical                      :: help                                       ',&
'     logical                      :: version                                    ',&
'                                                                                ',&
' The data conforms to the Fortran NAMELIST group input syntax with the          ',&
' delimiter set to a double-quote except extraneous spaces are not allowed,      ',&
' and names may optionally be preceded by " --" or " /" and if no equal          ',&
' appears after a keyword "=T" is assumed to appear more like typical            ',&
' Unix long options or MSWindows commands. So these would all be                 ',&
' equivalent:                                                                    ',&
'                                                                                ',&
'      bookkeeper type="end" msg="message text"                                  ',&
'      bookkeeper /type="end" /msg="message text"                                ',&
'      bookkeeper --type="end" --msg="message text"                              ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'    type     "start","check","end","stop","message"                             ',&
'    name     a label, typically the name of the procedure that was tested.      ',&
'    passed   "passed","failed","untested"                                       ',&
'    msg      a description of the test, or a descriptive message                ',&
'    date     YYYY-MM-DDTHH:MM:SS-HH:MM                                          ',&
'    clicks   for type="end" assumed to be the time in clicks since the          ',&
'             previous type="start"                                              ',&
'    silent|brief|verbose   silent produces no output from unit_check_* routines ',&
'                           brief only shows failed tests, verbose shows all     ',&
'                           messages including compiler and options.             ',&
'    help     display this help and exit                                         ',&
'    version  output version information and exit                                ',&
'                                                                                ',&
'EXAMPLES                                                                        ',&
'    Sample commands                                                             ',&
'                                                                                ',&
'       fpm test -- command=bookkeeper                                           ',&
'       fpm test --target=''time*'' -- command=bookkeeper luns=6                   ',&
'                                                                                ',&
'                                                                                ',&
'SEE ALSO                                                                        ',&
'    M_framework(3f), unit_test(3f), unit_test_mode(3f)                          ',&
'AUTHOR                                                                          ',&
'   John S. Urban                                                                ',&
'LICENSE                                                                         ',&
'   Public Domain                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
end subroutine printhelp
subroutine printversion()
implicit none
character(len=*),parameter     :: ident="@(#)printhelp(3f): prints help information"
character(len=:),allocatable :: help_text(:)
integer                        :: i
help_text=[ CHARACTER(LEN=128) :: &
!12345678901234567890123456789012345678901234567890123456789012345678901234567890',&
'                                                                                ',&
'PRODUCT:        Fortran Unit Testing Harness                                    ',&
'PROGRAM:        bookkeeper(1)                                                   ',&
'DESCRIPTION:    filter data from M_framework(3f) Unit Testing Framework         ',&
'VERSION:        1.0, 20230507                                                   ',&
'AUTHOR:         John S. Urban                                                   ',&
'REPORTING BUGS: http://www.urbanjost.altervista.org/                            ',&
'HOME PAGE:      https://github.com/urbanjost/M_framework                        ',&
'LICENSE:        Public Domain. This is free software: you are free to change    ',&
'                and redistribute it. There is NO WARRANTY,                      ',&
'                to the extent permitted by law.                                 ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
end subroutine printversion

function here_and_now() ! 2023-05-01 23:05:12 UTC-0400
character :: date*(8), time*(10), zone*(5)
character(len=25) :: here_and_now
   call date_and_time(DATE=date,TIME=time,ZONE=zone)
   !write(here_and_now,'(*(g0))')date(1:4),'-',date(5:6),'-',date(7:8),' ',time(1:2),':',time(3:4),':',time(5:6),' UTC',zone
   ! rfc-3339"   ==> %Y-%M-%DT%h:%m:%s%z  2023-05-02T18:40:25-04:00; except ZONE is in
   write(here_and_now,'(*(g0))')&
   & date(1:4),'-',date(5:6),'-',date(7:8),'T',time(1:2),':',time(3:4),':',time(5:6),zone(1:3),':',zone(4:5)
end function here_and_now

logical function fileexists(filename) result(r)
character(len=*), intent(in) :: filename
integer :: size
    inquire(file=filename, exist=r,size=size)
    if(r)then
       if(size.le.0)r=.false.
    endif
end function fileexists

logical function unitexists(lun) result(r)
integer, intent(in) :: lun
integer :: size
    inquire(unit=lun, exist=r,size=size)
    if(r)then
       if(size.le.0)r=.false.
    endif
end function unitexists

subroutine header()
if ( .not.exists('bookkeeper.csv') )then
   write(csvfile,g0)'"name","date","passed","msg"'
endif
if ( .not.exists('bookkeeper.html') )then
   call get_html_header()
   write(htmlfile,'(a)')(trim(html_header(i)),i=1,size(html_header))
endif
if ( .not.exists('bookkeeper_clicks.csv') )then
   write(clicksfile,g0)'"name","date","clicks","msg"'
endif
if ( .not.exists('bookkeeper.nml') )then
   write(nmlfile,g0)' Fortran NAMELIST group file containing test results'
   write(nmlfile,g0)' that may be read with a simple program and used to'
   write(nmlfile,g0)' generate reports in other formats.'
   write(nmlfile,g0)' '
endif
end subroutine header

subroutine get_html_header()
html_header=[ CHARACTER(LEN=128) :: &
'<html>',&
'<head>',&
'<title>unit_test_results</title>',&
'<style>        ',&
'body {xfont-style: italic}',&
'body {                    ',&
'background-color:#FFF; color:#333; font-family:Verdana, Arial, Helvetica, sans-serif; font-size:1em; line-height:1.3em; }',&
'a:visited { color:#666; }                                                                                                ',&
'h1,h2,h3,h4,h5,h6 { color:#333; font-family:georgia, verdana, sans-serif; }                                              ',&
'h1 { font-size:200%; }                                                                                                   ',&
'h2 { font-size:173%; }                                                                                                   ',&
'h3 { font-size:144%; }                                                                                                   ',&
'h4 { font-size:120%; }                                                                                                   ',&
'h5,h6 { font-size:100% }                                                                                                 ',&
'a.nav,a:link.nav, a:visited.nav { background-color:#FFF; color:#000; }                                                   ',&
'/*                                                                                                                       ',&
'td { border:thin solid #888; word-break: break-all; width: 1em}                                                          ',&
'*/                                                                                                                       ',&
'td { border:thin solid #888; overflow-wrap: break-word; }                                                                ',&
'li { margin-bottom:0.5em; }                                                                                              ',&
'blockquote { display:block; font-size:90%; font-style:italic; line-height:1.5em; margin:0 0 1.5em; padding:0 2.5em; }    ',&
'pre { background-color:#DDD; font-size:90%; overflow:auto; padding:1em; }                                                ',&
'a,li span { color:#000; }                                                                                                ',&
'a:hover, a.nav:hover, a:hover math { background-color:#000; color:#FFF; }                                                ',&
'#Container { margin:0 10px; text-align:center; }                                                                         ',&
'#Content { border-top:none; margin:auto; padding:0.3em; text-align:left; width:100%; max-width:58em; }                   ',&
'span.webName { font-size:.5em; }                                                                                         ',&
'textarea#content { font-size: 1em; line-height: 1.125em; }                                                               ',&
'h1#pageName { line-height:1.4em; margin:0.2em 0 0.2em 0; padding:0; }                                                    ',&
'h2{ line-height:1.2em; margin:0.2em 0 0.2em 0; padding:0; color:blue;}                                                   ',&
'.property { color:#666; font-size:80%; }                                                                                 ',&
'a.existingWikiWord[title]{ //border: 1px dashed #BBB; }                                                                  ',&
'.byline { color:#666; font-size:.8em; font-style:italic; margin-bottom:1em; padding-top:1px; }                           ',&
'/* table takes its dimensions from the <table> element width and                                                         ',&
'the width of the first row of cells (or the <col> elements, if you have them).*/                                         ',&
'table { table-layout: fixed ; width: 100% ; border:double #000; border-collapse:collapse; }                              ',&
'td { width: 25% ; }                                                                                                      ',&
'table#unit_test { table-layout: fixed ; width: 100% ; border-collapse: collapse ; border: 1px black solid ; }            ',&
'table#unit_test td { width: 25% ; border: 1px black solid ; padding: 10px ; }                                            ',&
'table#unit_test caption { font-style: italic ; font-weight: 600; color: gray;}                                           ',&
'/*                                                                                                                       ',&
'col:nth-child(even) {background: #FFF}                                                                                   ',&
'col:nth-child(odd) {background: #CCC}                                                                                    ',&
'*/                                                                                                                       ',& 
'tr:nth-child(even) {background: #CCC}                                                                                    ',&
'tr:nth-child(odd) {background: #FFF}                                                                                     ',&
'</style>                                                                                                                 ',&
'<script language="JavaScript1.1" type="text/javascript">                                                                 ',&
'//<![CDATA[                                                                                                              ',&
'   //---------------------------------------------------------------------------                                         ',&
'   // turn on/off visibility of class                                                                                    ',&
'   function toggleHiddenClass(objectClassName) {                                                                         ',&
'                                                                                                                         ',&
'      const collection = document.getElementsByClassName(objectClassName);                                               ',&
'                                                                                                                         ',&
'      for (let i = 0; i < collection.length; i++) {                                                                      ',&
'         if(collection[i].style.display == "none" ) {                                                                    ',&
'            collection[i].style.display = "";                                                                            ',&
'         }else{                                                                                                          ',&
'            collection[i].style.display = "none";                                                                        ',&
'         }                                                                                                               ',&
'      }                                                                                                                  ',&
'   }                                                                                                                     ',&
'   //---------------------------------------------------------------------------                                         ',&
'   //alert("A1");                                                                                                        ',&
'   //---------------------------------------------------------------------------                                         ',&
'   // turn on/off visibility of id                                                                                       ',&
'   function toggleHiddenId(objectIdName) {                                                                               ',&
'      var TARGET = document.getElementById(objectIdName);                                                                ',&
'      if(TARGET.style.display == "none" ) {                                                                              ',&
'         TARGET.style.display = "";                                                                                      ',&
'      }else{                                                                                                             ',&
'         TARGET.style.display = "none";                                                                                  ',&
'      }                                                                                                                  ',&
'   //alert(TARGET.style.display);                                                                                        ',&
'   }                                                                                                                     ',& 
'   //---------------------------------------------------------------------------                                         ',&
'</script>                                                                                                                ',&
'<p>                                                                                                                      ',&
'   <form>                                                                                                                ',&
'      <input  type="button"  value="passed"   onclick="toggleHiddenClass(''passed'');" />                                ',&
'      <input  type="button"  value="failed"   onclick="toggleHiddenClass(''failed'');" />                                ',&
'      <input  type="button"  value="untested" onclick="toggleHiddenClass(''untested'');" />                              ',&
'      <input  type="button"  value="clicks"   onclick="toggleHiddenClass(''clicks'');" />                                ',&
'      <input  type="button"  value="message"  onclick="toggleHiddenClass(''message'');" />                               ',&
'      <input  type="button"  value="caption"  onclick="toggleHiddenClass(''caption'');" />                               ',&
'      <input  type="button"  value="header"   onclick="toggleHiddenClass(''header'');" />                                ',&
'  </form>                                                                                                                ',&
'</p>                                                                                                                     ',&
'</head>                                                                                                                  ',&
'<body>                                                                                                                   ',&
'<div id="Container">                                                                                                     ',&
'<div id="Content">                                                                                                       ',&
'']

html_footer=[ CHARACTER(LEN=128) :: &
'</div>',&
'</div>',&
'</body>',&
'</html>',&
'']
end subroutine get_html_header

end program bookkeeper
