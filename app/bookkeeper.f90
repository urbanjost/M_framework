program bookkeeper
! example program that can parse arguments given to optional unit_test command
!  o writing a CSV file to be read into spread sheets, SQLite3, ....
!  o writing a HTML document
!  o writing a NAMELIST group file for post-processing into formats like a TAP (Test Anything Protocol) document
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT, stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT
use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
use M_framework, only : wrt, str
! put something here into namelist and it becomes an argument
character(len=:),allocatable :: type    ; namelist /args/ type
character(len=:),allocatable :: name    ; namelist /args/ name
character(len=:),allocatable :: passed  ; namelist /args/ passed
character(len=:),allocatable :: msg     ; namelist /args/ msg
character(len=25)            :: date    ; namelist /args/ date
integer(kind=int64)          :: clicks  ; namelist /args/ clicks
!integer                      :: level = -1         ; namelist /args/ level
!integer,allocatable          :: flags(:)           ; namelist /args/ flags
character(len=:),allocatable :: html_header(:), html_footer(:)
character(len=*),parameter   :: g='(*(g0,1x))'
character(len=*),parameter   :: g0='(*(g0))'
character(len=1),parameter   :: comma=',', quote='"'
character(len=:),allocatable :: color
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
   type = repeat(' ',4096)
   name = repeat(' ',4096)
   passed = repeat(' ',4096)
   msg = repeat(' ',4096)
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
   case("skipped");color='yellow'
   case("passed") ;color='#9F9'
   case("failed") ;color='red'
   case default   ;color='white'
   end select

   select case(type)

   case("start")
      write(htmlfile,g0)'<table id="unit_test">'
      write(htmlfile,g0)'<caption>',name,str(' -',if=msg.ne.''),' ',msg,'</caption>'
      write(htmlfile,g0)'<tbody>'
      write(htmlfile,g0)'<tr><!-- line -->'
      write(htmlfile,g0)' <th class="odd" style="width:25%;"> name   </th>'
      write(htmlfile,g0)' <th class="odd" style="width:10%;"> passed </th>'
      write(htmlfile,g0)' <th class="odd" style="width:25%;"> date   </th>'
      write(htmlfile,g0)' <th class="odd" style="width:40%;"> msg    </th>'
      write(htmlfile,g0)'</tr>'

   case("check")
      write(csvfile,g0),quote,name,quote,comma,quote,here_and_now(),quote,comma,quote,passed,quote,comma,quote,msg,quote

      write(htmlfile,g0)'<tr><!-- line -->'
      write(htmlfile,g0)' <td class="even" >', name    ,' </td>'
      write(htmlfile,g0)' <td class="even" bgcolor="',color,'">', passed  ,' </td>'
      write(htmlfile,g0)' <td class="even" >', date    ,' </td>'
      write(htmlfile,g0)' <td class="even" >', msg     ,' </td>'
      write(htmlfile,g0)'</tr>'

   case("end")
      if(passed.eq.'skipped')then
         write(csvfile,g0),quote,name,quote,comma,quote,here_and_now(),quote,comma,quote,passed,quote,comma,quote,msg,quote

         write(htmlfile,g0)'<tr><!-- line -->'
         write(htmlfile,g0)' <td class="odd" >', name    ,' </td>'
         write(htmlfile,g0)' <td class="odd" bgcolor="',color,'">', passed  ,' </td>'
         write(htmlfile,g0)' <td class="odd" >', date    ,' </td>'
         write(htmlfile,g0)' <td class="odd" >', msg     ,' </td>'
         write(htmlfile,g0)'</tr>'
      endif
      write(clicksfile,g0),quote,name,quote,comma,           &
                           quote,here_and_now(),quote,comma, &
                           quote,clicks,quote,comma,         &
                           quote,msg,quote

      write(htmlfile,g0)'</table>'

   case("stop")

   end select
contains
subroutine cmdline_()                                ! read arguments from command line as NAMELIST group input
character(len=4096), save :: input(3) = [character(len=4096) :: '&args', '', ' /']
character(len=256) :: message1, message2
integer :: i, j, k, ios, equal_pos

   do i = 1, command_argument_count()
      call get_command_argument(i, input(2))
      do j = 1, len_trim(input(2))                   ! blank out leading - or / so "--name=value" or "/name=value" works
         if (index('/- ', input(2) (j:j)) == 0) exit
         input(2) (j:j) = ' '
      enddo
      input(2) = ' '//adjustl(input(2))
      if (index(input(2), '=') == 0) input(2) = trim(input(2))//'=T' ! if no equal sign add =T
      read (input, nml=args, iostat=ios, iomsg=message1)
      if (ios /= 0) then                             ! assume first failure might be because of missing quotes
         equal_pos = index(input(2), '=')            ! find position of '='
         if (equal_pos /= 0) then
            ! requote and try again
            input(2) = input(2) (:equal_pos)//'"'//input(2) (equal_pos + 1:len_trim(input(2)))//'"'
            read (input, nml=args, iostat=ios, iomsg=message2)
            if (ios /= 0) then
               write(stderr,g) 'BOOKKEEPER:ERROR UNQUOTED:',trim(message1),': when reading ',trim(input(2))
               if(message1.ne.message2) write(stderr,g) 'BOOKKEEPER:ERROR QUOTED  :',trim(message2),': when reading ',trim(input(2))
               write (*, nml=args, delim='quote')
               stop 2
            endif
         else
            write(stderr,g)'ERROR:',trim(message1),': when reading ',trim(input(2))
            write (stderr, nml=args, delim='quote')
            stop 4
         endif
      endif
   enddo
end subroutine cmdline_

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
   write(nmlfile,g0)' that may be read with a simple file and used to'
   write(nmlfile,g0)' generate reports in other formats.'
   write(nmlfile,g0)' '
endif
end subroutine header

subroutine get_html_header()
html_header=[ CHARACTER(LEN=128) :: &
'<html>',&
'<head>',&
'<title>test_results</title>',&
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
'td { border:thin solid #888; }                                                                                           ',&
'li { margin-bottom:0.5em; }                                                                                              ',&
'blockquote { display:block; font-size:90%; font-style:italic; line-height:1.5em; margin:0 0 1.5em; padding:0 2.5em; }    ',&
'pre { background-color:#DDD; font-size:90%; overflow:auto; padding:1em; }                                                ',&
'a,li span { color:#000; }                                                                                                ',&
'a:hover, a.nav:hover, a:hover math { background-color:#000; color:#FFF; }                                                ',&
'#Container { margin:0 10px; text-align:center; }                                                                         ',&
'#Content { border-top:none; margin:auto; padding:0.3em; text-align:left; width:100%; max-width:55em; }                   ',&
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
'table#unit_test caption { font-style: italic ; }                                                                         ',&
'/*                                                                                                                       ',&
'col:nth-child(even) {background: #FFF}                                                                                   ',&
'col:nth-child(odd) {background: #CCC}                                                                                    ',&
'*/                                                                                                                       ',& 
'tr:nth-child(even) {background: #CCC}                                                                                    ',&
'tr:nth-child(odd) {background: #FFF}                                                                                     ',&
'</style>                                                                                                                 ',&
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
