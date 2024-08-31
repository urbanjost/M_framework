!>
!!##NAME
!!     M_framework__journal(3fm) - [M_framework__journal::INTRO] write
!!     program messages to stdout and/or
!!     a log file
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!     use, M_framework__journal , only : journal
!!##DESCRIPTION
!!
!!    For large interactive programs in particular it is useful if all
!!    messages go thru a JOURNAL(3f) call. This makes it easy to
!!    write messages to a log file as well as standard output; to toggle
!!    time prefixes on and off; to turn on and off debug-mode messages;
!!    control output paging and create replayable input journals.
!!
!!    The primary use of JOURNAL(3f) is to create journal files for
!!    interactive programs that
!!
!!        + provide various levels of verbosity on demand, often for
!!          debugging purposes.
!!        + can be replayed even when interactive input was provided
!!        + and/or be used to verify program executions
!!
!!    Typically, you would echo what the user typed to the trail file as-is,
!!    and write output you write to stdout as comments to the trail file so
!!    that the trail file can easily be read back in (by ignoring comments).
!!
!!    Even though there is essentially one procedure (journal(3f) calls
!!    that are more than just a single message have an action specified as
!!    the first parameter. This action might specify to open a log file, to
!!    change the "level" required by messages for them to appear, whether
!!    output it written as a comment or not, and on what files the output
!!    shoud appear. So the interface can be used in a very simple manner
!!    but has more options than is evident at first glance, as detailed in
!!    the man-page for journal(3f).
!!
!!    to stdout is written with WHERE='SC' in the JOURNAL(3f) call.
!!
!!     >      :
!!     >      :
!!     > call journal('O','my_trail_file')  ! open trail file
!!     > ! write output to stdout as-is and as comment to trail file
!!     > call journal(output)
!!     > ! echo message to trail/log file only
!!     > call journal('T',userline)
!!     > ! write to stdout as-is and trail as a comment.
!!     > ! up to twenty scalar values of any intrinsic type are allowed
!!     > call journal('SC','i1=',i1,'i2=',i2,'i3=',i3)
!!     > ! for more complex messages you can build them with non-advancing
!!     > ! I/O journal calls, or build the message with internal writes
!!     > ! into a string and print that.
!!
!!       I=10
!!       R=20.3
!!       ! write to stdout and trail file without advancing I/O
!!       call journal('+SC','I=',i)
!!       ! write to stdout and trail file without advancing I/O
!!       call journal('SC','AND R=',r)
!!
!!    writes to the trail file(s) are ignored unless a trail file was opened,
!!    but output continues to stdout by default.
!!
!!    That is, destinations 'T' and 'C' are ignored unless a trail file
!!    has been requested, allowing journal to be used with programs that
!!    do not generate trails or journals.
!!
!!    Note that with no parameters, the trail file is flushed.
!!
!!##EXAMPLES
!!
!!
!!    The man-page for journal(3f) describes all the options for the
!!    action field WHERE.  In addition to being used to generate a journal,
!!    the routine can be used for producing optional debug messages and
!!    timing information.
!!
!!    Sample program for debug messages:
!!
!!      program demo_journal
!!      !! showing creating debug messages
!!      use M_framework__journal, only : journal
!!      implicit none
!!      !! produces no output because trail is not on
!!      call journal('D','*demo* DEBUG MESSAGE 001 IGNORED')
!!      !! turn on debug messages
!!      call journal('>','debug on')
!!      !! produces output on stdout because debug mode
!!      !! is on but no named trail file
!!      call journal('D','*demo* DEBUG MESSAGE 002 ON STDOUT')
!!      !! open trail file
!!      call journal('O','mytrail.txt')
!!      !! debug messages now go to the trail file only
!!      call journal('D','*demo* DEBUG MESSAGE 003 TO TRAIL')
!!      !! or always to stdout and trail file only if on
!!      call journal('DS','*demo* DEBUG MESSAGE 003 TO TRAIL')
!!      !! close trail file so messages go only to stdout again
!!      call journal('O','')
!!      !! debug on stdout now
!!      call journal('D','*demo* DEBUG MESSAGE 004 TO STDOUT')
!!      !! turn off debug messages
!!      call journal('<','debug off')
!!      !! back to no output from the next message
!!      call journal('D','*demo* DEBUG MESSAGE 005 IGNORED')
!!      end program demo_journal
!!
!!
!!   Sample program for trail messages with optional timing information:
!!
!!      program testit
!!      use M_framework__journal,only : journal
!!      implicit none
!!      call journal('a single string A -should be on S')
!!
!!      ! add time prefix to output
!!      call journal('%','%Y-%M-%DT%h:%m:%s.%x%u:%b')
!!      !
!!      call journal('a single string B -should be on S with prefix')
!!      ! change to CPU time and number of calls prefix
!!      call journal('%','CPU_TIME: %c:CALLS: %C: %b')
!!      !
!!      call journal('a single string B-1 -should be on S with prefix')
!!      call journal('a single string B-2 -should be on S with prefix')
!!      call journal('a single string B-3 -should be on S with prefix')
!!      !  Other useful time formats:
!!      !     %E -- Unix Epoch time
!!      !     %e -- integer value of Unix Epoch time
!!      !     %C -- number of times this format is used
!!      !     %c -- CPU_time(3f) output
!!      !     %S -- seconds since last use of this format
!!      !     %k -- CPU time in seconds from system_clock
!!      call journal('%','') ! turn off time prefix
!!      !
!!      call journal('a single string C -should be on S')
!!      !
!!      call journal('O','aaa.out') ! turn on trail file
!!      call journal('a single string D -should be on SC')
!!      call journal('a single string E -should be on SC')
!!      call journal('a single string F -should be on SC')
!!      call journal('O','') ! turn off trail file
!!      !
!!      call journal('a single string G -should be on S')
!!      call journal('a single string H -should be on S')
!!      call journal('a single string I -should be on S')
!!
!!      ! build one line of output with intrinsic scalar values added
!!      call journal('+sc','APPEND:')
!!      call journal('+sc',' integer',         1234)
!!      call journal('+sc',' and real',        1234.5678)
!!      call journal('+sc',' and double',1234567890.123456d0)
!!      call journal('+sc',' and logical',    .true.)
!!      call journal('sc','')
!!      !
!!      end program testit
!!
!!##AUTHOR
!!     John S. Urban
!!##LICENSE
!!     Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module M_framework__journal
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT, INPUT_UNIT, OUTPUT_UNIT     ! access computing environment
use :: M_framework__msg,                      only : str
implicit none
private

!>
!!##NAME
!!      journal(3f) - [M_framework__journal] provides public message routine, no paging or graphic mode change
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!
!!    subroutine journal([where,],[VALUE(s)])
!!
!!     character(len=*),intent(in) :: where
!!     class(*),optional :: g1,g2,g3,g4,g5,g6,g7,g8,g9,ga,gb,gc,gd,ge,gf,gg,gh,gi,gj
!!
!!   WRITE MESSAGES
!!    basic messages
!!
!!       call journal(where,[VALUE(S)])
!!       call journal(message) # a shortcut for "call journal('sc',message)":
!!   OPEN OR CLOSE TRAIL FILE
!!    trail file
!!
!!       call journal('O',trailfile_name) # open trail file
!!       call journal('O','')             # close trail file
!!   SET OUTPUT TIME PREFIX
!!    set the function display format for timestamps. See the NOW(3f)
!!    procedure for allowable timestamp macros
!!
!!       call journal('%',time_stamp_prefix_specification)
!!
!!   MODES
!!
!!    Turn on/off writing DEBUG messages to trail file
!!
!!       call journal('>','debug on') # turn on debug mode
!!       call journal('<','debug off') # turn off debug mode
!!
!!   ASSIGN STDOUT TO AN ALTERNATE FILE
!!    change stdout to iunit and open filename; or close unit and go back to stdout if filename=''
!!
!!       call journal(iunit,filename)
!!
!!    change stdout to iunit to use a file already open
!!
!!       call journal(iunit)
!!
!!##DESCRIPTION
!!
!!    If a user procedure is used for outputting messages instead of calling
!!    WRITE(3f) it is easy to provide control of when messages are printed
!!    (ie. a "verbose" mode, or "quite" mode), creating files to replay
!!    program execution, duplicating output, ...
!!
!!##OPTIONS
!!   WHERE  indicates where messages are written. A combination of the
!!          following characters can be used...
!!
!!      Usually one of these to write to the standard output files ...
!!
!!      S   write to stdout or iounit set with journal(unit) or
!!          journal(unit,filename).
!!      E   write to stderr
!!
!!      And one of these to write to trail file (ignore if no trail file
!!      defined) ...
!!
!!      C   write to trail file as a comment (if file is open)
!!          Writing output "as a comment" means it is preceded by a pound(#)
!!          character.
!!      T   write to trail file (if file is open)
!!
!!      Usually used by itself
!!
!!      D   write to trail file as a comment with "DEBUG:" prefix in front
!!          of message (if file is open) if debug mode is on. Write to stdout
!!          if no trail file and debug mode is on.
!!
!!      Modifier for S|E|C|T|D specifiers
!!
!!      +   subsequent files are written to with advance='no'. Position is
!!          important. '+sc' does an advance='no' on both files, 's+c'
!!          only does the advance='no' for the trail file.
!!
!!      Mode changing options used by themselves:
!!
!!      >   turn off debug messages
!!      <   turn on debug messages
!!      O   open trail file using value of "message" parameter or close
!!          trail file if no filename or a blank filename.
!!      A   Auxiliary programs that also want to write to the current log file
!!          (a2b, z2a, ...) call this routine to see if there is a trail file
!!          being generated and then add to it so that a program like ush(1f)
!!          can call the auxiliary programs and still just generate one log file,
!!          but if the auxiliary program is used as a stand-alone program no trail
!!          is generated.
!!
!!   VALUES(S)   message to write to stdout, stderr, and the trail file.
!!               a numeric or character value to optionally be appended
!!               to the message. Up to twenty values are allowed. The WHERE
!!               field is required if there is anything other than a single
!!               character string or not values at all.
!!   FILENAME    when WHERE="O" to turn the trail file on or off, the "message"
!!               field becomes the trail filename to open. If blank, writing
!!               to the trail file is turned off.
!!   TIMEFORMAT  when WHERE="%" the message is treated as a time format
!!               specification as described under now(3f).
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_journal
!!    use M_framework__journal, only : journal
!!    !! BASIC USAGE
!!    call journal(&
!!    & 'write to standard output as-is, and trail file as a comment if open')
!!    ! since trail file is not yet open, only stdout will display output
!!    call journal('c','ignored, as trail file is not open')
!!    ! now open trail file "trail"
!!    call journal('o','trail')
!!    call journal('sc','same thing except now trail file is open')
!!    ! only write to trail file if open
!!    call journal('c',&
!!    & 'not ignored, as trail file is open. Written with # suffix')
!!    call journal('t',&
!!    & 'not ignored, as trail file is open. Written as-is')
!!    ! turn off trail file
!!    call journal('o','')
!!    end program demo_journal
!!
!!   Adding intrinsic scalar values to the message:
!!
!!    program test_journal
!!    use M_framework__journal, only: journal
!!    implicit none
!!       call journal('S','This is a test with no optional value')
!!       call journal('S','This is a test with a logical value',.true.)
!!       call journal('S', &
!!         & 'This is a test with a double value',1234567890.123456789d0)
!!       call journal('S', &
!!         & 'This is a test with a real value',1234567890.123456789)
!!       call journal('S','This is a test with an integer value',1234567890)
!!       call journal('STDC','This is a test using STDC',1234567890)
!!       call journal('stdc','This is a test using stdc',1234567890)
!!       call journal('o','journal.txt')  ! open trail file
!!       call journal('S', &
!!         & 1,12.34,56789.111111111d0,.false.,'a bunch of values')
!!       ! the combinations that make sense
!!       call journal('st','stdout and trail')
!!       call journal('s' ,'stdout only')
!!       call journal('t' ,'trail only')
!!       call journal('sc','stdout and trail_comment')
!!       call journal('c' ,'trail_comment only ')
!!       call journal('d' ,'debug only')
!!       call journal('e' ,'stderr only')
!!       call journal('o' ,' ') ! closing trail file
!!    end program test_journal
!!
!!    program testit
!!    ! this is a utility program that calls the module routines. It is
!!    ! typically built using ccall(1).
!!    use M_framework__journal, only : journal
!!    character(len=:),allocatable :: time_stamp_prefix
!!     call journal('s', &
!!     & '------------------------------------------------------------')
!!     call journal('s','SIMPLE WRITES')
!!     call one()
!!     call two()
!!     call journal('sc', &
!!     & 'called ONE() and TWO() but did not generate a log file')
!!     call journal('s', &
!!     & '------------------------------------------------------------')
!!     call journal('s','SIMPLE WRITES WITH LOG FILE')
!!     call journal('o','journal.txt')     ! open trail file
!!     call one()
!!     call two()
!!     call journal('sc', &
!!     & 'called ONE() and TWO() and generated log file journal.txt')
!!     call journal('','journal.txt')      ! close trail file
!!     call journal('s', &
!!     & '------------------------------------------------------------')
!!     call journal('s','SIMPLE WRITES WITH TIMING INFORMATION')
!!     ! change time prefix
!!     time_stamp_prefix='CPU_TIME=%c:CALLS=%C:SINCE=%S:%b'
!!     call journal('%',time_stamp_prefix) ! set a message time prefix
!!     call journal('o','timed.txt')       ! open trail file
!!     call one()
!!     call two()
!!     call journal('sc', &
!!     & 'called ONE() and TWO() and generate log file timed.txt')
!!     call journal('','timed.txt')        ! close trail file
!!     call journal('%','')                ! turn off time prefix
!!     call journal('o','timed.txt')       ! open trail file
!!     call journal('s', &
!!     & '------------------------------------------------------------')
!!
!!    contains
!!
!!       subroutine two()
!!          call journal('Entered subroutine two')
!!          call journal('Exited subroutine two')
!!       end subroutine two
!!
!!       subroutine one()
!!          call journal('Entered subroutine one')
!!          sum=-HUGE(1.0)
!!          do i=1,10000000
!!            sum=sum+sqrt(real(i))
!!          enddo
!!          write(*,*)'SUM=',sum
!!          call journal('Exited subroutine one')
!!       end subroutine one
!!
!!    end program testit
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
public journal

interface journal
   module procedure flush_trail               ! journal()                ! no options
   module procedure write_message_only        ! journal(c)               ! must have one string
   module procedure where_write_message_all   ! journal(where,[g1-g9])   ! must have two strings
   module procedure set_stdout_lun            ! journal(i)               ! first is not a string
end interface journal

! ident_1="@(#) M_framework__journal journal(3fg) provides public message routine no paging or graphic mode change"

! global variables

!integer,parameter,private  :: stdin=INPUT_UNIT
integer,save,private       :: my_stdout=OUTPUT_UNIT
logical,save               :: debug=.false.
integer,save               :: last_int=0

integer,parameter,private :: dp=kind(0.0d0)
real(kind=dp)             :: secday=86400.0d0              ! 24:00:00 hours as seconds

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine where_write_message(where,msg)

! ident_2="@(#) M_framework__journal where_write_message(3fp) basic message routine used for journal files"

character(len=*),intent(in)  :: where
character(len=*),intent(in)  :: msg
!
!  writes error messages and general information text to stdout and the trace file
!     where=*C* write to trail file as a comment (if file is open)
!     where=*D* write to trail file as a comment with DEBUG: prefix in front of message (if file is open and debug mode on)
!     where=*E* write to stderr
!     where=*S* write to stdout or iounit set with journal(unit) or journal(unit,filename)
!     where=*T* write to trail file (if file is open)
!     where=*+* subsequent writes for this call are written with advance='no'

!     where=> turn on debug messages (change mode), which are ones with WHERE='D'
!     where=< turn off debug messages  (change mode), which are ones with WHERE='D'

!     where=O open trail file "msg" or close trail file if blank filename is given
!     where=% set prefix to run thru now(3f) to generate time prefix strings, blank turns off time prefix
!     where=N open new file and assign stdout to the file unless file name is blank; then revert to my_stdout being original stdout.
!
!  the trail file messages are preceded by a pound character (#) by default so they can easily be interpreted as comments
!  if the trace file is subsequently used as input data for a program
!
logical,save                       :: trailopen=.false.
integer,save                       :: itrail
character,save                     :: comment='#'
integer                            :: i
integer                            :: ios
integer                            :: times             ! number of times written to stdout
character(len=3)                   :: adv               ! whether remaining writes from this call use advancing I/O

character(len=:),allocatable,save  :: prefix_template   ! string to run thru now_ex(3f) to make prefix
character(len=:),allocatable       :: prefix            ! the prefix string to add to output
logical,save                       :: prefix_it=.false. ! flag whether time prefix mode is on or not
character(len=4096)                :: mssge
!-----------------------------------------------------------------------------------------------------------------------------------
   adv='yes'
!-----------------------------------------------------------------------------------------------------------------------------------
   if(prefix_it)then
      prefix=now_ex(prefix_template)
   else
      prefix=''
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   times=0
   do i=1,len_trim(where)
      select case(where(i:i))
      case('T','t')
         if(trailopen) then
            write(itrail,'(a)',advance=adv)prefix//trim(msg)
         !!elseif(times == 0)then
         !!   write(my_stdout,'(a)',advance=adv)prefix//trim(msg)
         !!   times=times+1
         endif
      !-----------------------------------------------------------------------------------------------------------------------------
      case('S','s')
         write(my_stdout,'(a)',advance=adv)prefix//trim(msg)
         times=times+1
      !-----------------------------------------------------------------------------------------------------------------------------
      case('E','e')
         write(stderr,'(a)',advance=adv)prefix//trim(msg)
         times=times+1
      !-----------------------------------------------------------------------------------------------------------------------------
      case('+'); adv='no'
      !-----------------------------------------------------------------------------------------------------------------------------
      case('>'); debug=.true.
      !-----------------------------------------------------------------------------------------------------------------------------
      case('<'); debug=.false.
      !-----------------------------------------------------------------------------------------------------------------------------
      case('%')                       ! setting timestamp prefix
         if(msg == '')then            ! if message is blank turn off prefix
            prefix_it=.false.
         else                         ! store message as string to pass to now_ex() on subsequent calls to make prefix
            prefix_template=msg
            prefix_it=.true.
         endif
      !-----------------------------------------------------------------------------------------------------------------------------
      case('N')                                                   ! new name for my_stdout
         if(msg /= ' '.and.msg /= '#N#'.and.msg /= '"#N#"')then   ! if filename not special or blank open new file
            close(unit=last_int,iostat=ios)
            open(unit=last_int,file=adjustl(trim(msg)),iostat=ios)
            if(ios == 0)then
               my_stdout=last_int
            else
               write(*,*)'*journal* error opening redirected output file, ioerr=',ios
               write(*,*)'*journal* msg='//trim(msg)
            endif
         elseif(msg == ' ')then
            close(unit=last_int,iostat=ios)
            my_stdout=6
         endif
      !-----------------------------------------------------------------------------------------------------------------------------
      case('C','c')
         if(trailopen)then
            write(itrail,'(3a)',advance=adv)prefix,comment,trim(msg)
         elseif(times == 0)then
            !! write(my_stdout,'(2a)',advance=adv)prefix,trim(msg)
            !! times=times+1
         endif
      case('D','d')
         if(debug)then
            if(trailopen)then
               write(itrail,'(4a)',advance=adv)prefix,comment,'DEBUG: ',trim(msg)
            elseif(times == 0)then
               write(my_stdout,'(3a)',advance=adv)prefix,'DEBUG:',trim(msg)
               times=times+1
            endif
         endif
      case('F','f')
         flush(unit=itrail,iostat=ios,iomsg=mssge)
         if(ios /= 0)then
            write(*,'(a)') trim(mssge)
         endif
      case('A','a')
         if(msg /= '')then
            open(newunit=itrail,status='unknown',access='sequential',file=adjustl(trim(msg)),&
            & form='formatted',iostat=ios,position='append')
            trailopen=.true.
         endif
      case('O','o')
         if(msg /= '')then
            open(newunit=itrail,status='unknown',access='sequential', file=adjustl(trim(msg)),form='formatted',iostat=ios)
            trailopen=.true.
         else
            if(trailopen)then
               write(itrail,'(4a)',advance=adv)prefix,comment,'closing trail file:',trim(msg)
            endif
            close(unit=itrail,iostat=ios)
            trailopen=.false.
         endif
      case default
         write(my_stdout,'(a)',advance=adv)'*journal* bad WHERE value '//trim(where)//' when msg=['//trim(msg)//']'
      end select
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine where_write_message
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine flush_trail()

! ident_3="@(#) M_framework__journal flush_trail(3fp) flush trail file"

call where_write_message('F','IGNORE THIS STRING')
end subroutine flush_trail
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine set_stdout_lun(iounit)

! ident_4="@(#) M_framework__journal set_stdout_lun(3fp) change I/O logical unit value for standard writes"

integer,intent(in)                   :: iounit
   my_stdout=iounit
end subroutine set_stdout_lun
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    where_write_message_all(3f) - [M_framework__journal] converts any
!!    standard scalar type to a string and calls journal(3f)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine where_write_message_all(where,g0,g1,g2,..,gj,sep)
!!
!!     character(len=*),intent(in)   :: where
!!     class(*),intent(in)           :: g0
!!     class(*),intent(in),optional  :: g1,g2,g3,g4,g5,g6,g7,g8,g9
!!     class(*),intent(in),optional  :: ga,gb,gc,gd,ge,gf,gg,gh,gi,gj
!!     character,intent(in),optional :: sep
!!
!!##DESCRIPTION
!!    where_write_message_all(3f) builds and writes a space-separated string
!!    from up to twenty scalar values.
!!
!!##OPTIONS
!!
!!    where       string designating where to write message, as with journal(3f)
!!    g0          value to print. May
!!                be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION, COMPLEX,
!!                or CHARACTER.
!!    g[1-9a-j]   optional additional values to print the value of after g0.
!!    sep         separator to add between values. Default is a space. Should
!!                always be called with a keyword, as in "sep=VALUE".
!!##RETURNS
!!    where_write_message_all  description to print
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_wm_all
!!    use M_framework__journal, only : where_write_message_all
!!    implicit none
!!    end program program demo_wm_all
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine where_write_message_all(where,g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,ga,gb,gc,gd,ge,gf,gg,gh,gi,gj,sep)
implicit none

! ident_5="@(#) M_framework__journal where_write_message_all(3f) writes a message to a string composed of any standard scalar types"

character(len=*),intent(in)   :: where
class(*),intent(in)           :: g0
class(*),intent(in),optional  :: g1, g2, g3, g4, g5, g6, g7, g8 ,g9
class(*),intent(in),optional  :: ga, gb, gc, gd, ge, gf, gg, gh, gi, gj
character,intent(in),optional :: sep
call where_write_message(where,str(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ga, gb, gc, gd, ge, gf, gg, gh, gi, gj, sep=sep))
end subroutine where_write_message_all
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine write_message_only(message)

! ident_6="@(#) M_framework__journal write_message_only(3fp) calls JOURNAL('sc' message)"

character(len=*),intent(in)          :: message
!-----------------------------------------------------------------------------------------------------------------------------------
   call where_write_message('sc',trim(message))
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine write_message_only
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine d2j(dat,julian,ierr)
!-----------------------------------------------------------------------------------------------------------------------------------
! * Author:    John S. Urban
! * Version:   1.0 2015-12-21
! * Reference: From Wikipedia, the free encyclopedia 2015-12-19
! * There is no year zero
! * Julian Day must be non-negative
! * Julian Day starts at noon; while Civil Calendar date starts at midnight
!-----------------------------------------------------------------------------------------------------------------------------------
! ident_7="@(#) d2j(3f) Converts proleptic Gregorian date array to Julian Day"
integer,intent(in)         :: dat(8)   ! array like returned by DATE_AND_TIME(3f)
real(kind=dp),intent(out)  :: julian   ! Julian Day (non-negative, but may be non-integer)
integer,intent(out)        :: ierr     ! Error return, 0 for successful execution,-1=invalid year,-2=invalid month,-3=invalid day,
                                       ! -4=invalid date (29th Feb, non leap-year)
   integer                 :: year, month, day, utc, hour, minute
   real(kind=dp)           :: second
   integer                 :: A, Y, M, JDN
!-----------------------------------------------------------------------------------------------------------------------------------
   year   = dat(1)                        ! Year
   month  = dat(2)                        ! Month
   day    = dat(3)                        ! Day
   utc    = dat(4)*60                     ! Delta from UTC, convert from minutes to seconds
   hour   = dat(5)                        ! Hour
   minute = dat(6)                        ! Minute
   second = dat(7)-utc+dat(8)/1000.0d0    ! Second   ! correction for time zone and milliseconds
!-----------------------------------------------------------------------------------------------------------------------------------
   julian = -HUGE(99999)                  ! this is the date if an error occurs and IERR is < 0
!-----------------------------------------------------------------------------------------------------------------------------------
   if(year==0 .or. year .lt. -4713) then
      ierr=-1
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
!  You must compute first the number of years (Y) and months (M) since March 1st -4800 (March 1, 4801 BC)
   A=(14-month)/12 ! A will be 1 for January or Febuary, and 0 for other months, with integer truncation
   Y=year+4800-A
   M=month+12*A-3  ! M will be 0 for March and 11 for Febuary
!  All years in the BC era must be converted to astronomical years, so that 1BC is year 0, 2 BC is year "-1", etc.
!  Convert to a negative number, then increment towards zero
!  Staring from a Gregorian calendar date
   JDN=day + (153*M+2)/5 + 365*Y + Y/4 - Y/100 + Y/400 - 32045  !  with integer truncation
!  Finding the Julian date given the JDN (Julian day number) and time of day
   julian=JDN + dble(hour-12)/24.0d0 + dble(minute)/1440.0d0 + second/86400.0d0
!-----------------------------------------------------------------------------------------------------------------------------------
   if(julian.lt.0.d0) then                  ! Julian Day must be non-negative
      ierr=1
   else
      ierr=0
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine d2j
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine d2u(dat,unixtime,ierr)
! ident_8="@(#) d2u(3f) Converts date array to Unix Time (UT starts at 0000 on 1 Jan. 1970)"
integer,intent(in)         :: dat(8)                  ! date time array similar to that returned by DATE_AND_TIME
real(kind=dp),intent(out)  :: unixtime                ! Unix time (seconds)
integer,intent(out)        :: ierr                    ! return 0 on successful, otherwise 1
   real(kind=dp)           :: julian
   real(kind=dp),save      :: julian_at_epoch
   logical,save            :: first=.true.
!-----------------------------------------------------------------------------------------------------------------------------------
if(first) then                                        ! Compute zero of Unix Time in Julian days and save
   call d2j([1970,1,1,0,0,0,0,0],julian_at_epoch,ierr)
   if(ierr.ne.0) return                               ! Error
   first=.false.
endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call d2j(dat,julian,ierr)
   if(ierr.ne.0) return                               ! Error
   unixtime=(julian-julian_at_epoch)*secday
end subroutine d2u
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
FUNCTION d2o(dat) RESULT (ordinal)
! ident_9="@(#) d2o(3f) Converts date-time array to Ordinal day"
INTEGER,INTENT(IN)         :: dat(8)                  ! date time array similar to that returned by DATE_AND_TIME
INTEGER                    :: ordinal                 ! the returned number of days
   REAL(KIND=dp)           :: unixtime                ! Unix time (seconds)
   REAL(KIND=dp)           :: unix_first_day
   INTEGER                 :: ierr                    ! return 0 on successful, otherwise 1 from d2u(3f)
   CALL d2u(dat,unixtime,ierr)                        ! convert date to Unix Epoch Time
   IF(ierr.NE.0)THEN
      write(*,*)'*d2o* bad date array'
      ordinal=-1                                      ! initialize to bad value
   ELSE
      CALL d2u([dat(1),1,1,dat(4),0,0,0,0],unix_first_day,ierr)
      ordinal=int((unixtime-unix_first_day)/secday)+1
   ENDIF
END FUNCTION d2o
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
FUNCTION v2mo(imonth) RESULT(month_name)
! ident_10="@(#) v2mo(3f) returns the month name of a Common month"
CHARACTER(LEN=:),ALLOCATABLE :: month_name                                        ! string containing month name or abbreviation.
INTEGER,INTENT(IN)           :: imonth                                            ! the number of the month(1-12)
CHARACTER(LEN=*),PARAMETER   :: names(12)=[ character(len=9) ::  &
&'January  ', 'February ', 'March    ', 'April    ', 'May      ', 'June     ', &
&'July     ', 'August   ', 'September', 'October  ', 'November ', 'December ']
   SELECT CASE(imonth)
   CASE (1:12);  month_name=TRIM(names(imonth))
   CASE DEFAULT; month_name='UNKNOWN'
   END SELECT
END FUNCTION v2mo
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
FUNCTION now(format)
! ident_11="@(#) JSU 2015-10-24"
CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: format
CHARACTER(LEN=:),ALLOCATABLE         :: now
   INTEGER                           :: values(8)
!-----------------------------------------------------------------------------------------------------------------------------------
   CALL DATE_AND_TIME(VALUES=values)
   IF(PRESENT(format))THEN
      IF(format.NE.' ')THEN
         now=fmtdate(values,format)
      ELSE
         now=fmtdate(values,'%Y-%M-%D %h:%m:%s %z')
      ENDIF
   ELSE
      NOW=fmtdate(values,'%Y-%M-%D %h:%m:%s %z Julian date is %J Epoch time is %E ')
   ENDIF
END FUNCTION now
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
FUNCTION fmtdate(values,format) RESULT (timestring)
! Read the FORMAT string and replace the "%" strings per the following rules:
!-----------------------------------------------------------------------------------------------------------------------------------
! ident_12="@(#) fmtdate(3f) given date array return date as string using format"
CHARACTER(LEN=*),INTENT(IN)     :: format    ! input format string
INTEGER,DIMENSION(8),INTENT(IN) :: values    ! numeric time values as DATE_AND_TIME(3f) intrinsic returns
CHARACTER(LEN=:),ALLOCATABLE    :: timestring
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   INTEGER              :: i10
   LOGICAL              :: keyword   ! flag that previous character was a % character
   CHARACTER(LEN=9)     :: day       ! day of week
   CHARACTER(LEN=1)     :: chara     ! character being looked at in format string
   CHARACTER(LEN=4096)  :: text      ! character array
   INTEGER              :: iout
   INTEGER              :: weekday
   INTEGER              :: ierr
   INTEGER,SAVE         :: called=0
   LOGICAL,SAVE         :: since=.FALSE.
   REAL(KIND=dp)        :: julian
   REAL(KIND=dp)        :: cputime
   INTEGER              :: ii
   REAL(KIND=dp)        :: unixtime
   REAL(KIND=dp),save   :: unixtime_last
   INTEGER              :: systemclock, countrate
   INTEGER              :: iso_year, iso_week, iso_weekday
   CHARACTER(LEN=10)    :: iso_name
   CHARACTER(LEN=2)     :: dayend

   text=' '
!  write string, when encounter a percent character do a substitution
   keyword=.FALSE.
   iout=1
   DO i10=1,LEN(format)
      chara=format(i10:i10)
      IF(chara.eq.'%'.and..not.keyword)THEN
            keyword=.TRUE.
            CYCLE
      ENDIF
      IF(keyword)THEN
         keyword=.FALSE.
         SELECT CASE(chara)
         !=====================================================================================
         CASE('%'); WRITE(text(iout:),'(A1)')chara                        ! literal percent character
         !=====================================================================================
         CASE('b'); WRITE(text(iout:),'(A1)')' '                          ! space character
         !=====================================================================================
         CASE('c'); CALL cpu_time(cputime)                                ! CPU_TIME()
                    WRITE(text(iout:),'(G0)')cputime
         !=====================================================================================
         CASE('C'); called = called + 1                                   ! number of times this routine called
                    WRITE(text(iout:),'(I0)')called
         !=====================================================================================
         CASE('d');                                                       ! the day of the month 1st..31st
                    dayend='  '
                    select case(values(3))
                    case(1,21,31); dayend='st'
                    case(2,22); dayend='nd'
                    case(3,23); dayend='rd'
                    case(4:20,24:30); dayend='th'
                    case default
                    end select
                    WRITE(text(iout:),'(I2,a)')values(3),dayend
         !=====================================================================================
         CASE('D'); WRITE(text(iout:),'(I2.2)')values(3)                  ! the day of the month 1..31
         !=====================================================================================
         CASE('e'); CALL d2u(values,unixtime,ierr)                        ! integer Unix Epoch time in seconds
                    WRITE(text(iout:),'(G0)')int(unixtime)
         !=====================================================================================
         CASE('E'); CALL d2u(values,unixtime,ierr)                        ! Unix Epoch time in seconds
                    WRITE(text(iout:),'(G0)')unixtime
         !=====================================================================================
         CASE('h'); WRITE(text(iout:),'(I2.2)')values(5)                  ! the hour of the day, in the range of 0 to 23
         !=====================================================================================
         CASE('H'); ii=mod(values(5),12)                                  ! hour of day in range 1..12
                    if(ii.eq.0)then
                       ii=12
                    endif
                    WRITE(text(iout:),'(I2.2)')ii
         !=====================================================================================
         CASE('i'); CALL woy(values,iso_year,iso_week,iso_weekday,iso_name) ! ISO week of year
                    WRITE(text(iout:),'(I0)')iso_week
         !=====================================================================================
         CASE('I'); CALL woy(values,iso_year,iso_week,iso_weekday,iso_name) ! iso-8601 Week-numbering year date
                    WRITE(text(iout:),'(a)')iso_name
         !=====================================================================================
         CASE('j'); CALL d2j(values,julian,ierr)                          ! integer Julian date (truncated to integer)
                    WRITE(text(iout:),'(I0)')int(julian)
         !=====================================================================================
         CASE('J'); CALL d2j(values,julian,ierr)                          ! Julian date to milliseconds
                    WRITE(text(iout:),'(I0,".",i3.3)')int(julian),int((julian-int(julian))*1000.0)
         !=====================================================================================
         CASE('k'); call system_clock(count=systemclock,count_rate=countrate)  ! systemclock/countrate
                    WRITE(text(iout:),'(G0)')real(systemclock)/countrate
         !=====================================================================================
         CASE('l'); WRITE(text(iout:),'(A3)')v2mo(values(2))              ! three characters of the name of the month of the year
         !=====================================================================================
         CASE('L'); WRITE(text(iout:),'(A)')v2mo(values(2))               ! name of the month of the year
         !=====================================================================================
         CASE('m'); WRITE(text(iout:),'(I2.2)')values(6)                  ! the minutes of the hour, in the range 0 to 59
         !=====================================================================================
         CASE('M'); WRITE(text(iout:),'(I2.2)')values(2)                  ! month of year (1..12)
         !=====================================================================================
         CASE('N'); if( values(5).ge.12)then                              ! AM||PM
                       WRITE(text(iout:),'("PM")')
                    else
                       WRITE(text(iout:),'("AM")')
                    endif
         !=====================================================================================
         CASE('O'); WRITE(text(iout:),'(I3.3)')d2o(values)                ! Ordinal day of year
         !=====================================================================================
         CASE('s'); WRITE(text(iout:),'(I2.2)')values(7)                  ! the seconds of the minute, in the range 0 to 60
         !=====================================================================================
         CASE('S'); IF(.NOT.since)THEN                                    ! seconds since last called
                       since=.TRUE.
                       CALL d2u(values,unixtime_last,ierr)
                    ENDIF
                    CALL d2u(values,unixtime,ierr)
                    WRITE(text(iout:),'(G0)')unixtime-unixtime_last
                    unixtime_last=unixtime
         !=====================================================================================
         CASE('t'); WRITE(text(iout:),'(A1)')CHAR(9)                      ! tab character
         !=====================================================================================
         CASE('U'); CALL dow(values,weekday,day,ierr)                     ! Return the day of the week, 1..7 Sunday=1
                    WRITE(text(iout:),'(I1)')weekday
         !=====================================================================================
         CASE('u'); CALL dow(values,weekday,day,ierr)                     ! Return the day of the week, 1..7 Monday=1
                    WRITE(text(iout:),'(I1)')mod(weekday+5,7)+1
         !=====================================================================================
         CASE('W'); CALL dow(values,weekday,day,ierr)                     ! Return the name of the day of the week
                    WRITE(text(iout:),'(a)')day
         !=====================================================================================
         CASE('w'); CALL dow(values,weekday,day,ierr)                     ! Return the first three characters of the day of the week
                    WRITE(text(iout:),'(A3)')day(1:3)
         !=====================================================================================
         CASE('x'); WRITE(text(iout:),'(I3.3)')values(8)                  ! the milliseconds of the second, in the range 0 to 999
         !=====================================================================================
         CASE('Y'); WRITE(text(iout:),'(I4.4)')values(1)                  ! the year, including the century (for example, 1990)
         !=====================================================================================
         CASE('Z'); WRITE(text(iout:),'(SP,I5.4)')values(4)               ! time difference with respect to UTC in minutes
         !=====================================================================================
         CASE('z'); WRITE(text(iout:),'(I3.2,":",I2.2)')int(values(4)/60),abs(mod(values(4),60)) ! time from UTC as +-hh:mm
         !=====================================================================================
         CASE DEFAULT
            WRITE(text(iout:),'(A1)')chara
         !=====================================================================================
         END SELECT
         !=====================================================================================
         iout=len_trim(text)+1
      ELSE
         WRITE(text(iout:),'(A1)')chara;iout=iout+1
      ENDIF
   ENDDO
   timestring=trim(text)
END FUNCTION fmtdate
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine dow(values, weekday, day, ierr)
! ident_13="@(#) dow(3f) Return the day of the week"
real(kind=dp)                      :: julian    ! the julian day for which the weekday is required,
integer,intent(in)                 :: values(8) ! date and time array used to get time zone
integer,intent(out),optional       :: weekday   ! The day of the week, 1 = Sunday
character*(*),intent(out),optional :: day       ! The name of the day of the week, e.g. 'Sunday'. Minimum length = 9
integer,intent(out)                :: ierr      ! Error return,0=correct,-1=invalid Julian day,-2=neither day nor weekday specified
   integer                         :: iweekday

   call d2j(values,julian,ierr)                 ! need julian date to calculate day of week for first day of month
   ierr = 0

   if(julian < 0) then
      ierr = -1
      return
   endif

   if(.not.present(day).and. .not.present(weekday)) then
      ierr=-2
      return
   endif

   ! julian day starts at noon so add 1/2 day
   ! add time zone
   iweekday = mod(int((julian+dble(values(4)/60.0d0/24.0d0)+0.5d0)+1.0d0), 7)
   iweekday = iweekday +1

   if(present(day)) then
      select case(iweekday)
      case(1)     ;day = 'Sunday'
      case(2)     ;day = 'Monday'
      case(3)     ;day = 'Tuesday'
      case(4)     ;day = 'Wednesday'
      case(5)     ;day = 'Thursday'
      case(6)     ;day = 'Friday'
      case(7)     ;day = 'Saturday'
      case default;day = 'E-R-R-O-R'
      end select
   endif

   if(present(weekday))then
      weekday=iweekday
   endif

end subroutine dow
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine woy(dat,iso_year,iso_week,iso_weekday,iso_name)
!-----------------------------------------------------------------------------------------------------------------------------------
!  The ISO-8601 date and time standard was issued by the International Organization for Standardization (ISO).
!  It is used (mainly) in government and business for fiscal years, as well as in timekeeping.
!  The system specifies a week year atop the Gregorian calendar by defining a notation for ordinal weeks of the year.
!
!  An ISO week-numbering year (also called ISO year informally) has 52 or 53 full weeks.
!  That is 364 or 371 days instead of the usual 365 or 366 days.
!  The extra week is referred to here as a leap week, although ISO 8601 does not use this term.
!  Weeks start with Monday.
!  The first week of a year is the week that contains the first Thursday of the year (and, hence, always contains 4 January).
!  ISO week year numbering therefore slightly deviates from the Gregorian for some days close to 1 January.
!-----------------------------------------------------------------------------------------------------------------------------------
!CALCULATION:
!  The ISO-8601 week number of any date can be calculated, given its ordinal date (i.e. position within the year)
!  and its day of the week.

!METHOD:
!   Using ISO weekday numbers (running from 1 for Monday to 7 for Sunday),
!   subtract the weekday from the ordinal date, then add 10. Divide the result
!   by 7. Ignore the remainder; the quotient equals the week number. If
!   the week number thus obtained equals 0, it means that the given date
!   belongs to the preceding (week-based) year. If a week number of 53 is
!   obtained, one must check that the date is not actually in week 1 of the
!   following year.
! These two statements are assumed true when correcting the dates around January 1st ...
!   o  The number of weeks in a given year is equal to the corresponding week number of 28 December.
!   o  January 4th is always in the first week.
!
!ISO_NAME:
!  Week date representations are in the format YYYYWww-D.
!  o [YYYY] indicates the ISO week-numbering year which is slightly different from the traditional Gregorian calendar year.
!  o [Www] is the week number prefixed by the letter W, from W01 through W53.
!  o [D] is the weekday number, from 1 through 7, beginning with Monday and ending with Sunday.
!
!  For example, the Gregorian date 31 December 2006 corresponds to the Sunday of the 52nd week of 2006, and is written
!     2006-W52-7 (extended form)
!  or 2006W527 (compact form).
!
!REFERENCE:
!  From Wikipedia, the free encyclopedia 2015-12-19
!AUTHOR:
!  John S. Urban, 2015-12-19
!-----------------------------------------------------------------------------------------------------------------------------------
! ident_14="@(#) woy(3f) Calculate iso-8601 Week-numbering year date yyyy-Www-d"
integer,parameter               :: dp=kind(0.0d0)
integer,intent(in)              :: dat(8)     ! input date array
integer,intent(out)             :: iso_year, iso_week, iso_weekday
character(len=10),intent(out)   :: iso_name
integer                         :: shared_weekday
integer                         :: last_week_this_year
integer                         :: dec28_lastyear(8)   ! December 28th is always in last week
integer                         :: dec28_thisyear(8)   ! December 28th is always in last week
character(len=9)                :: day
integer                         :: ierr
   iso_year=dat(1)                                               ! initially assume the iso_year is the same as the data array year
   iso_week=uncorrected_week_of_year(dat)                        ! this is the week number unless around January 1st
   iso_weekday=shared_weekday                                    ! this is the number of the day of the week assuming Monday=1
   dec28_thisyear=[dat(1),12,28,dat(4),0,0,0,0]                  ! Dec 28th is always in last week; use this to get number of weeks
   last_week_this_year=uncorrected_week_of_year(dec28_thisyear)  ! get the number of the last week of the year (52 or 53)
   ! correct dates around January 1st
   if(iso_week  < 1)then                                         ! if week < 1 then week = lastWeek(year -1)
      dec28_lastyear=[dat(1)-1,12,28,dat(4),0,0,0,0]             ! Dec 28th is always in last week, we want its week number
      iso_week=uncorrected_week_of_year(dec28_lastyear)          ! got the week number for the last week of last year (52 or 53)
      iso_year=dat(1)-1                                          ! our date belongs to last year
   elseif(iso_week >last_week_this_year)then                     ! if week > lastweek(year) then week = 1
      iso_week=iso_week-last_week_this_year                      ! our date belongs to next year
      iso_year=dat(1)+1
   endif

   write(iso_name,'(i4.4,"-W",i2.2,"-",i1)')iso_year,iso_week,iso_weekday ! create ISO string designation for our date

contains
   function uncorrected_week_of_year(datin)
   implicit none
   integer            :: uncorrected_week_of_year
   integer,intent(in) :: datin(8)
      integer         :: ordinal
      call dow(datin,shared_weekday,day,ierr)                 ! formula needs day of week 1..7 where Monday=1
      shared_weekday=mod(shared_weekday+5,7)+1                ! change from Sunday=1 to Monday=1
      ordinal=d2o(datin)                                      ! formula needs ordinal day of year where Jan 1st=1
      uncorrected_week_of_year=(ordinal-shared_weekday+10)/7
   end function uncorrected_week_of_year

end subroutine woy
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function now_ex(format)

! ident_15="@(#) M_time now_ex(3f) use of now(3f) outside of a module"

character(len=*),intent(in),optional :: format
character(len=:),allocatable         :: now_ex
   now_ex=now(format)
end function now_ex
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_framework__journal
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
