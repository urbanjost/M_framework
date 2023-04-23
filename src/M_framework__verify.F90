!>
!!##NAME
!!    M_framework__verify(3fm) - [M_framework__verify::INTRO] a collection of Fortran routines for
!!                    supporting code development by providing error
!!                    processing, debugging procedures and unit testing.
!!                    (LICENSE:PD)
!!##SYNOPSIS
!!
!!
!!  Module procedures
!!
!!    use M_framework__verify, only : unit_check, unit_check_start,     &
!!                                    unit_check_done, unit_check_stop, &
!!                                    unit_check_good, unit_check_bad,  &
!!                                    unit_check_msg, unit_check_mode,  &
!!  Module values
!!
!!    use M_framework__verify, only : unit_check_level
!!
!!##QUOTE
!!    Do not let your victories go to your head, nor let your failures go
!!    to your heart.
!!
!!##DESCRIPTION
!!    The M_framework__verify(3fm) Fortran module provides procedures and variables
!!    useful for providing error processing, debugging capabilities, and
!!    unit testing.
!!
!!     o allows for a user-defined command to be called to collect results or
!!       mail failure alerts, ...
!!     o supports easily composing a message from up to nine scalar
!!       intrinsic values and different message levels
!!     o allows stopping on first failure or continuing
!!     o provides for a non-zero exit code if any tests fail
!!
!!    SET MODES
!!
!!       call unit_check_mode(command,keep_going,level)
!!
!!        command     name of command to execute. Defaults to the name
!!        keep_going  logical variable that can be used to turn off
!!                    program termination on errors.
!!        level       An integer that can be used to specify
!!                    different debug levels
!!
!!    UNIT TESTS
!!    unit_check_start(3f)   start tests of a procedure and optionally call
!!
!!                              command NAME start ...
!!    unit_check(3f)         if expression is false optionally call
!!
!!                              command NAME bad
!!
!!                           and stop program (by default)
!!
!!    unit_check_done(3f)    call
!!
!!                              command NAME good
!!
!!                           if no failures; else call
!!
!!                              command NAME bad
!!   unit_check_stop(3f)     stop program with exit value of 0 if no failures
!!                           else with an exit value of 1
!!
!!    unit_check_good(3f)    call command
!!
!!                              command NAME good
!!
!!    unit_check_bad(3f)     call command
!!
!!                              command NAME bad
!!
!!                           and stop program by default
!!    unit_check_msg(3f)     write message
!!
!!    For unit testing, the existence of a command called "goodbad" is
!!    initially assumed. This is generally a script that makes entries
!!    for each unit in an SQLite data file which is then used to create
!!    CSV and HTML reports on the status of each unit. A sample goodbad(1)
!!    command written in the bash(1) shell and using the sqlite3(1) command
!!    should be included in this distribution as an example.
!!
!!    The flexibility introduced by calling an external script or program
!!    is that The goodbad(1) command can be changed as desired to write CSV
!!    files or simple logs or to notify developers with e-mail as desired.
!!
!!    RELATED FUNCTIONS
!!
!!    The routines in M_framework__verify(3f) are often combined with the M_hashkeys(3fm)
!!    routines and various math and statistical routines to quickly create
!!    unit tests.
!!
!!    Comparisons of real values can be done with a tolerance with
!!    M_Compare_Float_Numbers(3fm), for example.
!!
!!    The intrinsics ANY(3f) and ALL(3f) are particularly useful in calls
!!    to unit_check(3f).
!!
!!##EXAMPLES
!!
!!   Sample program
!!
!!     !!program demo_unit_tests
!!     module M_framework__demo
!!     private
!!     public one !! regular routines
!!     public two !! regular routines
!!     public test_suite_M_demo !! special name for use with test_suite(1bash).
!!     contains
!!
!!     !!  regular routines
!!     subroutine one()
!!     end subroutine one
!!
!!     subroutine two()
!!     end subroutine two
!!
!!     !! unit test
!!     subroutine test_suite_M_demo
!!     use M_framework__verify, only: unit_check_start, unit_check
!!     use M_framework__verify, only: unit_check_good, unit_check_bad, unit_check_done
!!     use M_framework__verify, only: unit_check_msg, unit_check_stop
!!     implicit none
!!     integer :: i, j, k
!!     integer,allocatable :: array(:)
!!     integer :: arr(4)=[21,51,14,45]
!!     integer :: a=21, b=51, c=14, d=45
!!     ! TEST-DRIVEN DEVELOPMENT
!!     ! optional set-up       perform initialization operations common to all tests within a module
!!        i=1
!!        j=2
!!        k=3
!!        array=[10,20,30,40,50,60,70]
!!        call test_one()
!!        call test_two()
!!     ! optional tear-down    perform finalization operations common to all tests within a module
!!     contains
!!
!!     subroutine test_one()
!!     !  register an entry for specified name ("one") in database with status of zero (0)
!!     call unit_check_start('one')
!!
!!     !  if mask test fails, can
!!     !  * produce a SUCCESS: or FAIL: message and stop program
!!     !  * change database status for specified entry to -1 and stop program, else continue
!!     !  * produce a SUCCESS: or FAIL: message and keep going
!!     !  * produce a FAIL: message if test fails but no SUCCESS: message if test passes
!!     call unit_check('one',i > 0,msg='I > 0')
!!
!!     ! using ANY(3f) and ALL(3f)
!!     call unit_check('one',all([i,j,k] > 0),      'testing if everyone greater than zero')
!!     ! display message built of scalars as well
!!     call unit_check('one',all(.not.[i,j,k] == 4),'for set ',i,j,k,'testing if no one is equal to four')
!!
!!     ! for tests that are hard to reduce to a logical test just call unit_check_bad(3f) if fail
!!     if(i+j+k < 1)then
!!        call unit_check_bad('one')
!!     endif
!!
!!     call unit_check_done('one','checks on "one" ended')
!!     end subroutine test_one
!!
!!     subroutine test_two
!!     ! use of all(3f), any(3f), merge(3f) can be useful
!!     ! if you know what these would produce
!!     ! write(*,*)['A','X','X','X','X','B'] == 'B'      ! this would return an array, the last element having the value T, else F
!!     ! write(*,*)all(['A','X','X','X','X','X'] == 'X') ! this would return F
!!     ! write(*,*)any(['A','X','X','X','X','X'] == 'B') ! this would return F
!!     ! write(*,*)any(['A','X','X','X','X','B'] == 'B') ! this would return T
!!     ! write(*,*).not.all(array < 100)
!!     ! write(*,*)all(array < 100)
!!     ! write(*,*)all([a,b,c,d] == [21,51,14,45]) ! compare a list. This would return T
!!     ! write(*,*)all(arr == [21,51,14,45])       ! compare an array. This would return T
!!     ! you know how valuable ANY(3f) and ALL(3f) will be
!!     call unit_check_start('two','check on "two" passed')
!!     call unit_check('two', 1 > 0 .and. abs(10.10000-10.10001) < 0.0001,msg='two looks good')
!!     call unit_check_done('two','checks on "two" ended')
!!     end subroutine test_two
!!
!!     end subroutine test_suite_M_demo
!!
!!     end module M_framework__demo
!!
!!     program demo_M_framework__verify
!!     use M_framework__demo,   only: test_suite_M_demo
!!     use M_framework__verify, only: unit_check_mode
!!        call unit_check_mode(command='',level=0,keep_going=.true.)
!!        call test_suite_M_demo()
!!     end program demo_M_framework__verify
!!
!!   Expected output:
!!
!!     unit_check:       one                  SUCCESS:I > 0
!!     unit_check:       one                  SUCCESS:testing if everyone greater than zero
!!     unit_check:       one                  SUCCESS:for set 1 2 3 testing if no one is equal to four
!!     unit_check_done:  one                  PASSED   GOOD:3  BAD:0
!!
!!     unit_check:       two                  SUCCESS:two looks good
!!     unit_check_done:  two                  PASSED   GOOD:1  BAD:0
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module M_framework__verify
use, intrinsic :: iso_fortran_env,  only : int8, int16, int32, int64 !  1           2           4           8
use, intrinsic :: iso_fortran_env,  only : real32, real64, real128   !  4           8          10
use, intrinsic :: iso_fortran_env,  only : ERROR_UNIT,OUTPUT_UNIT    ! access computing environment
use            :: M_framework__msg, only : str, stderr
implicit none
private

logical,save                 :: G_virgin = .true.

character(len=20),save,public :: G_prefix=''
integer,save,public :: G_unit_check_lun=ERROR_UNIT      ! mutable copy of ERROR_UNIT, but initialized to the unit used for stderr
logical,save,public :: G_debug=.false.

integer,save,public          :: unit_check_level=0      ! a level that can be used to select different debug levels
logical,save                 :: G_keep_going=.false.    ! logical variable that can be used to turn off program termination on errors.
logical,save                 :: G_interactive=.false.
character(len=:),allocatable :: G_command               ! name of command to execute. Defaults to the name
logical,save                 :: G_no_news_is_good_news=.false.    ! flag on whether to display SUCCESS: messages

integer,parameter,public   :: realtime=kind(0.0d0)      ! type for julian days
integer,parameter,public   :: EXIT_SUCCESS=0
integer,parameter,public   :: EXIT_FAILURE=1
real(kind=realtime),save   :: duration=0.0d0
real(kind=realtime),save   :: duration_all=0.0d0
integer,save               :: clicks=0.0d0
integer,save               :: clicks_all=0.0d0

logical,save ::  STOP_G=.true.                    ! global value indicating whether failed unit checks should stop program or not
integer,save :: IPASSED_G=0                       ! counter of successes initialized by unit_check_start(3f)
integer,save :: IFAILED_G=0                       ! counter of failures  initialized by unit_check_start(3f)
integer,save :: IUNTESTED=0                       ! counter of untested  initialized by unit_check_start(3f)
integer,save :: IPASSED_ALL_G=0                   ! counter of successes initialized at program start
integer,save :: IFAILED_ALL_G=0                   ! counter of failures  initialized at program start
integer,save :: IUNTESTED_ALL=0                   ! counter of untested  initialized at program start

public unit_check_start
public unit_check
public unit_check_good
public unit_check_bad
public unit_check_done
public unit_check_stop
public unit_check_msg
public unit_check_mode

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    unit_check_msg(3f) - [M_framework__verify] converts up to nine standard scalar values to a message for unit testing
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function unit_check_msg(name,g1,g2g3,g4,g5,g6,g7,g8,g9)
!!
!!     character(len=*),intent(in)  :: name
!!     class(*),intent(in),optional :: g1,g2,g3,g4,g5,g6,g7,g8,g9
!!##DESCRIPTION
!!    unit_check_msg(3f) builds a string from up to nine scalar values and
!!    prints it to the error long.
!!
!!##OPTIONS
!!    name    name of unit being tested
!!    g[1-9]  optional value to print the value of after the message. May
!!            be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION, COMPLEX,
!!            or CHARACTER.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_unit_check_msg
!!    use M_framework__verify, only : unit_check_start,unit_check_msg,unit_check_done
!!    implicit none
!!
!!    call unit_check_start('myroutine')
!!    call unit_check_msg('myroutine','HUGE(3f) integers',huge(0),'and real',huge(0.0),'and double',huge(0.0d0))
!!    call unit_check_msg('myroutine','real            :',huge(0.0),0.0,12345.6789,tiny(0.0) )
!!    call unit_check_msg('myroutine','doubleprecision :',huge(0.0d0),0.0d0,12345.6789d0,tiny(0.0d0) )
!!    call unit_check_msg('myroutine','complex         :',cmplx(huge(0.0),tiny(0.0)) )
!!    call unit_check_done('myroutine')
!!
!!    end program demo_unit_check_msg
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine unit_check_msg(name,g1, g2, g3, g4, g5, g6, g7, g8, g9)
implicit none

! ident_1="@(#) M_framework__verify unit_check_msg(3f) writes a message to a string composed of any standard scalar types"

character(len=*),intent(in)   :: name
class(*),intent(in),optional  :: g1 ,g2 ,g3 ,g4 ,g5
class(*),intent(in),optional  :: g6 ,g7 ,g8 ,g9

   if(G_virgin)call cmdline_()

   ! write message to standard error
   call stderr(trim(G_prefix)//'check_msg:   '//atleast_(name,20)//' INFO    : '//str(g1,g2,g3,g4,g5,g6,g7,g8,g9))

end subroutine unit_check_msg
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!##NAME
!! unit_check(3f) - [M_framework__verify] report if logical expression is true or false,
!!   and optionally call command "goodbad NAME good|bad" and stop program if false
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine unit_check(name,expression,msg,msg1,msg2,msg3,msg4,msg5,msg6,msg7,msg8,msg9)
!!
!!     character(len=*),intent(in) :: name
!!     logical,intent(in) :: expression
!!     class(*),intent(in),optional :: msg,msg1,msg2,msg3,msg4,msg5,msg6,msg7,msg8,msg9
!!
!!##DESCRIPTION
!!    unit_check(3f) tests the expression and displays a message composed of the generic
!!    intrinsic values msg1 thorough msg9.  Additionally, if the expression  is false
!!
!!    o if unit_check_mode(command) is not blank optionally calls the
!!    specified  shell command
!!
!!         $COMMAND NAME "bad"
!!
!!    o if keep_going = .false. stop the program on a failed test
!!
!!##OPTIONS
!!     NAME             the unit test name passed on to the goodbad(1)
!!                      command
!!     EXPRESSION       the logical expression to evaluate
!!     MSG,MSG1...MSG9  optional message to display when performing test,
!!                      composed of any scalar intrinsics of type INTEGER,
!!                      REAL, DOUBLEPRECISION, COMPLEX, LOGICAL, or
!!                      CHARACTER. A space is placed between each value.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_unit_check
!!    use M_framework__verify, only: unit_check_start, unit_check, unit_check_done
!!    use M_framework__verify, only: unit_check_mode
!!    use M_framework__approx, only: almost
!!
!!    implicit none
!!    integer :: i
!!    integer :: x
!!    integer,allocatable :: arr(:)
!!    real,allocatable :: arr1(:)
!!    real,allocatable :: arr2(:)
!!
!!       call unit_check_mode(keep_going=.true.,debug=.false.,command='')
!!
!!       x=10
!!       arr1=[1.0,10.0,100.0]
!!       arr2=[1.0001,10.001,100.01]
!!       call unit_check_start('myroutine')
!!
!!       call unit_check('myroutine', x > 3 ,'test if big enough')
!!       call unit_check('myroutine', x < 100 ,'test if small enough')
!!
!!       do i=1,size(arr1)
!!          call unit_check('myroutine', almost(arr1(i),arr2(i),3.9,verbose=.true.) )
!!       enddo
!!
!!       arr=[10,20,30]
!!       call unit_check('myroutine', .not.any(arr < 0) ,'fail if any negative values in array ARR')
!!       call unit_check('myroutine', all(arr < 100) ,'fail unless all values are less than 100 in array ARR')
!!
!!       call unit_check_done('myroutine',msg='checks on "myroutine" all passed')
!!
!!    end program demo_unit_check
!!
!!   Sample output (varies with what goodbad(1) command is used):
!!
!!    unit_check:      myroutine        SUCCESS:test if big enough
!!    unit_check:      myroutine        SUCCESS:test if small enough
!!    unit_check:      myroutine        SUCCESS:test if any negative values in array ARR
!!    unit_check:      myroutine        SUCCESS:test if all values less than 100 in array ARR
!!     *almost* for values 1.00000000 1.00010002 agreement of 3.99997139 digits out of requested 3.90000010
!!     *almost* for values 10.0000000 10.0010004 agreement of 3.99986792 digits out of requested 3.90000010
!!     *almost* for values 100.000000 100.010002 agreement of 3.99995065 digits out of requested 3.90000010
!!    unit_check_good: myroutine        PASSED:checks on "myroutine" all passed
!!
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine unit_check(name,logical_expression,msg,msg1,msg2,msg3,msg4,msg5,msg6,msg7,msg8,msg9)

! ident_2="@(#) M_framework__verify unit_check(3f) if .not.expression call 'goodbad NAME bad' & stop program"

character(len=*),intent(in)          :: name
logical,intent(in)                   :: logical_expression
class(*),intent(in),optional         :: msg,msg1,msg2,msg3,msg4,msg5,msg6,msg7,msg8,msg9
character(len=:),allocatable         :: msg_local

   if(G_virgin)call cmdline_()
   msg_local=str(msg,msg1,msg2,msg3,msg4,msg5,msg6,msg7,msg8,msg9)

   if(.not.logical_expression)then
      call stderr(trim(G_prefix)//'check:       '//atleast_(name,20)//' FAILURE : '//trim(msg_local))
      if(G_command /= '')then
         call execute_command_line(G_command//' '//trim(name)//' bad')
      endif
      if(.not.G_keep_going) then
         call stderr(trim(G_prefix)//'check:         STOPPING PROGRAM ON FAILED TEST OF '//trim(name))
         stop 1
      endif
      IFAILED_G=IFAILED_G+1
      IFAILED_ALL_G=IFAILED_ALL_G+1
   else
      if(.not.G_no_news_is_good_news)then
         call stderr(trim(G_prefix)//'check:       '//atleast_(name,20)//' SUCCESS : '//trim(msg_local))
      endif
      IPASSED_G=IPASSED_G+1
      IPASSED_ALL_G=IPASSED_ALL_G+1
   endif

end subroutine unit_check
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    unit_check_start(3f) - [M_framework__verify] call command "goodbad NAME start" and optionally set options
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine unit_check_start(name,options,msg)
!!
!!     character(len=*),intent(in)          :: name
!!     character(len=*),intent(in),optional :: options
!!     character(len=*),intent(in),optional :: msg
!!
!!##DESCRIPTION
!!    unit_check_start(3f) is an initialization command that by default
!!    calls the shell command
!!
!!       goodbad NAME start [options]
!!
!!    The command can be changed by setting the environment variable
!!    UNIT_CHECK_COMMAND or the global module variable COMMAND via "CALL
!!    UNIT_CHECK_MODE(3f)" The environment variable overrides the global
!!    module variable.
!!
!!    By default if a unit_check(3f) logical expression is false or the
!!    unit_check_bad(3f) procedure is called the program will be stopped.
!!
!!    This has the same effect as setting the environment variable
!!    M_framework__verify_STOP to "FALSE" or the global module variable
!!    KEEP_GOING to .FALSE. using "unit_check_mode(3f)". Set the value to
!!    .true. and the program will continue even when tests fail.
!!
!!##OPTIONS
!!       NAME  name of the shell command to execute. If blank, no command
!!             is executed.
!!    OPTIONS  pass additional options to the shell command
!!
!!    MSG      print message
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_unit_check_start
!!     use M_framework__verify, only: unit_check_start, unit_check, unit_check_done
!!     implicit none
!!     integer :: ival
!!     call unit_check_start('myroutine')
!!     ! the goodbad(1) command called here takes many options
!!     ! used to build an SQLite3 entry
!!     call unit_check_start('myroutine_long',' &
!!       & -section        3                    &
!!       & -library        libGPF               &
!!       & -filename       `pwd`/M_framework__verify.FF     &
!!       & -documentation  y                    &
!!       & -prep           y                    &
!!       & -ccall          n                    &
!!       & -archive        GPF.a                &
!!       & ')
!!
!!     ival=10
!!     call unit_check('myroutine', ival > 3 ,   msg='test if big enough')
!!     call unit_check('myroutine', ival < 100 , msg='test if small enough')
!!
!!     call unit_check_done('myroutine',msg='completed checks of "myroutine"')
!!
!!     end program demo_unit_check_start
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine unit_check_start(name,options,msg)

! ident_3="@(#) M_framework__verify unit_check_start(3f) call 'goodbad NAME start'"

character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: options
character(len=*),intent(in),optional :: msg
character(len=4096)                  :: var
logical,save                         :: called=.false.

   if(G_virgin)call cmdline_()
   call get_environment_variable('UNIT_CHECK_COMMAND',var)
   if(var /= '')G_command=var

   if(present(options))then
      if(G_command /= '')then
         call execute_command_line(G_command//' '//trim(name)//' start '//trim(options))
      endif
   else
      if(G_command /= '')then
         call execute_command_line(G_command//' '//trim(name)//' start')
      endif
   endif

   call system_clock(clicks)
   duration=julian()
   if(.not.called)then
      call system_clock(clicks_all)
      duration_all=julian()
      called=.true.
   endif
   if(present(msg))then
     if(msg /= '')then
        call stderr(trim(G_prefix)//'check_start: '//atleast_(name,20)//' START   : '//trim(msg))
     endif
   endif
   call get_environment_variable('M_framework__verify_STOP',var)
   select case(var)
   case('FALSE','false','1','no','NO')
         G_keep_going=.false.
   end select

   IPASSED_G=0
   IFAILED_G=0
   IUNTESTED=0

end subroutine unit_check_start
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!##NAME
!!    unit_check_stop(3f) - [M_framework__verify] call command "goodbad NAME good" or
!!    goodbad NAME bad" depending on whether failures were found
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine unit_check_stop(name,opts,msg)
!!
!!     character(len=*),intent(in) :: name
!!     character(len=*),intent(in),optional :: opts
!!     character(len=*),intent(in),optional :: msg
!!
!!##DESCRIPTION
!!
!!     give a tally of all calls to unit_check(3f)
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_unit_check_stop
!!     use M_framework__verify, only: unit_check_start, unit_check_done, unit_check
!!     use M_framework__verify, only: unit_check_good, unit_check_stop, unit_check_bad
!!     use M_framework__verify, only: unit_check_mode
!!     implicit none
!!     integer :: x
!!
!!     call unit_check_mode(keep_going=.true.,debug=.false.,command='')
!!
!!     x=10
!!     call unit_check_start('myroutine')
!!
!!     call unit_check('myroutine', x > 3 ,'test if big enough')
!!     call unit_check('myroutine', x < 100 ,'test if small enough')
!!
!!     if(x /= 0)then
!!        call unit_check_bad  ('myroutine',msg='x /= 0' )
!!     endif
!!     call unit_check_done  ('myroutine',msg='checks on "myroutine"' )
!!
!!     call unit_check_stop()
!!     end program demo_unit_check_stop
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine unit_check_stop(msg)
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64

! ident_4="@(#) M_framework__verify unit_check_stop(3f) stop program with report on calls to unit_check(3f)"

character(len=*),intent(in),optional :: msg
character(len=:),allocatable         :: msg_local
character(len=4096)                  :: out
character(len=:),allocatable         :: PF
integer(kind=int64)                  :: milliseconds
integer                              :: clicks_now

   if(G_virgin)call cmdline_()

   if(present(msg))then
      msg_local=msg
   else
      msg_local=''
   endif
   call system_clock(clicks_now)
   milliseconds=(julian()-duration_all)*1000
   milliseconds=clicks_now-clicks_all
   PF=merge('PASSED  :','FAILED  :',ifailed_all_G == 0)
   if(PF == 'PASSED  :'.and.ipassed_all_G == 0)then
      PF='UNTESTED:'
   endif
   write(out,'(a,                               &
       & "check_stop:  TALLY                ",a,&
       & " GOOD:",i9,                           &
       & " BAD:",i9,                            &
       & " DURATION:",i14.14                    &
       & )')                                    &
       & trim(G_prefix),                        &
       & PF,                                    &
       & IPASSED_ALL_G,                         &
       & IFAILED_ALL_G,                         &
       & milliseconds
   if(present(msg))then
      call stderr(trim(out)//': '//trim(msg))
   else
      call stderr(out)
   endif
   if(IFAILED_ALL_G == 0)then
      stop EXIT_SUCCESS
   else
      stop EXIT_FAILURE
   endif
end subroutine unit_check_stop
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!##NAME
!!    unit_check_done(3f) - [M_framework__verify] call command "goodbad NAME good" or "goodbad NAME bad" depending on whether failures were found
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine unit_check_done(name,opts,msg)
!!
!!     character(len=*),intent(in) :: name
!!     character(len=*),intent(in),optional :: opts
!!     character(len=*),intent(in),optional :: msg
!!
!!##DESCRIPTION
!!
!!    If there have been no failures the shell command
!!
!!         goodbad NAME good [opts]
!!
!!    is executed, else the command
!!
!!         goodbad NAME bad [opts]
!!
!!    is executed and by default stops the program if their have been
!!    any failures.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_unit_check_done
!!     use M_framework__verify, only: unit_check_start
!!     use M_framework__verify, only: unit_check
!!     use M_framework__verify, only: unit_check_good, unit_check_done, unit_check_bad
!!
!!     implicit none
!!     integer :: x
!!     x=10
!!     call unit_check_start('myroutine')
!!
!!     call unit_check('myroutine', x > 3 ,'test if big enough')
!!     call unit_check('myroutine', x < 100 ,'test if small enough')
!!
!!     if(x /= 0)then
!!        call unit_check_done ('myroutine',msg='checks on "myroutine"' ) ! program execution stopped
!!     endif
!!
!!     end program demo_unit_check_done
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine unit_check_done(name,opts,msg)
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64

! ident_5="@(#) M_framework__verify unit_check_done(3f) call 'goodbad NAME bad'"

character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: opts
character(len=*),intent(in),optional :: msg
character(len=:),allocatable         :: msg_local
character(len=:),allocatable         :: opts_local
character(len=4096)                  :: out
character(len=9)                     :: pf
integer(kind=int64)                  :: milliseconds
integer                              :: clicks_now

   if(G_virgin)call cmdline_()

   if(present(msg))then
      msg_local=msg
   else
      msg_local=''
   endif

   if(present(opts))then
      opts_local=opts
   else
      opts_local=''
   endif

   if(G_command /= '')then                           ! if system command name is not blank call system command
      if(ifailed_g == 0)then
         call execute_command_line(G_command//' '//trim(name)//' bad '//trim(opts))
         if(.not.G_keep_going) stop 1             ! stop program depending on mode
      else
         call execute_command_line(G_command//' '//trim(name)//' good '//trim(opts))
      endif
   endif

   PF=merge('PASSED  :','FAILED  :',ifailed_G == 0)
   if(PF == 'PASSED  :'.and.ipassed_G == 0)then
      PF='UNTESTED:'
   endif
   if(duration /= 0.0d0)then
      call system_clock(clicks_now)
      milliseconds=(julian()-duration)*1000
      milliseconds=clicks_now-clicks
      write(out,'(a,"check_done:  ",a,    &
       & 1x,a,                            &
       & " GOOD:",i9,                     &
       & " BAD:",i9,                      &
       & " DURATION:",i14.14              &
       & )')                              &
       & trim(G_prefix),                  &
       & atleast_(name,20),                &
       & PF,                              &
       & IPASSED_G,                       &
       & IFAILED_G,                       &
       & milliseconds
   else
      write(out,'(a,"check_done:  ",a,1x,a," GOOD:",i0,1x," BAD:",i0)') &
       & trim(G_prefix),atleast_(name,20),PF,IPASSED_G,IFAILED_G
   endif
   if(present(msg))then
      call stderr(trim(out)//': '//trim(msg))
   else
      call stderr(out)
   endif

   IPASSED_G=0
   IFAILED_G=0
   IUNTESTED=0
   duration=0.0d0

end subroutine unit_check_done
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!##NAME
!!    unit_check_bad(3f) - [M_framework__verify] call command "goodbad NAME bad" and stop program
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine unit_check_bad(name,opts,msg)
!!
!!     character(len=*),intent(in) :: name
!!     character(len=*),intent(in),optional :: opts
!!     character(len=*),intent(in),optional :: msg
!!
!!##DESCRIPTION
!!
!!    unit_check_bad(3f) calls the shell command
!!
!!         goodbad NAME bad [opts]
!!
!!    and stops the program. It is just a shortcut for calling
!!         call unit_check(name,.false.)
!!         call unit_check_done(name,opts,msg)
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_unit_check_bad
!!     use M_framework__verify, only: unit_check_start
!!     use M_framework__verify, only: unit_check
!!     use M_framework__verify, only: unit_check_good, unit_check_bad
!!
!!     implicit none
!!     integer :: x
!!     x=10
!!     call unit_check_start('myroutine')
!!
!!     call unit_check('myroutine', x > 3 ,'test if big enough')
!!     call unit_check('myroutine', x < 100 ,'test if small enough')
!!
!!     if(x /= 0)then
!!        call unit_check_bad ('myroutine',msg='checks on "myroutine" failed') ! program execution stopped
!!     endif
!!
!!     end program demo_unit_check_bad
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine unit_check_bad(name,opts,msg)

! ident_6="@(#) M_framework__verify unit_check_bad(3f) call 'goodbad NAME bad'"

character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: opts
character(len=*),intent(in),optional :: msg
character(len=:),allocatable         :: msg_local
character(len=:),allocatable         :: opts_local

   if(G_virgin)call cmdline_()

   if(present(msg))then
      msg_local=msg
   else
      msg_local=''
   endif

   if(present(opts))then
      opts_local=opts
   else
      opts_local=''
   endif

   call unit_check(name,.false.)
   call unit_check_done(name,opts_local,msg_local)

end subroutine unit_check_bad
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!##NAME
!!    unit_check_good(3f) - [M_framework__verify] call command "goodbad NAME good"
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine unit_check_good(name,opts,msg)
!!
!!     character(len=*),intent(in)          :: name
!!     character(len=*),intent(in),optional :: opts
!!     character(len=*),intent(in),optional :: msg
!!
!!##DESCRIPTION
!!    A shortcut for
!!
!!       call unit_check(name,.true.)
!!       call unit_check_done(name,opts,msg)
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_unit_check_good
!!     use M_framework__verify, only: unit_check_start, unit_check_done
!!     use M_framework__verify, only: unit_check
!!     use M_framework__verify, only: unit_check_good, unit_check_bad
!!
!!     implicit none
!!     integer :: x
!!     x=10
!!     call unit_check_start('myroutine')
!!
!!     call unit_check('myroutine', x > 3 ,'test if big enough')
!!     call unit_check('myroutine', x < 100 ,'test if small enough')
!!
!!     call unit_check_good('myroutine',msg='checks on "myroutine" ')
!!
!!     end program demo_unit_check_good
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine unit_check_good(name,opts,msg)

! ident_7="@(#) M_framework__verify unit_check_good(3f) call 'goodbad NAME good'"

character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: opts
character(len=*),intent(in),optional :: msg
character(len=:),allocatable         :: msg_local
character(len=:),allocatable         :: opts_local

   if(G_virgin)call cmdline_()

   if(present(msg))then
      msg_local=msg
   else
      msg_local=''
   endif

   if(present(opts))then
      opts_local=opts
   else
      opts_local=''
   endif

   call unit_check(name,.true.,msg=msg_local)
   call unit_check_done(name,opts_local)

end subroutine unit_check_good
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function atleast_(line,length) result(strout)

! ident_8="@(#) M_framework__verify atleast_(3fp) return string padded to at least specified length"

character(len=*),intent(in)  ::  line
integer,intent(in)           ::  length
character(len=max(length,len(trim(line)))) ::  strout
   strout=line
end function atleast_
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function julian()
! REFERENCE: From Wikipedia, the free encyclopedia 2015-12-19

! ident_9="@(#) M_framework__verify julian(3f) Converts proleptic Gregorian DAT date-time array to Julian Date"

real(kind=realtime)              :: julian   ! Julian Date (non-negative, but may be non-integer)
integer                          :: dat(8)   ! array like returned by DATE_AND_TIME(3f)
integer                          :: year, month, day, utc, hour, minute
real(kind=realtime)              :: second
integer                          :: A, Y, M, JDN

   call date_and_time(values=dat)
   year   = dat(1)                        ! Year
   month  = dat(2)                        ! Month
   day    = dat(3)                        ! Day
   utc    = dat(4)*60                     ! Delta from UTC, convert from minutes to seconds
   hour   = dat(5)                        ! Hour
   minute = dat(6)                        ! Minute
   second = dat(7)-utc+dat(8)/1000.0d0    ! Second   ! correction for time zone and milliseconds

!  You must compute first the number of years (Y) and months (M) since March 1st -4800 (March 1, 4801 BC)
   A=(14-month)/12 ! A will be 1 for January or February, and 0 for other months, with integer truncation
   Y=year+4800-A
   M=month+12*A-3  ! M will be 0 for March and 11 for February
!  All years in the BC era must be converted to astronomical years, so that 1BC is year 0, 2 BC is year "-1", etc.
!  Convert to a negative number, then increment towards zero
!  Staring from a Gregorian calendar date
   JDN=day + (153*M+2)/5 + 365*Y + Y/4 - Y/100 + Y/400 - 32045  !  with integer truncation
!  Finding the Julian Calendar date given the JDN (Julian day number) and time of day
   julian=JDN + dble(hour-12)/24.0d0 + dble(minute)/1440.0d0 + second/86400.0d0
end function julian
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine cmdline_()
use,intrinsic :: iso_fortran_env, only: compiler_version, compiler_options
use,intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT, stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT
! define arguments and their default values
logical :: interactive                    ; namelist /args/ interactive
logical :: help                           ; namelist /args/ help

namelist /args/ G_debug                   ! debug mode
namelist /args/ unit_check_level          ! a level that can be used to select different debug levels
namelist /args/ G_keep_going              ! logical variable that can be used to turn off program termination on errors.
namelist /args/ G_command                 ! name of command to execute. Defaults to " ".
namelist /args/ G_no_news_is_good_news

!    Report the beginning and end of execution of each test case or suite
!    Only run cases or collections whose description contains the given string
!    Don't colorize the output

character(len=256),save :: input(3)=[character(len=256) :: '&args','','/']
character(len=256) :: message1, message2
integer :: i, j, ios, equal_pos

if(G_virgin)then
   G_virgin=.false.
   G_interactive=.false.
   G_command=repeat(' ',4096)
else
   G_command = G_command//repeat(' ',4096)
endif

! read arguments from command line
do i=1,command_argument_count()
   call get_command_argument(i,input(2))
   do j=1,len_trim(input(2)) ! blank out leading - or / so "--name=value" or "/name=value" works
      if(index('/- ',input(2)(j:j)).eq.0)exit
      input(2)(j:j)=' '
   enddo
   read(input,nml=args,iostat=ios,iomsg=message1)
   ! assume first failure might be because of missing quotes
   if(ios.ne.0)then
      equal_pos=index(input(2),'=')
      if(equal_pos.ne.0)then
         ! requote and try again
         input(2)=input(2)(:equal_pos)//'"'//input(2)(equal_pos+1:len_trim(input(2)))//'"'
         read(input,nml=args,iostat=ios,iomsg=message2)
         if(ios.ne.0)then
            write(*,*)'ERROR UNQUOTED:'//trim(message1)//': when reading '//trim(input(2))
            write(*,*)'ERROR QUOTED  :'//trim(message2)//': when reading '//trim(input(2))
            stop 2
         endif
      else
         write(*,*)'ERROR:'//trim(message1)//': when reading '//trim(input(2))
         stop 4
      endif
   endif
enddo

G_command=trim(G_command)
write(*,nml=args,delim='quote')

end subroutine cmdline_
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine unit_check_mode(debug, prefix, keep_going, level, command, no_news_is_good_news)
logical,optional, intent(in)          :: debug
logical,optional, intent(in)          :: keep_going
logical,optional, intent(in)          :: no_news_is_good_news
character(len=*),optional, intent(in) :: command
character(len=*),optional, intent(in) :: prefix
integer,optional, intent(in)          :: level

!integer,optional,intent(in) :: unit_check_lun

   if(G_virgin)then
      G_command=repeat(' ',4096)
      G_debug=.false.
      G_prefix=''
      G_keep_going=.true.
      unit_check_level=0
      G_virgin=.false.
      G_interactive=.false.
   endif

   if (present(command))              G_command=command
   if (present(debug))                G_debug=debug
   if (present(prefix))               G_prefix=prefix

   if (present(keep_going))           G_keep_going=keep_going
   if (present(level))                unit_check_level=level
   if (present(no_news_is_good_news)) G_no_news_is_good_news=no_news_is_good_news

   G_virgin=.false.

!integer,parameter,public   :: realtime=kind(0.0d0)    ! type for julian days
!integer,parameter,public   :: EXIT_SUCCESS=0
!integer,parameter,public   :: EXIT_FAILURE=1
end subroutine unit_check_mode
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_framework__verify
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
