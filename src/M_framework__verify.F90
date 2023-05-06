!>
!!##NAME
!!    M_framework__verify(3f) - [M_framework__verify::INTRO] unit test framework
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!
!!  Module procedures
!!
!!    use M_framework__verify, only : unit_test, unit_test_start,    &
!!                                    unit_test_end, unit_test_stop, &
!!                                    unit_test_msg, unit_test_mode, &
!!                                    unit_test_system
!!  Module values
!!
!!    use M_framework__verify, only : unit_test_level, unit_test_flags
!!
!!##QUOTE
!!    Do not let your victories go to your head, nor let your failures go
!!    to your heart.
!!
!!##DESCRIPTION
!!    The M_framework(3f) module is a collection of Fortran routines for
!!    supporting code development by providing logging, error processing,
!!    debugging, comparison and unit testing procedures.
!!
!!    The M_framework__verify(3f) module specifically brings together a
!!    few procedures for creating unit testing. It ...
!!
!!     o allows for a user-defined command to be called to collect results or
!!       produce mail alerts, or other custom bookkeeping operations.
!!
!!     o supports easily composing a message from up to twenty scalar
!!       intrinsic values and different strings
!!
!!     o allows stopping on failure or continuing
!!
!!     o provides for a non-zero exit code if any tests fail
!!
!!     o is designed for with integration with the fpm
!!      (Fortran Package Manager) "test" subcommand.
!!
!!    If default modes need changed it can be done via the unit_test_mode(3f)
!!    procedure or as command line options.
!!
!!    messages by default are writting to stderr, but may be written to any
!!    list of LUNs of preassigned or open files.
!!
!!    SET MODES
!!
!!    Some of the most common options are
!!
!!       call unit_test_mode(command,keep_going,level,luns=[K,L,M,N,...])
!!
!!        keep_going  logical variable that can be used to turn on or off
!!                    program termination on errors.
!!        luns        array of Fortran LUNs to write messages to
!!        level       An integer that can be used to specify
!!                    different debug levels
!!        command     name of optional command to execute for a start,
!!                    check, or finish.
!!
!!  PROCEDURES
!!
!!  The unit test procedures are
!!
!!       unit_test_start()     start tests of a procedure
!!       unit_test()           report if expression is false or true
!!                              and if .false. stop program when keep_going=.false.
!!       unit_test_end()       ends test of a procedure
!!       unit_test_msg()       write message
!!       unit_test_stop()      stop program with exit value of 0 if no failures
!!                              else with an exit value of 1
!!       unit_test_system()    execute system command, recursively if requested.
!!
!!    For custom unit testing reports, a command can be given that will be
!!    passed information on the command line in NAMELIST format.
!!
!!    This command is generally a script that makes entries for each unit,
!!    perhaps in an SQLite data file for example.
!!
!!    It might also send an email if a test fails in batch environments.
!!
!!    It could also convert the information to CSV for use in spreadsheets,
!!    or generate an HTML report, for example.
!!
!!    A sample command written in the bash(1) shell and using the sqlite3(1)
!!    command should be included in this distribution as an example.
!!
!!    The flexibility introduced by calling an external script or program
!!    is that the command can be changed as desired without changing the
!!    test programs.
!!
!!    RELATED FUNCTIONS
!!
!!    The routines in M_framework__verify(3f) are often combined with other
!!    small modules such as Comparisons of real values can be done with a
!!    tolerance with M_Compare_Float_Numbers(3f) and M_framework__approx(3f).
!!    M_hashkeys(3f) routines and various math and statistical routines can
!!    be helpful to quickly create unit tests.
!!
!!    The intrinsics ANY(3f) and ALL(3f) are particularly useful in calls
!!    to unit_test(3f).
!!
!!##EXAMPLES
!!
!!   Sample program
!!
!!     !! program demo_M_framework__verify
!!     module M_framework__demo
!!     private
!!     public one ! some regular routine
!!     public two ! some regular routine
!!     contains
!!
!!     subroutine one(array)
!!     integer,intent(out),allocatable :: array(:)
!!        array=[21,51,14,45]
!!     end subroutine one
!!
!!     subroutine two(array)
!!     integer,intent(inout),allocatable :: array(:)
!!        array=2*array
!!     end subroutine two
!!
!!     end module M_framework__demo
!!
!!     program demo_M_framework__verify
!!     use M_framework, only: unit_test_start, unit_test,   &
!!         & unit_test_end, unit_test_msg, unit_test_stop, &
!!         & unit_test_system, unit_test_mode
!!     use M_framework__demo,   only: one, two
!!     ! set-up
!!     call unit_test_mode(command='',flags=[0],keep_going=.true.)
!!     ! call a test procedure for each routine to test
!!        call test_one()
!!        call test_two()
!!     ! tear-down
!!     call unit_test_stop()
!!     contains
!!
!!     subroutine test_one()
!!     integer,allocatable :: results(:)
!!     integer,parameter   :: expected(*)=[21,51,14,45]
!!     call unit_test_start('one')
!!     call one(results)
!!     call unit_test('one',all(expected>0), &
!!        & 'testing if everyone greater than zero')
!!     call unit_test('one',all(expected==results), &
!!        & 'testing if all values are expected')
!!     call unit_test_end('one','checks on "one" ended')
!!     end subroutine test_one
!!
!!     subroutine test_two
!!     integer,allocatable :: results(:)
!!     integer,parameter   :: expected(*)=[2,20,200]
!!     results=[1,10,100]
!!     call two(results)
!!     call unit_test_start('two','check procedure "two" ')
!!     call unit_test('two', all(expected == results) .and. &
!!        & all(expected > 0) .and. maxval(expected) <201,msg='long expression')
!!     call unit_test_end('two','checks on "two" ended')
!!     end subroutine test_two
!!
!!     end program demo_M_framework__verify
!!
!!   Expected output:
!!
!!    check_start: one   START   :
!!    check:       one   SUCCESS : testing if everyone greater than zero
!!    check:       one   SUCCESS : testing if all values are expected
!!    check_done:  one   PASSED  : GOOD:  2 BAD:  0 DURATION:00000001
!!    check_start: two   START   :
!!    check:       two   SUCCESS : long expression
!!    check_done:  two   PASSED  : GOOD:   1 BAD:  0 DURATION:00000000
!!    check_stop:  TALLY PASSED  : GOOD:    3 BAD:  0 DURATION:00000001
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module M_framework__verify
use,intrinsic :: iso_fortran_env,  only : int8, int16, int32, int64, real32, real64, real128
use,intrinsic :: iso_fortran_env,  only : stderr=>ERROR_UNIT, stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT
use           :: M_framework__msg, only : str, wrt
implicit none
private

type called
   logical :: preset_globals=.true.
   logical :: cmdline=.true.
   logical :: unit_test_mode=.true.
end type called

type(called),save             :: G_virgin

integer,save,allocatable      :: G_luns(:)              ! output units
logical,save                  :: G_debug=.false.
logical,save                  :: G_verbose=.false.

integer,save,public             :: unit_test_level=0      ! a value that can be used to select different debug levels
integer,save,public,allocatable :: unit_test_flags(:)     ! an array of flags that can be used to select different options
logical,save                    :: G_keep_going=.false.    ! can be used to turn on program termination on errors.
logical,save                    :: G_interactive=.false.
character(len=:),allocatable    :: G_command                         ! name of command to execute. Defaults to the name
logical,save                    :: G_no_news_is_good_news=.false.    ! flag on whether to display SUCCESS: messages
logical,save                    :: G_cmdline=.true.                  ! flag whether to parse command line for arguments or not

integer,parameter,public   :: realtime=kind(0.0d0)      ! type for julian days
integer,parameter,public   :: EXIT_SUCCESS=0
integer,parameter,public   :: EXIT_FAILURE=1
real(kind=realtime),save   :: duration=0.0d0
real(kind=realtime),save   :: duration_all=0.0d0
integer(kind=int64),save   :: clicks=0_int64
integer(kind=int64),save   :: clicks_all=0_int64

logical,save :: STOP_G=.true.            ! global value indicating whether failed unit checks should stop program or not
integer,save :: IPASSED_G=0              ! counter of successes initialized by unit_test_start(3f)
integer,save :: IFAILED_G=0              ! counter of failures  initialized by unit_test_start(3f)
integer,save :: IUNTESTED=0              ! counter of untested  initialized by unit_test_start(3f)
integer,save :: IPASSED_ALL_G=0          ! counter of successes initialized at program start
integer,save :: IFAILED_ALL_G=0          ! counter of failures  initialized at program start
integer,save :: IUNTESTED_ALL=0          ! counter of untested  initialized at program start

public unit_test_mode    ! optionally set some non-default modes

public unit_test_start   ! start testing a procedure
  public unit_test       ! report results of a test
public unit_test_end     ! end  testing a procedure

public unit_test_stop    ! produce tally of all procedures tested and end program

public unit_test_msg     ! maybe write some message
public unit_test_system  ! usually used for recursive calls when testing program termination status

type :: force_kwargs_hack ! force keywords, using @awvwgk method
end type force_kwargs_hack
! so then any argument that comes afer "force_kwargs" is a compile time error
! if not done with a keyword unless someone "breaks" it by passing something
! of this type:
!    type(force_kwargs_hack), optional, intent(in) :: force_kwargs

!===================================================
! for backward compatibility 2023-04-30. Otherwise, ignore these
public unit_test_good  ! report results of a test
public unit_test_bad   ! report results of a test

interface  unit_test_done;    module  procedure  unit_test_end;    end  interface  unit_test_done;    public  unit_test_done
interface  unit_check_start;  module  procedure  unit_test_start;  end  interface  unit_check_start;  public  unit_check_start
interface  unit_check;        module  procedure  unit_test;        end  interface  unit_check;        public  unit_check
interface  unit_check_done;   module  procedure  unit_test_end;    end  interface  unit_check_done;   public  unit_check_done
interface  unit_check_mode;   module  procedure  unit_test_mode;   end  interface  unit_check_mode;   public  unit_check_mode
interface  unit_check_stop;   module  procedure  unit_test_stop;   end  interface  unit_check_stop;   public  unit_check_stop
interface  unit_check_msg;    module  procedure  unit_test_msg;    end  interface  unit_check_msg;    public  unit_check_msg
interface  unit_check_good;   module  procedure  unit_test_good;   end  interface  unit_check_good;   public  unit_check_good
interface  unit_check_bad;    module  procedure  unit_test_bad;    end  interface  unit_check_bad;    public  unit_check_bad

public unit_check_level
integer :: unit_check_level
equivalence (unit_test_level, unit_check_level)
!===================================================
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    unit_test_msg(3f) - [M_framework__verify] converts up to twenty
!!    standard scalar values to a message for unit testing
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function unit_test_msg(name, msg, &
!!    & g1,g2,g3,g4,g5,g6,g7,g8,g9,ga,gb,gc,gd,ge,gf,gg,gh,gi,gj,if)
!!
!!     character(len=*),intent(in)  :: name
!!     class(*),intent(in),optional :: msg, &
!!     & g1,g2,g3,g4,g5,g6,g7,g8,g9,ga,gb,gc,gd,ge,gf,gg,gh,gi,gj
!!##DESCRIPTION
!!    unit_test_msg(3f) builds a string from up to twenty scalar values and
!!    prints it to the error log.
!!
!!##OPTIONS
!!    name        name of unit being tested
!!    msg,g[1-j]  optional values to print the value of. May
!!                be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION,
!!                COMPLEX, or CHARACTER.
!!    if          expression must be true or message is not output.
!!                Must be specified by keyword as "if=expression".
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_unit_test_msg
!!    use M_framework__verify, only : unit_test_start,unit_test_msg, &
!!            & unit_test_end
!!    implicit none
!!
!!    call unit_test_start('myroutine')
!!    call unit_test_msg('myroutine','HUGE(3f) integers', &
!!            & huge(0),'and real',huge(0.0),'and double',huge(0.0d0))
!!    call unit_test_msg('myroutine','real            :', &
!!            & huge(0.0),0.0,12345.6789,tiny(0.0) )
!!    call unit_test_msg('myroutine','doubleprecision :', &
!!            & huge(0.0d0),0.0d0,12345.6789d0,tiny(0.0d0) )
!!    call unit_test_msg('myroutine','complex         :', &
!!            & cmplx(huge(0.0),tiny(0.0)) )
!!    call unit_test_end('myroutine')
!!
!!    end program demo_unit_test_msg
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine unit_test_msg(name,msg, g1, g2, g3, g4, g5, g6, g7, g8, g9 ,ga, gb, gc, gd, ge, gf, gg, gh, gi, gj,force_kwargs,if)
implicit none

! ident_1="@(#) M_framework__verify unit_test_msg(3f) writes a message to a string composed of any standard scalar types"

character(len=*),intent(in)   :: name
class(*),intent(in),optional  :: msg, g1 ,g2 ,g3 ,g4 ,g5, g6 ,g7 ,g8 ,g9, ga ,gb ,gc ,gd ,ge, gf ,gg ,gh ,gi, gj
type(force_kwargs_hack), optional, intent(in) :: force_kwargs
logical,intent(in),optional   :: if
character(len=:),allocatable  :: msg_all
logical                       :: if_local
if(present(if))then
   if_local=if
else
   if_local=.true.
endif
   if(G_virgin%cmdline) call cmdline_()

   if(if_local)then
      msg_all=str(msg,g1,g2,g3,g4,g5,g6,g7,g8,g9,ga,gb,gc,gd,ge,gf,gg,gh,gi,gj)

      if(.not.G_no_news_is_good_news)then
         ! write message to standard error
         call wrt(G_luns,'check_msg:   '//atleast_(name,20)//' INFO    : '// msg_all)
      endif
      if(G_command /= '') call run(G_command//' type="message"  name="'//trim(name)//'" msg="'//ndq(msg_all)//'"')
   endif

end subroutine unit_test_msg
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!##NAME
!!    unit_test(3f) - [M_framework__verify] report if logical expression is
!!    true or false, optionally call command and/or stop program.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine unit_test(name,expression,msg,g1,g2,g3,g4,g5,g6,g7,g8,g9,&
!!    & ga,gb,gc,gd,ge,gf,gg,gh,gi,gj,only_on_fail)
!!
!!     character(len=*),intent(in) :: name
!!     logical,intent(in) :: expression
!!     class(*),intent(in),optional :: msg,g1,g2,g3,g4,g5,g6,g7,g8,g9,&
!!     & ga,gb,gc,gd,ge,gf,gg,gh,gi,gj
!!     logical,intent(in),optional :: only_on_fail
!!
!!##DESCRIPTION
!!    unit_test(3f) tests the expression and displays a message composed
!!    of the generic intrinsic values msg, and g1 thorough gj. Additionally,
!!    if the expression is false
!!
!!    o if unit_test_mode(command) is not blank calls the
!!    specified shell command
!!
!!       $COMMAND name="NAME" type="check" passed="passed|failed" ...
!!       msg="all messages"
!!
!!    o if keep_going = .false. stop the program on a failed test
!!
!!##OPTIONS
!!     NAME          the unit test name
!!     EXPRESSION    the logical expression to evaluate
!!     msg,g1...gj   optional message to display when performing test,
!!                   composed of any scalar intrinsics of type INTEGER,
!!                   REAL, DOUBLEPRECISION, COMPLEX, LOGICAL, or
!!                   CHARACTER. A space is placed between each value.
!!     wordy         If .false. The message MSG is only displayed if the expression
!!                   is .false. . Must be used as a keyword. Default is .true. .
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!       program demo_unit_test
!!       use M_framework__verify, only: &
!!          & unit_test_mode,     &
!!          & unit_test_start,    &
!!          & unit_test,          &
!!          & unit_test_end,      &
!!          & unit_test_stop
!!       use M_framework__approx, only: almost
!!
!!       implicit none
!!       integer :: i
!!       integer :: x
!!       integer,allocatable :: arr(:)
!!       real,allocatable :: arr1(:)
!!       real,allocatable :: arr2(:)
!!
!!          call unit_test_mode(keep_going=.true.,debug=.false.,command='')
!!
!!          x=10
!!          arr1=[1.0,10.0,100.0]
!!          arr2=[1.0001,10.001,100.01]
!!          call unit_test_start('myroutine')
!!
!!          call unit_test('myroutine', x > 3 ,' if big enough')
!!          call unit_test('myroutine', x < 100 ,' if small enough')
!!
!!          do i=1,size(arr1)
!!             call unit_test('myroutine', &
!!             & almost(arr1(i),arr2(i),3.9,verbose=.true.) )
!!          enddo
!!
!!          arr=[10,20,30]
!!          call unit_test('myroutine', .not.any(arr < 0) , &
!!          & 'fail if any negative values in array ARR')
!!          call unit_test('myroutine', all(arr < 100) , &
!!          & 'fail unless all values are less than 100 in array ARR')
!!
!!          call unit_test_end('myroutine', &
!!          & msg='checks on "myroutine" all passed')
!!
!!          call unit_test_stop()
!!
!!       end program demo_unit_test
!!
!! Results:
!!
!!  Sample output (varies with what optional command or modes is used):
!!
!!      >check_start: myroutine            START   :
!!      >check:       myroutine            SUCCESS :  if big enough
!!      >check:       myroutine            SUCCESS :  if small enough
!!      >*almost* for values 1.00000000 1.00010002 agreement of 3.99997139 ...
!!      >digits out of requested 3.90000010
!!      >check:       myroutine            SUCCESS :
!!      >*almost* for values 10.0000000 10.0010004 agreement of 3.99986792 ...
!!      >digits out of requested 3.90000010
!!      >check:       myroutine            SUCCESS :
!!      >*almost* for values 100.000000 100.010002 agreement of 3.99995065 ...
!!      >digits out of requested 3.90000010
!!      >check:       myroutine            SUCCESS :
!!      >check:       myroutine            SUCCESS : fail if any negative ...
!!      >values in array ARR
!!      >check:       myroutine            SUCCESS : fail unless all values ...
!!      >are less than 100 in array ARR
!!      >check_done:  myroutine            PASSED  : GOOD:        7 BAD: ...
!!      >0 DURATION:00000000294709: checks on "myroutine" all passed
!!      >check_stop:  TALLY                PASSED  : GOOD:        7 BAD: ...
!!      >0 DURATION:00000000267059
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine unit_test(name,logical_expression,msg,g1,g2,g3,g4,g5,g6,g7,g8,g9,ga,gb,gc,gd,ge,gf,gg,gh,gi,gj,force_kwargs,wordy)

! ident_2="@(#) M_framework__verify unit_test(3f) assert if expression is .true. or .false. and optionally call command or stop on .false."

character(len=*),intent(in)          :: name
logical,intent(in)                   :: logical_expression
class(*),intent(in),optional         :: msg,g1,g2,g3,g4,g5,g6,g7,g8,g9,ga,gb,gc,gd,ge,gf,gg,gh,gi,gj
type(force_kwargs_hack),optional,intent(in) :: force_kwargs
logical,intent(in),optional          :: wordy
character(len=:),allocatable         :: msg_all
logical                              :: wordy_local

   if(present(wordy))then
      wordy_local=wordy
   else
      wordy_local=.true.
   endif

   if(G_virgin%cmdline) call cmdline_()

   msg_all=str(msg,g1,g2,g3,g4,g5,g6,g7,g8,g9,ga,gb,gc,gd,ge,gf,gg,gh,gi,gj)

   if(.not.logical_expression)then
      call wrt(G_luns,'check:       '//atleast_(name,20)//' FAILURE : '//msg_all)
      if(G_command /= '') call run(G_command//'type="check" name="'//trim(name)//'" passed="failed" msg="'//ndq(msg_all)//'"')
      if(.not.G_keep_going) then
         call wrt(G_luns,'check:         STOPPING PROGRAM ON FAILED TEST OF '//trim(name))
         stop 1
      endif
      IFAILED_G=IFAILED_G+1
      IFAILED_ALL_G=IFAILED_ALL_G+1
   else
      if(.not.G_no_news_is_good_news)then
         if(wordy_local)call wrt(G_luns,'check:       '//atleast_(name,20)//' SUCCESS : '//msg_all)
      endif
      if(G_command /= '') call run(G_command//' type="check" name="'//trim(name)//'" passed="passed" msg="'//ndq(msg_all)//'"')
      IPASSED_G=IPASSED_G+1
      IPASSED_ALL_G=IPASSED_ALL_G+1
   endif

end subroutine unit_test
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    unit_test_start(3f) - [M_framework__verify] reset counters
!!    and start a new test block
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine unit_test_start(name,msg,opts)
!!
!!     character(len=*),intent(in)          :: name
!!     character(len=*),intent(in),optional :: msg
!!     character(len=*),intent(in),optional :: opts
!!
!!##DESCRIPTION
!!    unit_test_start(3f) is an initialization procedure for starting a
!!    new procedure test.
!!
!!##OPTIONS
!!    NAME   name of the procedure to test
!!    MSG    message to print
!!    OPTS   pass additional options to the optional shell command
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_unit_test_start
!!     use M_framework__verify, only: unit_test_start, unit_test, &
!!      & unit_test_end, unit_test_mode
!!     implicit none
!!     integer :: ival
!!     call unit_test_mode(command='goodbad')
!!     call unit_test_start('myroutine')
!!     ! the example goodbad(1) command called here takes many options
!!     ! used to build an SQLite3 entry
!!     call unit_test_start('myroutine_long',opts='     &
!!       & --section        3                            &
!!       & --library        libGPF                       &
!!       & --filename       `pwd`/M_framework__verify.FF &
!!       & --documentation  y                            &
!!       & --prep           y                            &
!!       & --ccall          n                            &
!!       & --archive        GPF.a                        &
!!       & ')
!!
!!     ival=10
!!
!!     call unit_test('myroutine', ival > 3 ,   msg=' if big enough')
!!     call unit_test('myroutine', ival < 100 , msg=' if small enough')
!!
!!     call unit_test_end('myroutine',msg='completed checks of "myroutine"')
!!
!!     end program demo_unit_test_start
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine unit_test_start(name,msg,opts)

! ident_3="@(#) M_framework__verify unit_test_start(3f) start testing procedure "name""

character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: msg
character(len=*),intent(in),optional :: opts
character(len=:),allocatable         :: msg_local
logical,save                         :: called=.false.
   if(present(msg))then
      msg_local=trim(msg)
   else
      msg_local=''
   endif

   if(G_virgin%cmdline) call cmdline_()

   if(present(opts))then
      if(G_command /= '') call run(G_command//' type="start" name="'//trim(name)//'" msg="'//ndq(msg_local)//'" '//opts)
   else
      if(G_command /= '') call run(G_command//' type="start" name="'//trim(name)//'" msg="'//ndq(msg_local)//'"')
   endif

   call system_clock(clicks)
   duration=julian()
   if(.not.called)then
      call system_clock(clicks_all)
      duration_all=julian()
      called=.true.
   endif

   if(.not.G_no_news_is_good_news)then
      call wrt(G_luns,'check_start: '//atleast_(name,20)//' START   : '//msg_local)
   endif

   IPASSED_G=0
   IFAILED_G=0
   IUNTESTED=0

end subroutine unit_test_start
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!##NAME
!!    unit_test_stop(3f) - [M_framework__verify] report tally of all checks
!!    and stop program
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine unit_test_stop(name,msg,opts)
!!
!!     character(len=*),intent(in) :: name
!!     character(len=*),intent(in),optional :: msg
!!     character(len=*),intent(in),optional :: opts
!!
!!##DESCRIPTION
!!
!!    give a tally of all calls to unit_test(3f). If a command is set
!!    call it appending OPTS to the end of the command.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_unit_test_stop
!!     use M_framework__verify, only: unit_test_start, unit_test_end, &
!!     & unit_test, unit_test_stop, unit_test_mode
!!     implicit none
!!     integer :: x
!!
!!     call unit_test_mode(keep_going=.true.,debug=.false.,command='')
!!
!!     x=10
!!     call unit_test_start('myroutine')
!!
!!     call unit_test('myroutine', x > 3 ,' if big enough')
!!     call unit_test('myroutine', x < 100 ,' if small enough')
!!
!!     if(x /= 0)then
!!        call unit_test ('myroutine',.false.,msg='x /= 0' )
!!     endif
!!
!!     call unit_test_end  ('myroutine',msg='checks on "myroutine"' )
!!
!!     call unit_test_stop()
!!
!!     end program demo_unit_test_stop
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine unit_test_stop(msg)
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64

! ident_4="@(#) M_framework__verify unit_test_stop(3f) stop program with report on calls to unit_test(3f)"

character(len=*),intent(in),optional :: msg
character(len=:),allocatable         :: msg_local
character(len=4096)                  :: out
character(len=:),allocatable         :: PF
integer(kind=int64)                  :: milliseconds
integer(kind=int64)                  :: clicks_now

   if(G_virgin%cmdline) call cmdline_()

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
   write(out,'( &
       & "check_stop:  TALLY                ",a,&
       & " GOOD:",i9,                           &
       & " BAD:",i9,                            &
       & " DURATION:",i14.14                    &
       & )')                                    &
       & PF,                                    &
       & IPASSED_ALL_G,                         &
       & IFAILED_ALL_G,                         &
       & milliseconds
   if(present(msg))then
           if(.not.G_no_news_is_good_news.or.(IFAILED_ALL_G+IPASSED_ALL_G.eq.0).or.IFAILED_ALL_G.ne.0) &
                   & call wrt(G_luns,trim(out)//': '//trim(msg))
   else
           if(.not.G_no_news_is_good_news.or.(IFAILED_ALL_G+IPASSED_ALL_G.eq.0).or.IFAILED_ALL_G.ne.0) &
                   & call wrt(G_luns,out)
   endif

   if(PF=='UNTESTED')then
      if(G_command /= '') &
      & call run( str(G_command,' type="stop" passed="skipped" clicks=0 msg="',ndq(msg_local),'"',sep='') )
      stop ! EXIT_SUCCESS
   elseif(IFAILED_ALL_G == 0)then
      if(G_command /= '') &
      & call run( str(G_command,' type="stop" passed="passed" clicks=',milliseconds,' msg="',ndq(msg_local),'"',sep='') )
      stop ! EXIT_SUCCESS
   else
      if(G_command /= '') &
      & call run( str(G_command,' type="stop" passed="failed" clicks=',milliseconds,' msg="',ndq(msg_local),'"',sep='') )
      stop EXIT_FAILURE
   endif
end subroutine unit_test_stop
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!##NAME
!! unit_test_end(3f) - [M_framework__verify] end test of procedure started
!! by unit_test_start(3f)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine unit_test_end(name,msg,opts)
!!
!!     character(len=*),intent(in) :: name
!!     character(len=*),intent(in),optional :: msg
!!     character(len=*),intent(in),optional :: opts
!!
!!##DESCRIPTION
!!
!!    A message is shown including the duration of the tests
!!    If there have been no failures the optional shell command
!!
!!        $COMMAND name="name" type="end" passed="passed|failed|skipped" ...
!!        clicks=NNNN msg="message" opts
!!
!!    is executed
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_unit_test_end
!!     use M_framework__verify, only: unit_test_start
!!     use M_framework__verify, only: unit_test
!!     use M_framework__verify, only: unit_test_end
!!     implicit none
!!     integer :: x
!!     x=10
!!     call unit_test_start('myroutine')
!!
!!     call unit_test('myroutine', x > 3 ,'if big enough')
!!     call unit_test('myroutine', x < 100 ,'if small enough')
!!
!!     ! program execution stopped
!!     call unit_test_end ('myroutine',msg='checks on "myroutine"' )
!!
!!     end program demo_unit_test_end
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine unit_test_end(name,msg,opts)
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64

! ident_5="@(#) M_framework__verify unit_test_end(3f) end checking procedure "name""

character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: msg
character(len=*),intent(in),optional :: opts
character(len=:),allocatable         :: msg_local
character(len=:),allocatable         :: opts_local
character(len=4096)                  :: out
character(len=9)                     :: pf
integer(kind=int64)                  :: milliseconds
integer(kind=int64)                  :: clicks_now

   if(G_virgin%cmdline) call cmdline_()

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

   PF=merge('PASSED  :','FAILED  :',ifailed_G == 0)
   if(PF == 'PASSED  :'.and.ipassed_G == 0)then
      PF='UNTESTED:'
   endif
   if(duration /= 0.0d0)then
      call system_clock(clicks_now)
      milliseconds=(julian()-duration)*1000
      milliseconds=clicks_now-clicks
      write(out,'("check_done:  ",a,      &
       & 1x,a,                            &
       & " GOOD:",i9,                     &
       & " BAD:",i9,                      &
       & " DURATION:",i14.14              &
       & )')                              &
       & atleast_(name,20),               &
       & PF,                              &
       & IPASSED_G,                       &
       & IFAILED_G,                       &
       & milliseconds
   else
      milliseconds=0
      write(out,'("check_done:  ",a,1x,a," GOOD:",i0,1x," BAD:",i0)') &
       & atleast_(name,20),PF,IPASSED_G,IFAILED_G
   endif
   if(present(msg))then
      if(.not.G_no_news_is_good_news.or.(IFAILED_G+IPASSED_G.eq.0).or.IFAILED_G.ne.0) &
       & call wrt(G_luns,trim(out)//': '//trim(msg))
   else
      if(.not.G_no_news_is_good_news.or.(IFAILED_G+IPASSED_G.eq.0).or.IFAILED_G.ne.0) &
       & call wrt(G_luns,out)
   endif

   if(G_command /= '')then                           ! if system command name is not blank call system command
      if(ipassed_G+ifailed_G == 0)then
         call run(str(G_command,' type="end" name="',name,'" passed="skipped" clicks=0',' msg="',ndq(msg),'" ',sep='') )

      elseif(ifailed_G == 0)then
         call run(str(G_command,' type="end" name="',name,'" passed="passed" clicks=',milliseconds,' msg="',ndq(msg),'" ',sep='') )
      else
         call run(str(G_command,' type="end" name="',name,'" passed="failed" clicks=',milliseconds,' msg="',ndq(msg),'" ',sep='') )
      endif
   endif

   if(ifailed_G == 0)then
      if(.not.G_keep_going) stop 1             ! stop program depending on mode
   endif

   IPASSED_G=0
   IFAILED_G=0
   IUNTESTED=0
   duration=0.0d0

end subroutine unit_test_end
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!##NAME
!!    unit_test_bad(3f) - [M_framework__verify] call command "goodbad NAME
!!    bad" and stop program
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine unit_test_bad(name,msg,opts)
!!
!!     character(len=*),intent(in) :: name
!!     character(len=*),intent(in),optional :: msg
!!     character(len=*),intent(in),optional :: opts
!!
!!##DESCRIPTION
!!
!!    unit_test_bad(3f) calls the shell command
!!
!!         goodbad NAME bad [opts]
!!
!!    and stops the program. It is just a shortcut for calling
!!         call unit_test(name,.false.)
!!         call unit_test_end(name,msg,opts)
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_unit_test_bad
!!     use M_framework__verify, only: unit_test_start, unit_test
!!     use M_framework__verify, only: unit_test_end, unit_test_stop
!!     use M_framework__verify, only: unit_test_bad
!!
!!     implicit none
!!     integer :: x
!!     x=10
!!     call unit_test_start('myroutine')
!!
!!     call unit_test('myroutine', x > 3 ,'if big enough')
!!     call unit_test('myroutine', x < 100 ,'if small enough')
!!
!!     if(x /= 0)then
!!       call unit_test_bad ('myroutine',msg='checks on "myroutine" failed')
!!       ! program execution stopped
!!     endif
!!     call unit_test_end ('myroutine')
!!     call unit_test_stop ('myroutine')
!!
!!     end program demo_unit_test_bad
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine unit_test_bad(name,opts,msg)

! ident_6="@(#) M_framework__verify unit_test_bad(3f) call 'goodbad NAME bad'"

character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: opts
character(len=*),intent(in),optional :: msg
character(len=:),allocatable         :: msg_local
character(len=:),allocatable         :: opts_local

   if(G_virgin%cmdline) call cmdline_()

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

   call unit_test(name,.false.)
   call unit_test_end(name,opts_local,msg_local)

end subroutine unit_test_bad
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!
!!##NAME
!!    unit_test_good(3f) - [M_framework__verify] call command "goodbad
!!    NAME good"
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine unit_test_good(name,msg,opts)
!!
!!     character(len=*),intent(in)          :: name
!!     character(len=*),intent(in),optional :: opts
!!     character(len=*),intent(in),optional :: msg
!!
!!##DESCRIPTION
!!    A shortcut for
!!
!!       call unit_test(name,.true.)
!!       call unit_test_end(name,opts,msg)
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_unit_test_good
!!     use M_framework__verify, only: unit_test_start, unit_test_end
!!     use M_framework__verify, only: unit_test, unit_test_good
!!
!!     implicit none
!!     integer :: x
!!     x=10
!!     call unit_test_start('myroutine')
!!
!!     call unit_test('myroutine', x > 3 ,'if big enough')
!!     call unit_test('myroutine', x < 100 ,'if small enough')
!!
!!     call unit_test_good('myroutine',msg='checks on "myroutine" ')
!!
!!     end program demo_unit_test_good
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine unit_test_good(name,msg,opts)

! ident_7="@(#) M_framework__verify unit_test_good(3f) call 'goodbad NAME good'"

character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: opts
character(len=*),intent(in),optional :: msg
character(len=:),allocatable         :: msg_local
character(len=:),allocatable         :: opts_local

   if(G_virgin%cmdline) call cmdline_()

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

   call unit_test(name,.true.,msg=msg_local)
   call unit_test_end(name,opts_local)

end subroutine unit_test_good
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function atleast_(line,length) result(strout)

! ident_8="@(#) M_framework__verify atleast_(3f) return string padded to at least specified length"

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
subroutine preset_globals()
integer :: i
integer :: iostat
   G_virgin%preset_globals=.false.
   G_command=repeat(' ',4096)
   G_cmdline=.true.
   G_debug=.false.
   G_keep_going=.true.
   unit_test_level=0
   unit_test_flags=[integer :: ]
   G_luns=[integer :: ]
   G_interactive=.false.
   G_command=repeat(' ',4096)
   open(unit=999,status='scratch',iostat=iostat)
end subroutine preset_globals
!===================================================================================================================================
subroutine cmdline_()
use, intrinsic :: iso_fortran_env, only: compiler_version, compiler_options
! define arguments and their default values
! use naming convention of global variables to make parsing easier
logical             :: G_help = .false.
integer,allocatable :: G_flags(:)
integer             :: G_level
integer,allocatable :: G_luns_hold(:)

! NOTE: assume all names in namelist start with G_ or unit_test
namelist /args/ G_interactive
namelist /args/ G_help
namelist /args/ G_verbose
namelist /args/ G_level
namelist /args/ G_debug                   ! debug mode
namelist /args/ G_flags                   ! values that can be used to select different tests or any conditional integer test
namelist /args/ G_keep_going              ! logical variable that can be used to turn off program termination on errors.
namelist /args/ G_command                 ! name of command to execute. Defaults to " ".
namelist /args/ G_no_news_is_good_news
namelist /args/ G_luns

!    Report the beginning and end of execution of each test case or suite
!    Only run cases or collections whose description contains the given string
!    Don't colorize the output

character(len=256), save :: input(3) = [character(len=256) :: '&args', '', ' /']
character(len=256) :: message1, message2
integer :: i, j, k, ios, equal_pos

   if (G_virgin%preset_globals) then
      call preset_globals()
   endif

   if (G_virgin%cmdline) then
      G_virgin%cmdline = .false.
      ! read arguments from command line
      G_level=-1
      G_luns_hold=G_luns
      G_luns = [ -1, -1, -1, -1, -1, -1, -1, -1, -1, -1]
      G_flags = [(-1, i=1, 1000)]
      do i = 1, command_argument_count()
         call get_command_argument(i, input(2))
         do j = 1, len_trim(input(2)) ! blank out leading - or / so "--name=value" or "/name=value" works
            if (index('/- ', input(2) (j:j))  ==  0) exit
            input(2) (j:j) = ' '
         enddo
         ! if variable name does not start with "unit_test" add "G_" prefix so can use a nice name
         ! on command line, on unit_test_mode, and public variables
         input(2) = adjustl(input(2))
         if (index(input(2), 'unit_test_')  ==  1) then
            input(2) = ' '//adjustl(input(2))
         else
            input(2) = ' G_'//adjustl(input(2))
         endif
         ! if no equal sign add =T
         if (index(input(2), '=')  ==  0) then
            input(2) = trim(input(2))//'=T'
         endif

         read (input, nml=args, iostat=ios, iomsg=message1)
         ! assume first failure might be because of missing quotes

         if (ios  /=  0) then
            equal_pos = index(input(2), '=')        ! find position of '='
            G_luns = pack(G_luns, G_luns  /=  -1)     ! if G_luns is all negative at this point set it to [stderr]
            if (size(G_luns)  ==  0) G_luns = G_luns_hold
            if (size(G_luns)  ==  0) G_luns = [stderr]
            if (equal_pos  /=  0) then
               ! requote and try again
               input(2) = input(2) (:equal_pos)//'"'//input(2) (equal_pos + 1:len_trim(input(2)))//'"'
               read (input, nml=args, iostat=ios, iomsg=message2)
               if (ios  /=  0) then
                  call wrt(G_luns, 'ERROR UNQUOTED:'//trim(message1)//': when reading '//trim(input(2)))
                  call wrt(G_luns, 'ERROR QUOTED  :'//trim(message2)//': when reading '//trim(input(2)))
                  G_command=trim(G_command)
                  if(G_level == -1) G_level=unit_test_level
                  do k = 1, size(G_luns)
                     write (G_luns(k), nml=args, delim='quote')
                  enddo
                  stop 2
               endif
            else
               call wrt(G_luns, 'ERROR:'//trim(message1)//': when reading '//trim(input(2)))
               G_command=trim(G_command)
               if(G_level == -1) G_level=unit_test_level
               do k = 1, size(G_luns)
                  write (G_luns(k), nml=args, delim='quote')
               enddo
               stop 4
            endif
         endif
      enddo

      G_luns = pack(G_luns, G_luns  /=  -1)       ! make sure G_luns has at least one file
      if (size(G_luns)  ==  0) G_luns = G_luns_hold
      if (size(G_luns)  ==  0) G_luns = [stderr]
      G_command = trim(G_command)

      G_flags = pack(G_flags, G_flags  >=  0)
      if(G_verbose) G_flags=[G_flags,9997,9998,9999] ! turn on these flags if verbose mode
      if (size(G_flags)  /=  0) unit_test_flags = G_flags
      if(G_level /= -1) unit_test_level = G_level

      ! some pre-defined level numbers
      if (any(unit_test_flags  ==  9997)) then
         call wrt(G_luns, 'This file was compiled by ', compiler_version())
      endif

      if (any(unit_test_flags  ==  9998)) then
         call wrt(G_luns, ' using the options ', compiler_options())
      endif

      if (any(unit_test_flags  ==  9999)) then
         do i = 1, size(G_luns)
            write (G_luns(i), nml=args, delim='quote')
         enddo
      endif
   endif

   if (G_help) then
      write (*, '(g0)') [character(len=80) :: &
      '                                                                                ', &
      'unit test command line options:                                                 ', &
      '--level=N                   user-requested debug level. Sets "unit_test_level". ', &
      '--keep_going=F              turn on program termination on test failure         ', &
      '--no_news_is_good_news      do not display "SUCCESS" lines                      ', &
      '--flags=L,M,N,...           set value for user to set different test flags      ', &
      '                               values >= 9990 are reserved                      ', &
      '                                  * 9997 compiler version                       ', &
      '                                  * 9998 compiler options                       ', &
      '                                  * 9999 command line options NAMELIST group    ', &
      '--command="system_command"  program to call after each test                     ', &
      '--luns=L,M,N,...    list of unit numbers to write to, assumed opened by program ', &
      '                                  * 6   typically stdout                        ', &
      '                                  * 0   typically stderr                        ', &
      '                                  * 999 scratch file deleted when program ends  ', &
      '--help                      display this text and exit                          ', &
      '--verbose                   verbose mode                                        ', &
      '--interactive                                                                   ', &
      '--debug                                                                         ', &
      '                                                                                ', &
      'Note flags sets unit_test_flags(:) and level sets unit_test_level and           ', &
      'unit_test_flags(:) are public members of M_framework__verify.                   ', &
      'EXAMPLES                                                                        ', &
      ' sample commands:                                                               ', &
      '  fpm test                                                                      ', &
      '  fpm test -- luns=6 # write to stdout instead of stderr                        ', &
      '  fpm test ''regression*''  # run tests beginning with specified string         ', &
      '                                                                                ', &
      '  # run a test called "crash" with gdb(1)                                       ', &
      '  fpm test --target crash --runner "gdb -ex run --quiet"                        ', &
      '                                                                                ', &
      '  #To run all the tests in the gdb(1) debugger (you can enter                   ', &
      '  #"q" after each test has run; or enter gdb commands at the prompt):           ', &
      '  fpm test --target ''*'' --verbose \                                             ', &
      '     --runner ''gdb -ex "list, 0" -ex run --quiet --args'' \                      ', &
      '     -- flags=9997,9998,9999 luns=6 level=3                                     ', &
      ' ']
      G_help=.false.
      stop 1
   endif

end subroutine cmdline_
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    unit_test_mode(3f) - [M_framework__verify] set testing modes
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!
!!      subroutine unit_test_mode( keep_going, flags, luns, command, &
!!      no_news_is_good_news, interactive, CMDLINE, debug)
!!
!!      logical,intent(in) :: keep_going, no_news_is_good_news, interactive,debug
!!      integer,intent(in),allocatable :: luns(:), flags(:)
!!      character(len=*),intent(in) :: command
!!##DESCRIPTION
!!    unit_test_mode(3f) changes testing mode defaults
!!
!!##OPTIONS
!!    keep_going   keep running if a test fails. Default to TRUE
!!    flags        a list of integer values that can be accessed from
!!                 M_framework__verify as unit_test_flags(:) for use in
!!                 selecting various tests conditionally
!!    luns         list of Fortran units to unit test messages to. Defaults
!!                 to the the value of ERROR_UNIT from the intrinsic module
!!                 ISO_FORTRAN_ENV (ie. defaults to "stderr"). It is
!!                 Assumed the units have been opened by the program.
!!    command      ...
!!    no_news_is_good_news   Determines if "SUCCESS" messages are printed.
!!    interactive  ...
!!    cmdline      ...
!!    debug        ...
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_unit_test_mode
!!    use M_framework
!!    implicit none
!!
!!    call unit_test_mode(keep_going=.false.,luns=[6], &
!!            & no_news_is_good_news=.true.)
!!
!!    end program demo_unit_test_mode
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine unit_test_mode(debug, keep_going, level, flags, command, no_news_is_good_news,cmdline,interactive,luns)
logical,optional, intent(in)          :: debug
logical,optional, intent(in)          :: keep_going     ! logical variable that can be used to turn off program termination on errors.
logical,optional, intent(in)          :: cmdline        ! flag whether to parse command line for arguments or not
logical,optional, intent(in)          :: interactive
logical,optional, intent(in)          :: no_news_is_good_news   ! flag on whether to display SUCCESS: messages
character(len=*),optional, intent(in) :: command                ! name of command to execute. Defaults to the name
integer,optional, intent(in)          :: flags(:)       ! an  array that can be used to select different options
integer,optional, intent(in)          :: level          ! an  integer that can be used to select different debug levels
integer,optional, intent(in)          :: luns(:)        ! logical unit number to write output to

   if (G_virgin%preset_globals) then
      call preset_globals()
   endif

   if (present(luns))                 G_luns=luns
   if (present(command))              G_command=command
   if (present(debug))                G_debug=debug
   if (present(cmdline))              G_cmdline=cmdline
   if (present(interactive))          G_interactive=interactive
   if (present(keep_going))           G_keep_going=keep_going
   if (present(flags))                unit_test_flags=flags
   if (present(level))                unit_test_level=level
   if (present(no_news_is_good_news)) G_no_news_is_good_news=no_news_is_good_news

!integer,parameter,public   :: realtime=kind(0.0d0)    ! type for julian days
!integer,parameter,public   :: EXIT_SUCCESS=0
!integer,parameter,public   :: EXIT_FAILURE=1
end subroutine unit_test_mode
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    unit_test_system(3f) - [M_framework__verify] return status from
!!    system command
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function unit_test_system(cmd,verbose)
!!
!!     character(len=*),intent(in)  :: cmd
!!     logical,intent(in),optional  :: verbose
!!##DESCRIPTION
!!    unit_test_system(3f) executes a system command and returns the
!!    exit status of the command.
!!
!!##OPTIONS
!!    command    system command to execute. If it starts with "* " the
!!               asterisk is replaced by the name of the current command.
!!               If it starts with "** " the asterisks are replaced by
!!               the current command including arguments.
!!
!!    verbose    if .true. the executed command is echoed to output. The
!!               default is .false.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_unit_test_system
!!    use M_framework__verify, only: &
!!       unit_test_start,  &
!!       unit_test,        &
!!       unit_test_system, &
!!       unit_test_end
!!    implicit none
!!    if (command_argument_count()  ==  0) then
!!       call unit_test_start('myroutine')
!!       call unit_test('false', unit_test_system('false') == 0, 'check false')
!!       call unit_test('true', unit_test_system('true') == 0, 'check true')
!!       call unit_test('notthere', unit_test_system('notthere') == 0, &
!!       & 'check notthere')
!!       call unit_test('*',&
!!       & unit_test_system('* and options', verbose=.true.) == 0, 'check "*"')
!!       call unit_test_end('myroutine')
!!    else
!!       write (*, *) 'called with an option'
!!    endif
!!    end program demo_unit_test_system
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function unit_test_system(command,verbose) result(istat)
!  EXITSTAT contains the integer exit code of the command, as returned by SYSTEM.
!  CMDSTAT is set to zero if the command line was executed (whatever its exit status was).
!          If an error condition occurs and CMDSTAT is not present, error termination of execution of the image is initiated.
!     It is assigned
!      + the value -1 if the processor does not support command line execution,
!      + a processor-dependent positive value if an error condition occurs
!      + the value -2 if no error condition occurs but WAIT is present
!        with the value false and the processor does not support asynchronous
!        execution.
!      + Otherwise it is assigned the value 0.
!  CMDMSG is assigned an error message if an error has occurred. (exitstat or cmdstat or both?)
!         If an error condition occurs, it is assigned a processor- dependent explanatory message. Otherwise, it is unchanged.
character(len=*),intent(in)  :: command
logical,intent(in),optional  :: verbose
logical                      :: verbose_
integer                      :: istat
logical,parameter            :: wait=.true.
integer                      :: exitstat
integer                      :: cmdstat
character(len=256)           :: cmdmsg
character(len=:),allocatable :: command_
   if(present(verbose))then
      verbose_=verbose
   else
      verbose_=.false.
   endif
   command_=adjustl(command)//'   '
   if(index(command_,'* ') == 1)then
      command_=getname_()//command_(2:)
   elseif(index(command_,'** ') == 1)then
      command_=getall_()//command_(2:)
   endif
   if(verbose_)call wrt(G_luns,"command: ",command_)
   cmdmsg=' '
   call execute_command_line(command_,wait,exitstat,cmdstat,cmdmsg)
   flush(unit=6)
   if(cmdstat /= 0)then
      call wrt(G_luns,"cmdstat: ",cmdmsg,'for command :',command_)
   elseif(cmdmsg /= '')then
      call wrt(G_luns,"exitstat: ",cmdmsg,'for command :',command_)
   endif
   istat=merge(-cmdstat,exitstat,exitstat == 0.and.cmdstat /= 0)
end function unit_test_system
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    getname_(3f) - [M_io:QUERY] get name of the current executable
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function getname_() result(name)
!!
!!     character(len=:),allocatable         :: getname_
!!
!!##DESCRIPTION
!!    getname_(3f) returns the name of the current executable using
!!    get_command_argument(3f) and inquire(3f).
!!
!!##EXAMPLE
!!
!!    Sample getting a pathname of current executable:
!!
!!      program demo_getname_
!!      use M_io, only : getname_
!!      implicit none
!!         write(*,'(*(a))')'Running ',getname_()
!!      end program demo_getname_
!!
!!##AUTHOR
!!        John S. Urban
!!
!!##LICENSE
!!        Public Domain
function getname_() result(name)
! get the pathname of arg0
implicit none
character(len=:),allocatable :: arg0
integer                      :: arg0_length
integer                      :: ios
character(len=4096)          :: long_name
character(len=:),allocatable :: name
   arg0_length=0
   name=''
   long_name=''
   call get_command_argument(0,length=arg0_length,status=ios)
   if(ios == 0)then
      if(allocated(arg0))deallocate(arg0)
      allocate(character(len=arg0_length) :: arg0)
      call get_command_argument(0,arg0,status=ios)
      if(ios == 0)then
         inquire(file=arg0,iostat=ios,name=long_name)
         if(ios == 0)then
            name=trim(long_name)
         else
            name=arg0
         endif
      else
         arg0=''
      endif
   else
      arg0=''
   endif
end function getname_
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    getall_(3f) - [M_io:QUERY] get name of the current executable and options
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function getall_() result(name)
!!
!!     character(len=:),allocatable         :: getall_
!!
!!##DESCRIPTION
!!    getall_(3f) returns the name of the current executable
!!    and all the arguments surrounded with double-quotes
!!
!!##EXAMPLE
!!
!!    Sample getting a pathname of current executable:
!!
!!      program demo_getall_
!!      use M_io, only : getall_
!!      implicit none
!!         write(*,'(*(a))')'Running ',getall_()
!!      end program demo_getall_
!!
!!##AUTHOR
!!        John S. Urban
!!
!!##LICENSE
!!        Public Domain
function getall_() result(command)
! get the pathname of arg
implicit none
character(len=:),allocatable :: arg
integer                      :: length
integer                      :: ios
integer                      :: i
character(len=:),allocatable :: command
   length=0
   command=''
   do i=1,command_argument_count()
      call get_command_argument(i,length=length,status=ios)
      if(ios == 0)then
         if(allocated(arg))deallocate(arg)
         allocate(character(len=length) :: arg)
         call get_command_argument(0,arg,status=ios)
         if(ios == 0)then
                 command=command//' "'//arg//'"'
         endif
      endif
   enddo
   command=getname_()//command
end function getall_
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine run(command) ! convenience routine so command does not cause program to stop
character(len=*)             :: command
logical,parameter            :: wait=.true.
integer                      :: exitstat
integer                      :: cmdstat
character(len=256)           :: cmdmsg
   if(G_verbose)call wrt(G_luns,"+ ",command)
   cmdmsg=' '
   call execute_command_line(command,wait,exitstat,cmdstat,cmdmsg)
   if(cmdstat /= 0)then
      call wrt(G_luns,"cmdstat: ",cmdmsg,'for command :',command)
   elseif(cmdmsg /= '')then
      call wrt(G_luns,"exitstat: ",cmdmsg,'for command :',command)
   endif
end subroutine run
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! because of shell expansion just remove doublequote character from messages for now
function ndq(string) result(out)
character(len=*),intent(in)  :: string
character(len=len(string))   :: out
integer                      :: i
   do i=1,len(string)
      select case(string(i:i))
      case('"');    out(i:i)=' '
      case default; out(i:i)=string(i:i)
      end select
   enddo
end function ndq
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_framework__verify
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
