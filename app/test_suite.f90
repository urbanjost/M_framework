program maketest
   use, intrinsic :: iso_fortran_env, only: &
   & stdin => input_unit, &
   & stdout => output_unit, &
   & stderr => error_unit
   implicit none
   character(len=:), allocatable :: words(:)
   character(len=*), parameter :: g = '(*(g0))'
   logical, parameter :: T=.true., F=.false.
   integer :: out = stdout
   integer :: i
! a program to call the test_suite_* Fortran procedures
   write (out, g) "program runtest"
   write (out, g) "use, intrinsic :: iso_fortran_env, only: &"
   write (out, g) "& stdin => input_unit,   &"
   write (out, g) "& stdout => output_unit, &"
   write (out, g) "& stderr => error_unit"
   write (out, g) "use M_framework, only : unit_test_start, unit_test, unit_test_msg"
   write (out, g) "use M_framework, only : unit_test_end, unit_test_stop, unit_test_mode"
   write (out, g) "use M_framework, only : unit_test_level, unit_test_flags"
   write (out, g) "!use M_xxxx"
   write (out, g) "implicit none"
   write (out, g) "logical, parameter :: T=.true., F=.false."
   write (out, g) "logical            :: matched"
   write (out, g) "! optional call to change default modes"
   write (out, g) "   call unit_test_mode(       &"
   write (out, g) "       keep_going=T,           &"
   write (out, g) "       flags=[0],              &"
   write (out, g) "       luns=[stderr],          &"
   write (out, g) "       command='',             &"
   write (out, g) "       brief=F,                &"
   write (out, g) "       match='',               &"
   write (out, g) "       interactive=F,          &"
   write (out, g) "       CMDLINE=T,              &"
   write (out, g) "       debug=F)"
   write (out, g) ""
   write (out, g) '   unit_test_level=0'
   words = get_stack()
   if (size(words) .eq. 0) words = ["sample"]
   write (out, g) ''
   do i = 1, size(words)
      write (out, g) '   call test_suite_'//words(i)//'()'
   end do
   write (out, g) '   call unit_test_stop()'
   write (out, g) ''
   write (out, g) 'contains'
   do i = 1, size(words)
      write (out, g) ''
      write (out, g) 'subroutine test_suite_'//words(i)//'()'
      write (out, g) '   call unit_test_start("'//words(i)//'",msg="",matched=matched)'
      write (out, g) '   if(.not.matched)return'
      write (out, g) '   !!call unit_test("'//words(i)//'", 0 .eq. 0, "checking",100)'
      write (out, g) '   call unit_test_end("'//words(i)//'",msg="")'
      write (out, g) 'end subroutine test_suite_'//words(i)
   end do

   write (out, g) ''
   write (out, g) 'end program runtest'

contains

   function get_stack() result(args)
      character(len=:), allocatable :: args(:)
      integer :: ilength, ilongest, iargs, istatus, i
      ilength = 0
      ilongest = 1 ! get an error if try to get string of zero length in gfortran 7.0.4 so set to 1 instead of 0
      iargs = command_argument_count()
      GET_LONGEST: do i = 1, iargs                                              ! look at all arguments
         call get_command_argument(number=i, length=ilength, status=istatus)    ! get next argument
         if (istatus /= 0) then                                                 ! stop program on error
            write (stderr, *) '*get_stack* error obtaining argument ', i
            exit GET_LONGEST
         elseif (ilength .gt. 0) then
            ilongest = max(ilongest, ilength)
         end if
      end do GET_LONGEST
      allocate (character(len=ilongest) :: args(iargs))
      args(:) = ''
      GET_ARGS: do i = 1, command_argument_count()                                           ! copy array of arguments
         call get_command_argument(number=i, value=args(i), length=ilength, status=istatus)  ! get next argument
         if (istatus /= 0) then                                                              ! stop program on error
            write (stderr, *) '*get_stack* error obtaining argument ', i
            exit GET_ARGS
         end if
      end do GET_ARGS
   end function get_stack

end program maketest
