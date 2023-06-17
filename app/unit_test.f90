program maketest
! writes an individual file for each argument name
use, intrinsic :: iso_fortran_env, only: &
   & stdin => input_unit, &
   & stdout => output_unit, &
   & stderr => error_unit
implicit none
character(len=:), allocatable :: words(:)
character(len=*), parameter   :: g = '(*(g0))'
logical, parameter            :: T=.true., F=.false.
integer                       :: out, i, iostat
character(len=256)            :: iomsg
! a program to call the test_suite_* Fortran procedures
   words = get_stack()
   if (size(words) .eq. 0)words=['--help']
   if(words(1).eq.'--help')then
      write (stderr, g) '--------------------------------------------------------------------------------'
      write (stderr, g) 'NAME                                                                            '
      write (stderr, g) ' unit-test(1f) - create unit-test skeleton programs for use with M_framework(1) '
      write (stderr, g) 'SYNOPSIS                                                                        '
      write (stderr, g) '     unit_test ARG1 ARG2 ARG3 ARG4 ...                                          '
      write (stderr, g) 'DESCRIPTION                                                                     '
      write (stderr, g) '   generate skeleton programs in current directory named "unit_test_ARG.f90 for"'
      write (stderr, g) '   for use with "fpm test". Will not overwrite existing files.                  '
      write (stderr, g) 'EXAMPLE                                                                         '
      write (stderr, g) 'Create a new fpm project to try it out                                          '
      write (stderr, g) '   fpm new tryit;cd tryit/test  # go to test/ directory of an fpm(1) project    '
      write (stderr, g) ' unit_test a b c             # create three skeleton test files                 '
      write (stderr, g) 'Ensure your fpm.toml file includes M_framework as a dependency with lines like  '
      write (stderr, g) '[dev-dependencies]                                                              '
      write (stderr, g) '   M_framework    = { git = "https://github.com/urbanjost/M_framework.git" }    '
      write (stderr, g) 'Run tests                                                                       '
      write (stderr, g) '    fpm test   # run all tests                                                  '
      write (stderr, g) '    fpm test unit_test_c # run test "c"                                        '
      write (stderr, g) '    fpm test "timing_*" # run tests starting with "timing_"                     '
      write (stderr, g) '    fpm test -- command=bookkeeper  # use sample bookkeeper command to generate '
      write (stderr, g) '                                    # CSV and HTML and NAMELIST files.          '
      write (stderr, g) 'Now try it in a real fpm(1) test/ directory and add real tests or results in the'
      write (stderr, g) 'unit_test(3f) calls.                                                           '
      write (stderr, g) '--------------------------------------------------------------------------------'
!      write (stderr, g) '    [[test]]                                                                    '
!      write (stderr, g) '    name="test_suite"                                                           '
!      write (stderr, g) '    source-dir="test"                                                           '
!      write (stderr, g) '    main="check.f90"                                                            '
!      write (stderr, g) '    [test.dependencies]                                                         '
!      write (stderr, g) '    M_framework = { git = "https://github.com/urbanjost/M_framework.git" }      '
      stop
   endif
   do i = 1, size(words)
      open(newunit=out,file='unit_test_'//trim(words(i))//'.f90',status='new',iostat=iostat,iomsg=iomsg)
      if(iostat.ne.0)then
         write(stderr,g) trim(iomsg)
         cycle
      endif
      write (out, g) "program unit_test_",trim(words(i))
      write (out, g) "use, intrinsic :: iso_fortran_env, only: &"
      write (out, g) "& stdin => input_unit,   &"
      write (out, g) "& stdout => output_unit, &"
      write (out, g) "& stderr => error_unit"
      write (out, g) "use M_framework, only : unit_test_start, unit_test"
      write (out, g) "use M_framework, only : unit_test_end,   unit_test_stop"
      write (out, g) "use M_framework, only : unit_test_mode,  unit_test_msg"
      write (out, g) "use M_framework, only : unit_test_level, unit_test_flags"
      write (out, g) "!use M_xxxx"
      write (out, g) "implicit none"
      write (out, g) "logical, parameter :: T=.true., F=.false."
      write (out, g) "! optional call to change default modes"
      write (out, g) "   call unit_test_mode(       &"
      write (out, g) "       keep_going=T,           &"
      write (out, g) "       flags=[0],              &"
      write (out, g) "       luns=[stderr],          &"
      write (out, g) "       command='',             &"
      write (out, g) "       brief=F,                &"
      write (out, g) "       silent=F,               &"
      write (out, g) "       verbose=F,              &"
      write (out, g) "       interactive=F,          &"
      write (out, g) "       cmdline=T,              &"
      write (out, g) "       debug=F)"
      write (out, g) ""
      write (out, g) '   unit_test_level=0'
      write (out, g) ''
      write (out, g) '   call test_suite_'//trim(words(i))//'()'
      write (out, g) '   call unit_test_stop()'
      write (out, g) ''
      write (out, g) 'contains'
      write (out, g) ''
      write (out, g) 'subroutine test_suite_'//trim(words(i))//'()'
      write (out, g) '   call unit_test_start("'//trim(words(i))//'",msg="")'
      write (out, g) '   !!call unit_test("'//trim(words(i))//'", 0 .eq. 0, "checking",100)'
      write (out, g) '   call unit_test_end("'//trim(words(i))//'",msg="")'
      write (out, g) 'end subroutine test_suite_'//trim(words(i))
      write (out, g) ''
      write (out, g) "end program unit_test_",trim(words(i))
      close(unit=out,iostat=iostat)
   enddo

contains

   function get_stack() result(args)
      character(len=:), allocatable :: args(:)
      character(len=256) :: errmsg=''
      integer :: ilength, ilongest, iargs, istatus, i
      ilength = 0
      ilongest = 1 ! get an error if try to get string of zero length in gfortran 7.0.4 so set to 1 instead of 0
      iargs = command_argument_count()
      GET_LONGEST: do i = 1, iargs                                              ! look at all arguments
         call get_command_argument(number=i, length=ilength, status=istatus) !,errmsg=errmsg)    ! get next argument
         if (istatus /= 0) then                                                 ! stop program on error
            write (stderr, *) '*get_stack* error obtaining length for argument ', i, trim(errmsg)
            exit GET_LONGEST
         elseif (ilength .gt. 0) then
            ilongest = max(ilongest, ilength)
         end if
      end do GET_LONGEST
      allocate (character(len=ilongest) :: args(iargs))
      args(:) = ''
      GET_ARGS: do i = 1, command_argument_count()                                           ! copy array of arguments
         call get_command_argument(number=i, value=args(i), length=ilength, status=istatus) !,errmsg=errmsg)  ! get next argument
         if (istatus /= 0) then                                                              ! stop program on error
            write (stderr, *) '*get_stack* error obtaining argument ', i, trim(errmsg)
            exit GET_ARGS
         end if
      end do GET_ARGS
   end function get_stack

end program maketest
