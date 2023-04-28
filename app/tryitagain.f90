program M_test_suite_M_steam67
use M_framework, only : unit_check_start,unit_check,unit_check_done, &
                 unit_check_mode, unit_check_level, unit_check_stop
!use M_steam67
implicit none
double precision,allocatable :: expected(:), answers(:), input(:)
double precision,parameter :: PI=atan(1.0d0)*4
   call test_sqrt()
   call test_cos()
   call test_sin()
   call unit_check_stop()
contains
subroutine test_sqrt()
integer :: i
   call unit_check_mode(luns=[0,10])
   call unit_check_start('sqrt',msg='')
   call unit_check_mode(luns=[0,10])
   input   =[1.0d0,4.0d0,9.0d0]
   expected=[1.0d0,2.0d0,3.0d0]
   answers=[(sqrt(input(i)),i=1,size(input))]
   call unit_check('sqrt',           &
           all(expected.eq.answers), &
           'check table of values')
   call unit_check('sqrt',           &
           sqrt(25.0d0).eq.5.0d0,    &
           'got',sqrt(25.0d0),'expected',5.0d0)
   call unit_check_done('sqrt',msg='')
end subroutine test_sqrt
subroutine test_sin()
   call unit_check_start('sin',msg='')
   call unit_check_done('sin',msg='')
end subroutine test_sin
subroutine test_cos()
   call unit_check_start('cos',msg='')
   call unit_check_done('cos',msg='')
end subroutine test_cos
end program M_test_suite_M_steam67
