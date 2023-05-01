program runtest
use M_framework__verify, only : unit_check, unit_check_bad, unit_check_end, unit_check_good, unit_check_start, unit_check_stop
   call test_unit_check_start()
   call test_unit_check()
   call test_unit_check_end()
   call test_unit_check_bad()
   call test_unit_check_good()
   call test_unit_check_stop()
   call unit_check_stop()
   contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unit_check_start()
   call unit_check_start('unit_check_start',msg='')
!      call unit_check('unit_check_start', .true.,'expression is true')
!      call unit_check('unit_check_start', .false.,'expression is false')
   call unit_check_end('unit_check_start',msg='')
end subroutine test_unit_check_start
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unit_check()
   call unit_check_start('unit_check',msg='')
!      call unit_check('unit_check', .true.,'expression is true')
!      call unit_check('unit_check', .false.,'expression is false')
   call unit_check_end('unit_check',msg='')
end subroutine test_unit_check
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unit_check_end()
   call unit_check_start('unit_check_end',msg='')
!      call unit_check('unit_check_end', .true.,'expression is true')
!      call unit_check('unit_check_end', .false.,'expression is false')
   call unit_check_end('unit_check_end',msg='')
end subroutine test_unit_check_end
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unit_check_bad()
   call unit_check_start('unit_check_bad',msg='')
!      call unit_check('unit_check_bad', .true.,'expression is true')
!      call unit_check('unit_check_bad', .false.,'expression is false')
   call unit_check_end('unit_check_bad',msg='')
end subroutine test_unit_check_bad
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unit_check_good()
   call unit_check_start('unit_check_good',msg='')
!      call unit_check('unit_check_good', .true.,'expression is true')
!      call unit_check('unit_check_good', .false.,'expression is false')
   call unit_check_end('unit_check_good',msg='')
end subroutine test_unit_check_good
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unit_check_stop()
   call unit_check_start('unit_check_stop',msg='')
!      call unit_check_stop('unit_check_stop', .true.,'expression is true')
!      call unit_check_stop('unit_check_stop', .false.,'expression is false')
   call unit_check_end('unit_check_stop',msg='')
end subroutine test_unit_check_stop
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
end program runtest
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
