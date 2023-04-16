      program demo_unit_check_bad
      use M_framework__verify, only: unit_check_start
      use M_framework__verify, only: unit_check
      use M_framework__verify, only: unit_check_good, unit_check_bad

      implicit none
      integer :: x
      x=10
      call unit_check_start('myroutine')

      call unit_check('myroutine', x > 3 ,'test if big enough')
      call unit_check('myroutine', x < 100 ,'test if small enough')

      if(x /= 0)then
         call unit_check_bad ('myroutine',msg='checks on "myroutine" failed') ! program execution stopped
      endif

      end program demo_unit_check_bad
