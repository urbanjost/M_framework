      program demo_unit_check_bad
      use M_framework__verify, only: unit_check_start, unit_check
      use M_framework__verify, only: unit_check_end, unit_check_stop
      use M_framework__verify, only: unit_check_good, unit_check_bad

      implicit none
      integer :: x
      x=10
      call unit_check_start('myroutine')

      call unit_check('myroutine', x > 3 ,'if big enough')
      call unit_check('myroutine', x < 100 ,'if small enough')

      if(x /= 0)then
        call unit_check_bad ('myroutine',msg='checks on "myroutine" failed')
        ! program execution stopped
      endif
      call unit_check_end ('myroutine')
      call unit_check_stop ('myroutine')

      end program demo_unit_check_bad
