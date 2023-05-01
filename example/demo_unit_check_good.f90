      program demo_unit_check_good
      use M_framework__verify, only: unit_check_start, unit_check_end
      use M_framework__verify, only: unit_check
      use M_framework__verify, only: unit_check_good, unit_check_bad

      implicit none
      integer :: x
      x=10
      call unit_check_start('myroutine')

      call unit_check('myroutine', x > 3 ,'if big enough')
      call unit_check('myroutine', x < 100 ,'if small enough')

      call unit_check_good('myroutine',msg='checks on "myroutine" ')

      end program demo_unit_check_good
