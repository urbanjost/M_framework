      program demo_unit_check_stop
      use M_framework__verify, only: unit_check_start, unit_check_end, &
      & unit_check, unit_check_stop, unit_check_mode
      implicit none
      integer :: x

      call unit_check_mode(keep_going=.true.,debug=.false.,command='')

      x=10
      call unit_check_start('myroutine')

      call unit_check('myroutine', x > 3 ,' if big enough')
      call unit_check('myroutine', x < 100 ,' if small enough')

      if(x /= 0)then
         call unit_check ('myroutine',.false.,msg='x /= 0' )
      endif

      call unit_check_end  ('myroutine',msg='checks on "myroutine"' )

      call unit_check_stop()

      end program demo_unit_check_stop
