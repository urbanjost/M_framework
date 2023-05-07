      program demo_unit_test_stop
      use M_framework__verify, only: unit_test_start, unit_test_end, &
      & unit_test, unit_test_stop, unit_test_mode
      implicit none
      integer :: x

      call unit_test_mode(keep_going=.true.,debug=.false.,command='')

      ! do a test
      call unit_test_start('myroutine')
      x=10
      call unit_test('myroutine', x > 3 ,' if big enough')
      call unit_test('myroutine', x < 100 ,' if small enough')
      if(x /= 0)then
         call unit_test ('myroutine',.false.,msg='x /= 0' )
      endif
      call unit_test_end  ('myroutine',msg='checks on "myroutine"' )
      ! tally up test results and stop program
      call unit_test_stop()

      end program demo_unit_test_stop
