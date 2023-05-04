      program demo_unit_test_start
      use M_framework__verify, only: unit_test_start, unit_test, &
       & unit_test_end, unit_test_mode
      implicit none
      integer :: ival
      call unit_test_mode(command='goodbad')
      call unit_test_start('myroutine')
      ! the example goodbad(1) command called here takes many options
      ! used to build an SQLite3 entry
      call unit_test_start('myroutine_long',opts='     &
        & --section        3                            &
        & --library        libGPF                       &
        & --filename       `pwd`/M_framework__verify.FF &
        & --documentation  y                            &
        & --prep           y                            &
        & --ccall          n                            &
        & --archive        GPF.a                        &
        & ')

      ival=10

      call unit_test('myroutine', ival > 3 ,   msg=' if big enough')
      call unit_test('myroutine', ival < 100 , msg=' if small enough')

      call unit_test_end('myroutine',msg='completed checks of "myroutine"')

      end program demo_unit_test_start
