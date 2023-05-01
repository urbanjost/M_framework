      program demo_unit_check_start
      use M_framework__verify, only: unit_check_start, unit_check, &
       & unit_check_end
      implicit none
      integer :: ival
      call unit_check_start('myroutine')
      ! the goodbad(1) command called here takes many options
      ! used to build an SQLite3 entry
      call unit_check_start('myroutine_long',opts='     &
        & --section        3                            &
        & --library        libGPF                       &
        & --filename       `pwd`/M_framework__verify.FF &
        & --documentation  y                            &
        & --prep           y                            &
        & --ccall          n                            &
        & --archive        GPF.a                        &
        & ')

      ival=10

      call unit_check('myroutine', ival > 3 ,   msg=' if big enough')
      call unit_check('myroutine', ival < 100 , msg=' if small enough')

      call unit_check_end('myroutine',msg='completed checks of "myroutine"')

      end program demo_unit_check_start
