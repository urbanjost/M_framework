     program demo_unit_check_system
     use M_framework__verify, only: &
        unit_check_start, &
        unit_check, &
        unit_check_system, &
        unit_check_done
     implicit none
     if(command_argument_count().eq.0)then
        call unit_check_start('myroutine')
        call unit_check('false',unit_check_system('false') == 0, 'check false')
        call unit_check('true',unit_check_system('true') == 0, 'check true')
        call unit_check('notthere',unit_check_system('notthere') == 0, &
        & 'check notthere')
        call unit_check('*',&
        & unit_check_system('* and options',verbose=.true.) == 0, 'check "*"')
        call unit_check_done('myroutine')
     else
        write(*,*)'called with an option'
     endif
     end program demo_unit_check_system
