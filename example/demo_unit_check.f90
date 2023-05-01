     program demo_unit_check
     use M_framework__verify, only: unit_check_start, &
                           & unit_check, unit_check_end
     use M_framework__verify, only: unit_check_mode
     use M_framework__approx, only: almost

     implicit none
     integer :: i
     integer :: x
     integer,allocatable :: arr(:)
     real,allocatable :: arr1(:)
     real,allocatable :: arr2(:)

        call unit_check_mode(keep_going=.true.,debug=.false.,command='')

        x=10
        arr1=[1.0,10.0,100.0]
        arr2=[1.0001,10.001,100.01]
        call unit_check_start('myroutine')

        call unit_check('myroutine', x > 3 ,' if big enough')
        call unit_check('myroutine', x < 100 ,' if small enough')

        do i=1,size(arr1)
           call unit_check('myroutine', &
           & almost(arr1(i),arr2(i),3.9,verbose=.true.) )
        enddo

        arr=[10,20,30]
        call unit_check('myroutine', .not.any(arr < 0) , &
        & 'fail if any negative values in array ARR')
        call unit_check('myroutine', all(arr < 100) , &
        & 'fail unless all values are less than 100 in array ARR')

        call unit_check_end('myroutine', &
        & msg='checks on "myroutine" all passed')

     end program demo_unit_check
