     program demo_almost
     use M_framework__approx, only : almost
     implicit none
     real    :: x, y
     logical :: z
     integer :: i
     x=1.2345678
     y=1.2300000
     do i=1,8
        z=almost(x,y,i,verbose=.true.)
        write(*,*)i,z
     enddo
     end program demo_almost
