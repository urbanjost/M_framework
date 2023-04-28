program demo_help_command
use M_io,   only : swallow
use M_framework__help, only : help_command
implicit none
integer                       :: count, i, argument_length, istat
character(len=:), allocatable :: filename
character(len=:),allocatable  :: help_text(:)
integer                       :: position(2)
character(len=128)            :: string
   position=[0,23]
   count = command_argument_count()
   i=1
   if(count.gt.0)then
      call get_command_argument(number=i, length=argument_length)
      if (allocated(filename)) deallocate (filename)
      allocate (character(len=argument_length) :: filename)
      call get_command_argument(i, filename, status=istat)
      ! show the results
      write (*, '(i3.3,1x,i0.5,1x,i0.5,1x,"[",a,"]")') &
      & i, istat, argument_length, filename
      call swallow(filename,help_text)
   else
      call swallow('app/unit_test_wip.hlp',help_text)
   endif
   do
      write(*,'(*(g0))',advance='no')'enter topic>'
      read(*,'(a)')string
      if(string.eq.'stop')exit
      call help_command(help_text,string,position)
   enddo
end program demo_help_command


