program main
use M_framework, only: unit_test, unit_test_end, unit_test_start, unit_test_stop, unit_test_msg
use M_framework, only: str

implicit none
logical             :: allpassed=.true.
logical,allocatable :: tests(:)
  call unit_test_start('str','test building message strings')

  tests=[logical :: ]

  call add('INTEGER',str(10),'10','10')
  call add('LOGICAL',str(.false.),'F','F')
  call add('LOGICAL',str(.true.),'T','T')
  call add('REAL',str(100.0),'100.000000','100.0000')
  call add('COMPLEX',str((11.0,22.0)),'(11.0000000,22.0000000)','(11.00000,22.00000)')
  call add('COMPOUND',str(10,100.0,"string",(11.0,22.0),.false.), &
       & '10 100.000000 string (11.0000000,22.0000000) F',&
       & '10 100.0000 string (11.00000,22.00000) F')
  call unit_test_msg('str','tally is ',str(tests)//'') ! //'' for gfortran-11 bug
  call unit_test_end('str')
  call unit_test_stop('M_framework_msg')

contains
subroutine add(message,question,answer,answer2)
character(len=*),intent(in)   :: message
character(len=*),intent(in)   :: question
character(len=*),intent(in)   :: answer
character(len=*),intent(in)   :: answer2
logical                       :: passed
  passed=question .eq. answer
  if(passed)then
     call unit_test('str',passed,'testing',message,'expected',answer,'got',question)
  else
     passed=question .eq. answer2
     call unit_test('str',passed,'testing',message,'expected',answer2,'got',question)
  endif
end subroutine add
end program main
