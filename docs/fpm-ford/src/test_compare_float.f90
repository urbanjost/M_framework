program test_compare_float
use, intrinsic :: iso_fortran_env,  only : int8, int16, int32, int64
use, intrinsic :: iso_fortran_env,  only : real32, real64, real128
use, intrinsic :: iso_fortran_env,  only : error_unit,output_unit
use M_framework
use m_framework__verify, only : unit_test_start,unit_test,unit_test_done,unit_test_good,unit_test_bad,unit_test_msg,unit_test_mode
use m_framework__verify, only : unit_test_stop,unit_test_level
use m_framework__msg,    only : str,fmt
use m_framework__approx, only : compare_float, operator(.equalto.), operator(.lessthan.), operator(.greaterthan.)
implicit none
logical,parameter            :: T=.true., F=.false.
integer,parameter            :: nums = 5
character(len=*),parameter   :: sfmt='es20.13', dfmt='es27.20'
real(kind=real32), parameter :: single_number(nums) = &
    [  1.234567890123456e-16_real32, &
       1.234567890123456e-01_real32, &
       1.234567890123456e+01_real32, &
       1.234567890123456e+16_real32, &
       1.0_real32 ]
real(kind=real64), parameter :: double_number(nums) = &
    [  1.234567890123456e-16_real64, &
       1.234567890123456e-01_real64, &
       1.234567890123456e+01_real64, &
       1.234567890123456e+16_real64, &
       1.0_real64 ]
real(kind=real32),parameter  ::  sten    =     10.0_real32
real(kind=real64),parameter  ::  dten    =     10.0_real64
integer                      ::  i
real(kind=real32)            ::  x,  y1,  y2,  y3,  y4
real(kind=real64)            ::  xd, yd1, yd2, yd3, yd4

   call unit_test_mode(level=0,luns=[OUTPUT_UNIT])
  ! Scalar calls 
   do i = 1, nums
      x = single_number(i)
      y1 = nearest( x, 1.0_real32 )
      y2 = y1 - spacing( x )
      y3 = nearest( x, -1.0_real32 )
      y4 = y3 + spacing( x )
      call test_compare_float_single()
      call test_equalto_single()
      call test_greaterthan_single()
      call test_lessthan_single()
   
      xd = double_number(i)
      yd1 = nearest( xd, 1.0_real64 )
      yd2 = yd1 - spacing( xd )
      yd3 = nearest( xd, -1.0_real64 )
      yd4 = yd3 + spacing( xd )
      call test_compare_float_double()
      call test_equalto_double()
      call test_greaterthan_double()
      call test_lessthan_double()
   enddo
   call  test_elemental_single() 
   call  test_elemental_double()

   call unit_test_stop()
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_compare_float_single()
logical,allocatable :: expected(:)
character(len=*),parameter :: title='compare_float single'

   call unit_test_start(title,msg=title)

   call unit_test_msg(title,"    x  = ", fmt(x,sfmt) )
   call unit_test_msg(title,"    y1 = ", fmt(y1,sfmt), ":  NEAREST( x, 1.0 )" )
   call unit_test_msg(title,"    y2 = ", fmt(y2,sfmt), ":  y1 - SPACING( x )" )
   call unit_test_msg(title,"    y3 = ", fmt(y3,sfmt), ":  NEAREST( x,-1.0 )" )
   call unit_test_msg(title,"    y4 = ", fmt(y4,sfmt), ":  y3 + SPACING( x )" )

   select case(i)
   case(1); expected = [ F,T,T,F,T,T ]
   case(2); expected = [ F,T,T,F,T,T ]
   case(3); expected = [ F,T,T,F,T,T ]
   case(4); expected = [ F,T,T,F,T,T ]
   case(5); expected = [ F,T,T,T,T,T ]
   end select

   call unit_test_msg(title, 'expected',expected(1),expected(2),expected(3),expected(4),expected(5),expected(6))
   call unit_test_msg(title, ' regular', x.eq.y1, x.eq.y1, x.eq.y2, x.eq.y3, x.eq.y3, x.eq.y4)
   call unit_test(title, expected(1).eqv.compare_float(x,y1),       "Compare_Float(x, y1)       = ",compare_float(x,y1),i)
   call unit_test(title, expected(2).eqv.compare_float(x,y1,ulp=2), "Compare_Float(x, y1,ulp=2) = ",compare_float(x,y1,ulp=2),i)
   call unit_test(title, expected(3).eqv.compare_float(x,y2),       "Compare_Float(x, y2)       = ",compare_float(x,y2),i)
   call unit_test(title, expected(4).eqv.compare_float(x,y3),       "Compare_Float(x, y3)       = ",compare_float(x,y3),i)
   call unit_test(title, expected(5).eqv.compare_float(x,y3,ulp=2), "Compare_Float(x, y3,ulp=2) = ",compare_float(x,y3,ulp=2),i)
   call unit_test(title, expected(6).eqv.compare_float(x,y4),       "Compare_Float(x, y4)       = ",compare_float(x,y4),i)

   call unit_test_end(title, msg='')
end subroutine test_compare_float_single
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_compare_float_double()
logical,allocatable :: expected(:)
character(len=*),parameter :: title='compare_float double'

   call unit_test_start(title,msg=title)

   call unit_test_msg(title,"    x  = ", fmt(xd,dfmt) )
   call unit_test_msg(title,"    y1 = ", fmt(yd1,dfmt), ":  NEAREST( x, 1.0 )" )
   call unit_test_msg(title,"    y2 = ", fmt(yd2,dfmt), ":  y1 - SPACING( x )" )
   call unit_test_msg(title,"    y3 = ", fmt(yd3,dfmt), ":  NEAREST( x,-1.0 )" )
   call unit_test_msg(title,"    y4 = ", fmt(yd4,dfmt), ":  y3 + SPACING( x )" )

   select case(i)
   case(1); expected = [ F,T,T,F,T,T ]
   case(2); expected = [ F,T,T,F,T,T ]
   case(3); expected = [ F,T,T,F,T,T ]
   case(4); expected = [ F,T,T,F,T,T ]
   case(5); expected = [ F,T,T,T,T,T ]
   end select

   call unit_test_msg(title, 'expected',expected(1),expected(2),expected(3),expected(4),expected(5),expected(6))
   call unit_test_msg(title, ' regular',xd.eq.yd1,xd.eq.yd1,xd.eq.yd2,xd.eq.yd3,xd.eq.yd3,xd.eq.yd4)
   call unit_test(title,expected(1).eqv.compare_float(xd,yd1),      "Compare_Float(xd, yd1)      =",compare_float(xd,yd1),i)
   call unit_test(title,expected(2).eqv.compare_float(xd,yd1,ulp=2),"Compare_Float(xd, yd1,ulp=2)=",compare_float(xd,yd1,ulp=2),i)
   call unit_test(title,expected(3).eqv.compare_float(xd,yd2),      "Compare_Float(xd, yd2)      =",compare_float(xd,yd2),i)
   call unit_test(title,expected(4).eqv.compare_float(xd,yd3),      "Compare_Float(xd, yd3)      =",compare_float(xd,yd3),i)
   call unit_test(title,expected(5).eqv.compare_float(xd,yd3,ulp=2),"Compare_Float(xd, yd3,ulp=2)=",compare_float(xd,yd3,ulp=2),i)
   call unit_test(title,expected(6).eqv.compare_float(xd,yd4),      "Compare_Float(xd, yd4)      =",compare_float(xd,yd4),i)

   call unit_test_end(title, msg='')
end subroutine test_compare_float_double
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_elemental_single()
real(kind=real32),dimension(nums)    ::  xv,   yv1,   yv2,   yv3,   yv4
real(kind=real32),dimension(nums,2)  ::  xa,   ya1,   ya2,   ya3,   ya4
character(len=:),allocatable :: title

title='equalto single'

call unit_test_start(title,msg='vector tests')

call unit_test_msg(title, 'vector')

xv = single_number
yv1 = nearest( xv, (/ ( 1.0_real32, i = 1, nums) /) )
yv2 = yv1 - spacing( xv )
yv3 = nearest( xv, (/ (-1.0_real32, i = 1, nums) /) )
yv4 = yv3 + spacing( xv )

call unit_test_msg(title,"    xv  = ", str(xv) )
call unit_test_msg(title,"    yv1 = ", str(yv1), ":  NEAREST( x, 1.0 )" )
call unit_test_msg(title,"    yv2 = ", str(yv2), ":  y1 - SPACING( x )" )
call unit_test_msg(title,"    yv3 = ", str(yv3), ":  NEAREST( x,-1.0 )" )
call unit_test_msg(title,"    yv4 = ", str(yv4), ":  y3 + SPACING( x )" )

call unit_test(title,all([F,F,F,F,F] .eqv. compare_float(xv,yv1)),      "Compare_Float(xv, yv1)      ")
call unit_test(title,all([T,T,T,T,T] .eqv. compare_float(xv,yv1,ulp=2)),"Compare_Float(xv, yv1,ulp=2)")
call unit_test(title,all([T,T,T,T,T] .eqv. compare_float(xv,yv2)),      "Compare_Float(xv, yv2)      ")
call unit_test(title,all([F,F,F,F,T] .eqv. compare_float(xv,yv3)),      "Compare_Float(xv, yv3)      ")
call unit_test(title,all([T,T,T,T,T] .eqv. compare_float(xv,yv3,ulp=2)),"Compare_Float(xv, yv3,ulp=2)")
call unit_test(title,all([T,T,T,T,T] .eqv. compare_float(xv,yv4)),      "Compare_Float(xv, yv4)      ")

call unit_test(title, all([F,F,F,F,F] .eqv. (xv .equalto. yv1)), "( xv .equalto. yv1 )")
call unit_test(title, all([T,T,T,T,T] .eqv. (xv .equalto. yv2)), "( xv .equalto. yv2 )")
call unit_test(title, all([F,F,F,F,T] .eqv. (xv .equalto. yv3)), "( xv .equalto. yv3 )")
call unit_test(title, all([T,T,T,T,T] .eqv. (xv .equalto. yv4)), "( xv .equalto. yv4 )")

call unit_test(title, all([F,F,F,F,F] .eqv. (xv .greaterthan. yv1)), "( xv .greaterthan. yv1 )")
call unit_test(title, all([F,F,F,F,F] .eqv. (xv .greaterthan. yv2)), "( xv .greaterthan. yv2 )")
call unit_test(title, all([T,T,T,T,F] .eqv. (xv .greaterthan. yv3)), "( xv .greaterthan. yv3 )")
call unit_test(title, all([F,F,F,F,F] .eqv. (xv .greaterthan. yv4)), "( xv .greaterthan. yv4 )")

call unit_test(title, all([T,T,T,T,T] .eqv. (xv .lessthan. yv1)), "( xv .lessthan. yv1 )")
call unit_test(title, all([F,F,F,F,F] .eqv. (xv .lessthan. yv2)), "( xv .lessthan. yv2 )")
call unit_test(title, all([F,F,F,F,F] .eqv. (xv .lessthan. yv3)), "( xv .lessthan. yv3 )")
call unit_test(title, all([F,F,F,F,F] .eqv. (xv .lessthan. yv4)), "( xv .lessthan. yv4 )")

   
call unit_test_msg(title, 'matrix')

xa  = reshape((/single_number,single_number+(sten*spacing(single_number))/),(/nums,2/))
ya1 = nearest( xa, reshape((/ ( 1.0_real32, i = 1, nums*2) /),(/nums,2/)) )
ya2 = ya1 - spacing( xa )
ya3 = nearest( xa, reshape((/ (-1.0_real32, i = 1, nums*2) /),(/nums,2/)) )
ya4 = ya3 + spacing( xa )

call unit_test_msg(title,"    xa  = ", str([xa]) )
call unit_test_msg(title,"    ya1 = ", str([ya1]), ":  NEAREST( x, 1.0 )" )
call unit_test_msg(title,"    ya2 = ", str([ya2]), ":  y1 - SPACING( x )" )
call unit_test_msg(title,"    ya3 = ", str([ya3]), ":  NEAREST( x,-1.0 )" )
call unit_test_msg(title,"    ya4 = ", str([ya4]), ":  y3 + SPACING( x )" )

call unit_test(title,all(reshape([F,F,F,F,F,F,F,F,F,F],[nums,2]).eqv.compare_float(xa,ya1)),      "Compare_Float(xa,ya1)      ")
call unit_test(title,all(reshape([T,T,T,T,T,T,T,T,T,T],[nums,2]).eqv.compare_float(xa,ya1,ulp=2)),"Compare_Float(xa,ya1,ulp=2)")
call unit_test(title,all(reshape([T,T,T,T,T,T,T,T,T,T],[nums,2]).eqv.compare_float(xa,ya2)),      "Compare_Float(xa,ya2)      ")
call unit_test(title,all(reshape([F,F,F,F,T,F,F,F,F,F],[nums,2]).eqv.compare_float(xa,ya3)),      "Compare_Float(xa,ya3)      ")
call unit_test(title,all(reshape([T,T,T,T,T,T,T,T,T,T],[nums,2]).eqv.compare_float(xa,ya3,ulp=2)),"Compare_Float(xa,ya3,ulp=2)")
call unit_test(title,all(reshape([T,T,T,T,T,T,T,T,T,T],[nums,2]).eqv.compare_float(xa,ya4)),      "Compare_Float(xa,ya4)      ")

call unit_test(title,all(reshape([F,F,F,F,F,F,F,F,F,F],[nums,2]).eqv.(xa .equalto.ya1)),"( xa .equalto. ya1 )")
call unit_test(title,all(reshape([T,T,T,T,T,T,T,T,T,T],[nums,2]).eqv.(xa .equalto.ya2)),"( xa .equalto. ya2 )")
call unit_test(title,all(reshape([F,F,F,F,T,F,F,F,F,F],[nums,2]).eqv.(xa .equalto.ya3)),"( xa .equalto. ya3 )")
call unit_test(title,all(reshape([T,T,T,T,T,T,T,T,T,T],[nums,2]).eqv.(xa .equalto.ya4)),"( xa .equalto. ya4 )")

call unit_test(title,all(reshape([F,F,F,F,F,F,F,F,F,F],[nums,2]).eqv.(xa .greaterthan. ya1)),"( xa .greaterthan. ya1 )")
call unit_test(title,all(reshape([F,F,F,F,F,F,F,F,F,F],[nums,2]).eqv.(xa .greaterthan. ya2)),"( xa .greaterthan. ya2 )")
call unit_test(title,all(reshape([T,T,T,T,F,T,T,T,T,T],[nums,2]).eqv.(xa .greaterthan. ya3)),"( xa .greaterthan. ya3 )")
call unit_test(title,all(reshape([F,F,F,F,F,F,F,F,F,F],[nums,2]).eqv.(xa .greaterthan. ya4)),"( xa .greaterthan. ya4 )")

call unit_test(title,all(reshape([T,T,T,T,T,T,T,T,T,T],[nums,2]).eqv.(xa .lessthan. ya1)),"( xa .lessthan. ya1 )")
call unit_test(title,all(reshape([F,F,F,F,F,F,F,F,F,F],[nums,2]).eqv.(xa .lessthan. ya2)),"( xa .lessthan. ya2 )")
call unit_test(title,all(reshape([F,F,F,F,F,F,F,F,F,F],[nums,2]).eqv.(xa .lessthan. ya3)),"( xa .lessthan. ya3 )")
call unit_test(title,all(reshape([F,F,F,F,F,F,F,F,F,F],[nums,2]).eqv.(xa .lessthan. ya4)),"( xa .lessthan. ya4 )")

call unit_test_end(title, msg='')

end subroutine test_elemental_single
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_elemental_double()
real(kind=real64),dimension(nums)    ::  xvd,  yvd1,  yvd2,  yvd3,  yvd4
real(kind=real64),dimension(nums,2)  ::  xad,  yad1,  yad2,  yad3,  yad4
character(len=:),allocatable :: title

title='equalto double'

call unit_test_start(title,msg='vector tests')

call unit_test_msg(title, 'vector')

xvd = double_number
yvd1 = nearest( xvd, (/ ( 1.0_real64, i = 1, nums) /) )
yvd2 = yvd1 - spacing( xvd )
yvd3 = nearest( xvd, (/ (-1.0_real64, i = 1, nums) /) )
yvd4 = yvd3 + spacing( xvd )

call unit_test_msg(title,"    xvd  = ", str(xvd) )
call unit_test_msg(title,"    yvd1 = ", str(yvd1), ":  NEAREST( x, 1.0 )" )
call unit_test_msg(title,"    yvd2 = ", str(yvd2), ":  y1 - SPACING( x )" )
call unit_test_msg(title,"    yvd3 = ", str(yvd3), ":  NEAREST( x,-1.0 )" )
call unit_test_msg(title,"    yvd4 = ", str(yvd4), ":  y3 + SPACING( x )" )

call unit_test(title,all([F,F,F,F,F] .eqv. compare_float(xvd,yvd1)),      "Compare_Float(xvd, yvd1)      ")
call unit_test(title,all([T,T,T,T,T] .eqv. compare_float(xvd,yvd1,ulp=2)),"Compare_Float(xvd, yvd1,ulp=2)")
call unit_test(title,all([T,T,T,T,T] .eqv. compare_float(xvd,yvd2)),      "Compare_Float(xvd, yvd2)      ")
call unit_test(title,all([F,F,F,F,T] .eqv. compare_float(xvd,yvd3)),      "Compare_Float(xvd, yvd3)      ")
call unit_test(title,all([T,T,T,T,T] .eqv. compare_float(xvd,yvd3,ulp=2)),"Compare_Float(xvd, yvd3,ulp=2)")
call unit_test(title,all([T,T,T,T,T] .eqv. compare_float(xvd,yvd4)),      "Compare_Float(xvd, yvd4)      ")

call unit_test(title, all([F,F,F,F,F] .eqv. (xvd .equalto. yvd1)), "( xvd .equalto. yvd1 )")
call unit_test(title, all([T,T,T,T,T] .eqv. (xvd .equalto. yvd2)), "( xvd .equalto. yvd2 )")
call unit_test(title, all([F,F,F,F,T] .eqv. (xvd .equalto. yvd3)), "( xvd .equalto. yvd3 )")
call unit_test(title, all([T,T,T,T,T] .eqv. (xvd .equalto. yvd4)), "( xvd .equalto. yvd4 )")

call unit_test(title, all([F,F,F,F,F] .eqv. (xvd .greaterthan. yvd1)), "( xvd .greaterthan. yvd1 )")
call unit_test(title, all([F,F,F,F,F] .eqv. (xvd .greaterthan. yvd2)), "( xvd .greaterthan. yvd2 )")
call unit_test(title, all([T,T,T,T,F] .eqv. (xvd .greaterthan. yvd3)), "( xvd .greaterthan. yvd3 )")
call unit_test(title, all([F,F,F,F,F] .eqv. (xvd .greaterthan. yvd4)), "( xvd .greaterthan. yvd4 )")

call unit_test(title, all([T,T,T,T,T] .eqv. (xvd .lessthan. yvd1)), "( xvd .lessthan. yvd1 )")
call unit_test(title, all([F,F,F,F,F] .eqv. (xvd .lessthan. yvd2)), "( xvd .lessthan. yvd2 )")
call unit_test(title, all([F,F,F,F,F] .eqv. (xvd .lessthan. yvd3)), "( xvd .lessthan. yvd3 )")
call unit_test(title, all([F,F,F,F,F] .eqv. (xvd .lessthan. yvd4)), "( xvd .lessthan. yvd4 )")
   
call unit_test_msg(title, 'matrix')

xad  = reshape((/double_number,double_number+(dten*spacing(double_number))/),(/nums,2/))
yad1 = nearest( xad, reshape((/ ( 1.0_real64, i = 1, nums*2) /),(/nums,2/)) )
yad2 = yad1 - spacing( xad )
yad3 = nearest( xad, reshape((/ (-1.0_real64, i = 1, nums*2) /),(/nums,2/)) )
yad4 = yad3 + spacing( xad )

call unit_test_msg(title,"    xad  = ", str([xad]) )
call unit_test_msg(title,"    yad1 = ", str([yad1]), ":  NEAREST( x, 1.0 )" )
call unit_test_msg(title,"    yad2 = ", str([yad2]), ":  y1 - SPACING( x )" )
call unit_test_msg(title,"    yad3 = ", str([yad3]), ":  NEAREST( x,-1.0 )" )
call unit_test_msg(title,"    yad4 = ", str([yad4]), ":  y3 + SPACING( x )" )

call unit_test(title,all(reshape([F,F,F,F,F,F,F,F,F,F],[nums,2]).eqv.compare_float(xad,yad1)),      "Compare_Float(xad,yad1)      ")
call unit_test(title,all(reshape([T,T,T,T,T,T,T,T,T,T],[nums,2]).eqv.compare_float(xad,yad1,ulp=2)),"Compare_Float(xad,yad1,ulp=2)")
call unit_test(title,all(reshape([T,T,T,T,T,T,T,T,T,T],[nums,2]).eqv.compare_float(xad,yad2)),      "Compare_Float(xad,yad2)      ")
call unit_test(title,all(reshape([F,F,F,F,T,F,F,F,F,F],[nums,2]).eqv.compare_float(xad,yad3)),      "Compare_Float(xad,yad3)      ")
call unit_test(title,all(reshape([T,T,T,T,T,T,T,T,T,T],[nums,2]).eqv.compare_float(xad,yad3,ulp=2)),"Compare_Float(xad,yad3,ulp=2)")
call unit_test(title,all(reshape([T,T,T,T,T,T,T,T,T,T],[nums,2]).eqv.compare_float(xad,yad4)),      "Compare_Float(xad,yad4)      ")

call unit_test(title,all(reshape([F,F,F,F,F,F,F,F,F,F],[nums,2]).eqv.(xad .equalto.yad1)),"( xad .equalto. yad1 )")
call unit_test(title,all(reshape([T,T,T,T,T,T,T,T,T,T],[nums,2]).eqv.(xad .equalto.yad2)),"( xad .equalto. yad2 )")
call unit_test(title,all(reshape([F,F,F,F,T,F,F,F,F,F],[nums,2]).eqv.(xad .equalto.yad3)),"( xad .equalto. yad3 )")
call unit_test(title,all(reshape([T,T,T,T,T,T,T,T,T,T],[nums,2]).eqv.(xad .equalto.yad4)),"( xad .equalto. yad4 )")

call unit_test(title,all(reshape([F,F,F,F,F,F,F,F,F,F],[nums,2]).eqv.(xad .greaterthan. yad1)),"( xad .greaterthan. yad1 )")
call unit_test(title,all(reshape([F,F,F,F,F,F,F,F,F,F],[nums,2]).eqv.(xad .greaterthan. yad2)),"( xad .greaterthan. yad2 )")
call unit_test(title,all(reshape([T,T,T,T,F,T,T,T,T,T],[nums,2]).eqv.(xad .greaterthan. yad3)),"( xad .greaterthan. yad3 )")
call unit_test(title,all(reshape([F,F,F,F,F,F,F,F,F,F],[nums,2]).eqv.(xad .greaterthan. yad4)),"( xad .greaterthan. yad4 )")

call unit_test(title,all(reshape([T,T,T,T,T,T,T,T,T,T],[nums,2]).eqv.(xad .lessthan. yad1)),"( xad .lessthan. yad1 )")
call unit_test(title,all(reshape([F,F,F,F,F,F,F,F,F,F],[nums,2]).eqv.(xad .lessthan. yad2)),"( xad .lessthan. yad2 )")
call unit_test(title,all(reshape([F,F,F,F,F,F,F,F,F,F],[nums,2]).eqv.(xad .lessthan. yad3)),"( xad .lessthan. yad3 )")
call unit_test(title,all(reshape([F,F,F,F,F,F,F,F,F,F],[nums,2]).eqv.(xad .lessthan. yad4)),"( xad .lessthan. yad4 )")

call unit_test_end(title, msg='')

end subroutine test_elemental_double
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_equalto_double()

logical,allocatable :: expected(:)
character(len=*),parameter :: title='equalto double'

   call unit_test_start(title,msg=title)

   select case(i)
   case(1); expected = [F,T,F,T]
   case(2); expected = [F,T,F,T]
   case(3); expected = [F,T,F,T]
   case(4); expected = [F,T,F,T]
   case(5); expected = [F,T,T,T]
   end select

   call unit_test_msg(title, 'expected',expected(1),expected(2),expected(3),expected(4))
   call unit_test_msg(title, ' regular', xd.eq.yd1, xd.eq.yd1, xd.eq.yd2, xd.eq.yd3, xd.eq.yd3, xd.eq.yd4)
   call unit_test(title, expected(1) .eqv. (xd .equalto. yd1), "( x .equalto. y1 ) =",xd .equalto. yd1 ,'test',i)
   call unit_test(title, expected(2) .eqv. (xd .equalto. yd2), "( x .equalto. y2 ) =",xd .equalto. yd2 ,'test',i)
   call unit_test(title, expected(3) .eqv. (xd .equalto. yd3), "( x .equalto. y3 ) =",xd .equalto. yd3 ,'test',i)
   call unit_test(title, expected(4) .eqv. (xd .equalto. yd4), "( x .equalto. y4 ) =",xd .equalto. yd4 ,'test',i)

   call unit_test_end(title, msg='')

end subroutine test_equalto_double
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_equalto_single()

logical,allocatable :: expected(:)
character(len=*),parameter :: title='equalto single'

   call unit_test_start(title,msg=title)

   select case(i)
   case(1); expected = [F,T,F,T]
   case(2); expected = [F,T,F,T]
   case(3); expected = [F,T,F,T]
   case(4); expected = [F,T,F,T]
   case(5); expected = [F,T,T,T]
   end select

   call unit_test_msg(title, 'expected',expected(1),expected(2),expected(3),expected(4))
   call unit_test_msg(title, ' regular', x.eq.y1, x.eq.y1, x.eq.y2, x.eq.y3, x.eq.y3, xd.eq.y4)
   call unit_test(title, expected(1) .eqv. (x .equalto. y1), "( x .equalto. y1 ) =",x .equalto. y1 ,'test',i)
   call unit_test(title, expected(2) .eqv. (x .equalto. y2), "( x .equalto. y2 ) =",x .equalto. y2 ,'test',i)
   call unit_test(title, expected(3) .eqv. (x .equalto. y3), "( x .equalto. y3 ) =",x .equalto. y3 ,'test',i)
   call unit_test(title, expected(4) .eqv. (x .equalto. y4), "( x .equalto. y4 ) =",x .equalto. y4 ,'test',i)

   call unit_test_end(title, msg='')
end subroutine test_equalto_single
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lessthan_double()

logical,allocatable :: expected(:)
character(len=*),parameter :: title='lessthan double'

   call unit_test_start(title,msg=title)

   select case(i)
   case(1); expected=[T,F,F,F]
   case(2); expected=[T,F,F,F]
   case(3); expected=[T,F,F,F]
   case(4); expected=[T,F,F,F]
   case(5); expected=[T,F,F,F]
   end select

   call unit_test_msg(title, 'expected',expected(1),expected(2),expected(3),expected(4))
   call unit_test_msg(title, ' regular', xd.lt.yd1, xd.lt.yd1, xd.lt.yd2, xd.lt.yd3, xd.lt.yd3, xd.lt.yd4)
   call unit_test(title, expected(1) .eqv. (xd .lessthan. yd1), "( x .lessthan. y1 ) =",xd .lessthan. yd1 ,'test',i)
   call unit_test(title, expected(2) .eqv. (xd .lessthan. yd2), "( x .lessthan. y2 ) =",xd .lessthan. yd2 ,'test',i)
   call unit_test(title, expected(3) .eqv. (xd .lessthan. yd3), "( x .lessthan. y3 ) =",xd .lessthan. yd3 ,'test',i)
   call unit_test(title, expected(4) .eqv. (xd .lessthan. yd4), "( x .lessthan. y4 ) =",xd .lessthan. yd4 ,'test',i)

   call unit_test_end(title, msg='')

end subroutine test_lessthan_double
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lessthan_single()

logical,allocatable :: expected(:)
character(len=*),parameter :: title='lessthan single'

   call unit_test_start(title,msg=title)

   select case(i)
   case(1); expected=[T,F,F,F]
   case(2); expected=[T,F,F,F]
   case(3); expected=[T,F,F,F]
   case(4); expected=[T,F,F,F]
   case(5); expected=[T,F,F,F]
   end select

   call unit_test_msg(title, 'expected',expected(1),expected(2),expected(3),expected(4))
   call unit_test_msg(title, ' regular', x.lt.y1, x.lt.y1, x.lt.y2, x.lt.y3, x.lt.y3, x.lt.y4)
   call unit_test(title, expected(1) .eqv. (x .lessthan. y1), "( x .lessthan. y1 ) =",x .lessthan. y1 ,'test',i)
   call unit_test(title, expected(2) .eqv. (x .lessthan. y2), "( x .lessthan. y2 ) =",x .lessthan. y2 ,'test',i)
   call unit_test(title, expected(3) .eqv. (x .lessthan. y3), "( x .lessthan. y3 ) =",x .lessthan. y3 ,'test',i)
   call unit_test(title, expected(4) .eqv. (x .lessthan. y4), "( x .lessthan. y4 ) =",x .lessthan. y4 ,'test',i)

   call unit_test_end(title, msg='')
end subroutine test_lessthan_single
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_greaterthan_double()

logical,allocatable :: expected(:)
character(len=*),parameter :: title='greaterthan double'

   call unit_test_start(title,msg=title)

   select case(i)
   case(1); expected=[F,F,T,F]
   case(2); expected=[F,F,T,F]
   case(3); expected=[F,F,T,F]
   case(4); expected=[F,F,T,F]
   case(5); expected=[F,F,F,F]
   end select

   call unit_test_msg(title, 'expected',expected(1),expected(2),expected(3),expected(4))
   call unit_test_msg(title, ' regular', xd.gt.yd1, xd.gt.yd1, xd.gt.yd2, xd.gt.yd3, xd.gt.yd3, xd.gt.yd4)
   call unit_test(title, expected(1) .eqv. (xd .greaterthan. yd1), "( x .greaterthan. y1 ) =",xd .greaterthan. yd1 ,'test',i)
   call unit_test(title, expected(2) .eqv. (xd .greaterthan. yd2), "( x .greaterthan. y2 ) =",xd .greaterthan. yd2 ,'test',i)
   call unit_test(title, expected(3) .eqv. (xd .greaterthan. yd3), "( x .greaterthan. y3 ) =",xd .greaterthan. yd3 ,'test',i)
   call unit_test(title, expected(4) .eqv. (xd .greaterthan. yd4), "( x .greaterthan. y4 ) =",xd .greaterthan. yd4 ,'test',i)

   call unit_test_end(title, msg='')

end subroutine test_greaterthan_double
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_greaterthan_single()

logical,allocatable :: expected(:)
character(len=*),parameter :: title='greaterthan single'

   call unit_test_start(title,msg=title)

   select case(i)
   case(1); expected=[F,F,T,F]
   case(2); expected=[F,F,T,F]
   case(3); expected=[F,F,T,F]
   case(4); expected=[F,F,T,F]
   case(5); expected=[F,F,F,F]
   end select

   call unit_test_msg(title, 'expected',expected(1),expected(2),expected(3),expected(4))
   call unit_test_msg(title, ' regular', x.gt.y1, x.gt.y1, x.gt.y2, x.gt.y3, x.gt.y3, x.gt.y4)
   call unit_test(title, expected(1) .eqv. (x .greaterthan. y1), "( x .greaterthan. y1 ) =",x .greaterthan. y1 ,'test',i)
   call unit_test(title, expected(2) .eqv. (x .greaterthan. y2), "( x .greaterthan. y2 ) =",x .greaterthan. y2 ,'test',i)
   call unit_test(title, expected(3) .eqv. (x .greaterthan. y3), "( x .greaterthan. y3 ) =",x .greaterthan. y3 ,'test',i)
   call unit_test(title, expected(4) .eqv. (x .greaterthan. y4), "( x .greaterthan. y4 ) =",x .greaterthan. y4 ,'test',i)

   call unit_test_end(title, msg='')
end subroutine test_greaterthan_single
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
end program test_compare_float
!===================================================================================================================================
