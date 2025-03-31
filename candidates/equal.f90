! Test if two reals have the same representation at the current precision
elemental logical(FP_BOOL) function equal(a,b)
real(FP_REAL), intent(in) :: a,b
   equal = abs(a-b)<spacing(merge(a,b,abs(a)<abs(b)))
end function equal

