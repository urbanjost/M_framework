
#!/bin/bash
cd $(dirname $0)
html2f90 M_Compare_Float_Numbers.HTML|f90cpp|prep F90 TESPRG90  > ../M_Compare_Float_Numbers.f90
#	|f90cpp $(INC) -D$(TARGET_REV) -D$(MYTARGET) $(ALLEXTENSIONS) >$(TMPDIR)/$(*F).f90.tmp
cp ../../docs/man3.html ../../docs/index.html
cp ../../docs/BOOK_M_Compare_Float_Numbers.html ../../docs/index.html
