#!/bin/bash
cd $(dirname $0)
export GITHUB=TRUE
(
export PREP_DOCUMENT_DIR=$(pwd)
#prep F90 --comment doxygen --verbose -i ../../app/source/unique.[fF][fF] -o ../../app/unique.f90
export DEMO_OUTDIR=../../example/
export DEMO_SUBDIR=FALSE
GPF_build_module M_units M_constants
)
#cp ../../docs/man3.html ../../docs/index.html
#cp ../../docs/BOOK_M_units.html ../../docs/index.html
#ccall ../../test/test_suite_M_units.[fF]90
exit
