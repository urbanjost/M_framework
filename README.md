### REWRITING TO BE MORE PACKAGE-LIKE AND MAYBE MAKE SAFER FOR PARALLEL EXECUTION

The tools used by my modules for unit testing have been scattered within
the GPF (General Purpose Fortran) package.

Currently this is under rapid modification so if you use this I would
recommend you use a specific version.

This is the beginnings of an actual unit testing package based on the
most-used components so that it can be a registered fpm(1) package
(when version II of that becomes available), as unit testing is a
dependency of almost all my github repositories. As just parts of the
General Fortran Package it was no problem to call these, but for being
used by github repositories intended to be used via fpm(1) it needs more
formal structure.

I still find it useful to break this down into small modules so the 
general ones can easily be used from other repositories as well.

# Name
M_framework

## Description

M_framework(3f)  is a aggregate of modules useful for creating messages,
comparing expected values to results, writing logfiles and playback
journals and performing unit tests. It is composed of the following
individual modules:

 + **M\_framework\_\_msg** is a small module that can convert a list of
   variables of any of the most common default types to a string.

   It performs low-level operations that are often used by other larger
   modules so it is its own module to prevent circular dependencies.

 + **M\_framework\_\_journal__** allows for creating log and journal files

 + **M\_framework\_\_approx** contains routines for rounding and comparing
   floating-point values.

 + **M\_framework\_\_help** provides for creating a standard simple
   interactive help facility

Although of more general use than a dedicated unit test the components
are often used in unit testing in various ways. Only one is dedicated to
providing a unit testing framework:

 + **M\_framework\_\_verify__** contains procedures useful for generating
   unit tests

Since they are procedures they can be compiled in a variety of ways. After
using them for a long time my typical usage has changed completely several
times. 

As part of libGPF (the General Purpose Fortran library) I usually called
these routines from the files that defined the procedures they were using.

While trying to make modules on github with as few dependencies as
possible the tests were often done seperately so modules could be built
without the tests (and the infrastructure required to use them).

I used the optional system command to place results into databases
and SQLite3 files which was great for QA reports, but when using
**M_framework** in a distributed environment I do not use that at all.

Some of the tools I use like a script that runs nm(1) on the object files
to create an initial scaffold calling each routine found is too specific
to my particular platforms currently to distribute; but I find I am much
more likely to generate tests given a skeleton program that keeps reminding
me everything is "UNTESTED". 

Currently, I use this the most with fpm(1) repositories, and almost always
create a file called "test/test_suite_$PACKAGENAME.f90".

I do not create command line options for the program with something
like M_CLI2 but just use the built-in NAMELIST-group based one to select
options occasionally.

So generally I end up with something like:
 + make a subroutine for each component to be tested
 + in each test start it with a call to UNIT_CHECK_START(3f)
 + in each test end it with a call to UNIT_CHECK_DONE(3f)
 + make multiple calls to UNIT_CHECK(3f) in-between
 + call each of the test routines from the main program
 + end the main program with a call to UNIT_CHECK_STOP(3f)

```fortran
program M_test_suite_M_intrinsics
use M_framework, only : unit_check_start,unit_check,unit_check_done, &
                 unit_check_mode, unit_check_level, unit_check_stop
!use M_mymodule
implicit none
double precision,allocatable :: expected(:), answers(:), input(:)
double precision,parameter :: PI=atan(1.0d0)*4
!! setup
!! test each subroutine
   call test_sqrt()
   call test_cos()
   call test_sin()
!! teardown
   call unit_check_stop()
contains

subroutine test_sqrt()
integer :: i
   call unit_check_start('sqrt',msg='')
   input   =[1.0d0,4.0d0,9.0d0]
   expected=[1.0d0,2.0d0,3.0d0]
   answers=[(sqrt(input(i)),i=1,size(input))]
   call unit_check('sqrt', all(expected.eq.answers),&
           'check table of values')
   call unit_check('sqrt', sqrt(25.0d0).eq.5.0d0,&
           'got',sqrt(25.0d0),'expected',5.0d0)
   call unit_check_done('sqrt',msg='')
end subroutine test_sqrt

subroutine test_sin()
   call unit_check_start('sin',msg='')
   call unit_check_done('sin',msg='')
end subroutine test_sin

subroutine test_cos()
   call unit_check_start('cos',msg='')
   call unit_check_done('cos',msg='')
end subroutine test_cos

end program M_test_suite_M_intrinsics
```
The default output looks like this (note if no calls to unit_check routines are made 
between start and done the procedure gets an "UNTESTED" entry to remind you to make
some tests ..).
```text
check:       sqrt                 SUCCESS : check table of values
check:       sqrt                 SUCCESS : got 5.0000000000000000 expected 5.0000000000000000
check_done:  sqrt                 PASSED  : GOOD:        2 BAD:        0 DURATION:00000000000000:
check_done:  cos                  UNTESTED: GOOD:        0 BAD:        0 DURATION:00000000000000:
check_done:  sin                  UNTESTED: GOOD:        0 BAD:        0 DURATION:00000000000000:
check_stop:  TALLY                PASSED  : GOOD:        2 BAD:        0 DURATION:00000000000000
STOP 0
```
And then as you get fancier the other procedures in the framework become useful. See the tests 
for M_time and M_strings for more elaborata examples.

There are options to call a system command and use the initial string as options, to interactively
pause after each check, and to change options like which output file to write on, what error level
to use, and other things I will hopefully solidify and document here.

## Building the Module
A conventional GNU/Linux or Unix install:

```bash
     git clone https://github.com/urbanjost/M_framework.git
     cd M_framework/src
     # change Makefile if not using one of the listed compilers
     
     # for gfortran
     make clean
     make F90=gfortran gfortran
     
     # for ifort
     make clean
     make F90=ifort ifort

     # for nvfortran
     make clean
     make F90=nvfortran nvfortran
```
This will compile the Fortran module and basic test
programs 

Optionally
```bash
```

## Supports FPM ![fpm](docs/images/fpm_logo.gif)

Alternatively, download the github repository and
build it with fpm ( as described at [Fortran Package
Manager](https://github.com/fortran-lang/fpm) )

```bash
     git clone https://github.com/urbanjost/M_framework.git
     cd M_framework
     # make a sample program that calls the test libraries
     fpm run maketest -- a b c 
     # display help on the interactive command options
     fpm test -- --help
     # test the package with itself
     fpm test
```
<!--
  fpm test --compiler nvfortran --flag '-D__NVFORTRAN -Mbackslash'
-->

or just list it as a dependency in your fpm.toml project file.

```toml
     [dependencies]
     M_framework        = { git = "https://github.com/urbanjost/M_framework.git" }
```

## Documentation   ![docs](docs/images/docs.gif)
### User
   
 - man-pages in 
    + [manpages.zip](https://urbanjost.github.io/M_framework/manpages.zip) 
    + [manpages.tgz](https://urbanjost.github.io/M_framework/manpages.tgz) 

 - An [index](https://urbanjost.github.io/M_framework/man3.html) to HTML versions
   of the manpages 

 - single pages that uses javascript to combine all the HTML descriptions
   of the man-pages is at
    + [BOOK_M_framework__verify](https://urbanjost.github.io/M_framework/BOOK_M_framework__verify.html).
    + [BOOK_M_framework__approx](https://urbanjost.github.io/M_framework/BOOK_M_framework__approx.html).
    + [BOOK_M_framework__journal](https://urbanjost.github.io/M_framework/BOOK_M_framework__journal.html).
    + [BOOK_M_framework__msg](https://urbanjost.github.io/M_framework/BOOK_M_framework__msg.html).
    + [BOOK_M_framework__help](https://urbanjost.github.io/M_framework/BOOK_M_framework__help.html).

### Developer
   + [ford-generated developers' document](https://urbanjost.github.io/M_strings/fpm-ford/index.html).
   + [github action status](docs/STATUS.md)

## Additional Directory Descriptions

   - src/ is where the source for the M_framework(3f) module code resides 
   - docs/ contains HTML documentation and the manpage archives 
   - test/ contains simple test programs
   - example/ has demos

## References

 + [fpm(1) registry](https://github.com/fortran-lang/fpm-registry)
 + [Fortran Wiki unit testing list](https://fortranwiki.org/fortran/show/Unit+testing+frameworks)
 + [ford(1)](https://politicalphysicist.github.io/ford-fortran-documentation.html)
