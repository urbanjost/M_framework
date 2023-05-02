### REWRITING TO BE MORE PACKAGE-LIKE AND MAYBE MAKE SAFER FOR PARALLEL EXECUTION

Currently this is under rapid modification so if you use this I would
recommend you use a specific commit.

### M_framework
This project concentrates on creating a procedural unit testing framework
based on nothing but standard Fortran that integrates with the Fortran
package manager (fpm).

In its simplest form, given a module with a few procedures in it
```fortran
module mymodule
private
public sample1, sample2
contains

function sample1(in) result(answer)
integer,intent(in) :: in        
integer :: answer
   answer=in*2
end function sample1

function sample2(in) result(answer)
integer,intent(in) :: in        
integer :: answer
   answer=in-10
end function sample2

end module mymodule
```
and a file in the fpm(1) test/ directory called "test_suite_mymodule"
```fortran
program runtest
use M_framework
use mymodule
implicit none
! setup
! one routine for each procedure
call test_sample1()
call test_sample2()
call test_sample3()
! teardown
call unit_check_stop()

contains

subroutine test_sample1()
   call unit_check_start("sample1",msg="")
   call unit_check("sample1", sample1(0)   .eq. 0, "checking = 0")
   call unit_check("sample1", sample1(10)  .gt. 0, "checking > 0")
   call unit_check("sample1", sample1(-10) .lt. 0, "checking < 0")
   call unit_check_done("sample1",msg="")
end subroutine test_sample1

subroutine test_sample2()
   call unit_check_start("sample2",msg="")
   call unit_check("sample2", sample2(10) == 0, " testing if sample2(10) == 0;sample2(10)=",sample2(10))
   call unit_check("sample2", sample2(11) == 0, " testing if sample2(11) == 0;sample2(11)=",sample2(11))
   call unit_check_done("sample2",msg="")
end subroutine test_sample2

subroutine test_sample3()
   call unit_check_start("sample3",msg="")
   ! The test procedures can contain any code required for checking correct behavior.
   ! the simplest check in the unit_check(3f) procedure:
   !!call unit_check("sample3", 0 .eq. 0, "checking something",100/2,3.14,"build a message")
   call unit_check_done("sample3",msg="")
end subroutine test_sample3

end program runtest
```
running "fpm test" would produce
```text
check:       sample1              SUCCESS : checking = 0
check:       sample1              SUCCESS : checking > 0
check:       sample1              SUCCESS : checking < 0
check_done:  sample1              PASSED  : GOOD:        3 BAD:        0 DURATION:00000000000000:
check:       sample2              SUCCESS :  testing if sample2(10) == 0;sample2(10)= 0
check:       sample2              FAILURE :  testing if sample2(11) == 0;sample2(11)= 1
check_done:  sample2              FAILED  : GOOD:        1 BAD:        1 DURATION:00000000000001:
check_done:  sample3              UNTESTED: GOOD:        0 BAD:        0 DURATION:00000000000000:
check_stop:  TALLY                FAILED  : GOOD:        4 BAD:        1 DURATION:00000000000001
STOP 1
```
although there might be one procedure tested per file; or the tests could even be internal to
"mymodule", or the tests might call a system command that enters info into a database and so 
on this is a model I commonly use for basic numeric procedures in particular.

### MOTIVATION
The tools I have been using for unit testing have been scattered within
the GPF (General Purpose Fortran) package.

As I had the GPF on my development platforms it was no problem to
call what was needed for my personal needs, but to be used by github
fpm(1) repositories a more formal structure is needed, simplified and
standardized -- ie.  an fpm(1) package.

1. A standardized set of unit testing tools that is a repository package
   may be used as a dependency in the production of other packages.
   
    + Including unit testing is strongly encouraged for any package,
      particularly as it may be used in programming environments
      the package authors have never developed in.
   
    + The proposed rules for fpm(1) package repositories include that
      the packages themselves only have external dependencies that are
      also repository packages ( of course wrappers of C libraries or
      other existing libraries cannot easily be conformed to this).

2. If a CD/CI script creates a Fortran environment including fpm and just
   calls "fpm test" you can standardize your testing and use the same
   CD/CI scripts for any package.

   See the .github directory in [easy](https://github.com/urbanjost/easy)
   for examples.

## Making a skeleton test for each procedure

The first step is to make a skeleton for a test of each procedure.
Creating unit tests can be cumbersome and the reward for creating them
is often delayed until they uncover a problem you did not expect. 
So to encourage taking the first step there is an included program
called testmake(1). 

For an fpm(1) user a recommended process is to create and/or enter the
test/ directory and use the "unit_check" program. There is a case made for
each procedure or closely related groups of proceducers to have their
own test file a bit later, but for now assume I have the procedures
"a","b", and "c" that I want to create a test for, and that unit_check(1)
has been installed in your path:
```bash
    unit_check a b c >test_suite_mymodule.f90
```
If you then run "fpm test" the skeleton should run indicating the
procedures are not tested.  Change the routines to actually call the
"unit_check" procedures and you have the beginnings of a unit test for
your procedures.

The "unit_check(3f)" procedure in its simplest form takes a string that
is usually the procedure name and a logical expression, along with up
to twenty completely optional intrinsic values which will be used to 
create a message.

The example creates a sample call to unit_check_mode(3f) that you
can ignore for now.

Note that most of the modes allowed in unix_check_mode(3f) can be
overridden on the command line ...

    # options may be specified in NAMELIST input format with no 
    # extraneous spaces on the command line
    fpm test -- flags=100,200,300  
    # a little more Unix-like or MSWindows-like syntax is allowed, as 
    # leading -- or / strings are removed.
    fpm test -- --flags=100,200,300 --keep_going
    fpm test -- /flags=100,200,300 /keep_going=T

# suggest one test per program

There are advantages to each procedure being tested with a seperate
program.

fpm defaults to running all the tests, but can execute subgroups
easily because it can execute a list of tests and the names can
use simple globbing. 

individual procedure tests can be deleted or added or moved easily.

It is easy to debug an individual test in a debugger. For example to run
a test called "crash" with gdb(1) use
```bash
     fpm test --target crash --runner "gdb -ex run --quiet" 
```
To run all the tests in the gdb(1) debugger (you can enter
"q" after each test has run; or enter gdb commands at the prompt):
```bash
     fpm test --target '*' --verbose \
     --runner 'gdb -ex "list, 0" -ex run --quiet --args' \
     -- flags=9997,9998,9999 luns=6 level=3
```
This runs fpm(1) in verbose mode, executes all the test programs leaving
you at the gdb(1) prompt after each program runs, passing a few modes
as arguments.

This is true with other tools that you can use with --runner as well. 
See "fpm help runner" or "fpm manual >manual.txt" for more information.

### testing many procedures in a single file

If it is preferred one program tests multiple procedures the main
disadvantage is that the complete test suite is always run. One of the
uses of the unit_check_flags(:) array is to allow integer values to
be passed at execution time that can be tested to provide conditional
computation.

Of course any user-supplied method such as a file listing what tests to
execute or skip or build the tests conditionally with a preprocessor or
using procedure pointers in clever ways to point to a no-op procedure
are just a few examples of alternate methods.

# Name
M_framework

## Description

Unit testing allows you to automatically confirm changes are acceptable
so you can quickly and confidently make and release changes.

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

It is still broken down into small modules so the general ones can easily
be used from other repositories as well.

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
## Samples
+ comparing files numerically
+ comparing floating point scalar values and arrays
+ using the many Fortran capabilities in expressions (any(3f), all(3f), pack(3f), merge(3f), ...)
+ dealing with program execution being stopped
## 

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
     fpm run unit_check -- a b c 
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
    + [BOOK_M_framework__verify](https://urbanjost.github.io/M_framework/BOOK_M_framework__verify.html)
    + [BOOK_M_framework__approx](https://urbanjost.github.io/M_framework/BOOK_M_framework__approx.html)
    + [BOOK_M_framework__journal](https://urbanjost.github.io/M_framework/BOOK_M_framework__journal.html)
    + [BOOK_M_framework__msg](https://urbanjost.github.io/M_framework/BOOK_M_framework__msg.html)
    + [BOOK_M_framework__help](https://urbanjost.github.io/M_framework/BOOK_M_framework__help.html)

### Developer
   + [ford-generated developers' document](https://urbanjost.github.io/M_strings/fpm-ford/index.html)
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
<!--
====================================================================================================

DOES NOT WORK. 

If you put that in the "fpm.rsp" file as:

     @debug
     options test '*' --verbose --runner 'gdb -ex run --args' -- --flags=9997,9998,9999 --luns=6 

You can then just enter "fpm @debug"

====================================================================================================
-->
