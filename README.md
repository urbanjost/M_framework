[![](docs/images/framework.gif)](https://urbanjost.github.io/M_framework/fpm-ford/index.html)
# [M\_framework](https://urbanjost.github.io/M_framework/man3.html)

## Synopsis
M\_framework(3f) is an aggregate of Fortran modules useful for creating
and performing unit tests for Fortran.

The support modules are useful on their own for terminal messages,
comparing expected values to results, writing logfiles and playback
journals.

It supports easy integration with the fpm(1) "test" subcommand,
in particular.

 + basic timing is included
 + messages can be composed almost like list-directed I/O when calling the
   unit test procedures.
 + hooks are provided to external local applications
   M\_framework(3f) comes with a hook that allows calling your own programs to
   integrate with local logging tools, report generators, spreadsheets or other
   local infrastructure. The example program "bookkeeper" is included that
   uses the hook to write example report data files:

    + [CSV](https://urbanjost.github.io/M_framework/bookkeeper.csv),
    + [HTML](https://urbanjost.github.io/M_framework/bookkeeper.html)
    + [NAMELIST](https://urbanjost.github.io/M_framework/bookkeeper.nml)

   The hook can call any local program with an interface similar to
   "bookkeeper".  Modify the program for local needs such as sending e-mail
   alerts and so on without having to change the tests.

 + designed for integration with fpm(1).
   In conjunction with fpm(1) it is easy to run the tests with the --runner
   option, allowing for integration with other utilities as well such as
   the GNU debugger gdb(1), valgrind(1), and other tools.

### programs included to generate skeleton test program
Example programs are provided to create unit test skeleton programs to
ease usage.

### Easily used with github CD/CI
Example CD/CI scripts that can be used with github are in the .github/
directory that assume your tests can by run by using "fpm test".

```fortran
program M_test_suite_M_intrinsics
use,intrinsic :: iso_fortran_env, only : &
& stderr=>ERROR_UNIT, stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT
!
use M_framework, only : unit_test_start,unit_test,unit_test_end, &
                 unit_test_mode, unit_test_level, unit_test_stop
use M_framework, only : CHECK_PREFIX ! change column one of messages
!
!use M_mymodule ! load any modules you will be testing
implicit none
double precision,allocatable :: expected(:), answers(:), input(:)
double precision,parameter :: PI=atan(1.0d0)*4
!! setup
   !---------------------------------------------------
   !OPTIONAL:
   !  values used in prefix column for various messages
   !  the default is to set them all to the basename of
   !  the executable running tests, but they can be 
   !  altered. For example:
   CHECK_PREFIX=prefix(                  &
    check_MSG    =  'check_msg:   ', &
    check        =  'check:       ', &
    check_START  =  'check_start: ', &
    check_STOP   =  'check_stop:  ', &
    check_END    =  'check_end:   '  &
   )
   !---------------------------------------------------
   !OPTIONAL: 
   ! the options available at run-time on the command
   ! line can have their defaults selected. See the 
   ! man-page for the procedure for details.
   call unit_check_mode(
     ( keep_going=.true. ,
     flags=[character(len=0) ::], 
     luns=[stdout], 
     command, &
     brief=.false. ,
     interactive=.false. ,
     CMDLINE='',
     debug=.false. ,
     match
     )
   !---------------------------------------------------
!! test each subroutine
   call test_sqrt()
   call test_cos()
   call test_sin()
!! teardown
   call unit_test_stop()
contains
subroutine test_sqrt()
integer :: i
   call unit_test_start('sqrt',msg='calculate the square root')
   input   =[1.0d0,4.0d0,9.0d0]
   expected=[1.0d0,2.0d0,3.0d0]
   answers=[(sqrt(input(i)),i=1,size(input))]
   call unit_test('sqrt', all(expected.eq.answers),&
      & 'check table of values')
   call unit_test('sqrt', sqrt(25.0d0).eq.5.0d0,&
      & 'got',sqrt(25.0d0),'expected',5.0d0)
   call unit_test_end('sqrt',msg='')
end subroutine test_sqrt

subroutine test_sin()
   call unit_test_start('sin',msg='calculate the sine of a value')
   call unit_test_end('sin',msg='')
end subroutine test_sin

subroutine test_cos()
   call unit_test_start('cos',msg='calculate the cosine of a value')
   call unit_test_end('cos',msg='')
end subroutine test_cos

end program M_test_suite_M_intrinsics
```

Example output files from an fpm(1) package that uses
M_framework illustrate the different types of output that can be 
generated.
 + [CSV](https://urbanjost.github.io/M_framework/bookkeeper.csv).
   Generally. comma-separated files can be read directly into spreadsheet
   programs, sqlite3, and several common databases.
 + [CSV runtimes](https://urbanjost.github.io/M_framework/bookkeeper_clicks.csv)
   Another CSV file that is a record of the runtimes between a
   unit test start and end.
 + [HTML](https://urbanjost.github.io/M_framework/bookkeeper.html)
   An example of a formatted report that can be displayed in a browser.
 + [NAMELIST](https://urbanjost.github.io/M_framework/bookkeeper.nml).
   Essentially this is a metafile that records the unit test calls.
   It is very easy for a custom Fortran program to read back a NAMELIST
   file and generate custom outputs instead of modifying bookkeeper(1).

The bookkeeper(1) program is an example program that is expected to
be customized. It provides for parsing the parameters passed to a
M_frame external program.

------------------------------------------------------------------------------------------

## Supports FPM ![fpm](docs/images/fpm_logo.gif)

The impatient can try this, assuming git(1) and fpm(1) are installed.

Download the github repository and build it with fpm(1) 
and create a test fpm(1) project:

```bash
#!/bin/bash
# first you need to install a few example programs
cd /tmp
git clone https://github.com/urbanjost/M_framework.git
cd M_framework
# install the "unit_test", "bookkeeper", and "test_suite"
# example programs; assuming the default install puts
# them in your path:
fpm install
# "fpm help install" describes how to customize where the
# programs are installed.
#
# go to your fpm package test/ directory.
# here, we will make one up
fpm new /tmp/tryit  # create test project
cd /tmp/tryit/test
# so lets say you plan on adding procedures "a","b",and "c" to your src/tryit.f90
# project code. Set up individual skeleton tests for each procedure.
unit_test a b c                         # a file for each test
test_suite a b c > test_suite_tryit.f90 # or a single file
cd ..  # go to the top of the project 
# add M_framework to the developer dependencies
cat >> fpm.toml <<\EOF
[dev-dependencies]
M_framework    = { git = "https://github.com/urbanjost/M_framework.git" }
EOF
#
# test the package 
fpm test
# if will say the procedures are untested. Put real calls
# in to unit_test(3f) and see how the default report 
# changes
#
# so now to run the default tests is as simple as
fpm test
# run just one test
fpm test unit_test_a
# run tests using globbing; eg. all tests beginning with "unit\_"
fpm test 'unit_*'
# display help on the interactive command options
fpm test -- --help

# you can pass parameters and/or change the unit_test_mode(3f)
# calls to change some of the test behavior
```
So once you want to use this on your own projects, you would normally
just add M\_framework(3f) as a developer dependency
in your fpm.toml project file and start making tests that call M\_framework(3f).

The optional programs demonstrated ("unit\_test", "test\_suite") are just simple
stand-alone programs that make skeleton programs to run tests that you can 
customize (and rename too to avoid confusion).
```bash
# some useful things to try. Check out the man-pages for all the unit_test_*(3f)
# procedures.
# Then look at
unit_test --help
fpm test -- help
# run the demo bookkeeper script
fpm test -- command=bookkeeper
# and look at the bookkeeper*.* files in the top of the project
```
## Documentation   ![docs](docs/images/docs.gif)

### User

 - An [index](https://urbanjost.github.io/M_framework/man3.html) to HTML versions
   of the manpages

 - single pages that uses javascript to combine all the HTML descriptions
   of the man-pages is at
    + [BOOK_M_framework](https://urbanjost.github.io/M_framework/BOOK_M_framework.html)
    + [BOOK_M_framework__verify](https://urbanjost.github.io/M_framework/BOOK_M_framework__verify.html)
    + [BOOK_M_framework__approx](https://urbanjost.github.io/M_framework/BOOK_M_framework__approx.html)
    + [BOOK_M_framework__journal](https://urbanjost.github.io/M_framework/BOOK_M_framework__journal.html)
    + [BOOK_M_framework__msg](https://urbanjost.github.io/M_framework/BOOK_M_framework__msg.html)

 - man-pages in
    + [manpages.zip](https://urbanjost.github.io/M_framework/manpages.zip)
    + [manpages.tgz](https://urbanjost.github.io/M_framework/manpages.tgz)

### Developer
   + [ford-generated developers' document](https://urbanjost.github.io/M_framework/fpm-ford/index.html)
   + [github action status](docs/STATUS.md)

## Additional Directory Descriptions

   - src/ is where the source for the M_framework(3f) module code resides
   - docs/ contains HTML documentation and the manpage archives
   - test/ contains simple test programs
   - example/ has demos

## References
 + See the .github directory in [easy](https://github.com/urbanjost/easy)
 + for examples of CD/CI scripts that assume your package can be tested with an "fpm test" command.
 + examples that use M_framework in github fpm packages:
     * [M_strings](https://github.com/urbanjost/M_strings)
     * [M_process](https://github.com/urbanjost/M_process)

   These packages used a different reincarnation of the testing harness and are in the process of
   being changed to use M_framework(3f) more appropriately, but still contain some useful examples.

 + [Fortran Package Manager](https://github.com/fortran-lang/fpm)
 + [fpm(1) registry](https://github.com/fortran-lang/fpm-registry)
 + [Fortran Wiki: unit testing list](https://fortranwiki.org/fortran/show/Unit+testing+frameworks)
 + [ford(1)](https://politicalphysicist.github.io/ford-fortran-documentation.html) for generating documentation for your projects

## Note
M\_framework(3f) is subject to interface changes so the generally recommended
practice of using a specific commit when using it an an external
fpm(1) dependency is highly encouraged.

Any feedback (features, performance, ease of use, ...) is appreciated,
particularly in the ongoing development phase.
<!--
====================================================================================================

DOES NOT WORK.

If you put that in the "fpm.rsp" file as:

     @debug
     options test '*' --verbose --runner 'gdb -ex run --args' -- --flags=9997,9998,9999 --luns=6

You can then just enter "fpm @debug"

====================================================================================================
-->

<!--
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
-->
<!--
  fpm test --compiler nvfortran --flag '-D__NVFORTRAN -Mbackslash'
-->
<!--
Now add some real calls to unit_test(3f) in the new test files as you
develop the new project. Make sure to run "unit_test name" when you add
a new public procedure.
-->
<!--
I used the optional system command to place results into databases
and SQLite3 files which was great for QA reports, but when using
**M_framework** in a distributed environment I do not use that at all.

Some of the tools I use like a script that runs nm(1) on the object files
to create an initial scaffold calling each routine found is too specific
to my particular platforms currently to distribute; but I find I am much
more likely to generate tests given a skeleton program that keeps reminding
me everything is "UNTESTED".
-->
<!--
Command line options use the built-in NAMELIST-group
See the tests for M_time and M_strings for more elaborate examples.
## Samples
+ comparing files numerically
+ comparing floating point scalar values and arrays
+ using the many Fortran capabilities in expressions (any(3f), all(3f), pack(3f), merge(3f), ...)
+ dealing with program execution being stopped
-->
<!--
although there might be one procedure tested per file; or the tests
could even be internal to "mymodule", or the tests might call a system
command that enters info into a database and so on this is a model that
works well for basic numeric procedures in particular.
-->
<!--
There can be a lot more to it than this, but even such a simple report
generated with a simple command in a CLI or batch environment can be a
powerful tool for ensuring any deviations in behavior are caught quickly
and that testing becomes a natural part of the development cycle --
make a code change; run "fpm test"; correct any issues detected.

## Making a skeleton test for each procedure

The first step is to make a skeleton for a test of each procedure.
Creating unit tests can be cumbersome and the reward for creating them
is often delayed until they uncover a problem you did not expect.
So to encourage taking the first step there is an included program
called unit_test(1).

Initially autodiscovery was used to look through the code for procedures
called "test_suite_NAME" and a test program was created on the fly;
where the test procedures were typically included in the same files as
the procedures they tested. But when many packages are used in the tests
this quickly leads to the core package having many external dependencies
used only for testing. So although this approach works well in a stable
custom environment it complicates the distribution of fpm packages. And
the autodiscovery tools considered so far have a lot of infrastructure
requirements and/or are very system dependent. So the decision was made
to just use standard Fortran programs that make it very easy to generate
test program skeletons instead.
-->
<!--
Of course any user-supplied method such as a file listing which tests to
execute or skip or build the tests conditionally with a preprocessor or
using procedure pointers in clever ways to point to a no-op procedure
are just a few examples of alternate methods.
>
