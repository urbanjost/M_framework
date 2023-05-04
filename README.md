### REWRITING TO BE MORE PACKAGE-LIKE AND MAYBE MAKE SAFER FOR PARALLEL EXECUTION

Currently M_framework(3f)  is under rapid modification so if you use this I would
recommend you use a specific commit or make you own copy. ( 2023-05-03 )

Any feedback (features, performance, ease of use, ...) is appreciated,
particularly in this early development phase.

### Synopsis
Including unit testing is strongly encouraged for any software package,
but particularly when it is to be used in programming environments the
package was not developed in.

This is the typical case for public fpm(1) packages on github.

As a result -- this project concentrates on tools for a procedural
unit testing framework based on nothing but standard Fortran that
specifically can be used as an external fpm(1) dependency for fpm(1)
packages in public github repositories.

That is, it is designed to be integrated with the fpm(1) "test" subcommand
as an external dependency.

That being said, it can be used standalone as well.

Additional motivation for constructing the package is that the proposed
rules for registered fpm(1) package repositories include that 
package candidates themselves only have external dependencies that are also
registered repository packages ( of course wrappers of C libraries or
other existing libraries cannot easily be conformed to this).

Note that If a CD/CI github automated test script creates a Fortran
environment including fpm and just calls "fpm test" you can standardize
your testing and use the same CD/CI scripts for any package.

Basic timing information is produced as well as the expected
pass/fail/skipped report.

M_framework(1) comes with a hook that allows calling your own programs to
integrate into logging tools, report generators, spreadsheets or other
local infrastructure. The example program "bookkeeper" is included that
uses the hook to write CSV, HTML, and NAMELIST files.

Using fpm(1) it is easy to run the tests with the --runner option,
allowing for integration with other utilities such as the GNU debugger
gdb(1), valgrind(1), and other tools.

### Basic output

Using the framework on a simple project and running "fpm test" would produce
something like:
```text
check:       sample1              SUCCESS : checking = 0
check:       sample1              SUCCESS : checking > 0
check:       sample1              SUCCESS : checking < 0
check_done:  sample1              PASSED  : GOOD:        3 BAD:        0 DURATION:00000000000000:
check:       sample2              SUCCESS : testing if sample2(10) == 0;sample2(10)= 0
check:       sample2              FAILURE : testing if sample2(11) == 0;sample2(11)= 1
check_done:  sample2              FAILED  : GOOD:        1 BAD:        1 DURATION:00000000000001:
check_done:  sample3              UNTESTED: GOOD:        0 BAD:        0 DURATION:00000000000000:
check_stop:  TALLY                FAILED  : GOOD:        4 BAD:        1 DURATION:00000000000001
STOP 1
```
although there might be one procedure tested per file; or the tests
could even be internal to "mymodule", or the tests might call a system
command that enters info into a database and so on this is a model that
works well for basic numeric procedures in particular.

We will see there is a lot more to it than this, but even such a simple
report that can be run with a simple command straight in a CLI or batch
environment can be a powerful tool for ensuring any deviations in behavior
are caught quickly and are naturally a part of the development cycle --
make a code change; run "fpm test"; correct any issues detected.

## Making a skeleton test for each procedure

The first step is to make a skeleton for a test of each procedure.
Creating unit tests can be cumbersome and the reward for creating them
is often delayed until they uncover a problem you did not expect.
So to encourage taking the first step there is an included program
called unit_test(1)

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

So for an fpm(1) user a recommended process is to create and/or enter the
test/ directory and use the "unit_test" program. It will make a test
program called "unit_test_$NAME.f90" for each name given on the command
line.

There is a case made for closely related groups of proceducers to share
a single test file that will be made a little bit later. For that, the
command "test_suite" is used instead of "unit_test".

For now lets assume we have or plan to have the procedures
"a","b", and "c" in our package, and that we want to create a test for
each ( and that unit_test(1) has been installed in your path):
```bash
    cd $TOP_OF_FPM_PACKAGE
    cd test
    unit_test a b c 
```
If you then run "fpm test" the skeleton should run indicating the
procedures are not tested.  Change the routines to actually call the
"unit_test" procedure and you have the beginnings of a unit test for
your procedures.

The "unit_test(3f)" procedure in its simplest form takes a string that
is usually the procedure name and a logical expression, along with up
to twenty completely optional intrinsic scalar values which will be used
to create a message.

The example programs also contain a sample call to unit_test_mode(3f)
that you can ignore for now.

Note that most of the modes allowed in unix_check_mode(3f) can be
overridden on the command line ...

    # options may be specified in NAMELIST input format with no
    # extraneous spaces on the command line
    fpm test -- flags=100,200,300 keep_going=T
    # a little more Unix-like or MSWindows-like syntax is allowed, as
    # these are equivalent to the original command ...
    fpm test -- --flags=100,200,300 --keep_going
    fpm test -- /flags=100,200,300 /keep_going=T

There are fancier command line parsers such as M_CLI2 available, but
this simple built-in one works well enough in practice.

# suggest one test per program

There are advantages to each procedure being tested with a seperate
program.

fpm defaults to running all the tests, but can execute subgroups
easily because it can execute a list of tests and the names can
use simple globbing.

individual procedure tests can be deleted or added or moved easily.

It is easier to test with debuggers and other tools like gdb and valgrind
on small units.

This is true with other tools that you can use with --runner as well.
See "fpm help runner" or "fpm manual >manual.txt" for more information.

### testing many procedures in a single file

If it is preferred, one program can test multiple procedures. The main
disadvantage is that the complete test suite is always run. One of the
uses of the unit_test_flags(:) array is to allow integer values to
be passed at execution time that can be tested to provide conditional
testing.

Of course any user-supplied method such as a file listing what tests to
execute or skip or build the tests conditionally with a preprocessor or
using procedure pointers in clever ways to point to a no-op procedure
are just a few examples of alternate methods.

## Unique features: Calling an external program

A unique feature of the M_framework(3f) test harness is a hook to call
a custom program. You can ignore it is there, but it is a very powerful
feature if you want to do bookkeeping on the test results or enter
the results into a tracking tool. This allows you a way to sent alerts
if something fails in automated tests, to create spreadsheets with the
test results, to retain results in a database or SQLite3 file, or make
a custom tool to convert the data to something else like TAP (Test Anywhere
Protocol) reports.

An example program called "bookkeeper" is included that shows how to 
parse the information passed to the program that generates several files:
   
   + bookkeeper.csv
   + bookkeeper.html
   + bookkeeper.nml
   + bookkeeper_clicks.csv
  
To use the defaults you simply enter
```bash
    fpm test -- command=bookkeeper  
```

The CSV files can typically be read directly into a spreadsheet program.

They can also be read using the SQLite3 tool. This is very powerful,
letting you use SQL to select specific data, generate reports, and 
convert the subsquent results to HTML, CSV, flat text and other formats.

The NAMELIST output file is essentially a record of the harness calls
using the standardized NAMELIST format that is almost trivial to read
in with a custom Fortran program. This might be considered a metafile
format for a test run. You can replay the data and do whatever you want
with it including generating alternate output file types; plotting 
timing data and so on.

The HTML file is handy for importing into word processors or viewing
via browsers.

Here is an example SQLite3 Tool input file that can be run with 
```bash
     sqlite3 -batch -init test.sql bookkeeper.db .quit
```
if you have sqlite3(1) installed.

#### importing into SQLlite3
```text
.mode csv
.import bookkeeper.csv unit_check
.schema unit_check
---
--- show all data to show it worked
SELECT * FROM unit_check;
---
--- list unique names
SELECT name FROM unit_check GROUP BY name ORDER BY name ; 
---
--- tally up passed, failed, skipped in a text table
.header on
.mode column unit_check
.width 64 9 9 9 
SELECT name,
--- depending on SQL version a simpler IIF/IF, IFNULL, or TOTAL might be better but this works well
--- and is relatively "standard" as much as something like a standard is actually adherred to in
--- SQL-land.
CASE  sum( passed == 'passed'  ) WHEN NULL THEN 0 ELSE sum ( passed == 'passed')  END  AS 'ok',
CASE  sum( passed == 'failed'  ) WHEN NULL THEN 0 ELSE sum ( passed == 'failed')  END  AS 'not ok',
CASE  sum( passed == 'skipped' ) WHEN NULL THEN 0 ELSE sum ( passed == 'skipped') END  AS 'skip'
FROM unit_check
--- WHERE condition
GROUP BY name
ORDER BY name ; 
---
--- If the table already exists, the sqlite3 tool uses all the rows,
--- including the first row, in the CSV file as the actual data to import.
--- Therefore, you should delete the first row of the CSV file with the
--- header labels if adding to an existing table instead of creating
DROP TABLE IF EXISTS unit_check;
--- CREATE TABLE unit_check(
---   name   TEXT NOT NULL,
---   date   DATE NOT NULL,
---   passed TEXT NOT NULL,
---   msg    TEXT 
--- );
```
The last select generates a little tally table showing for all the runs
in the input DataBaseFile the number of pass, fail, and skips of the 
test names. you could a tally over a particular date range, the last test 
status, out in HTML or CSV or ...
```text
name                                          ok         not ok     skip     
--------------------------------------------  ---------  ---------  ---------
accdig                                        3          0          0        
almost                                        24         0          0        
flush_trail                                   0          0          3        
in_margin                                     30         0          0        
round                                         0          0          3        
set_stdout_lun                                0          0          3        
significant                                   15         1          2        
unit_test                                     0          0          3        
unit_test_bad                                 0          0          3        
unit_test_end                                 0          0          3        
unit_test_good                                0          0          3        
unit_test_start                               0          0          6        
unit_test_stop                                0          0          3        
where_write_message_all                       0          0          3        
write_message_only                            4          2          4        
```

## Unique features: fpm(1), gdb(1), and M_framework(3f) work together

It is easy to debug an individual test in a debugger. For example to run
a test called "crash" with gdb(1) use
```bash
     fpm test --target crash --runner "gdb -ex run --quiet"
```
A more elaborate example passing arguments to M_framework(3f) to change
default behavior to for example write output to stdout instead of stderr
and display compiler version and options and to run all the tests in
the gdb(1) debugger (you can enter "q" after each test has run; or enter
gdb commands at the prompt):
```bash
     fpm test --target '*' --verbose \
     --runner 'gdb -ex "list, 0" -ex run --quiet --args' \
     -- flags=9997,9998,9999 luns=6 level=3
```
This is long enough that an alias or script would facilitate its use, 
with one changing it to use one's favorite options.

## Unique features: fpm(1), gdb(1), and M_framework(3f) work together

If a CD/CI github automated test script creates a Fortran
environment including fpm and just calls "fpm test" you can standardize
your testing and use the same CD/CI scripts for any package. You can
set up "fpm test" without using M_framework(3f) but it has features
that work with in such a batch testing environment.

The .github subdirectory in the package contains examples of just such
files that you can copy as-is into your fpm project. Activate test 
actions in your github repository after adding the .github directory and
your package will be automatically tested on several different platforms
by the scripts. They set up the fpm command and a Fortran programming 
environment and then run "fpm test". 

------------------------------------------------------------------------------------------

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

So generally I end up with something like:
 + make a subroutine for each component to be tested
 + in each test start it with a call to UNIT_CHECK_START(3f)
 + in each test end it with a call to UNIT_CHECK_DONE(3f)
 + make multiple calls to UNIT_CHECK(3f) in-between
 + call each of the test routines from the main program
 + end the main program with a call to UNIT_CHECK_STOP(3f)

Command line options use the built-in NAMELIST-group


```fortran
program M_test_suite_M_intrinsics
use M_framework, only : unit_test_start,unit_test,unit_test_done, &
                 unit_test_mode, unit_test_level, unit_test_stop
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
   call unit_test_stop()
contains
subroutine test_sqrt()
integer :: i
   call unit_test_start('sqrt',msg='')
   input   =[1.0d0,4.0d0,9.0d0]
   expected=[1.0d0,2.0d0,3.0d0]
   answers=[(sqrt(input(i)),i=1,size(input))]
   call unit_test('sqrt', all(expected.eq.answers),&
           'check table of values')
   call unit_test('sqrt', sqrt(25.0d0).eq.5.0d0,&
           'got',sqrt(25.0d0),'expected',5.0d0)
   call unit_test_done('sqrt',msg='')
end subroutine test_sqrt

subroutine test_sin()
   call unit_test_start('sin',msg='')
   call unit_test_done('sin',msg='')
end subroutine test_sin

subroutine test_cos()
   call unit_test_start('cos',msg='')
   call unit_test_done('cos',msg='')
end subroutine test_cos

end program M_test_suite_M_intrinsics
```
The default output looks like this (note if no calls to unit_test routines are made
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


## Supports FPM ![fpm](docs/images/fpm_logo.gif)

To download the github repository and build it with fpm(1) 
and create
a test fpm(1) project:

```bash
#!/bin/bash
# first you need to install a few example programs
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
# so now to to run the default tests is as simple as
fpm test
# run just one test
fpm test unit_test_a
# run tests using globbing; eg. all tests beginning with "unit_"
fpm test 'unit_*'
# display help on the interactive command options
fpm test -- --help

# you can pass parameters and/or change the unit_test_mode(3f)
# calls to change some of the test behavior
```
So once you want to use this on your own projects, you would normally
just add M_framework(3f) as a developer dependency
in your fpm.toml project file and start making tests that call M_framework(3f).

The optional programs demonstrated ("unit_test", "test_suite") are just simple
stand-alone programs that make skeleton programs to run tests that you can 
customize (and rename too to avoid confusion).

```toml
     [dev-dependencies]
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
 + See the .github directory in [easy](https://github.com/urbanjost/easy)
 + for examples of CD/CI scripts that assume your package can be tested with an "fpm test" command.
 + examples that use M_framework in github fpm packages:
     * [M_strings](https://github.com/urbanjost/M_strings)
     * [M_strings](https://github.com/urbanjost/M_process)

 + [Fortran Package Manager](https://github.com/fortran-lang/fpm)
 + [fpm(1) registry](https://github.com/fortran-lang/fpm-registry)
 + [Fortran Wiki: unit testing list](https://fortranwiki.org/fortran/show/Unit+testing+frameworks)
 + [ford(1)](https://politicalphysicist.github.io/ford-fortran-documentation.html) for generating documentation for your projects
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
