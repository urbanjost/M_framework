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
```fortran
```

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
    + [BOOK_M_framework](https://urbanjost.github.io/M_framework/BOOK_M_framework__verify.html).
    + [BOOK_M_framework](https://urbanjost.github.io/M_framework/BOOK_M_framework__approx.html).
    + [BOOK_M_framework](https://urbanjost.github.io/M_framework/BOOK_M_framework__journal.html).
    + [BOOK_M_framework](https://urbanjost.github.io/M_framework/BOOK_M_framework__msg.html).
    + [BOOK_M_framework](https://urbanjost.github.io/M_framework/BOOK_M_framework__help.html).

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

