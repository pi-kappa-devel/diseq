# Changes in version 0.1.3
* Patched the `M1mac` additional issues and `macos` compilation errors that the last update revealed.
* Added a compilation flag for availability of `GSL`. The native code can be compiled also in systems without `GSL`, albeit offering an empty shell functionality for the moment.
* The safest approach that I have found to link with `GSL` and `tbb` is including `RcppGSL` and `RcppParallel` as dependencies, despite that none of their functionality is directly used. This is not needed anymore to build the package due to the new `_DISEQ_HAS_GSL_` flag, but it enables testing the behavior of the package when linked with the shared libraries in more target platforms of `rhub`. 

# Test environments 
## Windows
1. (R-devel win-builder.r-project.org) x86_64-w64-mingw32 (64-bit), R Under development (unstable) (2021-01-29 r79900)
2. (R-oldrelease win-builder.r-project.org) x86_64-w64-mingw32 (64-bit), R version 3.6.3 (2020-02-29)
3. (R-release win-builder.r-project.org) x86_64-w64-mingw32 (64-bit), R version 4.0.3 (2020-10-10)
4. (rhub windows-x86_64-devel) Windows Server 2008 R2 SP1, R-devel, 32/64 bit

## Linux

### GCC
5. (rhub x86_64-pc-linux-gnu) Ubuntu Linux 20.04.1 LTS, R-release, GCC, R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
6. (rhub fedora-gcc-devel) Fedora Linux, R-devel, GCC, R Under development (unstable) (2021-01-30 r79911) -- "Unsuffered Consequences"
7. (rhub debian-gcc-devel-nold) Debian Linux, R-devel, GCC, no long double, R Under development (unstable) (2021-01-30 r79911)
8. (rhub rocker-gcc-san) Debian Linux, R-devel, GCC ASAN/UBSAN, R Under development (unstable) (2020-07-31 r78945)
9. (local) Fedora 32, GCC, R version 4.0.3 (2020-10-10)

### LLVM
10. (rhub debian-clang-devel) Debian Linux, R-devel, clang, ISO-8859-15 locale, R Under development (unstable) (2021-01-30 r79911) -- "Unsuffered Consequences"
11. (rhub fedora-clang-devel) Fedora Linux, R-devel, clang, gfortran, R Under development (unstable) (2021-01-28 r79891) -- "Unsuffered Consequences"

## Solaris
12. (rhub solaris-x86-patched) Oracle Solaris 10, x86, 32 bit, R-release, R version 4.0.3 (2020-10-10)

## Macos
13. (rhub macos-highsierra-release-cran) macOS 10.13.6 High Sierra, R-release, CRAN's setup, R version 4.0.3 (2020-10-10)
14. (rhub macos-highsierra-release) macOS 10.13.6 High Sierra, R-release, brew, R version 4.0.3 (2020-10-10)

# Check results
## R CMD check results in (1) -- std=gnu++11, with GSL, no execution header -- 1 Note
Installation time in seconds: 172
Check time in seconds: 403
Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Days since last update: 2

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2307/1914215
    From: man/diseq.Rd
          man/minus_log_likelihood.Rd
    Status: 403
    Message: Forbidden

Found the following (possibly) invalid DOIs:
  DOI: 10.2307/1914215
    From: DESCRIPTION
    Status: Forbidden
    Message: 403
	
Log: https://win-builder.r-project.org/6n2vddlCUGM1/

## R CMD check results in (2) -- std=gnu++11, with GSL, no execution header -- 1 Note
Installation time in seconds: 168
Check time in seconds: 394
Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Days since last update: 2

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2307/1914215
    From: man/diseq.Rd
          man/minus_log_likelihood.Rd
    Status: 403
    Message: Forbidden

Found the following (possibly) invalid DOIs:
  DOI: 10.2307/1914215
    From: DESCRIPTION
    Status: Forbidden
    Message: 403
	
Log: https://win-builder.r-project.org/a9a1sCz5qKyh/

## R CMD check results in (3) -- std=gnu++11, with GSL, no execution header -- 1 Note
The files will be removed after roughly 72 hours.
Installation time in seconds: 174
Check time in seconds: 412
Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Days since last update: 2

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2307/1914215
    From: man/diseq.Rd
          man/minus_log_likelihood.Rd
    Status: 403
    Message: Forbidden

Found the following (possibly) invalid DOIs:
  DOI: 10.2307/1914215
    From: DESCRIPTION
    Status: Forbidden
    Message: 403
	
Log: https://win-builder.r-project.org/596JcrKNGD4E/

## R CMD check results in (4) -- std=gnu++11, with GSL, no execution header -- 1 Note
Build time:	11 minutes 57.8 seconds
Status: 1 NOTE
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Days since last update: 2
Log: https://builder.r-hub.io/status/original/diseq_0.1.3.tar.gz-e22e7ad7fd99461f9bd58d1320fef7da
## R CMD check results in (5) -- std=gnu++11, with GSL, no execution header -- 2 Notes
Build time:	1 hour 24 minutes 28.1 seconds
Status: 2 NOTEs
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Pantelis Karapanagiotis <pikappa.devel@gmail.com>’

Days since last update: 2

* checking installed package size ... NOTE
  installed size is  5.6Mb
  sub-directories of 1Mb or more:
    libs   3.4Mb
    R      1.5Mb
	
Log: https://builder.r-hub.io/status/original/diseq_0.1.3.tar.gz-fe7994eb0f8e4c3fb82d55d5f921c567
## R CMD check results in (6) -- std=c++17, with GSL, with execution header -- 2 Notes
Build time: 36m 33.4s
Status: 2 NOTEs

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Pantelis Karapanagiotis <pikappa.devel@gmail.com>’

Days since last update: 2

* checking installed package size ... NOTE
  installed size is  5.2Mb
  sub-directories of 1Mb or more:
    R      1.5Mb
    libs   3.1Mb
	
Log: https://builder.r-hub.io/status/original/diseq_0.1.3.tar.gz-045798a27ef94b5e9bbecabfd922bb3e

## R CMD check results in (7) -- std=c++17, with GSL, with execution header -- 1 Note
Build time:	1 hour 28 minutes 47.5 seconds
Status: 1 NOTE
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Pantelis Karapanagiotis <pikappa.devel@gmail.com>’

Days since last update: 2

Log: https://builder.r-hub.io/status/original/diseq_0.1.3.tar.gz-a92ad14daf9a472ba1ef5af79ffa4342
## R CMD check results in (8) -- std=c++17, with GSL, with execution header -- Ok
Build time:	2 hours 9.4 seconds

Log: https://builder.r-hub.io/status/original/diseq_0.1.3.tar.gz-53c7f302c530409ea922344770ff33b4

## R CMD check results in (9) -- std=c++17, no GSL, with execution header -- 1 Warning, 2 Notes
── R CMD check results ──────────────────────────────────────── diseq 0.1.3 ────
Duration: 6m 24.7s

❯ checking compiled code ... WARNING
  File ‘diseq/libs/diseq.so’:
    Found ‘abort’, possibly from ‘abort’ (C)
      Object: ‘equilibrium.o’
    Found ‘printf’, possibly from ‘printf’ (C)
      Object: ‘equilibrium.o’
  
  Compiled code should not call entry points which might terminate R nor
  write to stdout/stderr instead of to the console, nor use Fortran I/O
  nor system RNGs.
  
  See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.

❯ checking installed package size ... NOTE
    installed size is  5.2Mb
    sub-directories of 1Mb or more:
      R      1.5Mb
      libs   3.1Mb

❯ checking compilation flags used ... NOTE
  Compilation used the following non-portable flag(s):
    ‘-Werror=format-security’ ‘-Wp,-D_FORTIFY_SOURCE=2’
    ‘-Wp,-D_GLIBCXX_ASSERTIONS’

0 errors ✔ | 1 warning ✖ | 2 notes ✖

## R CMD check results in (10) -- std=c++17, with GSL, with execution header -- 1 Note
Build time:	1 hour 33 minutes 53.9 seconds
Status: 1 NOTE
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Days since last update: 2

Log: https://builder.r-hub.io/status/original/diseq_0.1.3.tar.gz-690ac90aeaa9474eb85940d1935c6377


## R CMD check results in (11) -- std=c++17, with GSL, with execution header -- 1 Note
Build time:	1 hour 23 minutes 48.2 seconds
Status: 1 NOTE
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Pantelis Karapanagiotis <pikappa.devel@gmail.com>’

Days since last update: 2
Log: https://builder.r-hub.io/status/original/diseq_0.1.3.tar.gz-45d41b35c1bd4c34878f52a04e9de49b

## R CMD check results in (12) -- std=gnu++11, with GSL, no execution header -- 1 Warning, 2 Notes
Build time:	38 minutes 19.8 seconds
Status: 1 WARNING, 2 NOTEs
WARNINGS:
* checking top-level files ... WARNING
  Output from running autoreconf:
  /opt/csw/share/aclocal/gtk.m4:7: warning: underquoted definition of AM_PATH_GTK
  /opt/csw/share/aclocal/gtk.m4:7:   run info Automake 'Extending aclocal'
  /opt/csw/share/aclocal/gtk.m4:7:   or see https://www.gnu.org/software/automake/manual/automake.html#Extending-aclocal
A complete check needs the 'checkbashisms' script.
See section ‘Configure and cleanup’ in the ‘Writing R Extensions’
manual.
Files ‘README.md’ or ‘NEWS.md’ cannot be checked without ‘pandoc’ being installed.
NOTES:
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Pantelis Karapanagiotis <pikappa.devel@gmail.com>’

Days since last update: 2

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2307/1914215
    From: man/diseq.Rd
          man/minus_log_likelihood.Rd
    Status: 403
    Message: Forbidden

Found the following (possibly) invalid DOIs:
  DOI: 10.2307/1914215
    From: DESCRIPTION
    Status: Forbidden
    Message: 403
* checking compilation flags used ... NOTE
Compilation used the following non-portable flag(s):
  ‘-march=pentiumpro’

Log: https://builder.r-hub.io/status/original/diseq_0.1.3.tar.gz-ca008ec28f6e410f978a7133bd6ad47d

## R CMD check results in (13) -- std=gnu++11, with GSL, no execution header -- 1 Warning, 1 Note
Build time:	36 minutes 59.9 seconds
Status: 1 WARNING, 1 NOTE
WARNINGS:
* checking top-level files ... WARNING
A complete check needs the 'checkbashisms' script.
See section ‘Configure and cleanup’ in the ‘Writing R Extensions’
manual.
NOTES:
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Pantelis Karapanagiotis <pikappa.devel@gmail.com>’

Days since last update: 2

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2307/1914215
    From: man/diseq.Rd
          man/minus_log_likelihood.Rd
    Status: 403
    Message: Forbidden

Found the following (possibly) invalid DOIs:
  DOI: 10.2307/1914215
    From: DESCRIPTION
    Status: Forbidden
    Message: 403
## R CMD check results in (14) -- std=gnu++11, with GSL, no execution header -- 1 Warning, 1 Note
Build time:	14 minutes 20.1 seconds
Status: 1 WARNING, 1 NOTE
WARNINGS:
* checking top-level files ... WARNING
A complete check needs the 'checkbashisms' script.
See section ‘Configure and cleanup’ in the ‘Writing R Extensions’
manual.
NOTES:
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Pantelis Karapanagiotis <pikappa.devel@gmail.com>’

Days since last update: 2

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2307/1914215
    From: man/diseq.Rd
          man/minus_log_likelihood.Rd
    Status: 403
    Message: Forbidden

Found the following (possibly) invalid DOIs:
  DOI: 10.2307/1914215
    From: DESCRIPTION
    Status: Forbidden
    Message: 403
	
Log: https://builder.r-hub.io/status/original/diseq_0.1.3.tar.gz-f04216f8cfdb4a4681f06422b372e6ee
