# Changes in version 0.1.2
- Added configuration file for compiling in various environments. Removed C++20 dependencies. Additionally tested in three environments that use the clang compiler. The results can be found in (9), (10), (11). Also tested on Solaris and macos, as can be seen in (8), (12), and (13). Sorry for the short time between the updates. This version is patching the compilation problems of version 0.1.1.

# Test environments 
1. (R-devel win-builder.r-project.org) x86_64-w64-mingw32 (64-bit), R Under development (unstable) (2021-01-25 r79883)
2. (R-oldrelease win-builder.r-project.org) x86_64-w64-mingw32 (64-bit), R version 3.6.3 (2020-02-29)
3. (R-release win-builder.r-project.org) x86_64-w64-mingw32 (64-bit), R version 4.0.3 (2020-10-10)
4. (rhub windows-x86_64-devel) Windows Server 2008 R2 SP1, R-devel, 32/64 bit
5. (rhub x86_64-pc-linux-gnu) Ubuntu Linux 16.04 LTS, R-release, GCC, R version 3.6.1 (2019-07-05)
6. (rhub fedora-gcc-devel) Fedora Linux, R-devel, GCC, R Under development (unstable) (2021-01-28 r79891) -- "Unsuffered Consequences"
7. (rhub debian-gcc-devel-nold) Debian Linux, R-devel, GCC, no long double, R Under development (unstable) (2021-01-25 r79883)
8. (rhub rocker-gcc-san) Debian Linux, R-devel, GCC ASAN/UBSAN, R Under development (unstable) (2020-07-31 r78945)
9. (rhub macos-highsierra-release-cran) macOS 10.13.6 High Sierra, R-release, CRAN's setup, R version 4.0.3 (2020-10-10)
10. (rhub debian-clang-devel) Debian Linux, R-devel, clang, ISO-8859-15 locale, 
11. (rhub fedora-clang-devel) Fedora Linux, R-devel, clang, gfortran, R Under development (unstable) (2021-01-28 r79891) -- "Unsuffered Consequences"
12. (rhub solaris-x86-patched) Oracle Solaris 10, x86, 32 bit, R-release, R version 4.0.3 (2020-10-10)
13. (rhub macos-highsierra-release) macOS 10.13.6 High Sierra, R-release, brew, R version 4.0.3 (2020-10-10)

# Check results
## R CMD check results in (1) -- 1 Note
Installation time in seconds: 169
Check time in seconds: 405
Status: 1 NOTE
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Days since last update: 2
Log: https://win-builder.r-project.org/yvL4483PvvAu

## R CMD check results in (2) -- 1 Note
Installation time in seconds: 172
Check time in seconds: 375
Status: 1 NOTE
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Days since last update: 2
Log: https://win-builder.r-project.org/kB5W6rPL8gBj/00check.log

## R CMD check results in (3) -- 1 Note
Installation time in seconds: 169
Check time in seconds: 409
Status: 1 NOTE
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Days since last update: 2
Log:https://win-builder.r-project.org/R70oFl8FBwb2/00check.log

## R CMD check results in (4) -- 1 Note
Build time:	11 minutes 59.2 seconds
NOTES:
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Days since last update: 2
Log: https://builder.r-hub.io/status/original/diseq_0.1.2.tar.gz-01187a78627c43c691eb43c7b8931b6b.

## R CMD check results in (5) -- 2 Notes
Build time:	1 hour 45 minutes 52.3 seconds
Status: 2 NOTES
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Pantelis Karapanagiotis <pikappa.devel@gmail.com>’

Days since last update: 2

* checking installed package size ... NOTE
  installed size is  5.5Mb
  sub-directories of 1Mb or more:
    libs   3.4Mb
    R      1.5Mb
Log: https://builder.r-hub.io/status/original/diseq_0.1.2.tar.gz-7643b36176184ae3834f11b2c8b61cbd


## R CMD check results in (6) -- 2 Notes
Build time:	1 hour 42 minutes 28.9 seconds
Status: 2 NOTES
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Pantelis Karapanagiotis <pikappa.devel@gmail.com>’

Days since last update: 2

* checking installed package size ... NOTE
  installed size is  5.1Mb
  sub-directories of 1Mb or more:
    R      1.5Mb
    libs   2.9Mb
Log: https://builder.r-hub.io/status/original/diseq_0.1.2.tar.gz-48af5610bb0e479d9e9b2ab7dd78acf0

## R CMD check results in (7) -- 1 Note
Build time:	1 hour 59 minutes 49.9 seconds
Status: 1 NOTE
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Pantelis Karapanagiotis <pikappa.devel@gmail.com>’

Days since last update: 2
Log: https://builder.r-hub.io/status/original/diseq_0.1.2.tar.gz-8e3cfc6d679f401fa538c772cb51ef9a

## R CMD check results in (8) -- Ok
Build time:	1 hour 14 minutes 22.6 seconds
Status: Ok... but I get

code for methods in class "Rcpp_cpp_equilibrium_model" was not checked for suspicious field assignments (recommended package 'codetools' not available?)

Log: https://builder.r-hub.io/status/original/diseq_0.1.2.tar.gz-5c08ea669fdf4e9a80e41ff216fb1434

## R CMD check results in (9) -- 1 Warning, 1 Note
Build time:	29 minutes 23 seconds
Status: 1 WARNING, 1 NOTE
WARNINGS:
* checking top-level files ... WARNING
A complete check needs the 'checkbashisms' script.
See section ‘Configure and cleanup’ in the ‘Writing R Extensions’
manual.
NOTES:
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Pantelis Karapanagiotis <pikappa.devel@gmail.com>’

Days since last update: 3

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
Log: https://builder.r-hub.io/status/original/diseq_0.1.2.tar.gz-3bfeb509eaa644c79fdfe5393a01d04d
	
## R CMD check results in (10) -- 1 Note
Build time:	1 hour 59 minutes 47 seconds
NOTES:
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Days since last update: 3
Log: https://builder.r-hub.io/status/original/diseq_0.1.2.tar.gz-43e4dd7fcefb4dfdbf7c609e397b885f

## R CMD check results in (11) -- 1 Note
Build time:	1 hour 47 minutes 33.5 seconds
NOTES:
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Pantelis Karapanagiotis <pikappa.devel@gmail.com>’

Days since last update: 2
Log: https://builder.r-hub.io/status/original/diseq_0.1.2.tar.gz-82a7da559de34d03b463fa422ea95e9c

## R CMD check results in (12) -- 1 Warning, 2 Notes
Build time:	34 minutes 0.1 seconds
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
Log: https://builder.r-hub.io/status/original/diseq_0.1.2.tar.gz-1fdd7fbb720747d6a494b6ad8e2baece

## R CMD check results in (13) -- 1 Warning, 1 Note
Build time:	12 minutes 12 seconds
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
Log: https://builder.r-hub.io/status/original/diseq_0.1.2.tar.gz-31e82d10978e4966825e58857402ace6
