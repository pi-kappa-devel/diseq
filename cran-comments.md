# Changes in version 0.2.1

* Re-factored gradient and likelihood calculation sources.
 - Removed partial derivatives slots that are not used besides for calculating gradients from model classes.
 - Using shorter variable names in derivative's calculations.
* Removed get prefix from access functions to reduce verbosity.
* Improved estimation option handling.
 - Added validation functions for estimation input variables `gradient`, `hessian`, and `standard_errors`. 
 - Introduced shorter input variable names and enumeration choices.
* Added houses dataset.
* Corrected bug in initialization of indicator variables.

# Test environments 
## Windows
1. (R-devel win-builder.r-project.org) x86_64-w64-mingw32 (64-bit), R Under development (unstable) (2021-02-27 r80043)
2. (R-oldrelease win-builder.r-project.org) x86_64-w64-mingw32 (64-bit), R version 3.6.3 (2020-02-29)
3. (R-release win-builder.r-project.org) x86_64-w64-mingw32 (64-bit), R version 4.0.4 (2021-02-15)
4. (rhub windows-x86_64-devel) Windows Server 2008 R2 SP1, R-devel, 32/64 bit

## Linux

### GCC
5. (rhub x86_64-pc-linux-gnu) Ubuntu Linux 20.04.1 LTS, R-release, GCC, R version 4.0.5 (2021-03-31) -- "Shake and Throw"
6. (rhub debian-gcc-devel) Debian Linux, R-devel, GCC, R Under development (unstable) (2020-07-31 r78945) -- "Unsuffered Consequences"
7. (rhub debian-gcc-devel-nold) Debian Linux, R-devel, GCC, no long double, R Under development (unstable) (2021-04-13 r80163) -- "Unsuffered Consequences"
8. (rhub rocker-gcc-san) Debian Linux, R-devel, GCC ASAN/UBSAN, R Under development (unstable) (2020-07-31 r78945) -- "Unsuffered Consequences"
9. (local) Fedora 32, GCC, R version 4.0.4 (2021-02-15)

### LLVM
10. (rhub debian-clang-devel) Debian Linux, R-devel, clang, ISO-8859-15 locale, R Under development (unstable) (2021-04-13 r80163) -- "Unsuffered Consequences"
11. (rhub fedora-clang-devel) Fedora Linux, R-devel, clang, gfortran, R Under development (unstable) (2021-04-13 r80163) -- "Unsuffered Consequences"

## Solaris
12. (rhub solaris-x86-patched) Oracle Solaris 10, x86, 32 bit, R-release, R version 4.0.4 (2021-02-15)

## Macos
13. (rhub macos-highsierra-release-cran) macOS 10.13.6 High Sierra, R-release, CRAN's setup, R version 4.0.5 (2021-03-31)
14. (rhub macos-highsierra-release) macOS 10.13.6 High Sierra, R-release, brew, R version 4.0.5 (2021-03-31)

# Check results
## R CMD check results in (1) -- std=gnu++11, with GSL, no execution header -- Error
Could not upload package:
ERROR: Access to the path 'C:\Inetpub\ftproot\R-release\diseq_0.2.1.tar.gz' is denied

## R CMD check results in (2) -- std=gnu++11, with GSL, no execution header -- Error
Could not upload package:
ERROR: Access to the path 'C:\Inetpub\ftproot\R-release\diseq_0.2.1.tar.gz' is denied

## R CMD check results in (3) -- std=gnu++11, with GSL, no execution header -- Error
Could not upload package:
ERROR: Access to the path 'C:\Inetpub\ftproot\R-release\diseq_0.2.1.tar.gz' is denied

## R CMD check results in (4) -- std=gnu++11, with GSL, no execution header -- Ok
Build time:	11 minutes 4.7 seconds

Log: https://builder.r-hub.io/status/original/diseq_0.2.1.tar.gz-ba6fe85c2aee416a85b33275cf184c23

## R CMD check results in (5) -- std=gnu++11, with GSL, no execution header -- Ok
Build time:	24 minutes 6.2 seconds

Log: https://builder.r-hub.io/status/original/diseq_0.2.1.tar.gz-2d53f4b3ad4f4418a544902d5f3a6a05
## R CMD check results in (6) -- std=c++17, with GSL, with execution header -- Ok
Build time:	47 minutes 30.3 seconds

Log: https://builder.r-hub.io/status/original/diseq_0.2.1.tar.gz-344946719dc24d7e88c5bd0b7a92a92f
## R CMD check results in (7) -- std=c++17, with GSL, with execution header -- 1 Note
Build time:	38 minutes 36.8 seconds

* checking for future file timestamps ... NOTE
unable to verify current time

Log: https://builder.r-hub.io/status/original/diseq_0.2.1.tar.gz-104a52973156417c8e69c87c83dea484
## R CMD check results in (8) -- std=c++17, with GSL, with execution header -- Ok
Build time:	46 minutes 15.6 seconds

Log: https://builder.r-hub.io/status/original/diseq_0.2.1.tar.gz-54c86ae8bb554af7a04cd02f9898630c
## R CMD check results in (9) -- std=c++17, with GSL, with execution header -- 1 Warning, 2 Notes
── R CMD check results ──────────────────────────────────────── diseq 0.2.1 ────
Duration: 4m 42.6s

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
    installed size is  5.0Mb
    sub-directories of 1Mb or more:
      libs   3.6Mb

❯ checking compilation flags used ... NOTE
  Compilation used the following non-portable flag(s):
    ‘-Werror=format-security’ ‘-Wp,-D_FORTIFY_SOURCE=2’
    ‘-Wp,-D_GLIBCXX_ASSERTIONS’

0 errors ✔ | 1 warning ✖ | 2 notes ✖

## R CMD check results in (10) -- std=c++17, with GSL, with execution header -- 2 Notes
Build time:	30 minutes 38.8 seconds

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2307/1913181
    From: man/houses.Rd
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.2307/1914215
    From: man/diseq.Rd
          man/houses.Rd
          man/minus_log_likelihood.Rd
    Status: 403
    Message: Forbidden

Found the following (possibly) invalid DOIs:
  DOI: 10.2307/1914215
    From: DESCRIPTION
    Status: Forbidden
    Message: 403

* checking for future file timestamps ... NOTE
unable to verify current time

Log: https://builder.r-hub.io/status/original/diseq_0.2.1.tar.gz-0b412a83184b4282a063a63549be8a12
## R CMD check results in (11) -- std=c++17, with GSL, with execution header -- Ok
Build time:	25 minutes 16.4 seconds

Log: https://builder.r-hub.io/status/original/diseq_0.2.1.tar.gz-cc7ff88b46714d1697d1b4fd3679440d

## R CMD check results in (12) -- std=gnu++11, with GSL, no execution header -- Error
      -----------------------------------
ERROR: dependency ‘RcppParallel’ is not available for package ‘diseq’
* removing ‘/export/home/X8gqWz9/Rtemp/RtmplP9SGN/Rinst3ed5f23c46/diseq’
      -----------------------------------
ERROR: package installation failed
STDERR:

Error: Failed to install 'diseq' from local:
  Failed to `R CMD build` package, try `build = FALSE`.
In addition: Warning message:
In i.p(...) :
  installation of package ‘RcppParallel’ had non-zero exit status
Execution halted

Log: https://builder.r-hub.io/status/original/diseq_0.2.1.tar.gz-9bf320ac32164918a08d8aaa416f889d
## R CMD check results in (13) -- std=gnu++11, with GSL, no execution header -- 1 Warning, 1 Note
Build time:	25 minutes 2 seconds

WARNINGS:
* checking top-level files ... WARNING
A complete check needs the 'checkbashisms' script.
See section ‘Configure and cleanup’ in the ‘Writing R Extensions’
manual.
NOTES:
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Pantelis Karapanagiotis <pikappa.devel@gmail.com>’

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2307/1913181
    From: man/houses.Rd
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.2307/1914215
    From: man/diseq.Rd
          man/houses.Rd
          man/minus_log_likelihood.Rd
    Status: 403
    Message: Forbidden

Found the following (possibly) invalid DOIs:
  DOI: 10.2307/1914215
    From: DESCRIPTION
    Status: Forbidden
    Message: 403

Log: https://builder.r-hub.io/status/original/diseq_0.2.1.tar.gz-390836e0c5f94b24aaf3e5f7e22ceeaf
## R CMD check results in (14) -- std=gnu++11, with GSL, no execution header -- 1 Warning, 1 Note
Build time:	9 minutes 30.1 seconds

WARNINGS:
* checking top-level files ... WARNING
A complete check needs the 'checkbashisms' script.
See section ‘Configure and cleanup’ in the ‘Writing R Extensions’
manual.
NOTES:
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Pantelis Karapanagiotis <pikappa.devel@gmail.com>’

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2307/1913181
    From: man/houses.Rd
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.2307/1914215
    From: man/diseq.Rd
          man/houses.Rd
          man/minus_log_likelihood.Rd
    Status: 403
    Message: Forbidden

Found the following (possibly) invalid DOIs:
  DOI: 10.2307/1914215
    From: DESCRIPTION
    Status: Forbidden
    Message: 403

Log: https://builder.r-hub.io/status/original/diseq_0.2.1.tar.gz-0c32fbcf1bc84801b7c725933b3d41a2
