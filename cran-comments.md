## Changes in version 0.1.1
- Introduced the option maximizing the equilibrium model likelihood using `GSL` through `Rcpp`. Added linting and formatting configuration files for R and C++ code. Cleaned C++ code. Reorganized R back-end classes. Fixed `M1mac` issues. Adjusted documentation and file names to reflect API changes. R

## Test environments
1. (local) x86_64-redhat-linux-gnu, R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
2. (R-devel win-builder.r-project.org) x86_64-w64-mingw32 (64-bit), R Under development (unstable) (2021-01-25 r79883)
3. (R-oldrelease win-builder.r-project.org) x86_64-w64-mingw32 (64-bit), R version 3.6.3 (2020-02-29)
4. (R-release win-builder.r-project.org) x86_64-w64-mingw32 (64-bit), R version 4.0.3 (2020-10-10)
5. (rhub windows-x86_64-devel) Windows Server 2008 R2 SP1, R-devel, 32/64 bit
6. (rhub x86_64-pc-linux-gnu) Ubuntu Linux 16.04 LTS, R-release, GCC, R version 3.6.1 (2019-07-05)
7. (rhub x86_64-pc-linux-gnu) Fedora Linux, R-devel, clang, gfortran, R Under development (unstable) (2021-01-25 r79883)
8. (rhub debian-gcc-devel-nold) Debian Linux, R-devel, GCC, no long double, R Under development (unstable) (2021-01-25 r79883)
9. (rhub rocker-gcc-san) Debian Linux, R-devel, GCC ASAN/UBSAN, R Under development (unstable) (2020-07-31 r78945)

## R CMD check results in (1)
── R CMD check results ──────────────────────────────────────── diseq 0.1.1 ────
Duration: 6m 42.4s

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
    installed size is  5.7Mb
    sub-directories of 1Mb or more:
      R      1.5Mb
      libs   3.6Mb

❯ checking compilation flags used ... NOTE
  Compilation used the following non-portable flag(s):
    ‘-Werror=format-security’ ‘-Wp,-D_FORTIFY_SOURCE=2’
    ‘-Wp,-D_GLIBCXX_ASSERTIONS’

0 errors ✔ | 1 warning ✖ | 2 notes ✖

### Comments on the results
Concerning the warning, I suspect that it is an issue coming from the GSL libraries that are installed in my home system. There are not any direct calls to `abort` or `printf` in my C++ sources. If compiled with error handling enabled, some routines of GSL abort and print a short error message on failure by default. Fedora adds `_GLIBCXX_ASSERTIONS` to the compilation flags which links `abort`. See https://cran.r-project.org/doc/manuals/r-devel/R-admin.html#Linux for details. I do not obtain this warning in any of the checks performed in remote machines.

## R CMD check results in (2)
Installation time in seconds: 245
Check time in seconds: 524
Status: OK

## R CMD check results in (3)
Installation time in seconds: 225
Check time in seconds: 494
Status: OK

## R CMD check results in (4)
Installation time in seconds: 246
Check time in seconds: 587
Status: OK

## R CMD check results in (5)
Build time:	12 minutes 46.1 seconds
Status: OK

## R CMD check results in (6)
Build time:	1 hour 28 minutes 30 seconds
Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Pantelis Karapanagiotis <pikappa.devel@gmail.com>’

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

## R CMD check results in (7)
Build time:	1 hour 50 minutes 46.3 seconds
Status: OK

## R CMD check results in (8)
Build time:	2 hours 9 minutes 57.7 seconds
Status: OK

## R CMD check results in (9)
Build time:	3 hours 49 minutes 56.3 seconds
Status: OK
