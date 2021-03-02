# Changes in version 0.1.5

* Deployed development documentation website. For the actual changes in the package see the changes in version 0.1.4 that follow.

# Changes in version 0.1.4

* Cumulative patch of CRAN version.
* Provided standard methods for market model classes (`show`, `summary`, and `plot`).
* Various improvements in `README`, `DESCRIPTION`, and documentation.

# Test environments 
## Windows
1. (R-devel win-builder.r-project.org) x86_64-w64-mingw32 (64-bit), R Under development (unstable) (2021-02-27 r80043)
2. (R-oldrelease win-builder.r-project.org) x86_64-w64-mingw32 (64-bit), R version 3.6.3 (2020-02-29)
3. (R-release win-builder.r-project.org) x86_64-w64-mingw32 (64-bit), R version 4.0.4 (2021-02-15)
4. (rhub windows-x86_64-devel) Windows Server 2008 R2 SP1, R-devel, 32/64 bit

## Linux

### GCC
5. (rhub x86_64-pc-linux-gnu) Ubuntu Linux 20.04.1 LTS, R-release, GCC, R version 4.0.4 (2021-02-15) -- "Lost Library Book"
6. (rhub debian-gcc-devel) Debian Linux, R-devel, GCC, R Under development (unstable) (2021-02-27 r80043) -- "Unsuffered Consequences"
7. (rhub debian-gcc-devel-nold) Debian Linux, R-devel, GCC, no long double, R Under development (unstable) (2021-02-27 r80043) -- "Unsuffered Consequences"
8. (rhub rocker-gcc-san) Debian Linux, R-devel, GCC ASAN/UBSAN, R Under development (unstable) (2020-07-31 r78945) -- "Unsuffered Consequences"
9. (local) Fedora 32, GCC, R version 4.0.3 (2020-10-10)

### LLVM
10. (rhub debian-clang-devel) Debian Linux, R-devel, clang, ISO-8859-15 locale, R Under development (unstable) (2021-02-27 r80043) -- "Unsuffered Consequences"
11. (rhub fedora-clang-devel) Fedora Linux, R-devel, clang, gfortran, R Under development (unstable) (2021-02-27 r80043) -- "Unsuffered Consequences"

## Solaris
12. (rhub solaris-x86-patched) Oracle Solaris 10, x86, 32 bit, R-release, R version 4.0.4 (2021-02-15)

## Macos
13. (rhub macos-highsierra-release-cran) macOS 10.13.6 High Sierra, R-release, CRAN's setup, R version 4.0.4 (2021-02-15)
14. (rhub macos-highsierra-release) macOS 10.13.6 High Sierra, R-release, brew, R version 4.0.4 (2021-02-15)

# Check results
## R CMD check results in (1) -- std=gnu++11, with GSL, no execution header -- 1 Note
Installation time in seconds: 239
Check time in seconds: 526
Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2307/1913181
    From: README.md
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.2307/1914215
    From: man/diseq.Rd
          man/minus_log_likelihood.Rd
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.2307/2526311
    From: README.md
    Status: 403
    Message: Forbidden
	
Log: https://win-builder.r-project.org/1c65N6NRNv7i

## R CMD check results in (2) -- std=gnu++11, with GSL, no execution header -- 1 Note
Installation time in seconds: 223
Check time in seconds: 480
Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

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
	
Log: https://win-builder.r-project.org/QYMX3sFna5Vv/

## R CMD check results in (3) -- std=gnu++11, with GSL, no execution header -- 1 Note
Installation time in seconds: 252
Check time in seconds: 584
Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2307/1913181
    From: README.md
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.2307/1914215
    From: man/diseq.Rd
          man/minus_log_likelihood.Rd
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.2307/2526311
    From: README.md
    Status: 403
    Message: Forbidden

Found the following (possibly) invalid DOIs:
  DOI: 10.2307/1914215
    From: DESCRIPTION
    Status: Forbidden
    Message: 403
	
Log: https://win-builder.r-project.org/BG0r7YgzaAXy/

## R CMD check results in (4) -- std=gnu++11, with GSL, no execution header -- Ok
Build time:	13 minutes 34.2 seconds
Status: Ok

Log: https://builder.r-hub.io/status/original/diseq_0.1.4.tar.gz-32c5a63c77a44a89975c2a3142e1572f
## R CMD check results in (5) -- std=gnu++11, with GSL, no execution header -- 1 Note
Build time:	1 hour 53 minutes 53.8 seconds
Status: 1 NOTE
* checking installed package size ... NOTE
  installed size is  5.6Mb
  sub-directories of 1Mb or more:
    libs   3.4Mb
    R      1.7Mb
	
Log: https://builder.r-hub.io/status/original/diseq_0.1.4.tar.gz-c0aee4ccc1a04764adc19b3f0540fca9

## R CMD check results in (6) -- std=c++17, with GSL, with execution header -- Ok
Build time:	1 hour 6 minutes 15.3 seconds
Status: Ok

Log: https://builder.r-hub.io/status/original/diseq_0.1.4.tar.gz-18d6570969f34661924b20478589a5e5

## R CMD check results in (7) -- std=c++17, with GSL, with execution header -- Ok
Build time:	2 hours 7 minutes 58.7 seconds
Status: Ok

Log: https://builder.r-hub.io/status/original/diseq_0.1.4.tar.gz-2a0459fc1ab5412a8f7e0bb2cd0a9a7e

## R CMD check results in (8) -- std=c++17, with GSL, with execution header -- Ok
Build time:	3 hours 49 minutes 15.3 seconds

Log: https://builder.r-hub.io/status/original/diseq_0.1.4.tar.gz-fe931436de9a423882a55b05dc72ab84

## R CMD check results in (9) -- std=c++17, with GSL, with execution header -- 1 Warning, 2 Notes
── R CMD check results ──────────────────────────────────────── diseq 0.1.4 ────
Duration: 7m 12.2s

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
    installed size is  5.8Mb
    sub-directories of 1Mb or more:
      R      1.6Mb
      libs   3.6Mb

❯ checking compilation flags used ... NOTE
  Compilation used the following non-portable flag(s):
    ‘-Werror=format-security’ ‘-Wp,-D_FORTIFY_SOURCE=2’
    ‘-Wp,-D_GLIBCXX_ASSERTIONS’

0 errors ✔ | 1 warning ✖ | 2 notes ✖

## R CMD check results in (10) -- std=c++17, with GSL, with execution header -- Ok
Build time:	2 hours 20 minutes 50.2 seconds
Status: Ok

Log: https://builder.r-hub.io/status/original/diseq_0.1.4.tar.gz-c2655587018c4b469203397f1930dba8

## R CMD check results in (11) -- std=c++17, with GSL, with execution header -- Ok
1 hour 53 minutes 42.3 seconds
Status: 1 Ok

Log: https://builder.r-hub.io/status/original/diseq_0.1.4.tar.gz-47ee1d3ddfd04ad6bd7f0af63bf8a486

## R CMD check results in (12) -- std=gnu++11, with GSL, no execution header -- 1 Warning, 2 Notes
Build time:	37 minutes 51.2 seconds
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

Log: https://builder.r-hub.io/status/original/diseq_0.1.4.tar.gz-c726e9b54b3c493f9c7d0fc500d110f6

## R CMD check results in (13) -- std=gnu++11, with GSL, no execution header -- 1 Warning, 1 Note
Build time:	35 minutes 8.1 seconds
Status: 1 WARNING, 1 NOTE

WARNINGS:
* checking top-level files ... WARNING
A complete check needs the 'checkbashisms' script.
See section ‘Configure and cleanup’ in the ‘Writing R Extensions’
manual.
NOTES:
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

Log: https://builder.r-hub.io/status/original/diseq_0.1.4.tar.gz-0333d2b05d8d4010a921edc3e68350c8

## R CMD check results in (14) -- std=gnu++11, with GSL, no execution header -- 1 Warning, 1 Note
Build time:	13 minutes 31.7 seconds
Status: 1 WARNING, 1 NOTE

WARNINGS:
* checking top-level files ... WARNING
A complete check needs the 'checkbashisms' script.
See section ‘Configure and cleanup’ in the ‘Writing R Extensions’
manual.
NOTES:
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
	
Log: https://builder.r-hub.io/status/original/diseq_0.1.4.tar.gz-8041f9aaaabb42b9a1f3feae97c82a28
