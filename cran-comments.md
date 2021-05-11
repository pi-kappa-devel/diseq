# Changes in version 0.3.1

* Added software package article in vignettes.
* Bug fixes in `show` and `summary` methods of `diseq_stochastic_adjustment`. Fixed bug in calculation of clustered standard errors.
* Changes in model simulation.
 - Simplified simulation calls (changes the user space).
 - Re-factored simulation code and exported additional functions. 
* Added marginal system effect methods, and unified marginal probabilities effects methods (changes the user space). Added `prefixed_quantity_variable` method.
* Improvements in documentation. Added class implementation figure.


# Test environments 
## Windows
1. (R-devel win-builder.r-project.org) x86_64-w64-mingw32 (64-bit), R version 4.1.0 beta (2021-05-06 r80268)
2. (R-oldrelease win-builder.r-project.org) x86_64-w64-mingw32 (64-bit), R version 3.6.3 (2020-02-29)
3. (R-release win-builder.r-project.org) x86_64-w64-mingw32 (64-bit), R version 4.0.5 (2021-03-31)
4. (rhub windows-x86_64-devel) Windows Server 2008 R2 SP1, R-devel, 32/64 bit, R Under development (unstable) (2021-04-15 r80175) 

## Linux

### GCC
5. (rhub x86_64-pc-linux-gnu) Ubuntu Linux 20.04.1 LTS, R-release, GCC, R version 4.0.5 (2021-03-31) -- "Shake and Throw"
6. (rhub debian-gcc-devel) Debian Linux, R-devel, GCC, R Under development (unstable) (2021-05-10 r80282) -- "Unsuffered Consequences"
7. (rhub debian-gcc-devel-nold) Debian Linux, R-devel, GCC, no long double, R Under development (unstable) (2021-05-10 r80282) -- "Unsuffered Consequences"
8. (rhub rocker-gcc-san) Debian Linux, R-devel, GCC ASAN/UBSAN, R Under development (unstable) (2020-07-31 r78945) -- "Unsuffered Consequences"
9. (local) Fedora 34 (Workstation Edition), GCC 11.1.1, R version 4.0.4 (2021-02-15) -- "Lost Library Book"

### LLVM
10. (rhub debian-clang-devel) Debian Linux, R-devel, clang, ISO-8859-15 locale, R Under development (unstable) (2021-05-10 r80282) -- "Unsuffered Consequences"
11. (rhub fedora-clang-devel) Fedora Linux, R-devel, clang, gfortran, R Under development (unstable) (2021-05-10 r80282) -- "Unsuffered Consequences"

## Solaris
12. (rhub solaris-x86-patched) Oracle Solaris 10, x86, 32 bit, R-release, R version 4.0.5 (2021-03-31)

## Macos
13. (rhub macos-highsierra-release-cran) macOS 10.13.6 High Sierra, R-release, CRAN's setup, R version 4.0.5 (2021-03-31)
14. (rhub macos-highsierra-release) macOS 10.13.6 High Sierra, R-release, brew, R version 4.0.5 (2021-03-31)

# Check results
## R CMD check results in (1) -- std=gnu++11, with GSL, no execution header -- 1 Note
Installation time in seconds: 87
Check time in seconds: 263
Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2307/1913181
    From: man/houses.Rd
          README.md
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.2307/1914215
    From: man/diseq.Rd
          man/houses.Rd
          man/minus_log_likelihood.Rd
          README.md
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

Log: https://win-builder.r-project.org/T9StB2o1FHoI/

## R CMD check results in (2) -- std=gnu++11, with GSL, no execution header -- 1 Note
Installation time in seconds: 80
Check time in seconds: 247
Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2307/1913181
    From: man/houses.Rd
    Status: 426
    Message: Upgrade Required
  URL: https://doi.org/10.2307/1914215
    From: man/diseq.Rd
          man/houses.Rd
          man/minus_log_likelihood.Rd
    Status: 426
    Message: Upgrade Required

Found the following (possibly) invalid DOIs:
  DOI: 10.2307/1914215
    From: DESCRIPTION
    Status: Upgrade Required
    Message: 426

Log: https://win-builder.r-project.org/jBu5Ix60nG4g/

## R CMD check results in (3) -- std=gnu++11, with GSL, no execution header -- 1 Note
Installation time in seconds: 91
Check time in seconds: 268
Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2307/1913181
    From: man/houses.Rd
          README.md
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.2307/1914215
    From: man/diseq.Rd
          man/houses.Rd
          man/minus_log_likelihood.Rd
          README.md
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

Log: https://win-builder.r-project.org/cbOsMOYVHldr/

## R CMD check results in (4) -- std=gnu++11, with GSL, no execution header -- 2 Notes
Build time:	27 minutes 1.3 seconds

NOTES:
* checking for future file timestamps ... NOTE
unable to verify current time
* checking sizes of PDF files under 'inst/doc' ... NOTE
Unable to find GhostScript executable to run checks on size reduction

Log: https://builder.r-hub.io/status/original/diseq_0.3.1.tar.gz-700bcdb3e02a464fb2cd29a8b23ffa1f

## R CMD check results in (5) -- std=gnu++11, with GSL, no execution header -- 1 Note
Build time:	24 minutes 39.7 seconds

Status: 1 NOTE

* checking installed package size ... NOTE
  installed size is  5.8Mb
  sub-directories of 1Mb or more:
    libs   3.4Mb

Log: https://builder.r-hub.io/status/original/diseq_0.3.1.tar.gz-1a166711aee442a6a526113a757b8375

## R CMD check results in (6) -- std=c++17, with GSL, with execution header -- Ok
Build time:	23 minutes 39.5 seconds

Log: https://builder.r-hub.io/status/original/diseq_0.3.1.tar.gz-2c5514f365fb4534a5ed1f8ee8278b47

## R CMD check results in (7) -- std=c++17, with GSL, with execution header -- Ok
Build time:	25 minutes 21.4 seconds

Log: https://builder.r-hub.io/status/original/diseq_0.3.1.tar.gz-132b8fc3902d447a9640ba11f5b9266f

## R CMD check results in (8) -- std=c++17, with GSL, with execution header -- Ok
Build time:	47 minutes 6.7 seconds

Log: https://builder.r-hub.io/status/original/diseq_0.3.1.tar.gz-cca359d508164507aa6c204d90bdcb59

## R CMD check results in (9) -- std=c++17, with GSL, with execution header -- 1 Note
── R CMD check results ──────────────────────────────────────── diseq 0.3.1 ────
Duration: 7m 10.5s

❯ checking compilation flags used ... NOTE
  Compilation used the following non-portable flag(s):
    ‘-Werror=format-security’ ‘-Wp,-D_FORTIFY_SOURCE=2’
    ‘-Wp,-D_GLIBCXX_ASSERTIONS’

0 errors ✔ | 0 warnings ✔ | 1 note ✖

## R CMD check results in (10) -- std=c++17, with GSL, with execution header -- Ok
Build time:	30 minutes 55.6 seconds

Log: https://builder.r-hub.io/status/original/diseq_0.3.1.tar.gz-af69542397154418a1085a2ebb29b86e

## R CMD check results in (11) -- std=c++17, with GSL, with execution header -- Ok
Build time:	25 minutes 37.8 seconds

Log: https://builder.r-hub.io/status/original/diseq_0.3.1.tar.gz-3fed35835fdd4d3c96050c4ff279df8d

## R CMD check results in (12) -- std=gnu++11, with GSL, no execution header -- 1 Warning, 2 Notes
Build time:	38 minutes 4.8 seconds

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

* checking compilation flags used ... NOTE
Compilation used the following non-portable flag(s):
  ‘-march=pentiumpro’
  
Log: https://builder.r-hub.io/status/original/diseq_0.3.1.tar.gz-243eca0d533048b2b6a2e6018035f361

## R CMD check results in (13) -- std=gnu++11, with GSL, no execution header -- 1 Warning, 1 Note
Build time:	41 minutes 4.2 seconds

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

Log: https://builder.r-hub.io/status/original/diseq_0.3.1.tar.gz-458b62284431440e8bce5b8f7b3d8473

## R CMD check results in (14) -- std=gnu++11, with GSL, no execution header -- 1 Warning, 1 Note
Build time:	16 minutes 45 seconds

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

Log: https://builder.r-hub.io/status/original/diseq_0.3.1.tar.gz-2156934ce34b494ea8e50e307c6d557e
