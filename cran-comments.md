# Changes in version 0.4.1

* Version 0.4 introduces user space changes.
 - Model can be initialized using formulas
 - Introduced functions for single call initialization and estimation of models. The old methods for constructing and estimating models are still exported.
 - Introduced estimation output class `market_fit`. The class further unifies the user interface for accessing market models.
 - Estimation output can be summarized by calling `summary` with `market_fit` objects.
 - Added new plotting functionality on the estimation output.
 - Added coefficient access method `coef`.
 - Added variance-covariance access method `vcov`.
 - Added `logLik` object access method.
 - Added `formula` object access method.
* Documentation changes.
 - Examples and vignettes were adjusted to exemplify the new user interface.
 - Documentation entry added for model initialization based on formulas.
 - Added vignette `more_details.Rmd` with initialization and estimation details.

# Test environments 
## Windows
1. (R-devel win-builder.r-project.org) x86_64-w64-mingw32 (64-bit), R Under development (unstable) (2022-01-09 r81462 ucrt)
2. (R-oldrelease win-builder.r-project.org) x86_64-w64-mingw32 (64-bit), R version 4.0.5 (2021-03-31)
3. (R-release win-builder.r-project.org) x86_64-w64-mingw32 (64-bit), R version 4.1.2 (2021-11-01)
4. (rhub windows-x86_64-devel) Windows Server 2022, R-devel, 64 bit, R Under development (unstable) (2021-12-17 r81389 ucrt)

## Linux

### GCC
5. (rhub ubuntu-gcc-release) Ubuntu Linux 20.04.1 LTS, R-release, GCC, R version 4.1.2 (2021-11-01)
6. (rhub debian-gcc-devel) Debian Linux, R-devel, GCC, R Under development (unstable) (2022-01-09 r81462)
7. (rhub debian-gcc-devel-nold) Debian Linux, R-devel, GCC, no long double, R Under development (unstable) (2022-01-09 r81462)
8. (rhub rocker-gcc-san) Debian Linux, R-devel, GCC ASAN/UBSAN, R Under development (unstable) (2022-01-03 r81439) -- "Unsuffered Consequences"
9. (local) Ubuntu 20.04.3 LTS (in WSL2 under Windows 11), GCC 9.3.0, R version 3.6.3 (2020-02-29) -- "Holding the Windsock"

### LLVM
10. (rhub debian-clang-devel) Debian Linux, R-devel, clang, ISO-8859-15 locale, R Under development (unstable) (2022-01-09 r81462)
11. (rhub fedora-clang-devel) Fedora Linux, R-devel, clang, gfortran, R Under development (unstable) (2022-01-09 r81462)

## Solaris
12. (rhub solaris-x86-patched) Oracle Solaris 10, x86, 32 bit, R-release, R version 4.1.2 (2021-11-01)

## Macos
13. (rhub macos-highsierra-release-cran) macOS 10.13.6 High Sierra, R-release, CRAN's setup, R version 4.1.1 (2021-08-10)
14. (rhub macos-highsierra-release) macOS 10.13.6 High Sierra, R-release, brew, R version 4.1.1 (2021-08-10)

# Check results
## R CMD check results in (1) -- std=gnu++11, with GSL, no execution header -- 1 Note
Installation time in seconds: 58
Check time in seconds: 243
Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Version contains large components (0.4.0.9006)

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

Output: https://win-builder.r-project.org/w04yW12vk0Hn/

## R CMD check results in (2) -- std=gnu++11, with GSL, no execution header -- 1 Note
Installation time in seconds: 81
Check time in seconds: 318
Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Version contains large components (0.4.0.9006)

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

Output: https://win-builder.r-project.org/RbOCzmp2Mslg


## R CMD check results in (3) -- std=gnu++11, with GSL, no execution header -- 1 Note
Installation time in seconds: 82
Check time in seconds: 318
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

Output: https://win-builder.r-project.org/ucK44VaMtomH/


## R CMD check results in (4) -- std=gnu++11, with GSL, no execution header -- 3 Notes
Build time:	18 minutes 3.2 seconds
Status: 3 NOTEs

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Version contains large components (0.4.0.9006)

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
	
* checking sizes of PDF files under 'inst/doc' ... NOTE
Unable to find GhostScript executable to run checks on size reduction

* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'

Output: https://artifacts.r-hub.io/diseq_0.4.0.9006.tar.gz-6f5cf5982f5f42a1b50ac2724ca426db/diseq.Rcheck/

## R CMD check results in (5) -- std=gnu++11, with GSL, no execution header -- 2 Notes
Build time: 42 minutes 1.4 seconds

Status: 2 NOTEs

* checking CRAN incoming feasibility ... NOTE
Maintainer: â€˜Pantelis Karapanagiotis <pikappa.devel@gmail.com>â€™

Version contains large components (0.4.0.9006)

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

* checking installed package size ... NOTE
  installed size is  6.1Mb
  sub-directories of 1Mb or more:
    libs   3.4Mb
	
Output: https://artifacts.r-hub.io/diseq_0.4.0.9006.tar.gz-55a04afe006a4cfcb24d85857c3bec2f/diseq.Rcheck/

## R CMD check results in (6) -- std=c++17, with GSL, with execution header -- Ok
Build time:	37 minutes 44.5 seconds
Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: â€˜Pantelis Karapanagiotis <pikappa.devel@gmail.com>â€™

Version contains large components (0.4.0.9006)

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

Output: https://artifacts.r-hub.io/diseq_0.4.0.9006.tar.gz-9c323eeab30648779c34ad31e0b49012/diseq.Rcheck/

## R CMD check results in (7) -- std=c++17, with GSL, with execution header -- Ok
Build time:	47 minutes 48.2 seconds

* checking CRAN incoming feasibility ... NOTE
Maintainer: â€˜Pantelis Karapanagiotis <pikappa.devel@gmail.com>â€™

Version contains large components (0.4.0.9006)

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

Output: https://artifacts.r-hub.io/diseq_0.4.0.9006.tar.gz-719f60dd83084eb6a15adecfb8fbaabb/diseq.Rcheck/
## R CMD check results in (8) -- std=c++17, with GSL, with execution header -- Ok
Build time:	58 minutes 10.9 seconds
Status: Ok

Output: https://builder.r-hub.io/status/original/diseq_0.4.0.9006.tar.gz-10978bf1f53848588cc36fa2990230d0

## R CMD check results in (9) -- std=c++17, with GSL, with execution header -- 1 Note
── R CMD check results ─────────────────────────────────────── diseq 0.4.0.9006 ────
Duration: 2m 16.5s

❯ checking installed package size ... NOTE
    installed size is  6.6Mb
    sub-directories of 1Mb or more:
      libs   4.0Mb

0 errors ✔ | 0 warnings ✔ | 1 note ✖

## R CMD check results in (10) -- std=c++17, with GSL, with execution header -- 1 Note
Build time:	48 minutes 18.1 seconds
Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Version contains large components (0.4.0.9006)

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

Output: https://artifacts.r-hub.io/diseq_0.4.0.9006.tar.gz-48289942ff7a45e8a57a16109873da0e/diseq.Rcheck/

## R CMD check results in (11) -- std=c++17, with GSL, with execution header -- 1 Note
Build time:	34 minutes 44.2 seconds

Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: â€˜Pantelis Karapanagiotis <pikappa.devel@gmail.com>â€™

Version contains large components (0.4.0.9006)

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
	
Output: https://artifacts.r-hub.io/diseq_0.4.0.9006.tar.gz-9e306d3fda764ea38e7fcecc65d72d61/diseq.Rcheck/


## R CMD check results in (12) -- std=gnu++11, with GSL, no execution header -- 1 Warning, 2 Notes
Build time:	1 hour 3 minutes 26.2 seconds
Status: 1 WARNING, 2 NOTEs

* checking CRAN incoming feasibility ... NOTE
Maintainer: â€˜Pantelis Karapanagiotis <pikappa.devel@gmail.com>â€™

Version contains large components (0.4.0.9006)

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

* checking top-level files ... WARNING
  Output from running autoreconf:
  /opt/csw/share/aclocal/gtk.m4:7: warning: underquoted definition of AM_PATH_GTK
  /opt/csw/share/aclocal/gtk.m4:7:   run info Automake 'Extending aclocal'
  /opt/csw/share/aclocal/gtk.m4:7:   or see https://www.gnu.org/software/automake/manual/automake.html#Extending-aclocal
A complete check needs the 'checkbashisms' script.
See section â€˜Configure and cleanupâ€™ in the â€˜Writing R Extensionsâ€™
manual.
Files â€˜README.mdâ€™ or â€˜NEWS.mdâ€™ cannot be checked without â€˜pandocâ€™ being installed.

* checking compilation flags used ... NOTE
Compilation used the following non-portable flag(s):
  â€˜-march=pentiumproâ€™

Output: https://artifacts.r-hub.io/diseq_0.4.0.9006.tar.gz-a5e3908fd8a34c7492a8f0b3032ab65d/diseq.Rcheck/

## R CMD check results in (13) -- std=gnu++11, with GSL, no execution header -- 1 Warning, 1 Note
Build time:	9 minutes 17.5 seconds
Status: 1 WARNING, 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: â€˜Pantelis Karapanagiotis <pikappa.devel@gmail.com>â€™

Version contains large components (0.4.0.9006)

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
	
* checking top-level files ... WARNING
A complete check needs the 'checkbashisms' script.
See section â€˜Configure and cleanupâ€™ in the â€˜Writing R Extensionsâ€™
manual.

Output: https://artifacts.r-hub.io/diseq_0.4.0.9006.tar.gz-c995c1faf58a4a238a55359a95ee458b/diseq.Rcheck/


## R CMD check results in (14) -- std=gnu++11, with GSL, no execution header -- 1 Warning, 1 Note
Build time: 9 minutes 21.2 seconds
Status: 1 WARNING, 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: â€˜Pantelis Karapanagiotis <pikappa.devel@gmail.com>â€™

Version contains large components (0.4.0.9006)

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
	
* checking top-level files ... WARNING
A complete check needs the 'checkbashisms' script.
See section â€˜Configure and cleanupâ€™ in the â€˜Writing R Extensionsâ€™
manual.

Output: https://artifacts.r-hub.io/diseq_0.4.0.9006.tar.gz-882b4f8acd8549a08ded75c3166f94c8/diseq.Rcheck/



