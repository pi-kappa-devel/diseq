# Changes in version 0.4.1

* Version 0.4.2 does not have user space changes. It polishes the 0.4.1 release.
 - Specialized calculation of initializing values.
 - Harmonized `coef` output.
 - Added shortage analysis methods for the equilibrium model.
* Documentation changes.
 - Minor typos corrections.

# Test environments 
## Windows
1. (R-devel win-builder.r-project.org) x86_64-w64-mingw32 (64-bit), R Under development (unstable) (2022-02-13 r81727 ucrt)
2. (R-oldrelease win-builder.r-project.org) x86_64-w64-mingw32 (64-bit), R version 4.0.5 (2021-03-31)
3. (R-release win-builder.r-project.org) x86_64-w64-mingw32 (64-bit), R version 4.1.2 (2021-11-01)
4. (rhub windows-x86_64-devel) Windows Server 2022, R-devel, 64 bit, R Under development (unstable) (2022-02-07 r81667 ucrt)

## Linux

### GCC
5. (rhub ubuntu-gcc-release) Ubuntu Linux 20.04.1 LTS, R-release, GCC, R version 4.1.2 (2021-11-01)
6. (rhub debian-gcc-devel) Debian Linux, R-devel, GCC, R Under development (unstable) (2022-02-06 r81658)
7. (rhub debian-gcc-devel-nold) Debian Linux, R-devel, GCC, no long double, R Under development (unstable) (2022-02-06 r81658)
8. (rhub rocker-gcc-san) Debian Linux, R-devel, GCC ASAN/UBSAN, R Under development (unstable) (2022-01-03 r81439) -- "Unsuffered Consequences"
9. (local) Ubuntu 20.04.3 LTS (in WSL2 under Windows 11), GCC 9.3.0, R version 3.6.3 (2020-02-29) -- "Holding the Windsock"

### LLVM
10. (rhub debian-clang-devel) Debian Linux, R-devel, clang, ISO-8859-15 locale, R Under development (unstable) (2022-02-06 r81658)
11. (rhub fedora-clang-devel) Fedora Linux, R-devel, clang, gfortran, R Under development (unstable) (2022-02-06 r81658)

## Solaris
12. (rhub solaris-x86-patched) Oracle Solaris 10, x86, 32 bit, R-release, R version 4.1.2 (2021-11-01)

## Macos
13. (rhub macos-highsierra-release-cran) macOS 10.13.6 High Sierra, R-release, CRAN's setup, R version 4.1.1 (2021-08-10)
14. (rhub macos-highsierra-release) macOS 10.13.6 High Sierra, R-release, brew, R version 4.1.1 (2021-08-10)

# Check results
## R CMD check results in (1) -- std=gnu++11, with GSL, no execution header -- 1 Note
Installation time in seconds: 60
Check time in seconds: 310
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

Artifacts: https://win-builder.r-project.org/as505snZe1w7/

## R CMD check results in (2) -- std=gnu++11, with GSL, no execution header -- 1 Note
Installation time in seconds: 182
Check time in seconds: 502
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

Artifacts: https://win-builder.r-project.org/0W6h0Uz90DSp/


## R CMD check results in (3) -- std=gnu++11, with GSL, no execution header -- 1 Note
Installation time in seconds: 170
Check time in seconds: 458
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

Output: https://win-builder.r-project.org/K1igfb5OAfFm/


## R CMD check results in (4) -- std=gnu++11, with GSL, no execution header -- 3 Notes
Build time:	12 minutes 58.5 seconds
Status: 3 NOTEs

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

  DOI: 10.2307/1914215
Found the following (possibly) invalid DOIs:
    From: DESCRIPTION
    Status: Forbidden
    Message: 403
* checking sizes of PDF files under 'inst/doc' ... NOTE
Unable to find GhostScript executable to run checks on size reduction
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'

Artifacts: https://artifacts.r-hub.io/diseq_0.4.0.9006.tar.gz-6f5cf5982f5f42a1b50ac2724ca426db/diseq.Rcheck/

## R CMD check results in (5) -- std=gnu++11, with GSL, no execution header -- 2 Notes
Build time: 22 minutes 22.4 seconds

Status: 2 NOTEs

* checking CRAN incoming feasibility ... NOTE
Maintainer: â€˜Pantelis Karapanagiotis <pikappa.devel@gmail.com>â€™

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
    doc    1.0Mb
    libs   3.4Mb
	
Artifacts: https://artifacts.r-hub.io/diseq_0.4.2.tar.gz-e555548640b344c28694083a1c7e0451/diseq.Rcheck/

## R CMD check results in (6) -- std=c++17, with GSL, with execution header -- 1 Note
Build time:	22 minutes 21.1 seconds
Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: â€˜Pantelis Karapanagiotis <pikappa.devel@gmail.com>â€™

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

Artifacts: https://artifacts.r-hub.io/diseq_0.4.2.tar.gz-3b96e370a3e54393b66cd048191e08e6/diseq.Rcheck/

## R CMD check results in (7) -- std=c++17, with GSL, with execution header -- 1 Note
Build time:	25 minutes 16.3 seconds

* checking CRAN incoming feasibility ... NOTE
Maintainer: â€˜Pantelis Karapanagiotis <pikappa.devel@gmail.com>â€™

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

Artifacts: https://artifacts.r-hub.io/diseq_0.4.2.tar.gz-2d768fd288a14cd7a4a17e24be859f8c/diseq.Rcheck/

## R CMD check results in (8) -- std=c++17, with GSL, with execution header -- Preperror
Build time: 23 minutes 36.4 seconds

First error is:
ERROR: dependencies ‘minqa’, ‘Rcpp’, ‘RcppEigen’ are not available for package ‘lme4’

Then, there is a chain of package installation failures up to diseq.

Output: https://builder.r-hub.io/status/original/diseq_0.4.2.tar.gz-593f1ca0d768490c8199cdd8f20f1004

## R CMD check results in (9) -- std=c++17, with GSL, with execution header -- 1 Note
── R CMD check results ──────────────────────────────────────────────────────────────────────────────────── diseq 0.4.2 ────
Duration: 2m 26.5s

❯ checking installed package size ... NOTE
    installed size is  6.6Mb
    sub-directories of 1Mb or more:
      doc    1.0Mb
      libs   4.0Mb

0 errors ✔ | 0 warnings ✔ | 1 note ✖

## R CMD check results in (10) -- std=c++17, with GSL, with execution header -- 1 Note
Build time:	30 minutes 41.3 seconds
Status: 1 NOTE

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

Artifacts: https://artifacts.r-hub.io/diseq_0.4.2.tar.gz-d019ef5b50744495b8cc1dc3213afb50/diseq.Rcheck/

## R CMD check results in (11) -- std=c++17, with GSL, with execution header -- 1 Note
Build time:	22 minutes 50.4 seconds

Status: 1 NOTE

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
	
Artifacts: https://artifacts.r-hub.io/diseq_0.4.2.tar.gz-335883b313e846e5a326383a1726fec2/diseq.Rcheck/


## R CMD check results in (12) -- std=gnu++11, with GSL, no execution header -- Preperror
Build time:	16 minutes 8.1 seconds

Point of failure most probably is:
ERROR: compilation failed for package ‘nloptr’

Then, there is a chain of package installation failures up to diseq.

Artifacts: https://builder.r-hub.io/status/original/diseq_0.4.2.tar.gz-c9e4f1cd3cd84290912d1730b6a370fe

## R CMD check results in (13) -- std=gnu++11, with GSL, no execution header -- 1 Warning, 1 Note
Build time:	9 minutes 31.9 seconds
Status: 1 WARNING, 1 NOTE

* checking top-level files ... WARNING
A complete check needs the 'checkbashisms' script.
See section ‘Configure and cleanup’ in the ‘Writing R Extensions’
manual.

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

Artifacts: https://artifacts.r-hub.io/diseq_0.4.2.tar.gz-e2d91c6e31fa4ba0b36b87eb6ebf4ba5/diseq.Rcheck/


## R CMD check results in (14) -- std=gnu++11, with GSL, no execution header -- 1 Warning, 1 Note
Build time: 8 minutes 23.7 seconds
Status: 1 WARNING, 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: â€˜Pantelis Karapanagiotis <pikappa.devel@gmail.com>â€™

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

Artifacts: https://artifacts.r-hub.io/diseq_0.4.2.tar.gz-5c31d76abcb14927943b377f371c5596/diseq.Rcheck/



