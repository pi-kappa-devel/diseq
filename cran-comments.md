## Changes in version 0.0.12
- Adressed 'noLD' issues.
- Minor changes and in documentation (see NEWS.md for details). 

## Test environments
1. x86_64-redhat-linux-gnu, R version 3.6.3 (2020-02-29)
2. (winbuilder) x86_64-w64-mingw32 (64-bit), R Under development (unstable) (2020-06-24 r78746)
3. (winbuilder) x86_64-w64-mingw32 (64-bit), R version 4.0.2 (2020-06-22)
4. (rhub) Ubuntu Linux 16.04 LTS, R-release, GCC, R version 3.6.1 (2019-07-05)
5. (rhub) Fedora Linux, R-devel, clang, gfortran, R version 3.6.1 (2019-07-05)
6. rhub::check(, platform = "debian-gcc-devel-nold")

## R CMD check results in (1)
Duration: 5m 31.3s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded

## R CMD check results in (2)
Installation time in seconds: 122
Check time in seconds: 381

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

Status: 1 NOTE

## R CMD check results in (3)
Installation time in seconds: 130
Check time in seconds: 392

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

Status: 1 NOTE

## R CMD check results in (4)
Build time:	21 minutes 22.1 seconds

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

Status: 1 NOTE

## R CMD check results in (5)
Build time:	24 minutes 58 seconds

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

Status: 1 NOTE

## R CMD check results in (6)
Build time:	28 minutes 24.6 seconds

Status: OK
