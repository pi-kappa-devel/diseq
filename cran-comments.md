## Change in version 0.0.13
- Added sections `A quick model tour`, `Alternative packages`, and `Planned extensions` in README.
- Reconfirmed that there are 'noLD' issues. I have sent emails to both CRAN-submissions@R-project.org and CRAN@r-project.org regarding the removal of the package from the repository, despite that I have addressed the 'noLD' problem well in advance from the deadline. I did not receive any feedback, but please let me know If I should do something more.

## Test environments
1. (local) x86_64-redhat-linux-gnu, R version 4.0.2 (2020-06-22) -- "Taking Off Again"
2. (R-devel win-builder.r-project.org) x86_64-w64-mingw32 (64-bit), R Under development (unstable) (2020-09-09 r79174)
3. (R-oldrelease win-builder.r-project.org) x86_64-w64-mingw32 (64-bit), R version 3.6.3 (2020-02-29)
4. (R-release win-builder.r-project.org) x86_64-w64-mingw32 (64-bit), R version 4.0.2 (2020-06-22)
5. (rhub windows-x86_64-devel) Windows Server 2008 R2 SP1, R-devel, 32/64 bit
6. (rhub x86_64-pc-linux-gnu) Ubuntu Linux 16.04 LTS, R-release, GCC, R version 3.6.1 (2019-07-05)
7. (rhub x86_64-pc-linux-gnu) Fedora Linux, R-devel, clang, gfortran, R Under development (unstable) (2020-09-13 r79194)
8. (rhub debian-gcc-devel-nold) Debian Linux, R-devel, GCC, no long double, R Under development (unstable) (2020-09-13 r79194)

## R CMD check results in (1)
── R CMD check results ─────────────────────────────────────── diseq 0.0.13 ────
Duration: 5m 38s

❯ checking for future file timestamps ... NOTE
  unable to verify current time

0 errors ✔ | 0 warnings ✔ | 1 note ✖

## R CMD check results in (2)
Installation time in seconds: 180
Check time in seconds: 505
Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

New submission

Package was archived on CRAN

Possibly mis-spelled words in DESCRIPTION:
  Karapanagiotis (15:31)
  Maddala (13:56)

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

## R CMD check results in (3)
Installation time in seconds: 174
Check time in seconds: 447
Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

New submission

Package was archived on CRAN

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

## R CMD check results in (4)
Installation time in seconds: 190
Check time in seconds: 478
Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

New submission

Package was archived on CRAN

Possibly mis-spelled words in DESCRIPTION:
  Karapanagiotis (15:31)
  Maddala (13:56)

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

## R CMD check results in (5)
Status: OK

## R CMD check results in (6)
Status: 2 NOTES

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Pantelis Karapanagiotis <pikappa.devel@gmail.com>’

New submission

Package was archived on CRAN

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
	
* checking for future file timestamps ... NOTE
unable to verify current time

## R CMD check results in (7)
Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: â€˜Pantelis Karapanagiotis <pikappa.devel@gmail.com>â€™

New submission

Package was archived on CRAN

Possibly mis-spelled words in DESCRIPTION:
  Karapanagiotis (15:31)
  Maddala (13:56)

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

## R CMD check results in (8)
Status: OK
