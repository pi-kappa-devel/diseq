## Thanks for the comments on versions 0.0.6, 0.0.7, and 0.0.10
- Corrected the order of the arguments in hyperlinks, as it was suggested.
- Quoted software names in the DESCRIPTION file.
- Reduced build time (see NEWS.md for details). 
- Added examples in exported functions. Some enclosed in \donttest{} as suggested.
- Added missing \value Rd-tags.
- Reintroduced references in DESCRIPTION.

## Test environments
1. x86_64-redhat-linux-gnu, R version 3.6.3 (2020-02-29)
2. x86_64-w64-mingw32, R version 3.6.2 (2019-12-12)
3. (winbuilder) x86_64-w64-mingw32, R version 4.0.0 beta (2020-04-21 r78276)
4. (rhub) Windows Server 2008 R2 SP1, R Under development (unstable) (2020-04-22 r78281)

## R CMD check results in (1)
Duration: 5m 8s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded

## R CMD check results in (2)
Duration: 8m 7.3s

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded

## R CMD check results in (3)
Installation time in seconds: 134
Check time in seconds: 368
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

New submission

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

## R CMD check results in (4)
10 minutes 51.6 seconds
N  checking CRAN incoming feasibility (2.4s)
   
   Possibly mis-spelled words in DESCRIPTION:
     Karapanagiotis (15:31)
     Maddala (13:56)
   Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'
   New submission

