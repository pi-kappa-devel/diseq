## Thanks for the comments on versions 0.0.6 and 0.0.7
- Corrected the order of the arguments in hyperlinks, as it was suggested.
- Quoted software names in the DESCRIPTION file.
- Reduced build time (see NEWS.md for details). 
- Added examples in exported funtions. Some enclosed in \donttest{} as suggested.

## Test environments
1. x86_64-redhat-linux-gnu, R version 3.6.3 (2020-02-29)
2. x86_64-w64-mingw32, R version 3.6.2 (2019-12-12)
3. (winbuilder) x86_64-w64-mingw32, R version 4.0.0 beta (2020-04-13 r78215)
4. (rhub) Windows Server 2008 R2 SP1, R-devel, 32/64 bit
5. (rhub) Ubuntu Linux 16.04 LTS, R-release, GCC
6. (rhub) Fedora Linux, R-devel, clang, gfortran

## R CMD check results in (1)
Duration: 4m 57.1s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded

## R CMD check results in (2)
Duration: 7m 15.2s

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded

## R CMD check results in (3)
Installation time in seconds: 93
Check time in seconds: 313
Status: 1 NOTE

a. Possibly mis-spelled words in DESCRIPTION:
  Karapanagiotis (17:5)
  Maddala (15:14)

b.Found the following (possibly) invalid DOIs:
  DOI: 10.2307/1914215
    From: DESCRIPTION
    Status: Forbidden
    Message: 403

## R CMD check results in (4)
Build time:	11m 2.5s
checking CRAN incoming feasibility ... NOTE
  
a. Possibly mis-spelled words in DESCRIPTION:
  Karapanagiotis (17:5)
  Maddala (15:14)

## R CMD check results in (5)
Build time:	1 hour 5 minutes 21.2 seconds
checking CRAN incoming feasibility ... NOTE
  
b. Found the following (possibly) invalid DOIs:
  DOI: 10.2307/1914215
    From: DESCRIPTION
    Status: Forbidden
    Message: 403

## R CMD check results in (6)
Build time:	1 hour 26 minutes 10 seconds
checking CRAN incoming feasibility ... NOTE
  
a. Possibly mis-spelled words in DESCRIPTION:
  Karapanagiotis (17:5)
  Maddala (15:14)
  diseq (8:18)
  
## Justifications for notes in (3), (4), (5), (6)
All the words in (a) are names. The DOI in (b) is valid but authentication is required to access the article.
