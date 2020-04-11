## Thanks for the comment on version 0.0.6
I have corrected the order of the arguments, as it was suggested.

## Test environments
1. x86_64-redhat-linux-gnu, R version 3.6.3 (2020-02-29)
2. x86_64-w64-mingw32, R version 3.6.2 (2019-12-12)
3. (winbuilder) x86_64-w64-mingw32, R version 4.0.0 alpha (2020-03-26 r78078)
4. (rhub) Windows Server 2008 R2 SP1, R-devel, 32/64 bit
5. (rhub) Ubuntu Linux 16.04 LTS, R-release, GCC
6. (rhub) Fedora Linux, R-devel, clang, gfortran

## R CMD check results in (1)
Duration: 6m 10.2s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded

## R CMD check results in (2)
Duration: 7m 28.8s

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded

## R CMD check results in (3)
Check time in seconds: 644
Status: 1 NOTE

a. Possibly mis-spelled words in DESCRIPTION:
  Karapanagiotis (17:5)
  Maddala (15:14)
  diseq (8:18)

b.Found the following (possibly) invalid DOIs:
  DOI: 10.2307/1914215
    From: DESCRIPTION
    Status: Forbidden
    Message: 403

## R CMD check results in (4)
Build time:	12 minutes 1.4 seconds
checking CRAN incoming feasibility ... NOTE
  
a. Possibly mis-spelled words in DESCRIPTION:
  Karapanagiotis (17:5)
  Maddala (15:14)
  diseq (8:18)

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
