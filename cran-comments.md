## Test environments
1. x86_64-redhat-linux-gnu, R version 3.6.3 (2020-02-29)
2. x86_64-w64-mingw32, R version 3.6.2 (2019-12-12)
3. (winbuilder) x86_64-w64-mingw32, R version 4.0.0 alpha (2020-03-26 r78078)

## R CMD check results in (1)
Duration: 4m 20.7s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded

## R CMD check results in (2)
Duration: 7m 24.1s

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded

## R CMD check results in (3)
Check time in seconds: 654
Status: 1 NOTE

a. Possibly mis-spelled words in DESCRIPTION:
  Karapanagiotis (17:5)
  Maddala (15:14)
  diseq (8:18)

b. Found the following (possibly) invalid file URI:
  URI: Karapanagiotis (2020)
    From: man/minus_log_likelihood.Rd

c. Found the following (possibly) invalid DOIs:
  DOI: 10.2307/1914215
    From: DESCRIPTION
    Status: Forbidden
    Message: 403

All the words in (a) are names. The link of (b) is valid. The DOI in (c) is valid but requires authentication to access.
