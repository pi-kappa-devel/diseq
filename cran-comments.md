# Changes in version 0.4.3

* Version 0.4.3 patches 0.4.2 unit test failure in M1 machines.
 - All platforms were checked once more.
 - Added an m1 build platform. The new results are in (15).

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
8. (rhub rocker-gcc-san) Debian Linux, R-devel, GCC ASAN/UBSAN 
9. (local) Ubuntu 20.04.3 LTS (in WSL2 under Windows 11), GCC 9.3.0, R version 3.6.3 (2020-02-29) -- "Holding the Windsock"

### LLVM
10. (rhub debian-clang-devel) Debian Linux, R-devel, clang, ISO-8859-15 locale, R Under development (unstable) (2022-02-06 r81658)
11. (rhub fedora-clang-devel) Fedora Linux, R-devel, clang, gfortran, R Under development (unstable) (2022-02-06 r81658)

## Solaris
12. (rhub solaris-x86-patched) Oracle Solaris 10, x86, 32 bit, R-release

## Macos
13. (rhub macos-highsierra-release-cran) macOS 10.13.6 High Sierra, R-release, CRAN's setup, R version 4.1.1 (2021-08-10)
14. (rhub macos-highsierra-release) macOS 10.13.6 High Sierra, R-release, brew, R version 4.1.1 (2021-08-10)
15. (https://mac.r-project.org/macbuilder/submit.html) r-release-macosx-arm64|4.1.1|macosx|macOS 11.5.2 (20G95)|Mac mini|Apple M1||en_US.UTF-8

# Check results
## R CMD check results in (1) -- std=gnu++11, with GSL, no execution header -- 1 Note
Installation time in seconds: 131
Check time in seconds: 517
Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Days since last update: 2

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

Artifacts: https://win-builder.r-project.org/At12xfhZMkIn/

## R CMD check results in (2) -- std=gnu++11, with GSL, no execution header -- 1 Note
Installation time in seconds: 152
Check time in seconds: 581
Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Days since last update: 2

Artifacts: https://win-builder.r-project.org/ehWjs5gl2G6W/


## R CMD check results in (3) -- std=gnu++11, with GSL, no execution header -- 1 Note
Installation time in seconds: 154
Check time in seconds: 576
Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Days since last update: 2

Output: https://win-builder.r-project.org/2O3khlQsZtqR/


## R CMD check results in (4) -- std=gnu++11, with GSL, no execution header -- OK
Build time: 12 minutes 36.7 seconds
Status: OK

Artifacts: https://artifacts.r-hub.io/diseq_0.4.3.tar.gz-1cfd5a641ebf4d48a264e2abede981db/

## R CMD check results in (5) -- std=gnu++11, with GSL, no execution header -- 1 Note
Build time: 27 minutes 59.9 seconds
Status: 1 NOTE

* checking installed package size ... NOTE
  installed size is  6.1Mb
  sub-directories of 1Mb or more:
    doc    1.0Mb
    libs   3.4Mb
	
Artifacts: https://artifacts.r-hub.io/diseq_0.4.3.tar.gz-2c533eeb07474a1faa50984733673d08/diseq.Rcheck/

## R CMD check results in (6) -- std=c++17, with GSL, with execution header -- OK
Build time:	32 minutes 45.3 seconds
Status: OK

Artifacts: https://artifacts.r-hub.io/diseq_0.4.3.tar.gz-5fd25822a0e84d369082452e187a886a/

## R CMD check results in (7) -- std=c++17, with GSL, with execution header -- OK
Build time:	32 minutes 28.1 seconds
Status: OK

Artifacts: https://artifacts.r-hub.io/diseq_0.4.3.tar.gz-6dbf31312b4e4e489d51dcff505fd09d/diseq.Rcheck/

## R CMD check results in (8) -- std=c++17, with GSL, with execution header -- Preperror
Build time: 36 minutes 42 seconds

There might be an issue with the gcc version. The first error is 
"""
* installing *source* package ‘Rcpp’ ...
** package ‘Rcpp’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -fsanitize=undefined,bounds-strict -fno-omit-frame-pointer -std=gnu++98 -I"/usr/local/lib/R/include" -DNDEBUG -I../inst/include/  -I/usr/local/include   -fpic  -g -O2 -Wall -pedantic -mtune=native  -c api.cpp -o api.o
g++ -fsanitize=undefined,bounds-strict -fno-omit-frame-pointer -std=gnu++98 -I"/usr/local/lib/R/include" -DNDEBUG -I../inst/include/  -I/usr/local/include   -fpic  -g -O2 -Wall -pedantic -mtune=native  -c attributes.cpp -o attributes.o
attributes.cpp: In member function ‘std::string Rcpp::attributes::Attribute::customRSignature() const’:
attributes.cpp:404:20: error: ‘std::string’ {aka ‘class std::__cxx11::basic_string<char>’} has no member named ‘back’
  404 |             if(sig.back() == '}')
      |                    ^~~~
attributes.cpp:408:20: error: ‘std::string’ {aka ‘class std::__cxx11::basic_string<char>’} has no member named ‘front’
  408 |             if(sig.front() == '{')
      |                    ^~~~~
attributes.cpp: In function ‘bool Rcpp::attributes::checkRSignature(const Rcpp::attributes::Function&, std::string)’:
attributes.cpp:2813:45: error: ‘>>’ should be ‘> >’ within a nested template argument list
 2813 |             Rcpp::as<std::vector<std::string>>(pargs_cv);
      |                                             ^~
      |                                             > >
make: *** [/usr/local/lib/R/etc/Makeconf:177: attributes.o] Error 1
ERROR: compilation failed for package ‘Rcpp’
"""

Then, there is a chain of package installation failures up to diseq.

Output: https://builder.r-hub.io/status/original/diseq_0.4.3.tar.gz-91faef586ec24491be4ae220bce43418

## R CMD check results in (9) -- std=c++17, with GSL, with execution header -- 1 Note
── R CMD check results ──────────────────────────────────────────────────────────────────────────────────── diseq 0.4.2 ────
Duration: 2m 35.7s

❯ checking installed package size ... NOTE
    installed size is  6.6Mb
    sub-directories of 1Mb or more:
      doc    1.0Mb
      libs   4.0Mb

0 errors ✔ | 0 warnings ✔ | 1 note ✖

## R CMD check results in (10) -- std=c++17, with GSL, with execution header -- OK
Build time:	37 minutes 51.4 seconds
Status: OK

Artifacts: https://artifacts.r-hub.io/diseq_0.4.2.tar.gz-d019ef5b50744495b8cc1dc3213afb50/diseq.Rcheck/

## R CMD check results in (11) -- std=c++17, with GSL, with execution header -- OK
Build time:	28 minutes 39.7 seconds
Status: OK
	
Artifacts: https://artifacts.r-hub.io/diseq_0.4.3.tar.gz-a8fa07f9fd324fd9850766f1155bd0b5/diseq.Rcheck/


## R CMD check results in (12) -- std=gnu++11, with GSL, no execution header -- Preperror
Build time:	16 minutes 8.1 seconds

Point of failure most probably is:
ERROR: compilation failed for package ‘nloptr’

This error message might be relevant:
"
using NLopt via local cmake build on i86pc 
tools/cmake_call.sh: syntax error at line 3: `(' unexpected
"

Then, there is a chain of package installation failures up to diseq.

Artifacts: https://builder.r-hub.io/status/original/diseq_0.4.3.tar.gz-b481aca53dcd48a49f5945718a150a47

## R CMD check results in (13) -- std=gnu++11, with GSL, no execution header -- OK
Build time: 8 minutes 9.7 seconds
Status: OK

Artifacts: https://artifacts.r-hub.io/diseq_0.4.3.tar.gz-e8ccbecae28149338c8ceadec3b1ba7f/


## R CMD check results in (14) -- std=gnu++11, with GSL, no execution header -- OK
Build time: 9 minutes 15.4 seconds
Status: OK


Artifacts: https://artifacts.r-hub.io/diseq_0.4.3.tar.gz-6a2f9af870aa47f5b9fc6b512721b71a/

## R CMD check results in (15) -- std=gnu++11, with GSL, no execution header -- 1 Note
elapsed time (check, wall clock): 1:29
Status: 1 NOTE

* checking installed package size ... NOTE
  installed size is  6.4Mb
  sub-directories of 1Mb or more:
    doc    1.0Mb
    libs   3.8Mb

Artifacts: https://mac.r-project.org/macbuilder/results/1645003132-f2208e53f4dd83a2/
