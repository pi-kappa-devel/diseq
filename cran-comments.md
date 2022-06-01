# Changes in version 0.4.6

* Added deprecation messages in documentation and on package attaching.

# Test Environments and Results (version 1.0.1)
## Windows
### (1) R-devel win-builder.r-project.org -- 1 NOTE
Installation time in seconds: 56
Check time in seconds: 277
Status: 1 NOTE
R Under development (unstable) (2022-05-31 r82437 ucrt)

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

Results: https://win-builder.r-project.org/T9XfoCLFpQMP/

### (2) R-oldrelease win-builder.r-project.org -- 1 NOTE
Installation time in seconds: 159
Check time in seconds: 602
Status: 1 NOTE
R version 4.1.3 (2022-03-10)

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

Results: https://win-builder.r-project.org/eD93qKX5o3TP/

### (3) R-release win-builder.r-project.org -- 1 NOTE
Installation time in seconds: 62
Check time in seconds: 281
Status: 1 NOTE
R version 4.2.0 (2022-04-22 ucrt)

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2307/1913181
    From: README.md
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
	
Results: https://win-builder.r-project.org/DLf5wY0Mmkv4/

### (4) rhub windows-x86_64-devel -- OK
Build ID:	diseq_0.4.6.tar.gz-dba7cbab20234dbbad900e5a89f216ee
Platform:	Windows Server 2022, R-devel, 64 bit
Submitted:	9 minutes 44.4 seconds ago
Build time:	9 minutes 39.6 seconds

See the full build log: https://artifacts.r-hub.io/diseq_0.4.6.tar.gz-dba7cbab20234dbbad900e5a89f216ee/

## Linux, GCC
### (5) rhub ubuntu-gcc-release -- 1 NOTE
Build ID:	diseq_0.4.6.tar.gz-2bb85047903c4d0ab5d1f329c44b8dc2
Platform:	Ubuntu Linux 20.04.1 LTS, R-release, GCC
Submitted:	33 minutes 52.8 seconds ago
Build time:	33 minutes 48.3 seconds

* checking installed package size ... NOTE
  installed size is  6.1Mb
  sub-directories of 1Mb or more:
    doc    1.0Mb
    libs   3.4Mb

See the full build log: https://artifacts.r-hub.io/diseq_0.4.6.tar.gz-2bb85047903c4d0ab5d1f329c44b8dc2/diseq.Rcheck/00check.log


### (6) rhub debian-gcc-devel -- OK
Build ID:	diseq_0.4.6.tar.gz-7b670e8c7c2c488694d7f6ae8be6a679
Platform:	Debian Linux, R-devel, GCC
Submitted:	37 minutes 48.1 seconds ago
Build time:	37 minutes 42.9 seconds

See the full build log: https://builder.r-hub.io/status/original/diseq_0.4.6.tar.gz-7b670e8c7c2c488694d7f6ae8be6a679

### (7) rhub debian-gcc-devel-nold -- OK
Build ID:	diseq_0.4.6.tar.gz-9cc8f6aef50643c38cf1b83c290fcd22
Platform:	Debian Linux, R-devel, GCC, no long double
Submitted:	37 minutes 50.3 seconds ago
Build time:	37 minutes 45.6 seconds

See the full build log: https://builder.r-hub.io/status/original/diseq_0.4.6.tar.gz-9cc8f6aef50643c38cf1b83c290fcd22

### (8) rhub rocker-gcc-san -- PREPERROR
Build ID:	diseq_0.4.6.tar.gz-89ee68bbb670404facd1ce2f1f982bc3
Platform:	Debian Linux, R-devel, GCC ASAN/UBSAN
Submitted:	53 minutes 38.8 seconds ago
Build time:	53 minutes 34.7 seconds

Error : Bioconductor does not yet build and check packages for R version 4.3; see
  https://bioconductor.org/install
ERROR: dependency ‘systemfit’ is not available for package ‘diseq’
* removing ‘/home/docker/R/diseq’
Warning messages:
1: In i.p(...) : installation of package ‘lme4’ had non-zero exit status
2: In i.p(...) :
  installation of package ‘pbkrtest’ had non-zero exit status
3: In i.p(...) : installation of package ‘car’ had non-zero exit status
4: In i.p(...) :
  installation of package ‘systemfit’ had non-zero exit status
5: In i.p(...) :
  installation of package ‘/tmp/Rtmp03F3vy/file1337e17e1eb/diseq_0.4.4.tar.gz’ had non-zero exit status
> 
> 
Error : Bioconductor does not yet build and check packages for R version 4.3; see
  https://bioconductor.org/install
  
See the full build log: https://builder.r-hub.io/status/original/diseq_0.4.6.tar.gz-89ee68bbb670404facd1ce2f1f982bc3

### (9) Local (Ubuntu 20.04.3 LTS in WSL2 under Windows 11) -- 1 NOTE
── R CMD check results ──────────────────────────────────────────── diseq 0.4.6 ────
Duration: 2m 28.1s

❯ checking installed package size ... NOTE
    installed size is  6.7Mb
    sub-directories of 1Mb or more:
      doc    1.0Mb
      libs   4.0Mb

0 errors ✔ | 0 warnings ✔ | 1 note ✖

## Linux, LLVM
### (10) rhub debian-clang-devel -- OK
Build ID:	diseq_0.4.6.tar.gz-1074f7001f3b43dbb2522dc3d346c141
Platform:	Debian Linux, R-devel, clang, ISO-8859-15 locale
Submitted:	42 minutes 4.8 seconds ago
Build time:	42 minutes 3.2 seconds

See the full build log: https://artifacts.r-hub.io/diseq_0.4.6.tar.gz-1074f7001f3b43dbb2522dc3d346c141/

### (11) rhub fedora-clang-devel -- OK
Build ID:	diseq_0.4.6.tar.gz-f03b2239065a4073a805ac741ace8ab6
Platform:	Fedora Linux, R-devel, clang, gfortran
Submitted:	34 minutes 13.3 seconds ago
Build time:	34 minutes 11.1 seconds

See the full build log: https://artifacts.r-hub.io/diseq_0.4.6.tar.gz-f03b2239065a4073a805ac741ace8ab6/


## Solaris
### (12) rhub solaris-x86-patched -- PREPERROR
Build ID:	diseq_0.4.5.tar.gz-837f52265dcb4589a4c89ac94bffe25c
Platform:	Oracle Solaris 10, x86, 32 bit, R-release
Submitted:	31 minutes 17.1 seconds ago
Build time:	30 minutes 40.1 seconds

ERROR: package installation failed
STDERR:

Error: Failed to install 'diseq' from local:
  Failed to `R CMD build` package, try `build = FALSE`.
In addition: Warning messages:
1: In i.p(...) : installation of package ‘ps’ had non-zero exit status
2: In i.p(...) :
  installation of package ‘processx’ had non-zero exit status
3: In i.p(...) : installation of package ‘callr’ had non-zero exit status
4: In i.p(...) :
  installation of package ‘testthat’ had non-zero exit status
5: In i.p(...) : installation of package ‘nloptr’ had non-zero exit status
6: In i.p(...) : installation of package ‘lme4’ had non-zero exit status
7: In i.p(...) :
  installation of package ‘pbkrtest’ had non-zero exit status
8: In i.p(...) : installation of package ‘car’ had non-zero exit status
9: In i.p(...) :
  installation of package ‘systemfit’ had non-zero exit status
Execution halted

See the full build log: https://builder.r-hub.io/status/original/diseq_0.4.6.tar.gz-9b3b13c5e97f461e884afcf1ccd464ff

## Macos
### (13) rhub macos-highsierra-release-cran -- OK
Build ID:	diseq_0.4.6.tar.gz-7c0b578eaa4f42f9963098edff973ff9
Platform:	macOS 10.13.6 High Sierra, R-release, brew
Submitted:	9 minutes 12.9 seconds ago
Build time:	9 minutes 10.5 seconds

See the full build log: https://artifacts.r-hub.io/diseq_0.4.6.tar.gz-7c0b578eaa4f42f9963098edff973ff9/

### (14) rhub macos-highsierra-release -- OK
Build ID:	diseq_0.4.6.tar.gz-1d6c17296f7c4e018833cdbff133c46a
Platform:	macOS 10.13.6 High Sierra, R-release, CRAN's setup
Submitted:	8 minutes 56.8 seconds ago
Build time:	8 minutes 53.2 seconds


See the full build log: https://artifacts.r-hub.io/diseq_0.4.6.tar.gz-1d6c17296f7c4e018833cdbff133c46a/

### (15) Mac mini at https://mac.r-project.org/macbuilder/submit.html -- 1 NOTE
Build system: r-devel-macosx-arm64|4.2.0|macosx|macOS 11.5.2 (20G95)|Mac mini|Apple M1||en_US.UTF-8


* checking installed package size ... NOTE
  installed size is  6.4Mb
  sub-directories of 1Mb or more:
    doc    1.0Mb
    libs   3.8Mb

Results: https://mac.r-project.org/macbuilder/results/1654099213-e4124ab438dc9dcf/
