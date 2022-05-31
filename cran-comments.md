# Changes in version 0.4.5

* Added deprecation messages in documentation and on package loading.

# Test Environments and Results (version 1.0.1)
## Windows
### (1) R-devel win-builder.r-project.org -- 2 NOTES
Installation time in seconds: 66
Check time in seconds: 278
Status: 2 NOTEs
R Under development (unstable) (2022-05-30 r82436 ucrt)

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
* checking R code for possible problems ... [58s] NOTE
File 'diseq/R/zzz.R':
  .onLoad calls:
    packageStartupMessage(message, appendLF = TRUE)

See section 'Good practice' in '?.onAttach'.

Results: https://win-builder.r-project.org/U4lFkZq51gZP/

### (2) R-oldrelease win-builder.r-project.org -- 2 NOTES
Installation time in seconds: 152
Check time in seconds: 579
Status: 2 NOTEs
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
* checking R code for possible problems ... [58s] NOTE
File 'diseq/R/zzz.R':
  .onLoad calls:
    packageStartupMessage(message, appendLF = TRUE)

See section 'Good practice' in '?.onAttach'.

Results: https://win-builder.r-project.org/7ZJhOcndZXYK/

### (3) R-release win-builder.r-project.org -- 2 NOTES
Installation time in seconds: 60
Check time in seconds: 277
Status: 2 NOTEs
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
* checking R code for possible problems ... [26s] NOTE
File 'diseq/R/zzz.R':
  .onLoad calls:
    packageStartupMessage(message, appendLF = TRUE)

See section 'Good practice' in '?.onAttach'.
	
Results: https://win-builder.r-project.org/m337T3wMXqUi/

### (4) rhub windows-x86_64-devel -- 1 NOTE
Build ID:	diseq_0.4.5.tar.gz-bd2ceb46cbd44a4899203dc18cec6c7d
Platform:	Windows Server 2022, R-devel, 64 bit
Submitted:	9 minutes 38.5 seconds ago
Build time:	9 minutes 32.4 seconds

NOTES:
* checking R code for possible problems ... NOTE
File 'diseq/R/zzz.R':
  .onLoad calls:
    packageStartupMessage(message, appendLF = TRUE)

See section 'Good practice' in '?.onAttach'.

See the full build log: https://artifacts.r-hub.io/diseq_0.4.5.tar.gz-bd2ceb46cbd44a4899203dc18cec6c7d/

## Linux, GCC
### (5) rhub ubuntu-gcc-release -- 2 NOTES
Build ID:	diseq_0.4.4.tar.gz-83c180aef28649c1979ab1b650c03367
Platform:	Ubuntu Linux 20.04.1 LTS, R-release, GCC
Submitted:	29 minutes 16 seconds ago
Build time:	28 minutes 59.4 seconds

* checking installed package size ... NOTE
  installed size is  6.1Mb
  sub-directories of 1Mb or more:
    doc    1.0Mb
    libs   3.4Mb
* checking R code for possible problems ... NOTE
File â€˜diseq/R/zzz.Râ€™:
  .onLoad calls:
    packageStartupMessage(message, appendLF = TRUE)

See section â€˜Good practiceâ€™ in '?.onAttach'.

See the full build log: https://artifacts.r-hub.io/diseq_0.4.5.tar.gz-040c7671adfd4c13a2ab1517849ac8a7/diseq.Rcheck/


### (6) rhub debian-gcc-devel -- 1 NOTE
Build ID:	diseq_0.4.5.tar.gz-4bd6ff79f7c045dbba80cb9a8d4c9a4a
Platform:	Debian Linux, R-devel, GCC
Submitted:	31 minutes 38.8 seconds ago
Build time:	31 minutes 27.1 seconds

* checking R code for possible problems ... NOTE
File â€˜diseq/R/zzz.Râ€™:
  .onLoad calls:
    packageStartupMessage(message, appendLF = TRUE)

See section â€˜Good practiceâ€™ in '?.onAttach'.

See the full build log: https://artifacts.r-hub.io/diseq_0.4.5.tar.gz-4bd6ff79f7c045dbba80cb9a8d4c9a4a/diseq.Rcheck/

### (7) rhub debian-gcc-devel-nold -- 1 NOTE
Build ID:	diseq_0.4.5.tar.gz-04047f9230e1492b805c7fba08919aba
Platform:	Debian Linux, R-devel, GCC, no long double
Submitted:	32 minutes 3.3 seconds ago
Build time:	31 minutes 26.8 seconds

* checking R code for possible problems ... NOTE
File â€˜diseq/R/zzz.Râ€™:
  .onLoad calls:
    packageStartupMessage(message, appendLF = TRUE)

See section â€˜Good practiceâ€™ in '?.onAttach'.

See the full build log: https://artifacts.r-hub.io/diseq_0.4.5.tar.gz-04047f9230e1492b805c7fba08919aba/

### (8) rhub rocker-gcc-san -- PREPERROR
Build ID:	diseq_0.4.5.tar.gz-d4d5dc22914c43e7bf2bfa183cb270dd
Platform:	Debian Linux, R-devel, GCC ASAN/UBSAN
Submitted:	45 minutes 11.8 seconds ago
Build time:	44 minutes 45.6 seconds

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
  
See the full build log: https://builder.r-hub.io/status/original/diseq_0.4.5.tar.gz-d4d5dc22914c43e7bf2bfa183cb270dd

### (9) Local (Ubuntu 20.04.3 LTS in WSL2 under Windows 11) -- 2 NOTES
── R CMD check results ──────────────────────────────────────────────────────────────────────── diseq 0.4.5 ────
Duration: 2m 7.5s

❯ checking installed package size ... NOTE
    installed size is  6.7Mb
    sub-directories of 1Mb or more:
      doc    1.0Mb
      libs   4.0Mb

❯ checking R code for possible problems ... NOTE
  File ‘diseq/R/zzz.R’:
    .onLoad calls:
      packageStartupMessage(message, appendLF = TRUE)
  
  See section ‘Good practice’ in '?.onAttach'.

0 errors ✔ | 0 warnings ✔ | 2 notes ✖

## Linux, LLVM
### (10) rhub debian-clang-devel -- 1 NOTE
Build ID:	diseq_0.4.5.tar.gz-57249085617f46a49ed5b13f8b369eac
Platform:	Debian Linux, R-devel, clang, ISO-8859-15 locale
Submitted:	36 minutes 9.9 seconds ago
Build time:	35 minutes 36.3 seconds

NOTES:
* checking R code for possible problems ... NOTE
File 'diseq/R/zzz.R':
  .onLoad calls:
    packageStartupMessage(message, appendLF = TRUE)

See section 'Good practice' in '?.onAttach'.

See the full build log: https://artifacts.r-hub.io/diseq_0.4.5.tar.gz-57249085617f46a49ed5b13f8b369eac/

### (11) rhub fedora-clang-devel -- 1 NOTE
Build ID:	diseq_0.4.5.tar.gz-ef07c15a24cc462a9e83eeace3810800
Platform:	Fedora Linux, R-devel, clang, gfortran
Submitted:	28 minutes 58.1 seconds ago
Build time:	28 minutes 37.4 seconds

NOTES:
* checking R code for possible problems ... NOTE
File ‘diseq/R/zzz.R’:
  .onLoad calls:
    packageStartupMessage(message, appendLF = TRUE)

See section ‘Good practice’ in '?.onAttach'.

See the full build log: https://artifacts.r-hub.io/diseq_0.4.5.tar.gz-ef07c15a24cc462a9e83eeace3810800/


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

See the full build log: https://builder.r-hub.io/status/original/diseq_0.4.5.tar.gz-837f52265dcb4589a4c89ac94bffe25c

## Macos
### (13) rhub macos-highsierra-release-cran -- 1 NOTE
Build ID:	diseq_0.4.5.tar.gz-49e36c1c15084b24b60e1b11438186a9
Platform:	macOS 10.13.6 High Sierra, R-release, brew
Submitted:	8 minutes 51.9 seconds ago
Build time:	8 minutes 17.3 seconds

NOTES:
* checking R code for possible problems ... NOTE
File ‘diseq/R/zzz.R’:
  .onLoad calls:
    packageStartupMessage(message, appendLF = TRUE)

See section ‘Good practice’ in '?.onAttach'.

See the full build log: https://artifacts.r-hub.io/diseq_0.4.5.tar.gz-49e36c1c15084b24b60e1b11438186a9/

### (14) rhub macos-highsierra-release -- 1 NOTE
Build ID:	diseq_0.4.5.tar.gz-3a2b3f161a5c4bd7a292c3035506b624
Platform:	macOS 10.13.6 High Sierra, R-release, CRAN's setup
Submitted:	8 minutes 48.1 seconds ago
Build time:	8 minutes 35.2 seconds

NOTES:
* checking R code for possible problems ... NOTE
File ‘diseq/R/zzz.R’:
  .onLoad calls:
    packageStartupMessage(message, appendLF = TRUE)

See section ‘Good practice’ in '?.onAttach'.

See the full build log: https://artifacts.r-hub.io/diseq_0.4.5.tar.gz-3a2b3f161a5c4bd7a292c3035506b624/

### (15) Mac mini at https://mac.r-project.org/macbuilder/submit.html -- 2 NOTES
Build system: r-devel-macosx-arm64|4.2.0|macosx|macOS 11.5.2 (20G95)|Mac mini|Apple M1||en_US.UTF-8

* checking installed package size ... NOTE
  installed size is  6.4Mb
  sub-directories of 1Mb or more:
    doc    1.0Mb
    libs   3.8Mb
* checking R code for possible problems ... [6s/6s] NOTE
File ‘diseq/R/zzz.R’:
  .onLoad calls:
    packageStartupMessage(message, appendLF = TRUE)

See section ‘Good practice’ in '?.onAttach'.

Results: https://mac.r-project.org/macbuilder/results/1654025596-7e1cb2991911de6c/

