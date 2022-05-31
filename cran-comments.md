# Changes in version 0.4.4

* Added deprecation warning in documentation and on package loading.

# Test Environments and Results (version 1.0.1)
## Windows
### (1) R-devel win-builder.r-project.org -- 1 WARNING, 2 NOTES
Installation time in seconds: 61
Check time in seconds: 275
Status: 1 WARNING, 2 NOTEs
R Under development (unstable) (2022-05-30 r82436 ucrt)

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2307/1913181
    From: README.md
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.2307/1914215
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
* checking whether package 'diseq' can be installed ... WARNING
Found the following significant warnings:
  Warning: Package diseq is deprecated. Please use package markets instead.
See 'd:/RCompile/CRANguest/R-devel/diseq.Rcheck/00install.out' for details.
* checking whether the namespace can be loaded with stated dependencies ... NOTE
Warning: Package diseq is deprecated. Please use package markets instead.
This warning is displayed once every 8 hours.

A namespace must be able to be loaded with just the base namespace
loaded: otherwise if the namespace gets loaded by a saved object, the
session will be unable to start.

Probably some imports need to be declared in the NAMESPACE file.

Results: https://win-builder.r-project.org/X27u3OoMMC4U/

### (2) R-oldrelease win-builder.r-project.org -- 1 WARNING, 3 NOTES
Installation time in seconds: 151
Check time in seconds: 568
Status: 1 WARNING, 3 NOTEs
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
* checking whether package 'diseq' can be installed ... WARNING
Found the following significant warnings:
  Warning: Package diseq is deprecated. Please use package markets instead.
See 'd:/RCompile/CRANguest/R-oldrelease/diseq.Rcheck/00install.out' for details.
** checking whether the namespace can be loaded with stated dependencies ... NOTE
Warning: Package diseq is deprecated. Please use package markets instead.
This warning is displayed once every 8 hours.

A namespace must be able to be loaded with just the base namespace
loaded: otherwise if the namespace gets loaded by a saved object, the
session will be unable to start.

Probably some imports need to be declared in the NAMESPACE file.
** checking whether the namespace can be loaded with stated dependencies ... NOTE
Warning: Package diseq is deprecated. Please use package markets instead.
This warning is displayed once every 8 hours.

A namespace must be able to be loaded with just the base namespace
loaded: otherwise if the namespace gets loaded by a saved object, the
session will be unable to start.

Probably some imports need to be declared in the NAMESPACE file.

Results: https://win-builder.r-project.org/qB9n2I3fOerB/00check.log

### (3) R-release win-builder.r-project.org -- 1 WARNING, 2 NOTES
Installation time in seconds: 56
Check time in seconds: 281
Status: 1 WARNING, 2 NOTEs
R version 4.2.0 (2022-04-22 ucrt)

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Pantelis Karapanagiotis <pikappa.devel@gmail.com>'

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.2307/1913181
    From: README.md
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.2307/1914215
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
* checking whether package 'diseq' can be installed ... WARNING
Found the following significant warnings:
  Warning: Package diseq is deprecated. Please use package markets instead.
See 'd:/RCompile/CRANguest/R-release/diseq.Rcheck/00install.out' for details.
* checking whether the namespace can be loaded with stated dependencies ... NOTE
Warning: Package diseq is deprecated. Please use package markets instead.
This warning is displayed once every 8 hours.

A namespace must be able to be loaded with just the base namespace
loaded: otherwise if the namespace gets loaded by a saved object, the
session will be unable to start.

Probably some imports need to be declared in the NAMESPACE file.

Results: https://win-builder.r-project.org/98eu7i4bDHW2/

### (4) rhub windows-x86_64-devel -- 1 WARNING, 1 NOTE
Build ID:	diseq_0.4.4.tar.gz-a9b2152c7d784844a29923ea52f08287
Platform:	Windows Server 2022, R-devel, 64 bit
Submitted:	9 minutes 37.8 seconds ago
Build time:	9 minutes 31.9 seconds

WARNINGS:
* checking whether package 'diseq' can be installed ... WARNING
Found the following significant warnings:
  Warning: Package diseq is deprecated. Please use package markets instead.
See 'C:/Users/USERwndEgJHckb/diseq.Rcheck/00install.out' for details.
NOTES:
* checking whether the namespace can be loaded with stated dependencies ... NOTE
Warning: Package diseq is deprecated. Please use package markets instead.
This warning is displayed once every 8 hours.

session will be unable to start.

loaded: otherwise if the namespace gets loaded by a saved object, the
A namespace must be able to be loaded with just the base namespace
Probably some imports need to be declared in the NAMESPACE file.

See the full build log: https://artifacts.r-hub.io/diseq_0.4.4.tar.gz-a9b2152c7d784844a29923ea52f08287/

## Linux, GCC
### (5) rhub ubuntu-gcc-release -- 1 WARNING, 2 NOTES
Build ID:	diseq_0.4.4.tar.gz-83c180aef28649c1979ab1b650c03367
Platform:	Ubuntu Linux 20.04.1 LTS, R-release, GCC
Submitted:	29 minutes 16 seconds ago
Build time:	28 minutes 59.4 seconds

* checking whether package â€˜diseqâ€™ can be installed ... WARNING
Found the following significant warnings:
  Warning: Package diseq is deprecated. Please use package markets instead.
See â€˜/home/docker/diseq.Rcheck/00install.outâ€™ for details.
* checking installed package size ... NOTE
  installed size is  6.1Mb
  sub-directories of 1Mb or more:
    doc    1.0Mb
    libs   3.4Mb
* checking whether the namespace can be loaded with stated dependencies ... NOTE
Warning: Package diseq is deprecated. Please use package markets instead.
This warning is displayed once every 8 hours.

A namespace must be able to be loaded with just the base namespace
loaded: otherwise if the namespace gets loaded by a saved object, the
session will be unable to start.

Probably some imports need to be declared in the NAMESPACE file.

See the full build log: https://artifacts.r-hub.io/diseq_0.4.4.tar.gz-83c180aef28649c1979ab1b650c03367/diseq.Rcheck/


### (6) rhub debian-gcc-devel -- 1 WARNING, 1 NOTE
Build ID:	diseq_0.4.4.tar.gz-a3099650d8e44b4cac7bf95c3e889adc
Platform:	Debian Linux, R-devel, GCC
Submitted:	32 minutes 49.5 seconds ago
Build time:	32 minutes 42.3 seconds

* checking whether package â€˜diseqâ€™ can be installed ... WARNING
Found the following significant warnings:
  Warning: Package diseq is deprecated. Please use package markets instead.
* checking whether the namespace can be loaded with stated dependencies ... NOTE
Warning: Package diseq is deprecated. Please use package markets instead.
This warning is displayed once every 8 hours.

A namespace must be able to be loaded with just the base namespace
loaded: otherwise if the namespace gets loaded by a saved object, the
session will be unable to start.

Probably some imports need to be declared in the NAMESPACE file.

See the full build log: https://artifacts.r-hub.io/diseq_0.4.4.tar.gz-a3099650d8e44b4cac7bf95c3e889adc/

### (7) rhub debian-gcc-devel-nold -- 1 WARNING, 1 NOTE
Build ID:	diseq_0.4.4.tar.gz-331b9e8a55f145acb4d37e9d85ba3dbf
Platform:	Debian Linux, R-devel, GCC, no long double
Submitted:	32 minutes 51.5 seconds ago
Build time:	32 minutes 26.3 seconds

* checking whether package â€˜diseqâ€™ can be installed ... WARNING
Found the following significant warnings:
  Warning: Package diseq is deprecated. Please use package markets instead.
See â€˜/home/docker/diseq.Rcheck/00install.outâ€™ for details
* checking whether the namespace can be loaded with stated dependencies ... NOTE
Warning: Package diseq is deprecated. Please use package markets instead.
This warning is displayed once every 8 hours.

A namespace must be able to be loaded with just the base namespace
loaded: otherwise if the namespace gets loaded by a saved object, the
session will be unable to start.

Probably some imports need to be declared in the NAMESPACE file.

See the full build log: https://artifacts.r-hub.io/diseq_0.4.4.tar.gz-331b9e8a55f145acb4d37e9d85ba3dbf/diseq.Rcheck/

### (8) rhub rocker-gcc-san -- PREPERROR
Build ID:	diseq_0.4.4.tar.gz-a1d14d7bf323440b9138dd81cd7da739
Platform:	Debian Linux, R-devel, GCC ASAN/UBSAN
Submitted:	46 minutes 45.3 seconds ago
Build time:	46 minutes 26.5 seconds

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
  
See the full build log: https://builder.r-hub.io/status/original/diseq_0.4.4.tar.gz-a1d14d7bf323440b9138dd81cd7da739

### (9) Local (Ubuntu 20.04.3 LTS in WSL2 under Windows 11) -- 1 WARNING, 2 NOTES
── R CMD check results ──────────────────────────────────────────────────────────────────────── diseq 0.4.4 ────
Duration: 2m 9.5s

❯ checking whether package ‘diseq’ can be installed ... WARNING
  See below...

❯ checking installed package size ... NOTE
    installed size is  6.7Mb
    sub-directories of 1Mb or more:
      doc    1.0Mb
      libs   4.0Mb

❯ checking whether the namespace can be loaded with stated dependencies ... NOTE
  Warning: Package diseq is deprecated. Please use package markets instead.
  This warning is displayed once every 8 hours.
  
  A namespace must be able to be loaded with just the base namespace
  loaded: otherwise if the namespace gets loaded by a saved object, the
  session will be unable to start.
  
  Probably some imports need to be declared in the NAMESPACE file.

0 errors ✔ | 1 warning ✖ | 2 notes ✖

## Linux, LLVM
### (10) rhub debian-clang-devel -- 1 WARNING, 1 NOTE
Build ID:	diseq_0.4.4.tar.gz-40c287a8b0944a72b870c2b4e4b9c0a1
Platform:	Debian Linux, R-devel, clang, ISO-8859-15 locale
Submitted:	37 minutes 37.5 seconds ago
Build time:	37 minutes 1.1 seconds

WARNINGS:
* checking whether package 'diseq' can be installed ... WARNING
Found the following significant warnings:
  Warning: Package diseq is deprecated. Please use package markets instead.
See '/home/docker/diseq.Rcheck/00install.out' for details.
NOTES:
* checking whether the namespace can be loaded with stated dependencies ... NOTE
Warning: Package diseq is deprecated. Please use package markets instead.
This warning is displayed once every 8 hours.

A namespace must be able to be loaded with just the base namespace
loaded: otherwise if the namespace gets loaded by a saved object, the
session will be unable to start.

Probably some imports need to be declared in the NAMESPACE file.

See the full build log: https://artifacts.r-hub.io/diseq_0.4.4.tar.gz-40c287a8b0944a72b870c2b4e4b9c0a1/diseq.Rcheck/

### (11) rhub fedora-clang-devel -- 1 WARNING, 1 NOTE
Build ID:	diseq_0.4.4.tar.gz-ff19ed3a914c4efdb0aaa346bbf77b18
Platform:	Fedora Linux, R-devel, clang, gfortran
Submitted:	30 minutes 0.4 seconds ago
Build time:	29 minutes 37.8 seconds

WARNINGS:
* checking whether package ‘diseq’ can be installed ... WARNING
Found the following significant warnings:
  Warning: Package diseq is deprecated. Please use package markets instead.
See ‘/home/docker/diseq.Rcheck/00install.out’ for details.
NOTES:
* checking whether the namespace can be loaded with stated dependencies ... NOTE
Warning: Package diseq is deprecated. Please use package markets instead.
This warning is displayed once every 8 hours.

A namespace must be able to be loaded with just the base namespace
loaded: otherwise if the namespace gets loaded by a saved object, the
session will be unable to start.

Probably some imports need to be declared in the NAMESPACE file.

See the full build log: https://artifacts.r-hub.io/diseq_0.4.4.tar.gz-ff19ed3a914c4efdb0aaa346bbf77b18/


## Solaris
### (12) rhub solaris-x86-patched -- PREPERROR
Build ID:	diseq_0.4.4.tar.gz-9172cbe42d504021b882d1813381a979
Platform:	Oracle Solaris 10, x86, 32 bit, R-release
Submitted:	30 minutes 58.5 seconds ago
Build time:	30 minutes 28.1 seconds

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

See the full build log: https://builder.r-hub.io/status/original/diseq_0.4.4.tar.gz-9172cbe42d504021b882d1813381a979

## Macos
### (13) rhub macos-highsierra-release-cran -- 1 WARNING, 1 NOTE
Build ID:	diseq_0.4.4.tar.gz-566d87ac14d24afdb0fdde8e8ebfc7fd
Platform:	macOS 10.13.6 High Sierra, R-release, brew
Submitted:	8 minutes 39.8 seconds ago
Build time:	8 minutes 16.2 seconds

WARNINGS:
* checking whether package ‘diseq’ can be installed ... WARNING
Found the following significant warnings:
  Warning: Package diseq is deprecated. Please use package markets instead.
See ‘/Users/useramAiCww6/diseq.Rcheck/00install.out’ for details.
NOTES:
* checking whether the namespace can be loaded with stated dependencies ... NOTE
Warning: Package diseq is deprecated. Please use package markets instead.
This warning is displayed once every 8 hours.

A namespace must be able to be loaded with just the base namespace
loaded: otherwise if the namespace gets loaded by a saved object, the
session will be unable to start.

Probably some imports need to be declared in the NAMESPACE file.

See the full build log: https://artifacts.r-hub.io/diseq_0.4.4.tar.gz-566d87ac14d24afdb0fdde8e8ebfc7fd/

### (14) rhub macos-highsierra-release -- 1 WARNING, 1 NOTE
Build ID:	diseq_0.4.4.tar.gz-2bc3fd5c5d6149699e1b2fb6e48e5c2f
Platform:	macOS 10.13.6 High Sierra, R-release, CRAN's setup
Submitted:	8 minutes 38.4 seconds ago
Build time:	8 minutes 6.2 seconds

WARNINGS:
* checking whether package ‘diseq’ can be installed ... WARNING
Found the following significant warnings:
  Warning: Package diseq is deprecated. Please use package markets instead.
See ‘/Users/userWFWpEODk/diseq.Rcheck/00install.out’ for details.
NOTES:
* checking whether the namespace can be loaded with stated dependencies ... NOTE
Warning: Package diseq is deprecated. Please use package markets instead.
This warning is displayed once every 8 hours.

A namespace must be able to be loaded with just the base namespace
loaded: otherwise if the namespace gets loaded by a saved object, the
session will be unable to start.

See the full build log: https://artifacts.r-hub.io/diseq_0.4.4.tar.gz-2bc3fd5c5d6149699e1b2fb6e48e5c2f/

### (15) Mac mini at https://mac.r-project.org/macbuilder/submit.html -- 1 WARNING, 2 NOTES
Build system: r-devel-macosx-arm64|4.2.0|macosx|macOS 11.5.2 (20G95)|Mac mini|Apple M1||en_US.UTF-8

* checking whether package ‘diseq’ can be installed ... [16s/18s] WARNING
Found the following significant warnings:
  Warning: Package diseq is deprecated. Please use package markets instead.
See ‘/Volumes/PkgBuild/work/1653985522-8003ce744f219e3a/packages/big-sur-arm64/results/4.2/diseq.Rcheck/00install.out’ for details.
* checking installed package size ... NOTE
  installed size is  6.4Mb
  sub-directories of 1Mb or more:
    doc    1.0Mb
    libs   3.8Mb
* checking whether the namespace can be loaded with stated dependencies ... NOTE
Warning: Package diseq is deprecated. Please use package markets instead.
This warning is displayed once every 8 hours.

A namespace must be able to be loaded with just the base namespace
loaded: otherwise if the namespace gets loaded by a saved object, the
session will be unable to start.

Probably some imports need to be declared in the NAMESPACE file.

Results: https://mac.r-project.org/macbuilder/results/1653985522-8003ce744f219e3a/

