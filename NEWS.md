# diseq 0.3.1

* Cumulative update of CRAN version.

# diseq 0.3.0.9004

* Included package article in documentation website.

# diseq 0.3.0.9003

* Added package article.

# diseq 0.3.0.9002

* Fixed bug in `show` and `summary` methods of `diseq_stochastic_adjustment`.
* Fixed bug in calculation of clustered standard errors.
* Changed input arguments of marginal effect calls to match the interface of the remaining post-analysis calls (changes the user space).
* Added `prefixed_quantity_variable` method.
* Added implementation figure.

# diseq 0.3.0.9001

* Fixed R check missing documentation entries.

# diseq 0.3.0.9000

* Changes in model simulation.
 - Simplified simulation calls (changes the user space).
 - Re-factored simulation code and exported additional functions. 
 - Added marginal system effect methods, and unified marginal probabilities effects methods (changes the user space).
* Improvements in documentation.
 - Minor typos corrections.
 - Added new examples.
 - Documented formulas in system and equation classes. 
 - Modified some of the examples to use the `houses` dataset.
 - Grouped documentation entries.

# diseq 0.2.1

* Cumulative update of CRAN version.

# diseq 0.2.0.9010

* Reduced file size of stochastic adjustment model's derivative calculations.

# diseq 0.2.0.9009

* Reduced file size of directional model's derivative calculations.

# diseq 0.2.0.9008

* Reduced file size of basic model's derivative calculations.

# diseq 0.2.0.9007

* Reduced file size of deterministic adjustment gradient calculation.

# diseq 0.2.0.9006

* Reduced file size of equilibrium gradient calculation.

# diseq 0.2.0.9005

* Removed get from access functions to reduce the verbosity of function calls. 

# diseq 0.2.0.9004

* Added validation functions for estimation input variables `gradient`, `hessian`, and `standard_errors`. 

# diseq 0.2.0.9003

* The input variable `gradient` controls whether the gradient is calculated by analytic expression or is numerically approximated. Switched from Boolean input to passing sting options so that the user interface for choosing gradient and hessian options is consistent.

# diseq 0.2.0.9002

* Fixed bug in `equilibrium_model` plot functionality.
* Minor improvements in `houses` documentation.

# diseq 0.2.0.9001

* Better options for hessian estimation: Consolidated all three potential options in the `hessian` input variable of `estimate`.
* Better options for adjusted standard errors: Consolidated all three potential options in the `standard_errors` input variable of `estimate`.

# diseq 0.1.5.9003

* Added houses dataset

# diseq 0.1.5.9002

* Fixed option class concerning the calculation of the Hessian in estimation calls. Models can be now estimated by skipping the Hessian, calculating it based on the analytic expressions, or calculating it numerically.

# diseq 0.1.5.9001

* Corrected bug in initialization of indicator variables.

# diseq 0.1.5

* Deployed development documentation website.

# diseq 0.1.4

* Cumulative patch of CRAN version.

# diseq 0.1.3.9012

* Updated the description entry of the `DESCRIPTION` file.
* Shortened `use_heteroscedasticity_consistent_errors` variable of `estimate` method to `use_heteroscedastic_errors`.

# diseq 0.1.3.9011

* Added python script for creating the `README` figures.

# diseq 0.1.3.9010

* Corrected calls to `system.file`.

# diseq 0.1.3.9009

* Updated `README.md`.
* Enclosed the plot example with `dontrun` instead of `donttest`.

# diseq 0.1.3.9008

* Added `png` and `grid` to dependencies.

# diseq 0.1.3.9007

* Added `plot` method for the all model classes.

# diseq 0.1.3.9006

* Added `plot` method for the equilibrium and basic disequilibrium model classes.
* Patched model initialization to avoid mutate warnings.

# diseq 0.1.3.9005

* Added `summary` method for the front-end model classes.
* Documentation improvements.
* Added online documentation link in `README.Rmd`
* Corrected link in the documentation of the summary method.

# diseq 0.1.3.9004

* Added `show` method for the front-end model classes.

# diseq 0.1.3.9003

* Removed `compile_commands.json` from source control.
* Modified the simulation parameters of the market clearing assessment vignette.

# diseq 0.1.3.9002

* Included a reference section title in the README file.

# diseq 0.1.3.9001

* Added documentation URL in DESCRIPTION.
* Added bibliography in the README file.

# diseq 0.1.3

* Patched `M1mac` additional issues: Added compilation flag for availability of `GSL`. The native code can be compiled also in systems without `GSL`, albeit offering an empty shell functionality for the moment.
* Documented changes in `maximize_log_likelihood` function.

# diseq 0.1.2

* Added `autotools` configuration script for cross-platform compilation. 
* Removed dependence on `C++20`. The sources are now `C++11` compliant and only use `C++17` and `libtbb` if it is available on the target machine. 
* Patch for `clang` compilation failure: reverting to sequential execution when compiling with clang and `libc++`. 

# diseq 0.1.1.9001

* Restructured and added unit tests to increase test coverage. 

# diseq 0.1.1

* Prepared CRAN submission. Small adjustments to README style. Updated CRAN comments.

# diseq 0.1.0.9004

* Adjusted file names so that they are consistent with the  API changes.

# diseq 0.1.0.9003

* Fixed `M1mac` issues. Adjusted README to API changes. 
* Replaced `href` with `doi` whenever relevant. 

# diseq 0.1.0.9002

* Added macro checks for C++20 execution policies features in C++ sources. 
* Removed calls to `std::ragnes::iota_view` and `std::reduce` to ensure C++11 compatibility.

# diseq 0.1.0.9001

* Adjusted vignettes to API changes. 

# diseq 0.1.0.9000

* Introduced the option maximizing the equilibrium model likelihood using `GSL` through `Rcpp`. 
* Added linting and formatting configuration files for R and C++ code. Cleaned C++ code. 
* Reorganized R back-end classes.

# diseq 0.0.14.9004

* Improved README file style.

# diseq 0.0.14.9003

* Corrected style attributes of README file.

# diseq 0.0.14.9002

* Corrected calculation of clustered standard errors by accounting for the number of used classes. 

# diseq 0.0.14.9001

* Changes to adjust for depreciating functionality of `dplyr` (as of 0.7.0)

# diseq 0.0.14

* Added option and documentation for estimating clustered standards errors.

# diseq 0.0.13.9002

* Added documentation for the function that return the scores.

# diseq 0.0.13.9001

* Added option for estimating heteroscedasticity-consistent (Huber-White) standard errors. 
* Added functionality for extracting the score matrices of the estimated models. 

# diseq 0.0.13.9000

* Corrected documentation typos.

# diseq 0.0.13

* Corrections of non-canonical web-links in README. Adjustments before CRAN submission.

# diseq 0.0.12.9002

* Added sections `A quick model tour`, `Alternative packages`, and `Planned extensions` in README.

# diseq 0.0.12.9001

* Added `noLD` in word exceptions list.

# diseq 0.0.12

* Fixed `noLD` issues.

# diseq 0.0.11.9002

* Renamed assessment vignette.

# diseq 0.0.11.9001

* Enabled BFGS-based estimation with numerical gradient. 
* Added CRAN installation instructions in README.

# diseq 0.0.11.9000

* Corrected punctuation errors in documentation.

# diseq 0.0.11

* Removed `get_correlation_variable` from exported functions. 
* Improved the documentation of `minus_log_likelihood`. 
* Reintroduced references in description.

# diseq 0.0.10

* Removed references from description to avoid CRAN notes.

# diseq 0.0.9

* Ignoring README.html from build. Removed links from description. Improved documentation examples.

# diseq 0.0.8

* Added examples to constructors, estimation, aggregation, and marginal effect functions.

# diseq 0.0.7.9002

* Skipping directional and stochastic adjustment tests on CRAN to reduce build time.

# diseq 0.0.7.9001

* Quoted all package names in DESCRIPTION. 
* To reduce build time: 1. Removed direction model estimation from equilibrium assessment vignette, 2. Decreased estimation accuracy of basic usage vignette to six digits.

# diseq 0.0.7

* Fixed order of arguments in web-link of estimation documentation. 
* Improved simulation documentation.

# diseq 0.0.6

* Corrected documentation typos. Fixed web-links.

# diseq 0.0.5.9009

* Improved documentation.

# diseq 0.0.5.9008

* Removed unused parameter from the constructor of the equilibrium two stage least square model.

# diseq 0.0.5.9007

* Removed dependence on `pastecs` package.

# diseq 0.0.5.9006

* Reformatted code using the `styler` package. Removed the `lintr` based test.

# diseq 0.0.5.9005

* Adjustments to address breaking changes of the `tibble` package.

# diseq 0.0.5.9004

* Added a vignette with an equilibrium assessment example.

# diseq 0.0.5.9003

* Added model-specific simulation functions.

# diseq 0.0.5.9002

* Refactored simulation code.

# diseq 0.0.5.9001

* Added simulation generating processes for all supported models. 

# diseq 0.0.4.9013

* Separated auto-generated derivative code to dedicated derivative files. 

# diseq 0.0.4.9012

* Allowed estimation of full information maximum likelihood, equilibrium, deterministic adjustment, and stochastic adjustment with one-sided inclusion of prices.
* Modified model titles' generation.

# diseq 0.0.4.9011

* Added basic_usage vignette.
* Added simulation function at model_base level. 
* Added a `NEWS.md` file to track changes to the package.
