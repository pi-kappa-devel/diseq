# diseq 0.0.11

* Removed "get_correlation_variable" from exported functions. Improved the documentation of "minus_log_likelihood". Reintroduced references in description.

# diseq 0.0.10

* Removed references from description to avoid CRAN notes.

# diseq 0.0.9

* Ignoring README.html from build. Removed links from description. Improved documentation examples.

# diseq 0.0.8

* Added examples to constructors, estimation, aggregation, and marginal effect functions.

# diseq 0.0.7.9002

* Skipping directional and stochastic adjustment tests on CRAN to reduce build time.

# diseq 0.0.7.9001

* Quoted all package names in DESCRIPTION. To reduce build time: 1. Removed direction model estimation from equilibrium assessment vignette, 2. Decreased estimation accuracy of basic usage vignette to six digits.

# diseq 0.0.7

* Fixed order of arguments in web-link of estimation documentation. Improved simulation documentation.

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
