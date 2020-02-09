
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Models for Markets in Disequilibrium <img src='man/figures/logo.png' align="right" height="36" />

<!-- badges: start -->

<!-- badges: end -->

The diseq package provides tools to estimate and analyze an equilibrium
and four disequilibrium models. The equilibrium model can be estimated
with either two-stage least squares or with full information maximum
likelihood. The methods are asymptotically equivalent. The
disequilibrium models are estimated using full information maximum
likelihood. All maximum likelihood models can be estimated both with
independent and correlated demand and supply shocks.

## Installation

<!--
You can install the released version of diseq from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("diseq")
```
-->

You can download the source code of the development version from
[GitHub](https://github.com/pi-kappa-devel/diseq).

After you install it, there is a basic-usage example installed with it.
To see it type the command

    vignette('basic_usage')

You can find the documentation of the package by typing

    ?? diseq

## Example

This is a basic example which shows you how estimate a model. The
package is loaded in the standard way.

``` r
library(diseq)
#> Loading required package: bbmle
#> Loading required package: stats4
```

The example uses simulated data. The diseq package offers a function to
simulate data from data generating processes that correspond to the
models that the package provides.

``` r
model_tbl <- simulate_model_data(
  "diseq_basic", 10000, 5,
  -1.9, 4.9, c(2.1, -0.7), c(3.5, 6.25),
  2.8, 1.2, c(0.65), c(1.15, 4.2),
  NA, NA, c(NA),
  seed = 42
)
```

Models are initialized by a constructor. In this example, a basic
disequilibrium model is estimated. There are also other models available
(see [Design and functionality](#design-and-functionality)). The
constructor sets the model’s parameters and performs the necessary
initialization processes. The following variables specify this example’s
parameterization.

  - The key is the combination of columns that uniquely identify a
    record of the dataset. For panel data, this should be a vector of
    the entity identifier and the time columns.

<!-- end list -->

``` r
key_columns <- c("id", "date")
```

  - The quantity variable.

<!-- end list -->

``` r
quantity_column <- "Q"
```

  - The price variable.

<!-- end list -->

``` r
price_column <- "P"
```

  - The sepecification of the system’s equations. Each specification
    sets the right hand side of one system equation. The expressions are
    specified similarly to the expressions of formulas of linear models.
    Indicator variables and interactions are created automatically by
    the constructor.

<!-- end list -->

``` r
demand_specification <- paste0(price_column, " + Xd1 + Xd2 + X1 + X2")
supply_specification <- "Xs1 + X1 + X2"
```

  - The verbosity level controls the level of messaging. The object
    displays
      - error: always,
      - warning: \(\ge\) 1,
      - info: \(\ge\) 2,
      - verbose: \(\ge\) 3 and
      - debug: \(\ge\) 4.

<!-- end list -->

``` r
verbose <- 4
```

  - Should the model estimation allow for correlated demand and supply
    shocks?

<!-- end list -->

``` r
use_correlated_shocks <- TRUE
```

``` r
mdl <- new(
  "diseq_basic",
  key_columns,
  quantity_column, price_column, demand_specification, paste0(price_column, " + ", supply_specification),
  model_tbl,
  use_correlated_shocks = use_correlated_shocks, verbose = verbose
)
#> Info: This is 'Basic with correlated shocks' model
#> Verbose: Using columns id, date, Q, P, Xd1, Xd2, X1, X2, Xs1.
```

The model is estimated with default options by a simple call. See the
documentation of `estimate` for more details and options.

``` r
est <- estimate(mdl)
#> Verbose: Initializing using linear regression estimations.
#> Debug: Using starting values: D_P = 2.1334612971289, D_CONST = 1.74512294884244, D_Xd1 = 0.371173675755609, D_Xd2 = -0.134123191634723, D_X1 = 1.55733325744955, D_X2 = 4.57681889311855, S_P = 2.13368618959257, S_CONST = 0.754830637264177, S_Xs1 = 0.530640038804247, S_X1 = 1.5539938204713, S_X2 = 4.56958594244215, D_VARIANCE = 1, S_VARIANCE = 1, RHO = 0
summary(est)
#> Maximum likelihood estimation
#> 
#> Call:
#> `bbmle::mle2`(list(skip.hessian = FALSE, start = c(D_P = 2.1334612971289, 
#> D_CONST = 1.74512294884244, D_Xd1 = 0.371173675755609, D_Xd2 = -0.134123191634723, 
#> D_X1 = 1.55733325744955, D_X2 = 4.57681889311855, S_P = 2.13368618959257, 
#> S_CONST = 0.754830637264177, S_Xs1 = 0.530640038804247, S_X1 = 1.5539938204713, 
#> S_X2 = 4.56958594244215, D_VARIANCE = 1, S_VARIANCE = 1, RHO = 0
#> ), method = "BFGS", minuslogl = function(...) minus_log_likelihood(object, ...), 
#>     gr = function(...) gradient(object, ...)))
#> 
#> Coefficients:
#>              Estimate Std. Error  z value  Pr(z)    
#> D_P        -1.9017291  0.0358194 -53.0921 <2e-16 ***
#> D_CONST     4.9053673  0.1415429  34.6564 <2e-16 ***
#> D_Xd1       2.0822199  0.0180697 115.2327 <2e-16 ***
#> D_Xd2      -0.7088348  0.0136147 -52.0640 <2e-16 ***
#> D_X1        3.5124797  0.0194037 181.0213 <2e-16 ***
#> D_X2        6.2667695  0.0180124 347.9143 <2e-16 ***
#> S_P         2.7970361  0.0068834 406.3475 <2e-16 ***
#> S_CONST     1.1906485  0.0398902  29.8481 <2e-16 ***
#> S_Xs1       0.6485039  0.0057989 111.8317 <2e-16 ***
#> S_X1        1.1467542  0.0061024 187.9191 <2e-16 ***
#> S_X2        4.2091141  0.0060775 692.5783 <2e-16 ***
#> D_VARIANCE  1.0121758  0.0195476  51.7801 <2e-16 ***
#> S_VARIANCE  1.0162146  0.0074433 136.5269 <2e-16 ***
#> RHO        -0.0131056  0.0325131  -0.4031 0.6869    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> -2 log L: 139600.5
```

## Design and functionality

There are two equilibrium models available, namely

  - `eq_2sls` and
  - `eq_fiml`.

In total, there are four disequilibrium models, i.e.

  - `diseq_basic`,
  - `diseq_directional`,
  - `diseq_deterministic_adjustment`, and
  - `diseq_stochastic_adjustment`.

The package organizes the models in a simple object oriented hierarchy.

<img src='man/figures/design.png' align="center" />

Concerning post estimation analysis, the package offers functionality to
calculate

  - shortage probabilities,
  - marginal effects on shortage probabilities,
  - point estimates of normalized shortages,
  - point estimates of relative shortages,
  - aggregate demand and supply,
  - post-estimation classification of observations in demand and supply.

## Contributors

Pantelis Karapanagiotis

Feel free to join, share, contribute, distribute.

## License

The code is distributed under the MIT License.
