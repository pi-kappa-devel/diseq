
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
simulate data and initialize a model in one step. See the documentation
of `simulate_model` for details. Models can be initialized with a
dataset using a constructor.

In this example, a basic disequilibrium model is estimated. There are
also other models available (see [Design and
functionality](#design-and-functionality)).

``` r
mdl <- simulate_model(
  "diseq_basic", 10000, 5,
  -1.9, 4.9, c(2.1, -0.7), c(3.5, 6.25),
  2.8, 1.2, c(0.65), c(1.15, 4.2),
  NA, NA, c(NA),
  seed = 42
)
```

The model is estimated with default options by a simple call. See the
documentation of `estimate` for more details and options.

``` r
est <- estimate(mdl)
summary(est)
#> Maximum likelihood estimation
#> 
#> Call:
#> `bbmle::mle2`(list(skip.hessian = FALSE, start = c(D_P = 2.13612848679375, 
#> D_CONST = 1.81846891329318, D_Xd1 = 0.360519700208155, D_Xd2 = -0.127953324215315, 
#> D_X1 = 1.55709136453431, D_X2 = 4.55420183606562, S_P = 2.13273424671937, 
#> S_CONST = 0.738905062339338, S_Xs1 = 0.548743057706831, S_X1 = 1.55631931181641, 
#> S_X2 = 4.55094892540246, D_VARIANCE = 1, S_VARIANCE = 1, RHO = 0
#> ), method = "BFGS", minuslogl = function(...) minus_log_likelihood(object, ...), 
#>     gr = function(...) gradient(object, ...)))
#> 
#> Coefficients:
#>              Estimate Std. Error  z value  Pr(z)    
#> D_P        -1.9257636  0.0350306 -54.9737 <2e-16 ***
#> D_CONST     4.8180838  0.1370628  35.1524 <2e-16 ***
#> D_Xd1       2.0950102  0.0179847 116.4885 <2e-16 ***
#> D_Xd2      -0.6807079  0.0133580 -50.9589 <2e-16 ***
#> D_X1        3.5170578  0.0191700 183.4668 <2e-16 ***
#> D_X2        6.2743935  0.0177504 353.4789 <2e-16 ***
#> S_P         2.8003013  0.0068342 409.7471 <2e-16 ***
#> S_CONST     1.2107950  0.0395546  30.6107 <2e-16 ***
#> S_Xs1       0.6591005  0.0057569 114.4882 <2e-16 ***
#> S_X1        1.1421241  0.0060785 187.8966 <2e-16 ***
#> S_X2        4.1903780  0.0059934 699.1651 <2e-16 ***
#> D_VARIANCE  1.0119257  0.0190813  53.0325 <2e-16 ***
#> S_VARIANCE  0.9927877  0.0072733 136.4972 <2e-16 ***
#> RHO        -0.0066240  0.0312731  -0.2118 0.8323    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> -2 log L: 138660.1
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
  - normalized point estimates of shortages,
  - relative point estimates of shortages,
  - aggregate demand and supply,
  - post-estimation classification of observation in demand and supply.

## Contributors

Pantelis Karapanagiotis 

Feel free to join, share, contribute, distribute.

## License

The code is distributed under the MIT License. 
