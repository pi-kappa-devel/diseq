---
output: github_document
---



# Models for Markets in Disequilibrium  <img src='man/figures/logo.png' align="right" height="36" />

<!-- badges: start -->
<!-- badges: end -->

The *diseq* package provides tools to estimate and analyze an equilibrium and four disequilibrium models. The equilibrium model can be estimated with either two-stage least squares or with full information maximum likelihood. The methods are asymptotically equivalent. The disequilibrium models are estimated using full information maximum likelihood. All maximum likelihood models can be estimated both with independent and correlated demand and supply shocks.

## Installation

The released version of *diseq* can be installed from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("diseq")
```

The source code of the in-development version can be download from [GitHub](https://github.com/pi-kappa-devel/diseq).

After installing it, there is a basic-usage example installed with it. To see it type the command
```
vignette('basic_usage')
```

You can find the documentation of the package by typing
```
?? diseq
```

## Example

This is a basic example which shows you how to estimate a model. The package is loaded in the standard way.


```r
library(diseq)
```

The example uses simulated data. The *diseq* package offers a function to simulate data from data generating processes that correspond to the models that the package provides.

```r
model_tbl <- simulate_model_data(
  "diseq_basic", 10000, 5,
  -1.9, 12.9, c(2.1, -0.7), c(3.5, 6.25),
  2.8, 10.2, c(0.65), c(1.15, 4.2),
  NA, NA, c(NA),
  seed = 42
)
```

Models are initialized by a constructor. In this example, a basic disequilibrium model is estimated. There are also other models available (see [Design and functionality]). The constructor sets the model's parameters and performs the necessary initialization processes. The following variables specify this example's parameterization.

 * The key is the combination of columns that uniquely identify a record of the dataset. For panel data, this should be a vector of the entity identifier and the time columns.

```r
key_columns <- c("id", "date")
```
 
 * The quantity variable.

```r
quantity_column <- "Q"
```

 * The price variable. 

```r
price_column <- "P"
```

 * The specification of the system's equations. Each specification sets the right hand side of one system equation. The expressions are specified similarly to the expressions of formulas of linear models. Indicator variables and interactions are created automatically by the constructor. 

```r
demand_specification <- paste0(price_column, " + Xd1 + Xd2 + X1 + X2")
supply_specification <- "Xs1 + X1 + X2"
```

 * The verbosity level controls the level of messaging. The object displays
     * error: always,
     * warning: $\ge$ 1, 
     * info: $\ge$ 2, 
     * verbose: $\ge$ 3 and
     * debug: $\ge$ 4.

```r
verbose <- 0
```

 * Should the model estimation allow for correlated demand and supply shocks?

```r
use_correlated_shocks <- TRUE
```


```r
mdl <- new(
  "diseq_basic",
  key_columns,
  quantity_column, price_column, demand_specification, paste0(price_column, " + ", supply_specification),
  model_tbl,
  use_correlated_shocks = use_correlated_shocks, verbose = verbose
)
```

The model is estimated with default options by a simple call. See the documentation of `estimate` for more 
details and options.

```r
est <- estimate(mdl)
bbmle::summary(est)
#> Maximum likelihood estimation
#> 
#> Call:
#> `bbmle::mle2`(list(skip.hessian = FALSE, start = c(D_P = 2.20196877751699, 
#> D_CONST = 11.2388922841303, D_Xd1 = 0.270903396323925, D_Xd2 = -0.0866785170449159, 
#> D_X1 = 1.44062383641357, D_X2 = 4.46769000498207, S_P = 2.19994905762293, 
#> S_CONST = 10.2218850028638, S_Xs1 = 0.59622703822817, S_X1 = 1.43857649730766, 
#> S_X2 = 4.46672975897316, D_VARIANCE = 1, S_VARIANCE = 1, RHO = 0
#> ), method = "BFGS", minuslogl = function (...) 
#> minus_log_likelihood(object, ...), gr = function (...) 
#> gradient(object, ...)))
#> 
#> Coefficients:
#>              Estimate Std. Error  z value  Pr(z)    
#> D_P        -1.9277826  0.0643871 -29.9405 <2e-16 ***
#> D_CONST    12.7187450  0.1665723  76.3557 <2e-16 ***
#> D_Xd1       2.1041794  0.0386208  54.4831 <2e-16 ***
#> D_Xd2      -0.6396308  0.0293756 -21.7742 <2e-16 ***
#> D_X1        3.4902260  0.0398746  87.5300 <2e-16 ***
#> D_X2        6.2935478  0.0385864 163.1028 <2e-16 ***
#> S_P         2.8065335  0.0120090 233.7023 <2e-16 ***
#> S_CONST    10.1644425  0.0494381 205.5993 <2e-16 ***
#> S_Xs1       0.6782140  0.0097622  69.4737 <2e-16 ***
#> S_X1        1.1295126  0.0104475 108.1137 <2e-16 ***
#> S_X2        4.1981877  0.0103540 405.4635 <2e-16 ***
#> D_VARIANCE  1.0177756  0.0303119  33.5767 <2e-16 ***
#> S_VARIANCE  1.0026251  0.0074199 135.1273 <2e-16 ***
#> RHO        -0.0238756  0.0376718  -0.6338 0.5262    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> -2 log L: 138110.5
```

## Design and functionality

There are two equilibrium models available, namely

* `eq_2sls` and
* `eq_fiml`.

In total, there are four disequilibrium models, i.e.

* `diseq_basic`, 
* `diseq_directional`, 
* `diseq_deterministic_adjustment`, and
* `diseq_stochastic_adjustment`.

The package organizes the models in a simple object oriented hierarchy. 

<img src='man/figures/design.png' align="center" />

Concerning post estimation analysis, the package offers functionality to calculate

* shortage probabilities,
* marginal effects on shortage probabilities,
* point estimates of normalized shortages,
* point estimates of relative shortages,
* aggregate demand and supply,
* post-estimation classification of observations in demand and supply.

## Contributors

Pantelis Karapanagiotis 

Feel free to join, share, contribute, distribute.

## License

The code is distributed under the MIT License. 
