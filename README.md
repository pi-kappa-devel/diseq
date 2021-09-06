Models For Markets in Equilibrium and Disequilibrium
================

<img src='man/figures/logo.png' align="right" height="48"/>

<!-- badges: start -->

![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)
![](https://cranlogs.r-pkg.org/badges/grand-total/diseq)
![](https://www.r-pkg.org/badges/version/diseq)
![](https://www.r-pkg.org/badges/last-release/diseq)
<!-- badges: end -->

The *diseq* package provides tools to estimate and analyze an
equilibrium and four disequilibrium models. The equilibrium model can be
estimated with either two-stage least squares or with full information
maximum likelihood. The methods are asymptotically equivalent. The
disequilibrium models are estimated using full information maximum
likelihood. The likelihoods can be estimated both with independent and
correlated demand and supply shocks and the optimization of the
likelihoods can be performed either using analytic expressions or
numerical approximations of their gradients.

# A quick model tour

The five models of the package are described by systems of simultaneous
equations, with the equilibrium system being the only linear, while the
disequilibrium systems being non-linear. All models specify the demand
and the supply side of the market by a linear (in parameters) equation.
The remaining equations of each model, if any, further specify the
structure of the market.

## The equilibrium model

The equilibrium model adds the market-clearing condition to the demand
and supply equations of the system. For the system to be identifiable,
at least one variable in the demand side must not be present in the
supply side and vice versa. This model assumes that the market
observations always represent equilibrium points in which the demanded
and supplied quantities are equal. The model can be estimated using
two-stage least squares (Theil 1953) or full information maximum
likelihood (Karapanagiotis, n.d.). Asymptotically, these methods are
equivalent (Balestra and Varadharajan-Krishnakumar 1987).

<img src='https://render.githubusercontent.com/render/math?math=%5Cbegin%7Baligned%7D%0AD_%7Bn%20t%7D%20%26%3D%20X_%7Bd%2C%20n%20t%7D%27%5Cbeta_%7Bd%7D%20%2B%20P_%7Bn%20t%7D%5Calpha_%7Bd%7D%20%2B%20u_%7Bd%2C%20n%20t%7D%20%5C%5C%0AS_%7Bn%20t%7D%20%26%3D%20X_%7Bs%2C%20n%20t%7D%27%5Cbeta_%7Bs%7D%20%2B%20P_%7Bn%20t%7D%5Calpha_%7Bs%7D%20%2B%20u_%7Bs%2C%20n%20t%7D%20%5C%5C%0AQ_%7Bn%20t%7D%20%26%3D%20D_%7Bn%20t%7D%20%3D%20S_%7Bn%20t%7D%0A%5Cend%7Baligned%7D%20'/> <img src='man/figures/equilibrium_model.png'/>

## The basic disequilibrium model

The basic model is the simplest disequilibrium model of the package as
it basically imposes no assumption on the structure of the market
regarding price movements (Fair and Jaffee 1972; Maddala and Nelson
1974). In contrast with the equilibrium model, the market-clearing
condition is replaced by the short-side rule, which stipulates that the
minimum between the demanded and supplied quantities is observed. The
econometrician does not need to specify whether an observation belongs
to the demand or the supply side since the estimation of the model will
allocate the observations on the demand or supply side so that the
likelihood is maximized.

<img src='https://render.githubusercontent.com/render/math?math=%5Cbegin%7Baligned%7D%0AD_%7Bn%20t%7D%20%26%3D%20X_%7Bd%2C%20n%20t%7D%27%5Cbeta_%7Bd%7D%20%2B%20P_%7Bn%20t%7D%5Calpha_%7Bd%7D%20%2B%20u_%7Bd%2C%20n%20t%7D%20%5C%5C%0AS_%7Bn%20t%7D%20%26%3D%20X_%7Bs%2C%20n%20t%7D%27%5Cbeta_%7Bs%7D%20%2B%20P_%7Bn%20t%7D%5Calpha_%7Bs%7D%20%2B%20u_%7Bs%2C%20n%20t%7D%20%5C%5C%0AQ_%7Bn%20t%7D%20%26%3D%20D_%7Bn%20t%7D%20%3D%20S_%7Bn%20t%7D%0A%5Cend%7Baligned%7D%20'/> <img src='man/figures/diseq_basic.png'/>

## The directional disequilibrium model

The directional model attaches an additional equation to the system of
the basic model. The added equation is a sample separation condition
based on the direction of the price movements (Fair and Jaffee 1972;
Maddala and Nelson 1974). When prices increase at a given date, an
observation is assumed to belong on the supply side. When prices fall,
an observation is assumed to belong in the demand side. In short, this
condition separates the sample before the estimation and uses this
separation as additional information in the estimation procedure.
Although, when appropriate, more information improves estimations, it
also, when inaccurate, intensifies misspecification problems. Therefore,
the additional structure of the directional model does not guarantee
better estimates in comparison with the basic model.

<img src='https://render.githubusercontent.com/render/math?math=%5Cbegin%7Baligned%7D%0AD_%7Bn%20t%7D%20%26%3D%20X_%7Bd%2C%20n%20t%7D%27%5Cbeta_%7Bd%7D%20%2B%20P_%7Bn%20t%7D%5Calpha_%7Bd%7D%20%2B%20u_%7Bd%2C%20n%20t%7D%20%5C%5C%0AS_%7Bn%20t%7D%20%26%3D%20X_%7Bs%2C%20n%20t%7D%27%5Cbeta_%7Bs%7D%20%2B%20P_%7Bn%20t%7D%5Calpha_%7Bs%7D%20%2B%20u_%7Bs%2C%20n%20t%7D%20%5C%5C%0AQ_%7Bn%20t%7D%20%26%3D%20D_%7Bn%20t%7D%20%3D%20S_%7Bn%20t%7D%0A%5Cend%7Baligned%7D%20'/> <img src='man/figures/diseq_directional.png'/>

## A disequilibrium model with deterministic price dynamics

The separation rule of the directional model classifies observations on
the demand or supply-side based in a binary fashion, which is not always
flexible, as observations that correspond to large shortages/surpluses
are treated the same with observations that correspond to small
shortages/ surpluses. The deterministic adjustment model of the package
replaces this binary separation rule with a quantitative one (Fair and
Jaffee 1972; Maddala and Nelson 1974). The magnitude of the price
movements is analogous to the magnitude of deviations from the
market-clearing condition. This model offers a flexible estimation
alternative, with one extra degree of freedom in the estimation of price
dynamics, that accounts for market forces that are in alignment with
standard economic reasoning. By letting \(\gamma\) approach zero, the
equilibrium model can be obtained as a limiting case of this model.

<img src='https://render.githubusercontent.com/render/math?math=%5Cbegin%7Baligned%7D%0AD_%7Bn%20t%7D%20%26%3D%20X_%7Bd%2C%20n%20t%7D%27%5Cbeta_%7Bd%7D%20%2B%20P_%7Bn%20t%7D%5Calpha_%7Bd%7D%20%2B%20u_%7Bd%2C%20n%20t%7D%20%5C%5C%0AS_%7Bn%20t%7D%20%26%3D%20X_%7Bs%2C%20n%20t%7D%27%5Cbeta_%7Bs%7D%20%2B%20P_%7Bn%20t%7D%5Calpha_%7Bs%7D%20%2B%20u_%7Bs%2C%20n%20t%7D%20%5C%5C%0AQ_%7Bn%20t%7D%20%26%3D%20D_%7Bn%20t%7D%20%3D%20S_%7Bn%20t%7D%0A%5Cend%7Baligned%7D%20'/> <img src='man/figures/diseq_deterministic_adjustment.png'/>

## A disequilibrium model with stochastic price dynamics

The last model of the package extends the price dynamics of the
deterministic adjustment model by adding additional explanatory
variables and a stochastic term. The latter term in particular makes the
price adjustment mechanism stochastic and, deviating from the structural
assumptions of models \((DA)\) and \((DM)\), abstains from imposing any
separation assumption on the sample (Maddala and Nelson 1974; Quandt and
Ramsey 1978). The estimation of this model offers the highest degree of
freedom, accompanied, however, by a significant increase in estimation
complexity, which can hinder the stability of the procedure and the
numerical accuracy of the outcomes.

<img src='https://render.githubusercontent.com/render/math?math=%5Cbegin%7Baligned%7D%0AD_%7Bn%20t%7D%20%26%3D%20X_%7Bd%2C%20n%20t%7D%27%5Cbeta_%7Bd%7D%20%2B%20P_%7Bn%20t%7D%5Calpha_%7Bd%7D%20%2B%20u_%7Bd%2C%20n%20t%7D%20%5C%5C%0AS_%7Bn%20t%7D%20%26%3D%20X_%7Bs%2C%20n%20t%7D%27%5Cbeta_%7Bs%7D%20%2B%20P_%7Bn%20t%7D%5Calpha_%7Bs%7D%20%2B%20u_%7Bs%2C%20n%20t%7D%20%5C%5C%0AQ_%7Bn%20t%7D%20%26%3D%20D_%7Bn%20t%7D%20%3D%20S_%7Bn%20t%7D%0A%5Cend%7Baligned%7D%20'/> <img src='man/figures/diseq_stochastic_adjustment.png'/>

# Installation and documentation

The released version of
[*diseq*](https://CRAN.R-project.org/package=diseq) can be installed
from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("diseq")
```

The source code of the in-development version can be download from
[GitHub](https://github.com/pi-kappa-devel/diseq).

After installing it, there is a basic-usage example installed with it.
To see it type the command

``` r
vignette('basic_usage')
```

Online documentation is available for both the
[released](https://www.diseq.pikappa.eu) and
[in-development](https://www.diseq.pikappa.eu/dev/) versions of the
package. The documentation files can also accessed in `R` by typing

``` r
?? diseq
```

An overview of the package’s functionality was presented in the session
Trends, Markets, Models of the
[useR\!2021](https://user2021.r-project.org/) conference. The recording
of the session, including the talk for this package, can be found in the
video that follows. The presentation slides of the talk are also
available [here](https://talks.pikappa.eu/useR!2021/).

<a style="display:block;margin:auto" href="https://www.youtube.com/watch?v=Kkjkny94dgU" target="_blank"><img src="http://img.youtube.com/vi/Kkjkny94dgU/0.jpg"  alt="Session Recording" width="560" height="315" border="10" /></a>

# A practical example

This is a basic example that illustrates how a model of the package can
be estimated. The package is loaded in the standard way.

``` r
library(diseq)
```

The example uses simulated data. The *diseq* package offers a function
to simulate data from data generating processes that correspond to the
models that the package provides.

``` r
model_tbl <- simulate_data(
  "diseq_basic", 10000, 5,
  -1.9, 12.9, c(2.1, -0.7), c(3.5, 6.25),
  2.8, 10.2, c(0.65), c(1.15, 4.2),
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

  - The specification of the system’s equations. Each specification sets
    the right hand side of one system equation. The expressions are
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
      - warning: ≥ 1,
      - info: ≥ 2,
      - verbose: ≥ 3 and
      - debug: ≥ 4.

<!-- end list -->

``` r
verbose <- 0
```

  - Should the model estimation allow for correlated demand and supply
    shocks?

<!-- end list -->

``` r
correlated_shocks <- TRUE
```

``` r
mdl <- new(
  "diseq_basic",
  key_columns,
  quantity_column, price_column, demand_specification, paste0(price_column, " + ", supply_specification),
  model_tbl,
  correlated_shocks = correlated_shocks, verbose = verbose
)
```

The model is estimated with default options by a simple call. See the
documentation of `estimate` for more details and options.

``` r
est <- estimate(mdl)
bbmle::summary(est)
```

    ## Maximum likelihood estimation
    ## 
    ## Call:
    ## `bbmle::mle2`(list(skip.hessian = TRUE, start = c(D_P = 2.20196877751699, 
    ## D_CONST = 11.2388922841303, D_Xd1 = 0.270903396323925, D_Xd2 = -0.0866785170449159, 
    ## D_X1 = 1.44062383641357, D_X2 = 4.46769000498207, S_P = 2.19994905762293, 
    ## S_CONST = 10.2218850028638, S_Xs1 = 0.59622703822817, S_X1 = 1.43857649730767, 
    ## S_X2 = 4.46672975897316, D_VARIANCE = 1, S_VARIANCE = 1, RHO = 0
    ## ), method = "BFGS", minuslogl = function(...) minus_log_likelihood(object, ...), 
    ##     gr = function(...) gradient(object, ...)))
    ## 
    ## Coefficients:
    ##              Estimate Std. Error  z value  Pr(z)    
    ## D_P        -1.9277826  0.0643871 -29.9405 <2e-16 ***
    ## D_CONST    12.7187450  0.1665723  76.3557 <2e-16 ***
    ## D_Xd1       2.1041794  0.0386208  54.4831 <2e-16 ***
    ## D_Xd2      -0.6396308  0.0293756 -21.7742 <2e-16 ***
    ## D_X1        3.4902260  0.0398746  87.5300 <2e-16 ***
    ## D_X2        6.2935478  0.0385864 163.1028 <2e-16 ***
    ## S_P         2.8065335  0.0120090 233.7023 <2e-16 ***
    ## S_CONST    10.1644425  0.0494381 205.5993 <2e-16 ***
    ## S_Xs1       0.6782140  0.0097622  69.4737 <2e-16 ***
    ## S_X1        1.1295126  0.0104475 108.1137 <2e-16 ***
    ## S_X2        4.1981877  0.0103540 405.4635 <2e-16 ***
    ## D_VARIANCE  1.0177756  0.0303119  33.5767 <2e-16 ***
    ## S_VARIANCE  1.0026251  0.0074199 135.1273 <2e-16 ***
    ## RHO        -0.0238756  0.0376718  -0.6338 0.5262    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## -2 log L: 138110.5

# Design and functionality

The equilibrium model can be estimated either using two-stage least
squares or full information maximum likelihood. The two methods are
asymptotically equivalent. The class for which both of these estimation
methods are implemented is

  - `equilibrium_model`.

In total, there are four disequilibrium models, which are all estimated
using full information maximum likelihood. By default, the estimations
use analytically calculated gradient expressions, but the user has the
ability to override this behavior. The classes that implement the four
disequilibrium models are

  - `diseq_basic`,
  - `diseq_directional`,
  - `diseq_deterministic_adjustment`, and
  - `diseq_stochastic_adjustment`.

The package organizes these classes in a simple object oriented
hierarchy.

<img src='man/figures/design.png' align="center" />

Concerning post estimation analysis, the package offers functionality to
calculate

  - shortage probabilities,
  - marginal effects on shortage probabilities,
  - point estimates of normalized shortages,
  - point estimates of relative shortages,
  - aggregate demand and supply,
  - post-estimation classification of observations in demand and supply,
  - heteroscedasticity-adjusted (Huber-White) standard errors, and
  - clustered standard errors.

# Alternative packages

The estimation of the basic model is also supported by the package
[*Disequilibrium*](https://CRAN.R-project.org/package=Disequilibrium).
By default the *Disequilibrium* package numerically approximates the
gradient when optimizing the likelihood. In contrast, *diseq* uses
analytically calculated expressions for the likelihood, which can reduce
the duration of estimating the model. In addition, it allows the user to
override this behavior and use the numerically approximated gradient.
There is no alternative package that supports the out-of-the-box
estimation of the other three disequilibrium models of *diseq*.

# Planned extensions

The package is planned to be expanded in the following ways:

1.  The package should become more inclusive by adding additional market
    models.
2.  Single-command functionality for the market-clearing tests
    (e.g. (Karapanagiotis, n.d.; Hwang 1980; Quandt 1978)) should be
    included in the package.
3.  Alternative estimation methods (e.g (Zilinskas and Bogle 2006;
    Quandt and Ramsey 1978)) could also be implemented.

# Contributors

[Pantelis Karapanagiotis](https://www.pikappa.eu)

Feel free to join, share, contribute, distribute.

# License

The code is distributed under the MIT License.

# References

<div id="refs" class="references hanging-indent">

<div id="ref-balestra1987">

Balestra, Pietro, and Jayalakshmi Varadharajan-Krishnakumar. 1987. “Full
information estimations of a system of simultaneous equations with error
component structure.” *Econometric Theory* 3 (2): 223–46.
<https://doi.org/10.1017/S0266466600010318>.

</div>

<div id="ref-fair1972">

Fair, Ray C., and Dwight M. Jaffee. 1972. “Methods of Estimation for
Markets in Disequilibrium.” *Econometrica* 40 (3): 497.
<https://doi.org/10.2307/1913181>.

</div>

<div id="ref-hwang1980">

Hwang, Hae-shin. 1980. “A test of a disequilibrium model.” *Journal of
Econometrics* 12 (3): 319–33.
<https://doi.org/10.1016/0304-4076(80)90059-7>.

</div>

<div id="ref-karapanagiotis2020invisible">

Karapanagiotis, Pantelis. n.d. “The Assessment of Market-Clearing as a
Model Selection Problem.” Working Paper.
<https://doi.org/10.2139/ssrn.3525622>.

</div>

<div id="ref-maddala1974">

Maddala, G. S., and Forrest D. Nelson. 1974. “Maximum Likelihood Methods
for Models of Markets in Disequilibrium.” *Econometrica* 42 (6): 1013.
<https://doi.org/10.2307/1914215>.

</div>

<div id="ref-quandt1978tests">

Quandt, Richard E. 1978. “Tests of the Equilibrium vs. Disequilibrium
Hypotheses.” *International Economic Review* 19 (2): 435.
<https://doi.org/10.2307/2526311>.

</div>

<div id="ref-quandt1978estimating">

Quandt, Richard E., and James B. Ramsey. 1978. “Estimating mixtures of
normal distributions and switching regressions.” *Journal of the
American Statistical Association* 73 (364): 730–38.
<https://doi.org/10.1080/01621459.1978.10480085>.

</div>

<div id="ref-theil1953">

Theil, H. 1953. “Repeated least squares applied to complete equation
systems.” *The Hague: Central Planning Bureau*, 2–5.

</div>

<div id="ref-zilinskas2006">

Zilinskas, Julius, and Ian David Lockhart Bogle. 2006. “Balanced random
interval arithmetic in market model estimation.” *European Journal of
Operational Research* 175 (3): 1367–78.
<https://doi.org/10.1016/j.ejor.2005.02.013>.

</div>

</div>
