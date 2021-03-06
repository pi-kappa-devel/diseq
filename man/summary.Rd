% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/market_model.R
\name{summary,market_model-method}
\alias{summary,market_model-method}
\title{Summarizes the model.}
\usage{
\S4method{summary}{market_model}(object)
}
\arguments{
\item{object}{A model object.}
}
\description{
Prints basic information about the passed model object. In addition to the output of
the \code{\link{show}} method, \code{summary} prints
- the number of observations,
- the number of observations in each equation for models with sample separation, and
- various categories of variables.
}
\examples{
\donttest{
model <- simulate_model(
  "diseq_stochastic_adjustment", list(
    # observed entities, observed time points
    nobs = 500, tobs = 3,
    # demand coefficients
    alpha_d = -0.1, beta_d0 = 9.8, beta_d = c(0.3, -0.2), eta_d = c(0.6, -0.1),
    # supply coefficients
    alpha_s = 0.1, beta_s0 = 5.1, beta_s = c(0.9), eta_s = c(-0.5, 0.2),
    # price equation coefficients
    gamma = 1.2, beta_p0 = 3.1, beta_p = c(0.8)
  ),
  seed = 556
)

# print model summary
summary(model)
}
}
