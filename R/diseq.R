#' Estimation of models for markets in equilibrium and disequilibrium
#'
#' The \code{diseq} package provides tools to estimate and analyze an equilibrium and four
#' disequilibrium models. The equilibrium model can be estimated with either two-stage
#' least squares or with full information maximum likelihood.
#' The methods are asymptotically equivalent. The disequilibrium models are estimated using full
#' information maximum likelihood. All maximum likelihood models can be estimated both with
#' independent and correlated demand and supply shocks. The disequilibrium estimation is
#' based on Maddala and Nelson (1974) \doi{10.2307/1914215}. The package is
#' using the expressions of the gradients of the likelihoods derived in
#' Karapanagiotis (2020) \doi{10.2139/ssrn.3525622}.
#'
#' @details
#' ## Usage
#'
#' The easiest way to get accustomed with the functionality of the package is to check the
#' accompanying vignettes and the \href{../README.html}{README} file. These can be found
#' in the following links:
#'
#' \describe{
#'   \item{\href{../doc/basic_usage.html}{basic_usage}}{
#'     \code{vignette("basic_usage", package = "diseq")}}
#'   \item{\href{../doc/equilibrium_assessment.html}{equilibrium_assessment}}{
#'     \code{vignette("equilibrium_assessment", package = "diseq")}}
#' }
#'
#' ## Market model classes:
#'
#' The model hierarchy is described in the \href{../README.html}{\code{README}} file. See
#' the documentation of the classes for initialization details.
#'
#' ### Equilibrium model classes:
#'
#' \describe{
#'   \item{\code{\linkS4class{equilibrium_model}}}{Equilibrium model that can be estimated
#' using full information maximum likelihood or two-stage least squares}
#' }
#'
#' ### Disequilibrium model classes:
#'
#' \describe{
#'   \item{\code{\linkS4class{diseq_basic}}}{
#'     Disequilibrium model only with a basic short side rule}
#'   \item{\code{\linkS4class{diseq_directional}}}{
#'     Disequilibrium model with directional sample separation}
#'   \item{\code{\linkS4class{diseq_deterministic_adjustment}}}{
#'     Disequilibrium model with deterministic price dynamics}
#'   \item{\code{\linkS4class{diseq_stochastic_adjustment}}}{
#'     Disequilibrium model with stochastic price dynamics}
#' }
#'
#' @docType package
#' @name diseq
NULL

## usethis namespace: start
#' @useDynLib diseq, .registration = TRUE
## usethis namespace: end
NULL

#' @importFrom Rcpp loadModule
#' @importFrom RcppGSL LdFlags
#' @importFrom RcppParallel RcppParallelLibs
Rcpp::loadModule("diseq_module", TRUE)
