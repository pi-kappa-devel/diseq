#' diseq: A package for estimating models for markets in equilibrium and disequilibrium
#'
#' The diseq package provides tools to estimate and analyze an equilibrium and four disequilibrium
#' models. The equilibrium model can be estimated with either two-stage least squares or with full
#' information maximum likelihood.
#' The methods are asymptotically equivalent. The disequilibrium models are estimated using full
#' information maximum likelihood. All maximum likelihood models can be estimated both with
#' independent and correlated demand and supply shocks.
#'
#' @section Disequilibrium model classes:
#' \describe{
#' \item{\code{\linkS4class{eq_2sls}}}{Equilibrium two-stage least square model}
#' \item{\code{\linkS4class{eq_fiml}}}{Equilibrium full information maximum likelihood}
#' \item{\code{\linkS4class{diseq_basic}}}{Disequilibrium model with unknown sample separation}
#' \item{
#'   \code{\linkS4class{diseq_directional}}
#' }{Disequilibrium model with directional sample separation}
#' \item{
#'   \code{\linkS4class{diseq_deterministic_adjustment}}
#' }{Disequilibrium model with deterministic price dynamics}
#' \item{
#'   \code{\linkS4class{diseq_stochastic_adjustment}}
#' }{Disequilibrium model with stochastic price dynamics}
#' }
#'
#' @docType package
#' @name diseq
NULL
