#' @include market_model.R

setClass(
  "fitted_model",
  contains = "market_model",
  representation(
    fit = "list"
  ),
  prototype()
)

setMethod(
  "initialize", "fitted_model",
  function(.Object, market_model, estimate) {
    for (slot_name in slotNames(market_model)) {
      slot(.Object, slot_name) <- slot(market_model, slot_name)
    }
    .Object@fit <- list(estimate)

    .Object
  }
)

#' @describeIn summaries Summarizes the model's fit.
#' @description \code{fitted_model}: Prints basic information about the
#' passed model fit. In addition to the output of
#' the model's \code{summary} method, the function prints basic
#' estimation results. For a maximum likelihood estimation, the function prints
#' \itemize{
#' \item the used optimization method,
#' \item the maximum number of allowed iterations,
#' \item the relative convergence tolerance (see \code{\link[stats]{optim}}),
#' \item the convergence status,
#' \item the initializing parameter values,
#' \item the estimated coefficients, their standard errors, Z values, and P values, and
#' \item \eqn{-2 \log L} evaluated at the maximum.
#' }
#' For a linear estimation of the equilibrium system, the function prints the
#' estimation summary provided by \code{\link[systemfit]{systemfit}} in
#' addition to the model's \code{summary} output.
#' @export
setMethod("summary", signature(object = "fitted_model"), function(object) {
  (selectMethod("summary", "market_model"))(object)
  if (class(object@fit[[1]]) == "mle2") {
    summary <- (selectMethod("summary", class(object@fit[[1]])))(object@fit[[1]])
    args <- summary(bsmdl_est)@call[[2]]

    cat("\nMaximum likelihood estimation\n")
    cat(sprintf("  %-20s: %s\n", "Method", object@fit[[1]]@method))
    cat(sprintf(
      "  %-20s: %d\n", "Max Iterations",
      args$control$maxit
    ))
    cat(sprintf(
      "  %-20s: %g\n", "Relative Tolerance",
      args$control$reltol
    ))
    cat(sprintf(
      "  %-20s: %s\n", "Convergence Status",
      ifelse(!object@fit[[1]]@details$convergence, "success", "failure")
    ))
    cat(sprintf("  %-20s:\n", "Starting Values"))
    print(args$start, digits = 4)
    cat("\nCoefficients\n")
    print(summary@coef, digits = 4)
    cat(sprintf("\n%s: %g\n", "-2 log L", summary@m2logL))
  } else {
    summary(object@fit[[1]]$system_model)
  }
})
