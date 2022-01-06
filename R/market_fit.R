#' @include market_model.R

setClass(
  "market_fit",
  contains = "market_model",
  representation(
    fit = "list"
  ),
  prototype()
)

setMethod(
  "initialize", "market_fit",
  function(.Object, market_model, estimate) {
    for (slot_name in slotNames(market_model)) {
      slot(.Object, slot_name) <- slot(market_model, slot_name)
    }
    .Object@fit <- list(estimate)

    .Object
  }
)

#' @describeIn summaries Summarizes the model's fit.
#' @description \code{market_fit}: Prints basic information about the
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
setMethod("summary", signature(object = "market_fit"), function(object) {
  (selectMethod("summary", "market_model"))(object)
  if (class(object@fit[[1]]) == "mle2") {
    summary <- (selectMethod("summary", "mle2"))(object@fit[[1]])
    args <- summary@call[[2]]

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


#' Model estimation.
#'
#' All models are estimated using full information maximum likelihood. The
#' \code{\linkS4class{equilibrium_model}} can also be estimated using two-stage
#' least squares. The maximum likelihood estimation is based on
#' \code{\link[bbmle]{mle2}}. If no starting values are provided, the function uses
#' linear regression estimates as initializing values. The default optimization method is
#' BFGS. For other alternatives see \code{\link[bbmle]{mle2}}. The implementation of
#' the two-stage least square estimation of the \code{\linkS4class{equilibrium_model}}
#' is based on \code{\link[systemfit]{systemfit}}.
#' @param object A model object.
#' @param ... Named parameter used in the model's estimation. These are passed further
#' down to the estimation call. For the \code{\linkS4class{equilibrium_model}} model, the
#' parameters are passed to \code{\link[systemfit]{systemfit}}, if the method is set to
#' \code{2SLS}, or to \code{\link[bbmle]{mle2}} for any other method. For the rest of
#' the models, the parameters are passed to \code{\link[bbmle]{mle2}}.
#' @return The object that holds the estimation result.
#' @rdname estimate
#' @examples
#' \donttest{
#' # initialize the model using the houses dataset
#' model <- new(
#'   "diseq_deterministic_adjustment", # model type
#'   subject = ID, time = TREND, quantity = HS, price = RM,
#'   demand = RM + TREND + W + CSHS + L1RM + L2RM + MONTH,
#'   supply = RM + TREND + W + L1RM + MA6DSF + MA3DHF + MONTH,
#'   fair_houses(), # data
#'   correlated_shocks = FALSE # allow shocks to be correlated
#' )
#'
#' # estimate the model object (BFGS is used by default)
#' est <- estimate(model)
#'
#' # estimate the model by specifying the optimization details passed to the optimizer.
#' est <- estimate(model, control = list(maxit = 1e+6), method = "BFGS")
#'
#' # summarize results
#' summary(est)
#' }
#' @export
setGeneric("estimate", function(object, ...) {
  standardGeneric("estimate")
})

#' @describeIn estimate Full information maximum likelihood estimation.
#' @param gradient One of two potential options: `numerical` and `calculated`. By
#' default, all the models are estimated using the analytic expressions of their
#' likelihoods' gradients.
#' @param hessian One of three potential options: `skip`, `numerical`, and `calculated`.
#' The default is to use the `calculated` Hessian for the model that expressions are
#' available and the `numerical` Hessian in other cases. Calculated Hessian expressions
#' are available for the basic and directional models.
#' @param standard_errors One of three potential options: `homoscedastic`,
#' `heteroscedastic`, or a vector with variables names for which standard error
#' clusters are to be created. The default value is `homoscedastic`. If the option
#' `heteroscedastic` is passed, the variance-covariance matrix is calculated using
#' heteroscedasticity adjusted (Huber-White) standard errors. If the vector is
#' supplied, the variance-covariance matrix is calculated by grouping the score matrix
#' based on the passed variables.
setMethod(
  "estimate", signature(object = "market_model"),
  function(object, gradient = "calculated", hessian = "calculated",
           standard_errors = "homoscedastic", ...) {
    validate_gradient_option(object, gradient)
    validate_hessian_option(object, hessian)
    validate_standard_error_option(object, standard_errors)

    va_args <- list(...)

    if (hessian == "skip" ||
      ((object@model_type_string %in% c("Basic", "Directional")) &&
        hessian == "calculated")) {
      va_args$skip.hessian <- TRUE
    } else {
      hessian <- "numerical"
    }

    va_args$start <- prepare_initializing_values(object, va_args$start)

    if (is.null(va_args$method)) {
      va_args$method <- "BFGS"
    }

    va_args$minuslogl <- function(...) minus_log_likelihood(object, ...)
    bbmle::parnames(va_args$minuslogl) <- likelihood_variables(object@system)
    if (gradient == "calculated") {
      va_args$gr <- function(...) gradient(object, ...)
      bbmle::parnames(va_args$gr) <- likelihood_variables(object@system)
    }

    est <- do.call(bbmle::mle2, va_args)
    est@call.orig <- call("bbmle::mle2", va_args)

    if (hessian == "calculated") {
      print_verbose(object@logger, "Calculating hessian and variance-covariance matrix.")
      est@details$hessian <- hessian(object, est@coef)
      tryCatch(
        est@vcov <- MASS::ginv(est@details$hessian),
        error = function(e) print_warning(object@logger, e$message)
      )
    }

    if (length(standard_errors) == 1) {
      if (standard_errors == "heteroscedastic") {
        est <- set_heteroscedasticity_consistent_errors(object, est)
      } else if (standard_errors != "homoscedastic") {
        est <- set_clustered_errors(object, est, standard_errors)
      }
    } else {
      est <- set_clustered_errors(object, est, standard_errors)
    }

    new("market_fit", object, est)
  }
)

#' @describeIn estimate Equilibrium model estimation.
#' @param method A string specifying the estimation method. When the passed value is
#' among \code{Nelder-Mead}, \code{BFGS}, \code{CG}, \code{L-BFGS-B}, \code{SANN},
#' and \code{Brent}, the model is estimated using
#' full information maximum likelihood based on \code{\link[bbmle]{mle2}} functionality.
#' When \code{2SLS} is supplied, the model is estimated using two-stage least squares
#' based on \code{\link[systemfit]{systemfit}}. In this case, the function returns a
#' list containing the first and second stage estimates. The default value is
#' \code{BFGS}.
setMethod(
  "estimate", signature(object = "equilibrium_model"),
  function(object, method = "BFGS", ...) {
    if (method != "2SLS") {
      return(callNextMethod(object, method = method, ...))
    }

    ## create fitted variable
    fitted_column <- paste0(colnames(object@system@price_vector), "_FITTED")

    ## estimate first stage
    first_stage_controls <- unique(c(
      control_variables(object@system@demand),
      control_variables(object@system@supply)
    ))
    first_stage_controls <- first_stage_controls[
      first_stage_controls != "CONST"
    ]
    first_stage_formula <- paste0(
      colnames(object@system@price_vector),
      " ~ ", paste0(first_stage_controls, collapse = " + ")
    )

    first_stage_model <- lm(first_stage_formula, object@model_tibble)
    object@model_tibble[, fitted_column] <- first_stage_model$fitted.values

    ## create demand formula
    independent <- independent_variables(object@system@demand)
    independent <- independent[independent != "CONST"]
    demand_formula <- formula(paste0(
      colnames(object@system@quantity_vector),
      " ~ ", paste0(independent, collapse = " + ")
    ))

    ## create supply formula
    independent <- independent_variables(object@system@supply)
    independent <- independent[independent != "CONST"]
    supply_formula <- formula(paste0(
      colnames(object@system@quantity_vector),
      " ~ ", paste0(independent, collapse = " + ")
    ))

    inst <- formula(paste0(" ~ ", paste0(first_stage_controls, collapse = " + ")))
    system_model <- systemfit::systemfit(
      list(demand = demand_formula, supply = supply_formula),
      method = "2SLS", inst = inst, data = object@model_tibble,
      ...
    )

    new(
      "market_fit", object,
      list(first_stage_model = first_stage_model, system_model = system_model)
    )
  }
)


#' Estimated coefficients.
#'
#' Returns the coefficients of the fitted model.
#' @param object A fitted model object.
#' @return A vector of estimated model coefficients.
#' @rdname coef
#' @examples
#' \donttest{
#' # initialize the model using the houses dataset
#' model <- new(
#'   "diseq_deterministic_adjustment", # model type
#'   subject = ID, time = TREND, quantity = HS, price = RM,
#'   demand = RM + TREND + W + CSHS + L1RM + L2RM + MONTH,
#'   supply = RM + TREND + W + L1RM + MA6DSF + MA3DHF + MONTH,
#'   fair_houses(), # data
#'   correlated_shocks = FALSE # allow shocks to be correlated
#' )
#'
#' # estimate the model.
#' est <- estimate(model, control = list(maxit = 1e+5), method = "BFGS")
#'
#' # summarize results
#' coef(est)
#' }
#' @export
setMethod(
  "coef", signature(object = "market_fit"),
  function(object) {
    if (class(object@fit[[1]]) == "mle2") {
      object@fit[[1]]@coef
    } else {
      object@fit[[1]]$system_model$coefficients
    }
  }
)

#' Estimated coefficients.
#'
#' Specializes the \code{\link[stats]{logLik}} function for the market models
#' of the package estimated with full information maimumum likelihood. It
#' returns `NULL` for the equilibrium model estimated with
#' \code{\link[systemfit]{systemfit}}.
#' @param object A fitted model object.
#' @return A \code{\link[stats]{logLik}} object.
#' @rdname logLik
#' @examples
#' \donttest{
#' # initialize the model using the houses dataset
#' model <- new(
#'   "diseq_deterministic_adjustment", # model type
#'   subject = ID, time = TREND, quantity = HS, price = RM,
#'   demand = RM + TREND + W + CSHS + L1RM + L2RM + MONTH,
#'   supply = RM + TREND + W + L1RM + MA6DSF + MA3DHF + MONTH,
#'   fair_houses(), # data
#'   correlated_shocks = FALSE # allow shocks to be correlated
#' )
#'
#' # estimate the model.
#' est <- estimate(model, control = list(maxit = 1e+5), method = "BFGS")
#'
#' # summarize results
#' logLik(est)
#' }
#' @export
setMethod(
  "logLik", signature(object = "market_fit"),
  function(object) {
    ll <- NULL
    if (class(object@fit[[1]]) == "mle2") {
      ll <- structure(-object@fit[[1]]@min,
        df = length(object@fit[[1]]@coef), class = "logLik"
      )
    }
    ll
  }
)
