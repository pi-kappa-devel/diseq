#' @include market_model.R
#' @importFrom systemfit systemfit

#' @describeIn market_models Equilibrium model
#'
#' @description
#' \subsection{equilibrium_model}{
#' The equilibrium model consists of thee equations. The demand, the
#' supply and the market clearing equations. The model can be estimated using both full
#' information maximum likelihood and two-stage least squares.
#'
#' \deqn{D_{nt} = X_{d,nt}'\beta_{d} + P_{nt}\alpha_{d} + u_{d,nt},}
#' \deqn{S_{nt} = X_{s,nt}'\beta_{s} + P_{nt}\alpha_{s} + u_{s,nt},}
#' \deqn{Q_{nt} = D_{nt} = S_{nt}.}
#'
#' A necessary identification condition is that
#' there is at least one control that is exclusively part of the demand and one control
#' that is exclusively part of the supply equation. In the first stage of the two-stage
#' least square estimation, prices are regressed on remaining controls from both the
#' demand and supply equations. In the second stage, the demand and supply equation is
#' estimated using the fitted prices instead of the observed.
#' }
#' @export
setClass(
  "equilibrium_model",
  contains = "market_model",
  representation()
)

#' @describeIn initialize_market_model Equilibrium model constructor
#' @examples
#' simulated_data <- simulate_data(
#'   "equilibrium_model", 500, 3, # model type, observed entities and time points
#'   -0.9, 14.9, c(0.3, -0.2), c(-0.03, -0.01), # demand coefficients
#'   0.9, 3.2, c(0.3), c(0.5, 0.02) # supply coefficients
#' )
#'
#' # initialize the model
#' model <- new(
#'   "equilibrium_model", # model type
#'   c("id", "date"), "Q", "P", # keys, quantity, and price variables
#'   "P + Xd1 + Xd2 + X1 + X2", "P + Xs1 + X1 + X2", # equation specifications
#'   simulated_data, # data
#'   correlated_shocks = TRUE # allow shocks to be correlated
#' )
#'
#' show(model)
setMethod(
  "initialize", "equilibrium_model",
  function(
           .Object,
           key_columns, quantity_column, price_column,
           demand_specification, supply_specification,
           data,
           correlated_shocks = TRUE, verbose = 0) {
    .Object <- callNextMethod(
      .Object,
      "Equilibrium", verbose,
      key_columns, NULL,
      quantity_column, price_column, demand_specification, supply_specification, NULL,
      correlated_shocks,
      data,
      function(...) new("system_equilibrium", ...)
    )
    .Object@market_type_string <- "Equilibrium"

    .Object
  }
)

#' @rdname minus_log_likelihood
setMethod(
  "minus_log_likelihood", signature(object = "equilibrium_model"),
  function(object, parameters) {
    object@system <- set_parameters(object@system, parameters)
    -sum(object@system@llh)
  }
)


#' @rdname gradient
setMethod(
  "gradient", signature(object = "equilibrium_model"),
  function(object, parameters) {
    object@system <- set_parameters(object@system, parameters)
    g <- colSums(calculate_system_scores(object@system))

    as.matrix(-g)
  }
)

#' @rdname scores
setMethod(
  "scores", signature(object = "equilibrium_model"),
  function(object, parameters) {
    object@system <- set_parameters(object@system, parameters)
    -calculate_system_scores(object@system)
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
    fitted_column <- paste0(object@system@demand@price_variable, "_FITTED")

    ## estimate first stage
    first_stage_controls <- unique(c(
      object@system@demand@control_variables,
      object@system@supply@control_variables
    ))
    first_stage_formula <- paste0(
      object@system@demand@price_variable,
      " ~ ", paste0(first_stage_controls, collapse = " + ")
    )

    first_stage_model <- lm(first_stage_formula, object@model_tibble)
    object@model_tibble[, fitted_column] <- first_stage_model$fitted.values

    ## create demand formula
    independent <- object@system@demand@independent_variables
    demand_formula <- formula(paste0(
      object@system@quantity_variable,
      " ~ ", paste0(independent, collapse = " + ")
    ))

    ## create supply formula
    independent <- object@system@supply@independent_variables
    supply_formula <- formula(paste0(
      object@system@quantity_variable,
      " ~ ", paste0(independent, collapse = " + ")
    ))

    inst <- formula(paste0(" ~ ", paste0(first_stage_controls, collapse = " + ")))
    system_model <- systemfit::systemfit(
      list(demand = demand_formula, supply = supply_formula),
      method = "2SLS", inst = inst, data = object@model_tibble,
      ...
    )

    list(first_stage_model = first_stage_model, system_model = system_model)
  }
)

#' @rdname maximize_log_likelihood
setMethod(
  "maximize_log_likelihood", signature(object = "equilibrium_model"),
  function(object, start, step, objective_tolerance, gradient_tolerance) {
    start <- prepare_initializing_values(object, NULL)

    cpp_model <- new(cpp_equilibrium_model, object@system)
    cpp_model$minimize(
      start, step, objective_tolerance,
      gradient_tolerance
    )
  }
)
