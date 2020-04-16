#' @include eq_base.R
#' @importFrom systemfit systemfit

setOldClass(c("systemfit"))

#' @title Equilibrium model estimated using 2-stage least squares.
#'
#' @description In the first stage, prices are regressed on remaining controls from both the
#' demand and supply equations. In the second stage, the demand and supply equation is estimated
#' using the fitted prices instead of the observed. A necessary identification condition is that
#' there is at least one control that is exclusively part of the demand and one control
#' that is exclusively part of the supply equation.
#'
#' \deqn{D_{nt} = X_{d,nt}'\beta_{d} + P_{nt}\alpha_{d} + u_{d,nt},}
#' \deqn{S_{nt} = X_{s,nt}'\beta_{s} + P_{nt}\alpha_{s} + u_{s,nt},}
#' \deqn{Q_{nt} = D_{nt} = S_{nt}.}
#'
#' @slot first_stage_model An estimated first stage equation of type \code{\link[stats]{lm}}.
#' @slot system_model An estimated system of market equations of type
#'   \code{\link[systemfit]{systemfit}}.
#'
#' @examples
#' simulated_data <- simulate_model_data(
#'   "eq_2sls", 500, 3, # model type, observed entities and time points
#'   -0.9, 14.9, c(0.3, -0.2), c(-0.03, -0.01), # demand coefficients
#'   0.9, 3.2, c(0.3), c(0.5, 0.02) # supply coefficients
#' )
#'
#' # initialize the model
#' model <- new(
#'   "eq_2sls", # model type
#'   c("id", "date"), "Q", "P", # keys, quantity, and price variables
#'   "P + Xd1 + Xd2 + X1 + X2", "P + Xs1 + X1 + X2", # equation specifications
#'   simulated_data # data
#' )
#' @export
setClass(
  "eq_2sls",
  contains = "eq_base",
  representation(
    first_stage_model = "lm",
    system_model = "systemfit"
  )
)

#' @describeIn initialize_model_base Two stage least squares equilibrium model constructor
setMethod(
  "initialize", "eq_2sls",
  function(
           .Object,
           key_columns, quantity_column, price_column,
           demand_specification, supply_specification,
           data,
           verbose = 0) {
    .Object <- callNextMethod(
      .Object,
      "Equilibrium 2SLS", verbose,
      key_columns,
      quantity_column, price_column, demand_specification, supply_specification,
      FALSE,
      data,
      function(...) new("system_basic", ...)
    )

    .Object
  }
)

#' @describeIn estimate Equilibrium model estimation.
setMethod("estimate", signature(object = "eq_2sls"), function(object, ...) {
  ## create fitted variable
  fitted_column <- paste0(object@system@demand@price_variable, "_FITTED")

  ## estimate first stage
  first_stage_controls <-
    unique(c(object@system@demand@control_variables, object@system@supply@control_variables))
  first_stage_formula <-
    paste0(
      object@system@demand@price_variable,
      " ~ ", paste0(first_stage_controls, collapse = " + ")
    )

  object@first_stage_model <- lm(first_stage_formula, object@model_tibble)
  object@model_tibble[, fitted_column] <- object@first_stage_model$fitted.values

  ## create demand formula
  independent <- object@system@demand@independent_variables
  independent[object@system@demand@price_variable == independent] <- fitted_column
  demand_formula <- formula(paste0(
    object@system@quantity_variable,
    " ~ ", paste0(independent, collapse = " + ")
  ))

  ## create supply formula
  independent <- object@system@supply@independent_variables
  independent[object@system@supply@price_variable == independent] <- fitted_column
  supply_formula <- formula(paste0(
    object@system@quantity_variable,
    " ~ ", paste0(independent, collapse = " + ")
  ))

  inst <- formula(paste0(" ~ ", paste0(first_stage_controls, collapse = " + ")))
  object@system_model <- systemfit::systemfit(
    list(demand = demand_formula, supply = supply_formula),
    method = "2SLS", inst = inst, data = object@model_tibble,
    ...
  )

  object
})
