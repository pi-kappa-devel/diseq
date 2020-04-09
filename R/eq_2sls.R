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
#' \deqn{
#'   \begin{aligned}
#'   D_{nt} &= X_{d,nt}'\beta_{d} + P_{nt}\alpha_{d} + u_{d,nt}, \\
#'   S_{nt} &= X_{s,nt}'\beta_{s} + P_{nt}\alpha_{s} + u_{s,nt}, \\
#'   Q_{nt} &= D_{nt} = S_{nt} ,
#'   \end{aligned}
#' }
#'
#' @slot first_stage_model An estimated first stage equation of type \code{\link[lm]{lm}}.
#' @slot system_model An estimated system of market equations of type
#'   \code{\link[systemfit]{systemfit}}.
#' @seealso \code{\link{initialize_model}}
#' @export
setClass(
  "eq_2sls",
  contains = "eq_base",
  representation(
    first_stage_model = "lm",
    system_model = "systemfit"
  )
)

#' @describeIn initialize_model Two stage least squares equilibrium model constructor.
#' @export
setMethod(
  "initialize", "eq_2sls",
  function(
           .Object,
           verbose = 0,
           key_columns,
           quantity_column, price_column,
           demand_specification, supply_specification,
           data) {
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
