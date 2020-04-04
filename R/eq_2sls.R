setOldClass(c("systemfit"))

#' Equilibrium model estimated using 2-stage least squares.
#'
#' @include eq_base.R
#' @name eq_2sls-class
#' @import systemfit
#' @export
setClass(
  "eq_2sls",
  contains = "eq_base",
  representation(
    first_stage_model = "lm",
    system_model = "systemfit"
  )
)

setMethod(
  "initialize", "eq_2sls",
  function(
           .Object,
           verbose = 0,
           key_columns,
           quantity_column, price_column,
           demand_specification, supply_specification,
           use_correlated_shocks = TRUE,
           data) {
    .Object <- callNextMethod(
      .Object,
      "Equilibrium 2SLS", verbose,
      key_columns,
      quantity_column, price_column, demand_specification, supply_specification,
      use_correlated_shocks,
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
