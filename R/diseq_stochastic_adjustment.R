#' Disequilibrium model with stochastic price dynamics.
#'
#' @include diseq_base.R
#' @name diseq_stochastic_adjustment-class
#' @export
setClass(
  "diseq_stochastic_adjustment",
  contains = "diseq_base",
  representation(),
  prototype()
)

setMethod("initialize", "diseq_stochastic_adjustment", function(
  .Object,
  verbose = 0,
  key_columns, time_column,
  quantity_column, price_column, demand_specification, supply_specification, price_specification,
  use_correlated_shocks = TRUE,
  data
) {
  .Object <- callNextMethod(
    .Object,
    "Stochastic Adjustment", verbose,
    key_columns, time_column,
    quantity_column, price_column, demand_specification, supply_specification, price_specification,
    use_correlated_shocks,
    data,
    function(...) new("system_stochastic_adjustment", ...)
  )

  .Object
})

setMethod("get_shortage_variance", signature(object = "diseq_stochastic_adjustment"), function(object) {
  sqrt(object@system@demand@var + object@system@supply@var - 2 * object@system@demand@sigma *
         object@system@supply@sigma * object@system@rho_ds)
})

setMethod("get_initializing_values", signature(object = "diseq_stochastic_adjustment"), function(object) {
  start <- callNextMethod(object)

  len <- length(start)
  pos <- len - ifelse(object@system@correlated_shocks, 3, 2)
  start <- c(start[1:pos], object@system@price_equation@linear_model$coefficients, start[(pos + 1):len])

  len <- length(start)
  if (object@system@correlated_shocks) {
    start <- c(start[1:(len - 1)], 1, start[len], 0, 0)
    names(start)[len:length(start)] <- c(
      get_prefixed_variance_variable(object@system@price_equation),
      paste0(get_correlation_variable(object@system), c("_DS", "_DP", "_SP"))
    )
  }
  else {
    start <- c(start, price_variance = 1)
    names(start)[len + 1] <- get_prefixed_variance_variable(object@system@price_equation)
  }

  start
})

#' @rdname minus_log_likelihood
setMethod("minus_log_likelihood", signature(object = "diseq_stochastic_adjustment"), function(object, parameters) {
  object@system <- set_parameters(object@system, parameters)
  object@system <- calculate_system_loglikelihood(object@system)
  - sum(object@system@log_likelihood)
})

setMethod("gradient", signature(object = "diseq_stochastic_adjustment"), function(object, parameters) {
  object@system <- set_parameters(object@system, parameters)
  object@system <- calculate_system_gradient(object@system)
  -object@system@gradient
})
