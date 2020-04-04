#' Disequilibrium model with deterministic price dynamics.
#'
#' @include diseq_base.R
#' @include derivatives_deterministic_adjustment.R
#' @name diseq_deterministic_adjustment-class
#' @export
setClass(
  "diseq_deterministic_adjustment",
  contains = "diseq_base",
  representation(),
  prototype()
)

setMethod(
  "initialize", "diseq_deterministic_adjustment",
  function(
           .Object,
           verbose = 0,
           key_columns, time_column,
           quantity_column, price_column, demand_specification, supply_specification,
           use_correlated_shocks = TRUE,
           data) {
    .Object <- callNextMethod(
      .Object,
      "Deterministic Adjustment", verbose,
      key_columns, time_column,
      quantity_column, price_column, demand_specification, supply_specification, NULL,
      use_correlated_shocks,
      data,
      function(...) new("system_deterministic_adjustment", ...)
    )

    print_info(
      .Object@logger,
      "Sample separated with ", sum(.Object@system@demand@separation_subset),
      " rows in excess supply and ",
      sum(.Object@system@supply@separation_subset), " in excess demand regime."
    )

    .Object
  }
)

#' @rdname minus_log_likelihood
setMethod(
  "minus_log_likelihood", signature(object = "diseq_deterministic_adjustment"),
  function(object, parameters) {
    object@system <- set_parameters(object@system, parameters)
    object@system <- calculate_system_loglikelihood(object@system)
    -sum(object@system@log_likelihood)
  }
)

setMethod(
  "gradient", signature(object = "diseq_deterministic_adjustment"),
  function(object, parameters) {
    object@system <- set_parameters(object@system, parameters)
    object@system <- calculate_system_gradient(object@system)
    -object@system@gradient
  }
)
