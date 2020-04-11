#' @include model_base.R

#' @title Equilibrium model base class
setClass(
  "eq_base",
  contains = "model_base",
  representation()
)

setMethod(
  "initialize", "eq_base",
  function(
           .Object,
           model_type_string, verbose,
           key_columns,
           quantity_column, price_column,
           demand_specification, supply_specification,
           use_correlated_shocks,
           data,
           system_initializer) {
    .Object <- callNextMethod(
      .Object,
      model_type_string, verbose,
      key_columns, NULL,
      quantity_column, price_column,
      demand_specification, supply_specification, NULL,
      use_correlated_shocks,
      data,
      system_initializer
    )

    .Object
  }
)
