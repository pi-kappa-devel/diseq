#' @include equation_directional.R
#' @include system_basic.R
setClass(
  "system_directional",
  contains = "system_basic",
  representation(),
  prototype()
)

setMethod(
  "initialize", "system_directional",
  function(
           .Object, quantity, price,
           demand_specification, supply_specification, data, correlated_shocks) {
    demand_initializer <- function(...) {
      price_diff <- paste0(price, "_DIFF")
      excess_supply_subset <- data[, price_diff] < 0
      new("equation_directional", ..., excess_supply_subset)
    }
    supply_initializer <- function(...) {
      price_diff <- paste0(price, "_DIFF")
      excess_demand_subset <- data[, price_diff] >= 0
      new("equation_directional", ..., excess_demand_subset)
    }
    .Object <- callNextMethod(
      .Object, quantity, price, demand_specification, supply_specification, data, correlated_shocks,
      demand_initializer, supply_initializer
    )
    .Object@sample_separation <- TRUE
    .Object
  }
)
