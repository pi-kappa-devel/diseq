#' @include equation_directional.R
#' @include system_basic.R

#' @describeIn system_classes Directional system class
setClass(
  "system_directional",
  contains = "system_basic",
  representation(),
  prototype()
)

setMethod(
  "initialize", "system_directional",
  function(
           .Object, specification, data, correlated_shocks) {
    price_diff <- paste0(specification[[2]][[3]], "_DIFF")
    demand_initializer <- function(...) {
      excess_supply_subset <- data[, price_diff] < 0
      new("equation_directional", ..., excess_supply_subset)
    }
    supply_initializer <- function(...) {
      excess_demand_subset <- data[, price_diff] >= 0
      new("equation_directional", ..., excess_demand_subset)
    }
    .Object <- callNextMethod(
      .Object, specification, data, correlated_shocks,
      demand_initializer, supply_initializer
    )
    .Object@sample_separation <- TRUE
    .Object
  }
)
