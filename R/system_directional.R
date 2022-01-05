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
  function(.Object, specification, data, correlated_shocks) {
    price_diff <- paste0(
      all.vars(formula(specification, lhs = 2, rhs = 0)),
      "_DIFF"
    )
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

setMethod(
  "show_implementation", signature(object = "system_directional"),
  function(object) {
    callNextMethod(object)
    cat(sprintf("  %-18s: %s\n", "Separation Rule", paste0(
      price_differences_variable(object), " >= 0 then ",
      prefixed_quantity_variable(object@demand), " >= ",
      prefixed_quantity_variable(object@supply)
    )))
  }
)
