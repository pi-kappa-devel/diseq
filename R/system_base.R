#' @include equation_base.R
setClass(
  "system_base",
  representation(
    demand = "equation_base",
    supply = "equation_base",
    correlated_shocks = "logical",
    sample_separation = "logical",

    quantity_variable = "character",
    price_variable = "character",

    quantity_vector = "matrix",
    price_vector = "matrix",

    rho = "numeric",
    rho1 = "numeric",
    rho2 = "numeric"
  ),
  prototype(
    demand = NULL,
    supply = NULL,

    quantity_variable = NULL,
    price_variable = NULL,

    quantity_vector = matrix(NA_real_),
    price_vector = matrix(NA_real_),

    rho = 0,
    rho1 = 1,
    rho2 = 0
  )
)

setMethod(
  "initialize", "system_base",
  function(
           .Object, quantity, price,
           demand_specification, supply_specification, data, correlated_shocks,
           demand_initializer, supply_initializer) {
    .Object@demand <- demand_initializer(
      quantity, price,
      demand_specification, data, "Demand Equation", "D_"
    )
    .Object@supply <- supply_initializer(
      quantity, price,
      supply_specification, data, "Supply Equation", "S_"
    )
    .Object@correlated_shocks <- correlated_shocks
    .Object@sample_separation <- FALSE

    .Object@quantity_variable <- quantity
    .Object@price_variable <- price

    .Object@quantity_vector <- as.matrix(data[, quantity])
    .Object@price_vector <- as.matrix(data[, price])

    .Object
  }
)

setMethod("show_implementation", signature(object = "system_base"), function(object) {
  show_implementation(object@demand)
  show_implementation(object@supply)
})

setGeneric("summary_implementation", function(object) {
  standardGeneric("summary_implementation")
})

setMethod("summary_implementation", signature(object = "system_base"), function(object) {
  sample_separation_output <- ""
  if (object@sample_separation) {
    sample_separation_output <- sprintf(
      "Demand Obs = %d, Supply Obs = %d",
      sum(object@system@demand@separation_subset),
      sum(object@system@supply@separation_subset)
    )
  }
  else {
    sample_separation_output <- "Not Separated"
  }
  cat(sprintf("  %-18s: %s\n", "Sample Separation", sample_separation_output))
  cat(sprintf("  %-18s: %s\n", "Quantity Var", object@quantity_variable))
  cat(sprintf("  %-18s: %s\n", "Price Var", object@price_variable))
})

setGeneric("lagged_price_variable", function(object) {
  standardGeneric("lagged_price_variable")
})

setGeneric("price_differences_variable", function(object) {
  standardGeneric("price_differences_variable")
})

setGeneric("correlation_variable", function(object) {
  standardGeneric("correlation_variable")
})

setGeneric("likelihood_variables", function(object) {
  standardGeneric("likelihood_variables")
})

setGeneric("calculate_system_moments", function(object) {
  standardGeneric("calculate_system_moments")
})

setGeneric("calculate_system_likelihood", function(object) {
  standardGeneric("calculate_system_likelihood")
})

setGeneric("calculate_system_loglikelihood", function(object) {
  standardGeneric("calculate_system_loglikelihood")
})

setGeneric("calculate_system_gradient", function(object) {
  standardGeneric("calculate_system_gradient")
})

setGeneric("calculate_system_scores", function(object) {
  standardGeneric("calculate_system_scores")
})

setMethod("lagged_price_variable", signature(object = "system_base"), function(object) {
  paste0("LAGGED_", object@demand@price_variable)
})

setMethod("price_differences_variable", signature(object = "system_base"), function(object) {
  paste0(object@price_variable, "_DIFF")
})

setMethod("correlation_variable", signature(object = "system_base"), function(object) {
  "RHO"
})

setGeneric("calculate_llh", function(object) {
  standardGeneric("calculate_llh")
})

setMethod("likelihood_variables", signature(object = "system_base"), function(object) {
  likelihood_variables <- c(
    prefixed_price_variable(object@demand),
    prefixed_control_variables(object@demand),
    prefixed_price_variable(object@supply),
    prefixed_control_variables(object@supply),
    prefixed_variance_variable(object@demand),
    prefixed_variance_variable(object@supply)
  )

  if (object@correlated_shocks) {
    likelihood_variables <- c(likelihood_variables, correlation_variable(object))
  }

  likelihood_variables
})


setMethod("set_parameters", signature(object = "system_base"), function(object, parameters) {
  object@demand <- set_parameters(object@demand, parameters)
  object@supply <- set_parameters(object@supply, parameters)
  if (object@correlated_shocks) {
    object@rho <- parameters[correlation_variable(object)]
    object@rho <- ifelse(abs(object@rho) > 1, NA_real_, object@rho)
    object@rho1 <- 1 / sqrt(1 - object@rho**2)
    object@rho2 <- object@rho * object@rho1
  }
  object
})
