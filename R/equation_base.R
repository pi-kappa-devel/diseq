#' @import methods

#' @title Equation base class
setClass(
  "equation_base",
  representation(
    prefixed_specification = "character",
    formula = "formula",
    linear_model = "lm",

    name = "character",
    variable_prefix = "character",
    independent_variables = "vector",
    price_variable = "character",
    control_variables = "vector",

    independent_matrix = "matrix",
    price_vector = "matrix",
    control_matrix = "matrix",

    alpha_beta = "matrix",
    alpha = "numeric",
    beta = "matrix",
    var = "numeric",
    sigma = "numeric",

    equation_log_likelihood = "matrix",
    equation_gradient = "matrix"
  ),
  prototype(
    prefixed_specification = NULL,
    formula = NULL,
    linear_model = NULL,

    name = NULL,
    variable_prefix = NULL,
    independent_variables = NULL,
    price_variable = NULL,
    control_variables = NULL,

    independent_matrix = NULL,
    price_vector = NULL,
    control_matrix = NULL,

    alpha_beta = matrix(NA_real_),
    alpha = 0,
    beta = matrix(NA_real_),
    var = NA_real_,
    sigma = NA_real_,

    equation_log_likelihood = matrix(NA_real_),
    equation_gradient = matrix(NA_real_)
  )
)

setMethod(
  "initialize", "equation_base",
  function(.Object, quantity, price, specification, data, name, prefix) {
    tf <- formula(paste0(quantity, " ~ ", specification))
    tnames <- all.vars(tf)
    independent <- tnames[tnames != quantity]
    controls <- independent[independent != price]
    if (price %in% independent) {
      independent <- c(price, controls)
    }
    names(data) <- paste0(prefix, names(data))

    prefixed_specification <- stringr::str_replace_all(
      deparse(tf), stringr::str_c(paste0(tnames, collapse = "|")),
      function(from) paste0(prefix, from)
    )

    formula <- formula(prefixed_specification)
    linear_model <- stats::lm(formula, data)
    prefixed_variables <- names(linear_model$coefficients)
    prefixed_variables[prefixed_variables == "(Intercept)"] <- paste0(prefix, "CONST")
    names(linear_model$coefficients) <- prefixed_variables

    model_matrix <- model.matrix(linear_model)
    prefixed_variables <- colnames(model_matrix)
    prefixed_variables[prefixed_variables == "(Intercept)"] <- paste0(prefix, "CONST")

    .Object@prefixed_specification <- prefixed_specification
    .Object@formula <- formula
    .Object@linear_model <- linear_model

    .Object@name <- name
    .Object@variable_prefix <- prefix
    .Object@independent_variables <- independent
    .Object@price_variable <- price
    .Object@control_variables <- controls

    .Object@independent_matrix <- model_matrix
    colnames(.Object@independent_matrix) <- prefixed_variables

    price_selection <- paste0(prefix, price) == colnames(.Object@independent_matrix)
    .Object@price_vector <- as.matrix(.Object@independent_matrix[, price_selection])
    colnames(.Object@price_vector) <- colnames(.Object@independent_matrix)[price_selection]

    .Object@control_matrix <- as.matrix(.Object@independent_matrix[, !price_selection])
    colnames(.Object@control_matrix) <- colnames(.Object@independent_matrix)[!price_selection]

    .Object
  }
)

setGeneric("show_implementation", function(object) {
  standardGeneric("show_implementation")
})

setMethod("show_implementation", signature(object = "equation_base"), function(object) {
    cat(sprintf("  %-18s: %s\n", object@name, deparse(object@formula)))
})

#' Constant coefficient variable name.
#'
#' The constant coefficient name is constructed by concatenating the equation prefix
#' with \code{CONST}.
#' @param object An equation object.
#' @return The constant coefficient name.
#' @rdname get_prefixed_const_variable
#' @export
setGeneric("get_prefixed_const_variable", function(object) {
  standardGeneric("get_prefixed_const_variable")
})

#' Independent variable names.
#'
#' The names of the independent variables are constructed by concatenating the equation prefix
#' with the column names of the data \code{tibble}.
#' @param object An equation object.
#' @return A vector with the independent variable names.
#' @rdname get_prefixed_independent_variables
#' @export
setGeneric("get_prefixed_independent_variables", function(object) {
  standardGeneric("get_prefixed_independent_variables")
})

#' Price coefficient variable name.
#'
#' The price coefficient name is constructed by concatenating the equation prefix
#' with the name of the price column.
#' @param object An equation object.
#' @return The price coefficient variable name.
#' @rdname get_prefixed_price_variable
#' @export
setGeneric("get_prefixed_price_variable", function(object) {
  standardGeneric("get_prefixed_price_variable")
})

#' Control variable names.
#'
#' The controls of the equation are the independent variables without the price variable.
#' Their names are constructed by concatenating the equation prefix with the name of
#' the price column.
#' @param object An equation object.
#' @return A vector with the control variable names.
#' @rdname get_prefixed_control_variables
#' @export
setGeneric("get_prefixed_control_variables", function(object) {
  standardGeneric("get_prefixed_control_variables")
})

#' Variance variable name.
#'
#' The variance variables is constructed by concatenating the equation prefix with
#' "VARIANCE".
#' @param object An equation object.
#' @return The variable name for the variance of the shock of the equation.
#' @rdname get_prefixed_variance_variable
#' @export
setGeneric("get_prefixed_variance_variable", function(object) {
  standardGeneric("get_prefixed_variance_variable")
})

setGeneric("set_parameters", function(object, parameters) {
  standardGeneric("set_parameters")
})

setGeneric("calculate_equation_loglikelihood", function(object) {
  standardGeneric("calculate_equation_loglikelihood")
})

setGeneric("get_quantities", function(object) {
  standardGeneric("get_quantities")
})

setGeneric("get_aggregate", function(object) {
  standardGeneric("get_aggregate")
})

#' @rdname get_prefixed_const_variable
setMethod("get_prefixed_const_variable", signature(object = "equation_base"), function(object) {
  paste0(object@variable_prefix, "CONST")
})

#' @rdname get_prefixed_price_variable
setMethod(
  "get_prefixed_independent_variables", signature(object = "equation_base"),
  function(object) {
    colnames(object@independent_matrix)
  }
)

#' @rdname get_prefixed_price_variable
setMethod("get_prefixed_price_variable", signature(object = "equation_base"), function(object) {
  colnames(object@price_vector)
})

#' @rdname get_prefixed_price_variable
setMethod("get_prefixed_control_variables", signature(object = "equation_base"), function(object) {
  colnames(object@control_matrix)
})

#' @rdname get_prefixed_variance_variable
setMethod("get_prefixed_variance_variable", signature(object = "equation_base"), function(object) {
  paste0(object@variable_prefix, "VARIANCE")
})

setMethod("set_parameters", signature(object = "equation_base"), function(object, parameters) {
  object@alpha_beta <- as.matrix(parameters[get_prefixed_independent_variables(object)])
  if (!is.null(get_prefixed_price_variable(object))) {
    object@alpha <- parameters[get_prefixed_price_variable(object)]
  }
  object@beta <- as.matrix(parameters[get_prefixed_control_variables(object)])
  object@var <- parameters[get_prefixed_variance_variable(object)]
  if (object@var < 0) {
    object@var <- NA_real_
  }
  object@sigma <- sqrt(object@var)
  object
})

setMethod("get_quantities", signature(object = "equation_base"), function(object) {
  object@independent_matrix %*% object@alpha_beta
})

setMethod("get_aggregate", signature(object = "equation_base"), function(object) {
  sum(get_quantities(object))
})
