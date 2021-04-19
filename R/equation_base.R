#' @import methods

#' @title Equation classes
#'
#' @details Classes with data and functionality describing equations of model systems.
#' @name equation_classes
NULL

#' @describeIn equation_classes Equation base class
#' @slot prefixed_specification The equation formula using prefixed variables.
#' @slot formula The equation formula.
#' @slot linear_model The estimated equation using linear regression.
#' @slot name The name of the equation.
#' @slot variable_prefix A prefix string for the variables of the equation.
#' @slot independent_variables A vector with the right hand side variable names.
#' @slot price_variable The price variable name.
#' @slot control_variables Independent variables without the price variable.
#' @slot independent_matrix A model data matrix with columns corresponding to the set
#' of independent variables.
#' @slot price_vector The vector of prices.
#' @slot control_matrix A model data matrix with columns corresponding to the set
#' of independent variables without prices.
#' @slot alpha_beta A vector of right hand side coefficients.
#' @slot alpha The price coefficient.
#' @slot beta A vector of right hand side coefficient without the price coefficient.
#' @slot var The variance of the equation's shock.
#' @slot sigma The standard deviation of the equation's shock.
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
    quantity_variable = "character",

    independent_matrix = "matrix",
    price_vector = "matrix",
    control_matrix = "matrix",

    alpha_beta = "matrix",
    alpha = "numeric",
    beta = "matrix",
    var = "numeric",
    sigma = "numeric"
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
    sigma = NA_real_
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
    .Object@quantity_variable <- quantity

    .Object@independent_matrix <- model_matrix
    colnames(.Object@independent_matrix) <- prefixed_variables

    price_selection <- paste0(prefix, price) == colnames(.Object@independent_matrix)
    .Object@price_vector <- as.matrix(.Object@independent_matrix[, price_selection])
    colnames(.Object@price_vector) <- colnames(
      .Object@independent_matrix
    )[price_selection]

    .Object@control_matrix <- as.matrix(.Object@independent_matrix[, !price_selection])
    colnames(.Object@control_matrix) <- colnames(
      .Object@independent_matrix
    )[!price_selection]
    .Object@independent_matrix <- .Object@independent_matrix[
      ,
      c(colnames(.Object@price_vector), colnames(.Object@control_matrix))
    ]

    .Object
  }
)

setGeneric("show_implementation", function(object) {
  standardGeneric("show_implementation")
})

setMethod("show_implementation", signature(object = "equation_base"), function(object) {
  cat(sprintf("  %-18s: %s\n", object@name, deparse(object@formula)))
})

#' @title Variable name access
#' @description Methods that provide access to the prefixed variable names that the
#' package uses.
#' @param object An equation object.
#' @return The prefixed variable name(s).
#' @name variable_names
NULL

#' @describeIn variable_names Constant coefficient variable name.
#' @description \code{prefixed_const_variable}: The constant coefficient name is
#' constructed by concatenating the equation prefix with \code{CONST}.
#' @export
setGeneric("prefixed_const_variable", function(object) {
  standardGeneric("prefixed_const_variable")
})

#' @describeIn variable_names Independent variable names.
#' @description \code{prefixed_independent_variables}: The names of the independent
#' variables are constructed by concatenating the equation prefix with the column names
#' of the data \code{tibble}.
#' @export
setGeneric("prefixed_independent_variables", function(object) {
  standardGeneric("prefixed_independent_variables")
})

#' @describeIn variable_names Price coefficient variable name.
#' @description \code{prefixed_price_variable}: The price variable name is
#' constructed by concatenating the equation prefix with the name of the price column.
#' @export
setGeneric("prefixed_price_variable", function(object) {
  standardGeneric("prefixed_price_variable")
})

#' @describeIn variable_names Control variable names.
#' @description \code{prefixed_control_variables}: The controls of the equation are the
#' independent variables without the price variable. Their names are constructed by
#' concatenating the equation prefix with the name of the price column.
#' @export
setGeneric("prefixed_control_variables", function(object) {
  standardGeneric("prefixed_control_variables")
})

#' @describeIn variable_names Variance variable name.
#' @description \code{prefixed_control_variables}: The variance variable is
#' constructed by concatenating the equation prefix with \code{VARIANCE}.
#' @export
setGeneric("prefixed_variance_variable", function(object) {
  standardGeneric("prefixed_variance_variable")
})

#' @describeIn variable_names Quantity variable name.
#' @description \code{prefixed_quantity_variable}: The quantity variable name is
#' constructed by concatenating the equation prefix with the name of the quantity
#' column.
#' @export
setGeneric("prefixed_quantity_variable", function(object) {
  standardGeneric("prefixed_quantity_variable")
})

setGeneric("set_parameters", function(object, parameters) {
  standardGeneric("set_parameters")
})

setGeneric("calculate_equation_loglikelihood", function(object) {
  standardGeneric("calculate_equation_loglikelihood")
})

setGeneric("quantities", function(object) {
  standardGeneric("quantities")
})

#' @rdname variable_names
setMethod(
  "prefixed_const_variable", signature(object = "equation_base"),
  function(object) {
    paste0(object@variable_prefix, "CONST")
  }
)

#' @rdname variable_names
setMethod(
  "prefixed_independent_variables", signature(object = "equation_base"),
  function(object) {
    colnames(object@independent_matrix)
  }
)

#' @rdname variable_names
setMethod(
  "prefixed_price_variable", signature(object = "equation_base"),
  function(object) {
    colnames(object@price_vector)
  }
)

#' @rdname variable_names
setMethod(
  "prefixed_control_variables", signature(object = "equation_base"),
  function(object) {
    colnames(object@control_matrix)
  }
)

#' @rdname variable_names
setMethod(
  "prefixed_variance_variable", signature(object = "equation_base"),
  function(object) {
    paste0(object@variable_prefix, "VARIANCE")
  }
)

#' @rdname variable_names
setMethod(
  "prefixed_quantity_variable", signature(object = "equation_base"),
  function(object) {
    paste0(object@variable_prefix, object@quantity_variable)
  }
)

setMethod(
  "set_parameters", signature(object = "equation_base"),
  function(object, parameters) {
    object@alpha_beta <- as.matrix(parameters[prefixed_independent_variables(object)])
    if (!is.null(prefixed_price_variable(object))) {
      object@alpha <- parameters[prefixed_price_variable(object)]
    }
    object@beta <- as.matrix(parameters[prefixed_control_variables(object)])
    object@var <- parameters[prefixed_variance_variable(object)]
    if (object@var <= 0) {
      object@var <- NA_real_
    }
    object@sigma <- sqrt(object@var)
    object
  }
)

setMethod("quantities", signature(object = "equation_base"), function(object) {
  qs <- object@independent_matrix %*% object@alpha_beta
  colnames(qs) <- prefixed_quantity_variable(object)
  qs
})
