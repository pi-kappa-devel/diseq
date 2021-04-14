#' @include equation_base.R

#' @title Deterministic adjustment disequilibrium model equation class
setClass(
  "equation_deterministic_adjustment",
  contains = "equation_base",
  representation(
    separation_subset = "vector",

    mu_Q = "matrix",
    var_Q = "numeric",
    sigma_Q = "numeric",

    rho_QP = "numeric",
    rho_1QP = "numeric",
    rho_2QP = "numeric",

    sigma_QP = "numeric",

    h_Q = "matrix",

    z_PQ = "matrix",
    z_QP = "matrix",

    gr = "matrix"
  ),
  prototype(
    separation_subset = NULL,

    mu_Q = matrix(NA_real_),
    var_Q = NA_real_,
    sigma_Q = NA_real_,

    sigma_QP = NA_real_,

    rho_QP = NA_real_,
    rho_1QP = NA_real_,
    rho_2QP = NA_real_,

    h_Q = matrix(NA_real_),

    z_PQ = matrix(NA_real_),
    z_QP = matrix(NA_real_)
  )
)

setMethod(
  "initialize", "equation_deterministic_adjustment",
  function(.Object, quantity, price, specification, data, name, prefix, separation_subset) {
    .Object <- callNextMethod(.Object, quantity, price, specification, data, name, prefix)
    .Object@separation_subset <- separation_subset
    .Object
  }
)

setMethod(
  "set_parameters", signature(object = "equation_deterministic_adjustment"),
  function(object, parameters) {
    object <- callNextMethod(object, parameters)
    object
  }
)
