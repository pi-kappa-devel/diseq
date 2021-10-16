#' @include equation_base.R

#' @describeIn equation_classes Deterministic adjustment disequilibrium model equation
#' class
#' @slot mu_Q \deqn{\mu_{Q} = \mathrm{E}Q}
#' @slot var_Q \deqn{V_{Q} = \mathrm{Var}Q}
#' @slot sigma_Q \deqn{\sigma_{Q} = \sqrt{\mathrm{Var}Q}}
#' @slot rho_QP \deqn{\rho_{Q} =
#' \frac{\mathrm{Cov}(Q,P)}{\sqrt{\mathrm{Var}Q\mathrm{Var}P}}}
#' @slot rho_1QP \deqn{\rho_{1,QP} = \frac{1}{\sqrt{1 - \rho_{QP}}}}
#' @slot rho_2QP \deqn{\rho_{2,QP} = \rho_{QP}\rho_{1,QP}}
#' @slot sigma_QP \deqn{\sigma_{QP} = \mathrm{Cov}(Q,P)}
#' @slot h_Q As in slot \code{h}
#' @slot z_PQ As in slot \code{z}
#' @slot z_QP As in slot \code{z}
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
  function(.Object, specification, data, name, prefix, separation_subset) {
    .Object <- callNextMethod(.Object, specification, data, name, prefix)
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
