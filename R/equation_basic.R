#' @include equation_base.R

#' @describeIn equation_classes Basic disequilibrium model equation class
#'
#' @slot h \deqn{h_{x} = \frac{x - \mathrm{E} x}{\sqrt{\mathrm{Var} x}}}
#' @slot z \deqn{z_{xy} = \frac{h_{x} - \rho_{xy}h_{y}}{\sqrt{1 - \rho_{xy}^2}}}
#' @slot psi \deqn{\psi_{x} = \phi(h_{x})}
#' @slot Psi \deqn{\Psi_{x} = 1 - \Phi(z_{xy})}
setClass(
  "equation_basic",
  contains = "equation_base",
  representation(
    h = "matrix",
    z = "matrix",
    psi = "matrix",
    Psi = "matrix"
  ),
  prototype(
    h = matrix(NA_real_),
    z = matrix(NA_real_),
    psi = matrix(NA_real_),
    Psi = matrix(NA_real_)
  )
)

setMethod(
  "initialize", "equation_basic",
  function(.Object, specification, data, name, prefix) {
    .Object <- callNextMethod(.Object, specification, data, name, prefix)
    .Object
  }
)

setMethod(
  "set_parameters", signature(object = "equation_basic"),
  function(object, parameters) {
    object <- callNextMethod(object, parameters)
  }
)
