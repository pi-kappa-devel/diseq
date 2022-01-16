#' @include system_directional.R

setMethod(
  "calculate_system_moments",
  signature(object = "system_directional"),
  function(object) {
    object@demand@h <- ((object@quantity_vector -
      object@demand@independent_matrix %*% object@demand@alpha_beta) /
      object@demand@sigma)
    object@supply@h <- ((object@quantity_vector -
      object@supply@independent_matrix %*% object@supply@alpha_beta) /
      object@supply@sigma)

    object@demand@h[!object@demand@separation_subset] <- 0.0
    object@supply@h[!object@supply@separation_subset] <- 0.0

    object@demand@z <- (object@demand@h - object@supply@h * object@rho) * object@rho1
    object@supply@z <- (object@supply@h - object@demand@h * object@rho) * object@rho1
    object@demand@psi <- dnorm(object@demand@h) * dnorm(object@supply@z)
    object@supply@psi <- dnorm(object@supply@h) * dnorm(object@demand@z)
    object@demand@Psi <- dnorm(object@demand@h) * pnorm(object@supply@z,
      lower.tail = FALSE
    )
    object@supply@Psi <- dnorm(object@supply@h) * pnorm(object@demand@z,
      lower.tail = FALSE
    )

    object
  }
)

setMethod(
  "calculate_system_likelihood", signature(object = "system_directional"),
  function(object) {
    (object@demand@Psi * object@demand@separation_subset) / object@demand@sigma +
      (object@supply@Psi * object@supply@separation_subset) / object@supply@sigma
  }
)
