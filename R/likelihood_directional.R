#' @include system_directional.R

setMethod(
  "calculate_system_likelihood", signature(object = "system_directional"),
  function(object) {
    (object@demand@Psi * object@demand@separation_subset) / object@demand@sigma +
      (object@supply@Psi * object@supply@separation_subset) / object@supply@sigma
  }
)
