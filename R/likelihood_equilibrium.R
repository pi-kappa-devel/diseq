#' @include system_equilibrium.R

setMethod("calculate_system_moments", signature(object = "system_equilibrium"),
          function(object) {
  # nolint start
  ad <- object@demand@alpha
  as <- object@supply@alpha
  bd <- object@demand@beta
  bs <- object@supply@beta
  sd <- object@demand@sigma
  ss <- object@supply@sigma
  xd <- object@demand@control_matrix
  xs <- object@supply@control_matrix
  r <- object@rho
  dl <- object@delta
  xbd <- xd %*% bd
  xbs <- xs %*% bs
              
  object@mu_P <- ((xbd - xbs)/dl)
  object@var_P <- ((-2*r*sd*ss + sd**2 + ss**2)/dl**2)
  object@mu_Q <- ((as*xbd - ad*xbs)/dl)
  object@var_Q <- ((ad**2*ss**2 - 2*ad*as*r*sd*ss + as**2*sd**2)/dl**2)
  object@rho_QP <- ((ad*ss**2 + as*sd**2 - r*sd*ss*(ad + as))/(dl**2*sqrt((ad**2*ss**2 - 2*ad*as*r*sd*ss + as**2*sd**2)*(-2*r*sd*ss + sd**2 + ss**2)/dl**4)))
  # nolint end

  object@sigma_P <- sqrt(object@var_P)
  object@sigma_Q <- sqrt(object@var_Q)

  object@h_P <- (object@price_vector - object@mu_P) / object@sigma_P
  object@h_Q <- (object@quantity_vector - object@mu_Q) / object@sigma_Q

  if (is.na(object@rho_QP) || abs(object@rho_QP) >= 1) {
    object@rho_QP <- NA_real_
  }
  object@rho_1QP <- 1 / sqrt(1 - object@rho_QP**2)
  object@rho_2QP <- object@rho_QP * object@rho_1QP

  object@z_PQ <- object@rho_1QP * object@h_P - object@rho_2QP * object@h_Q
  object@z_QP <- object@rho_1QP * object@h_Q - object@rho_2QP * object@h_P

  object
})

setMethod("calculate_system_loglikelihood", signature(object = "system_equilibrium"),
          function(object) {
  # nolint start
  (-object@rho_1QP**2*(object@h_P*object@z_PQ/object@rho_1QP + object@h_Q*object@z_QP/object@rho_1QP + (log(object@sigma_P**2*object@sigma_Q**2/object@rho_1QP**2) + log(4*pi**2))/object@rho_1QP**2)/2)
  # nolint end
})
