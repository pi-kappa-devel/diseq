#' @include system_deterministic_adjustment.R

setMethod("calculate_system_moments",
          signature(object = "system_deterministic_adjustment"),
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
  gm <- object@gamma
  LP <- object@lagged_price_vector
  xbd <- xd %*% bd
  xbs <- xs %*% bs
              
  object@mu_P <- ((LP*gm + xbd - xbs)/dl)
  object@var_P <- ((-2*r*sd*ss + sd**2 + ss**2)/dl**2)
  object@demand@mu_Q <- (xbd + ad*object@mu_P)
  object@demand@var_Q <- (ad**2*object@var_P + 2*ad*(-r*sd*ss + sd**2)/dl + sd**2)
  object@demand@sigma_QP <- (ad*object@var_P + (-r*sd*ss + sd**2)/dl)
  object@supply@mu_Q <- (xbs + as*object@mu_P)
  object@supply@var_Q <- (as**2*object@var_P - 2*as*(-r*sd*ss + ss**2)/dl + ss**2)
  object@supply@sigma_QP <- (as*object@var_P - (-r*sd*ss + ss**2)/dl)
  # nolint end

  object@sigma_P <- sqrt(object@var_P)
  object@demand@sigma_Q <- sqrt(object@demand@var_Q)
  object@supply@sigma_Q <- sqrt(object@supply@var_Q)

  object@h_P <-
    (object@price_vector - object@mu_P) / object@sigma_P
  object@demand@h_Q <-
    (object@quantity_vector - object@demand@mu_Q) / object@demand@sigma_Q
  object@supply@h_Q <-
    (object@quantity_vector - object@supply@mu_Q) / object@supply@sigma_Q

  object@demand@rho_QP <- object@demand@sigma_QP / object@demand@sigma_Q / object@sigma_P
  if (!is.na(object@demand@rho_QP) && abs(object@demand@rho_QP) >= 1) {
    object@demand@rho_QP <- NA_real_
  }
  object@demand@rho_1QP <- 1 / sqrt(1 - object@demand@rho_QP**2)
  object@demand@rho_2QP <- object@demand@rho_QP * object@demand@rho_1QP

  object@supply@rho_QP <- object@supply@sigma_QP / object@supply@sigma_Q / object@sigma_P
  if (!is.na(object@supply@rho_QP) && abs(object@supply@rho_QP) >= 1) {
    object@supply@rho_QP <- NA_real_
  }
  object@supply@rho_1QP <- 1 / sqrt(1 - object@supply@rho_QP**2)
  object@supply@rho_2QP <- object@supply@rho_QP * object@supply@rho_1QP

  object@demand@z_PQ <- (
    object@demand@rho_1QP * object@h_P - object@demand@rho_2QP * object@demand@h_Q)
  object@supply@z_PQ <- (
    object@supply@rho_1QP * object@h_P - object@supply@rho_2QP * object@supply@h_Q)

  object@demand@z_QP <- (
    object@demand@rho_1QP * object@demand@h_Q - object@demand@rho_2QP * object@h_P)
  object@supply@z_QP <- (
    object@supply@rho_1QP * object@supply@h_Q - object@supply@rho_2QP * object@h_P)

  names(object@sigma_P) <- c("sigma_P")
  names(object@demand@sigma_Q) <- c("sigma_D")
  names(object@supply@sigma_Q) <- c("sigma_S")

  names(object@demand@rho_QP) <- c("rho_DP")
  names(object@demand@rho_1QP) <- c("rho_1DP")
  names(object@demand@rho_2QP) <- c("rho_2DP")

  names(object@supply@rho_QP) <- c("rho_SP")
  names(object@supply@rho_1QP) <- c("rho_1SP")
  names(object@supply@rho_2QP) <- c("rho_2SP")

  object
})

setMethod("calculate_system_loglikelihood",
          signature(object = "system_deterministic_adjustment"),
          function(object) {
  # nolint start
  lld <- (-object@demand@rho_1QP**2*(object@demand@h_Q*object@demand@z_QP/object@demand@rho_1QP + object@h_P*object@demand@z_PQ/object@demand@rho_1QP + (log(object@demand@sigma_Q**2*object@sigma_P**2/object@demand@rho_1QP**2) + log(4*pi**2))/object@demand@rho_1QP**2)/2)

  lls <- (-object@supply@rho_1QP**2*(object@h_P*object@supply@z_PQ/object@supply@rho_1QP + object@supply@h_Q*object@supply@z_QP/object@supply@rho_1QP + (log(object@sigma_P**2*object@supply@sigma_Q**2/object@supply@rho_1QP**2) + log(4*pi**2))/object@supply@rho_1QP**2)/2)
  # nolint end

  lld[!object@demand@separation_subset] <- 0
  lls[!object@supply@separation_subset] <- 0

  lld + lls
})
