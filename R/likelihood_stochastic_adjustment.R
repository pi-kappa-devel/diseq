#' @include system_stochastic_adjustment.R

validate_variance <- function(var) {
  if (!is.na(var) && var <= 0) {
    var <- NA_real_
  }
  var
}

validate_correlation <- function(rho) {
  if (!is.na(rho) && abs(rho) >= 1) {
    rho <- NA_real_
  }
  rho
}

setMethod("calculate_system_moments",
          signature(object = "system_stochastic_adjustment"),
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
  rds <- object@rho_ds
  rdp <- object@rho_dp
  rsp <- object@rho_sp
  gm <- object@gamma
  dl <- object@delta
  LP <- object@lagged_price_vector
  xp <- object@price_equation@control_matrix
  bp <- object@price_equation@beta
  sp <- object@price_equation@sigma
  xbd <- xd %*% bd
  xbs <- xs %*% bs
  xbp <- xp %*% bp
              
  object@mu_P <- ((LP*gm + xbd + xbp*gm - xbs)/(-ad + as + gm))
  object@var_P <- validate_variance((gm**2*sp**2 + 2*gm*(rdp*sd*sp - rsp*sp*ss) - 2*rds*sd*ss + sd**2 + ss**2)/(-ad + as + gm)**2)
  object@mu_D <- (xbd + ad*(LP*gm + xbd + xbp*gm - xbs)/(-ad + as + gm))
  object@var_D <- validate_variance(ad**2*(gm**2*sp**2 + 2*gm*(rdp*sd*sp - rsp*sp*ss) - 2*rds*sd*ss + sd**2 + ss**2)/(-ad + as + gm)**2 + 2*ad*(gm*rdp*sd*sp - rds*sd*ss + sd**2)/(-ad + as + gm) + sd**2)
  object@mu_S <- (xbs + as*(LP*gm + xbd + xbp*gm - xbs)/(-ad + as + gm))
  object@var_S <- validate_variance(as**2*(gm**2*sp**2 + 2*gm*(rdp*sd*sp - rsp*sp*ss) - 2*rds*sd*ss + sd**2 + ss**2)/(-ad + as + gm)**2 - 2*as*(-gm*rsp*sp*ss - rds*sd*ss + ss**2)/(-ad + as + gm) + ss**2)
  object@sigma_DS <- (ad*as*(gm**2*sp**2 + 2*gm*(rdp*sd*sp - rsp*sp*ss) - 2*rds*sd*ss + sd**2 + ss**2)/(-ad + as + gm)**2 - ad*(-gm*rsp*sp*ss - rds*sd*ss + ss**2)/(-ad + as + gm) + as*(gm*rdp*sd*sp - rds*sd*ss + sd**2)/(-ad + as + gm) + rds*sd*ss)
  object@sigma_DP <- (ad*(gm**2*sp**2 + 2*gm*(rdp*sd*sp - rsp*sp*ss) - 2*rds*sd*ss + sd**2 + ss**2)/(-ad + as + gm)**2 + (gm*rdp*sd*sp - rds*sd*ss + sd**2)/(-ad + as + gm))
  object@sigma_SP <- (as*(gm**2*sp**2 + 2*gm*(rdp*sd*sp - rsp*sp*ss) - 2*rds*sd*ss + sd**2 + ss**2)/(-ad + as + gm)**2 - (-gm*rsp*sp*ss - rds*sd*ss + ss**2)/(-ad + as + gm))
  # nolint end

  object@sigma_P <- sqrt(object@var_P)
  object@sigma_D <- sqrt(object@var_D)
  object@sigma_S <- sqrt(object@var_S)

  object@rho_DS <- object@sigma_DS / object@sigma_D / object@sigma_S
  object@rho_DS <- validate_correlation(object@rho_DS)

  object@rho_DP <- object@sigma_DP / object@sigma_P / object@sigma_D
  object@rho_DP <- validate_correlation(object@rho_DP)

  object@rho_SP <- object@sigma_SP / object@sigma_P / object@sigma_S
  object@rho_SP <- validate_correlation(object@rho_SP)

  object@zeta <- (
    1 - object@rho_DS**2 - object@rho_DP**2 - object@rho_SP**2 +
    2 * object@rho_DP * object@rho_DS * object@rho_SP
  )
  if (!is.na(object@zeta) && object@zeta < 0) {
    object@zeta <- NA_real_
  }
  object@zeta <- sqrt(object@zeta)

  object@h_D <- (object@quantity_vector - object@mu_D) / object@sigma_D
  object@h_S <- (object@quantity_vector - object@mu_S) / object@sigma_S
  object@h_P <- (object@price_vector - object@mu_P) / object@sigma_P

  object@zeta_DS <- object@rho_DS - object@rho_DP * object@rho_SP
  object@zeta_DP <- object@rho_DP - object@rho_DS * object@rho_SP
  object@zeta_SP <- object@rho_SP - object@rho_DP * object@rho_DS
  object@zeta_DD <- 1 - object@rho_SP**2
  object@zeta_SS <- 1 - object@rho_DP**2
  object@zeta_PP <- 1 - object@rho_DS**2

  object@z_DP <- (object@h_D - object@h_P * object@rho_DP) / sqrt(object@zeta_SS)
  object@z_PD <- (-object@h_D * object@rho_DP + object@h_P) / sqrt(object@zeta_SS)
  object@z_SP <- (-object@h_P * object@rho_SP + object@h_S) / sqrt(object@zeta_DD)
  object@z_PS <- (object@h_P - object@h_S * object@rho_SP) / sqrt(object@zeta_DD)

  object@omega_D <- (object@h_D * object@zeta_DD - object@h_S * object@zeta_DS -
                     object@h_P * object@zeta_DP) / sqrt(object@zeta_DD)
  object@omega_S <- (object@h_S * object@zeta_SS - object@h_D * object@zeta_DS -
                     object@h_P * object@zeta_SP) / sqrt(object@zeta_SS)

  object@w_D <- - (object@h_D**2 - 2 * object@h_D * object@h_P * object@rho_DP +
                object@h_P**2) / 2 / object@zeta_SS
  object@w_S <- - (object@h_S**2 - 2 * object@h_S * object@h_P * object@rho_SP +
                object@h_P**2) / 2 / object@zeta_DD

  object@psi_D <- dnorm(object@omega_D / object@zeta)
  object@psi_S <- dnorm(object@omega_S / object@zeta)
  object@Psi_D <- pnorm(object@omega_D / object@zeta, lower.tail = FALSE)
  object@Psi_S <- pnorm(object@omega_S / object@zeta, lower.tail = FALSE)

  object@g_D <- object@psi_D / object@Psi_D
  object@g_S <- object@psi_S / object@Psi_S

  object@L_D <- object@Psi_S * exp(object@w_D) / (
                        2 * pi * object@sigma_D * object@sigma_P * sqrt(object@zeta_SS))
  object@L_S <- object@Psi_D * exp(object@w_S) / (
                        2 * pi * object@sigma_S * object@sigma_P * sqrt(object@zeta_DD))

  object@g_D <- dnorm(object@omega_D / object@zeta) / pnorm(object@omega_D / object@zeta,
    lower.tail = FALSE
  )
  object@g_S <- dnorm(object@omega_S / object@zeta) / pnorm(object@omega_S / object@zeta,
    lower.tail = FALSE
  )

  names(object@sigma_P) <- c("sigma_P")
  names(object@sigma_D) <- c("sigma_D")
  names(object@sigma_S) <- c("sigma_S")

  names(object@rho_DS) <- c("rho_DS")
  names(object@rho_DP) <- c("rho_DP")
  names(object@rho_SP) <- c("rho_SP")

  object
})

setMethod("calculate_system_loglikelihood",
          signature(object = "system_stochastic_adjustment"),
          function(object) {
  log(object@L_D + object@L_S)
})
