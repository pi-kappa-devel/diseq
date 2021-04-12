#' @include system_deterministic_adjustment.R

setMethod("calculate_system_scores",
          signature(object = "system_deterministic_adjustment"),
          function(object) {
  # nolint start
  sd <- object@demand@sigma
  ss <- object@supply@sigma
  vd <- object@demand@var
  vs <- object@supply@var
  ad <- object@demand@alpha
  as <- object@supply@alpha
  bd <- object@demand@beta
  bs <- object@supply@beta
  xd <- object@demand@control_matrix
  xs <- object@supply@control_matrix
  Id <- object@demand@separation_subset
  Is <- object@supply@separation_subset
  r <- object@rho
  dl <- object@delta
  muP <- object@mu_P
  vP <- object@var_P
  sP <- object@sigma_P
  muD <- object@demand@mu_Q
  vD <- object@demand@var_Q
  sD <- object@demand@sigma_Q
  muS <- object@supply@mu_Q
  vS <- object@supply@var_Q
  sS <- object@supply@sigma_Q
  rDP <- object@demand@rho_QP
  r1DP <- object@demand@rho_1QP
  r2DP <- object@demand@rho_2QP
  rSP <- object@supply@rho_QP
  r1SP <- object@supply@rho_1QP
  r2SP <- object@supply@rho_2QP
  hP <- object@h_P
  hD <- object@demand@h_Q
  hS <- object@supply@h_Q
  zPD <- object@demand@z_PQ
  zDP <- object@demand@z_QP
  zPS <- object@supply@z_PQ
  zSP <- object@supply@z_QP
  gm <- object@gamma
  LP <- object@lagged_price_vector
  xbd <- xd %*% bd
  xbs <- xs %*% bs

  palpha_d <- -(Id*sS*sS**2*vS*(-dl*muP*r1DP*sD*sD**2*sP*sP*vD*vP*zPD - dl*muP*r1DP*sD*sD*sP*vP*vD*vP*zDP*(ad + dl) + dl*sD*sD**2*sP**3*vD*vP*(hD*hP*r1DP**2*rDP - hP**2*r1DP**2 + 1) + r1DP**2*sD**2*vP*(rDP*sD*sP*(dl*vP*vD + vP*(ad*dl**2*vP - ad*sd*(r*ss - sd) + dl*(ad**2*vP - r*sd*ss + vd))) - vD*vP*(2*ad*dl*vP + dl**2*vP - sd*(r*ss - sd)))*(-hD**2*r1DP**2*rDP + 2*hD*hP*r1DP**2*rDP**2 + hD*hP - hP**2*r1DP**2*rDP + rDP) + sD*sP*vP*vD*vP*(-hD**2*r1DP**2 + hD*hP*r1DP**2*rDP + 1)*(ad*dl**2*vP - ad*sd*(r*ss - sd) + dl*(ad**2*vP - r*sd*ss + vd))) + Is*sD*sD**2*vD*(-as*dl*muP*r1SP*sP*vP*sS*sS*vP*vS*zSP + as*sP*vP*sS*vP*vS*(as*dl*vP + ss*(r*sd - ss))*(hP*hS*r1SP**2*rSP - hS**2*r1SP**2 + 1) - dl*muP*r1SP*sP*sP*sS*sS**2*vP*vS*zPS + dl*sP**3*sS*sS**2*vP*vS*(-hP**2*r1SP**2 + hP*hS*r1SP**2*rSP + 1) + r1SP**2*vP*sS**2*(rSP*sP*sS*(as*vP*(as*dl*vP + ss*(r*sd - ss)) + dl*vP*vS) - vP*vS*(2*as*dl*vP + ss*(r*sd - ss)))*(-hP**2*r1SP**2*rSP + 2*hP*hS*r1SP**2*rSP**2 + hP*hS - hS**2*r1SP**2*rSP + rSP)))/(dl**2*sD**2*sD*vP*sP*sS**2*sS*vD*vP*vS)
  pbeta_d1 <- sweep(xd, MARGIN = 1, (Id*r1DP*sS*(sD*zPD + sP*zDP*(ad + dl)) + Is*r1SP*sD*(as*sP*zSP + sS*zPS))/(dl*sD*sP*sS), `*`)
  palpha_s <- (Id*sS*sS**2*vS*(-ad*dl*muP*r1DP*sD*sD*sP*vP*vD*vP*zDP + ad*sD*sP*vP*vD*vP*(ad*dl*vP - sd*(r*ss - sd))*(-hD**2*r1DP**2 + hD*hP*r1DP**2*rDP + 1) - dl*muP*r1DP*sD*sD**2*sP*sP*vD*vP*zPD + dl*sD*sD**2*sP**3*vD*vP*(hD*hP*r1DP**2*rDP - hP**2*r1DP**2 + 1) + r1DP**2*sD**2*vP*(rDP*sD*sP*(ad*vP*(ad*dl*vP - sd*(r*ss - sd)) + dl*vP*vD) - vD*vP*(2*ad*dl*vP - sd*(r*ss - sd)))*(-hD**2*r1DP**2*rDP + 2*hD*hP*r1DP**2*rDP**2 + hD*hP - hP**2*r1DP**2*rDP + rDP)) - Is*sD*sD**2*vD*(dl*muP*r1SP*sP*vP*sS*sS*vP*vS*zSP*(as - dl) + dl*muP*r1SP*sP*sP*sS*sS**2*vP*vS*zPS - dl*sP**3*sS*sS**2*vP*vS*(-hP**2*r1SP**2 + hP*hS*r1SP**2*rSP + 1) - r1SP**2*vP*sS**2*(rSP*sP*sS*(dl*vP*vS + vP*(-as*dl**2*vP + as*ss*(r*sd - ss) + dl*(as**2*vP - r*sd*ss + vs))) - vP*vS*(2*as*dl*vP - dl**2*vP + ss*(r*sd - ss)))*(-hP**2*r1SP**2*rSP + 2*hP*hS*r1SP**2*rSP**2 + hP*hS - hS**2*r1SP**2*rSP + rSP) - sP*vP*sS*vP*vS*(-as*dl**2*vP + as*ss*(r*sd - ss) + dl*(as**2*vP - r*sd*ss + vs))*(hP*hS*r1SP**2*rSP - hS**2*r1SP**2 + 1)))/(dl**2*sD**2*sD*vP*sP*sS**2*sS*vD*vP*vS)
  pbeta_s1 <- sweep(-xs, MARGIN = 1, (Id*r1DP*sS*(ad*sP*zDP + sD*zPD) + Is*r1SP*sD*(sP*zSP*(as - dl) + sS*zPS))/(dl*sD*sP*sS), `*`)
  pgamma <- (Id*sS*sS**2*vS*(ad*dl*r1DP*sD*sD*sP*vP*vD*vP*zDP*(LP - muP) + ad*sD*sP*vP*vD*vP*(ad*dl*vP - sd*(r*ss - sd))*(-hD**2*r1DP**2 + hD*hP*r1DP**2*rDP + 1) + dl*r1DP*sD*sD**2*sP*sP*vD*vP*zPD*(LP - muP) + dl*sD*sD**2*sP**3*vD*vP*(hD*hP*r1DP**2*rDP - hP**2*r1DP**2 + 1) + r1DP**2*sD**2*vP*(rDP*sD*sP*(ad*vP*(ad*dl*vP - sd*(r*ss - sd)) + dl*vP*vD) - vD*vP*(2*ad*dl*vP - sd*(r*ss - sd)))*(-hD**2*r1DP**2*rDP + 2*hD*hP*r1DP**2*rDP**2 + hD*hP - hP**2*r1DP**2*rDP + rDP)) + Is*sD*sD**2*vD*(as*dl*r1SP*sP*vP*sS*sS*vP*vS*zSP*(LP - muP) + as*sP*vP*sS*vP*vS*(as*dl*vP + ss*(r*sd - ss))*(hP*hS*r1SP**2*rSP - hS**2*r1SP**2 + 1) + dl*r1SP*sP*sP*sS*sS**2*vP*vS*zPS*(LP - muP) + dl*sP**3*sS*sS**2*vP*vS*(-hP**2*r1SP**2 + hP*hS*r1SP**2*rSP + 1) + r1SP**2*vP*sS**2*(rSP*sP*sS*(as*vP*(as*dl*vP + ss*(r*sd - ss)) + dl*vP*vS) - vP*vS*(2*as*dl*vP + ss*(r*sd - ss)))*(-hP**2*r1SP**2*rSP + 2*hP*hS*r1SP**2*rSP**2 + hP*hS - hS**2*r1SP**2*rSP + rSP)))/(dl**2*sD**2*sD*vP*sP*sS**2*sS*vD*vP*vS)
  pvar_d <- (Id*sS*sS**2*vS*(r1DP**2*sD**2*vP*(rDP*sD*sP*(vD*(r*ss - sd) + vP*(ad**2*(r*ss - sd) + ad*dl*(r*ss - 2*sd) - dl**2*sd)) - vD*vP*(2*ad*(r*ss - sd) + dl*(r*ss - 2*sd)))*(-hD**2*r1DP**2*rDP + 2*hD*hP*r1DP**2*rDP**2 + hD*hP - hP**2*r1DP**2*rDP + rDP) + sD*sD**2*sP*vD*vP*(r*ss - sd)*(hD*hP*r1DP**2*rDP - hP**2*r1DP**2 + 1) + sD*sP*vP*vD*vP*(ad**2*(r*ss - sd) + ad*dl*(r*ss - 2*sd) - dl**2*sd)*(-hD**2*r1DP**2 + hD*hP*r1DP**2*rDP + 1)) + Is*sD*sD**2*vD*(as*sP*vP*sS*vP*vS*(as*(r*ss - sd) - dl*r*ss)*(hP*hS*r1SP**2*rSP - hS**2*r1SP**2 + 1) + r1SP**2*vP*sS**2*(rSP*sP*sS*(as*vP*(as*(r*ss - sd) - dl*r*ss) + vS*(r*ss - sd)) - vP*vS*(2*as*(r*ss - sd) - dl*r*ss))*(-hP**2*r1SP**2*rSP + 2*hP*hS*r1SP**2*rSP**2 + hP*hS - hS**2*r1SP**2*rSP + rSP) + sP*sS*sS**2*vP*vS*(r*ss - sd)*(-hP**2*r1SP**2 + hP*hS*r1SP**2*rSP + 1)))/(2*dl**2*sD**2*sD*vP*sP*sS**2*sS*sd*vD*vP*vS)
  pvar_s <- (Id*sS*sS**2*vS*(ad*sD*sP*vP*vD*vP*(ad*(r*sd - ss) + dl*r*sd)*(-hD**2*r1DP**2 + hD*hP*r1DP**2*rDP + 1) + r1DP**2*sD**2*vP*(rDP*sD*sP*(ad*vP*(ad*(r*sd - ss) + dl*r*sd) + vD*(r*sd - ss)) - vD*vP*(2*ad*(r*sd - ss) + dl*r*sd))*(-hD**2*r1DP**2*rDP + 2*hD*hP*r1DP**2*rDP**2 + hD*hP - hP**2*r1DP**2*rDP + rDP) + sD*sD**2*sP*vD*vP*(r*sd - ss)*(hD*hP*r1DP**2*rDP - hP**2*r1DP**2 + 1)) - Is*sD*sD**2*vD*(r1SP**2*vP*sS**2*(rSP*sP*sS*(vP*(as**2*(-r*sd + ss) + as*dl*(r*sd - 2*ss) + dl**2*ss) - vS*(r*sd - ss)) + vP*vS*(2*as*(r*sd - ss) - dl*(r*sd - 2*ss)))*(-hP**2*r1SP**2*rSP + 2*hP*hS*r1SP**2*rSP**2 + hP*hS - hS**2*r1SP**2*rSP + rSP) + sP*vP*sS*vP*vS*(as**2*(-r*sd + ss) + as*dl*(r*sd - 2*ss) + dl**2*ss)*(hP*hS*r1SP**2*rSP - hS**2*r1SP**2 + 1) - sP*sS*sS**2*vP*vS*(r*sd - ss)*(-hP**2*r1SP**2 + hP*hS*r1SP**2*rSP + 1)))/(2*dl**2*sD**2*sD*vP*sP*sS**2*sS*ss*vD*vP*vS)
  Dl <- cbind(palpha_d, pbeta_d1, palpha_s, pbeta_s1, pgamma, pvar_d, pvar_s)
  if (object@correlated_shocks) {
      prho <- sd*ss*(Id*sS*sS**2*vS*(ad*sD*sP*vP*vD*vP*(ad + dl)*(-hD**2*r1DP**2 + hD*hP*r1DP**2*rDP + 1) + r1DP**2*sD**2*vP*(rDP*sD*sP*(ad*vP*(ad + dl) + vD) - vD*vP*(2*ad + dl))*(-hD**2*r1DP**2*rDP + 2*hD*hP*r1DP**2*rDP**2 + hD*hP - hP**2*r1DP**2*rDP + rDP) + sD*sD**2*sP*vD*vP*(hD*hP*r1DP**2*rDP - hP**2*r1DP**2 + 1)) + Is*sD*sD**2*vD*(as*sP*vP*sS*vP*vS*(as - dl)*(hP*hS*r1SP**2*rSP - hS**2*r1SP**2 + 1) + r1SP**2*vP*sS**2*(rSP*sP*sS*(as*vP*(as - dl) + vS) - vP*vS*(2*as - dl))*(-hP**2*r1SP**2*rSP + 2*hP*hS*r1SP**2*rSP**2 + hP*hS - hS**2*r1SP**2*rSP + rSP) + sP*sS*sS**2*vP*vS*(-hP**2*r1SP**2 + hP*hS*r1SP**2*rSP + 1)))/(dl**2*sD**2*sD*vP*sP*sS**2*sS*vD*vP*vS)
    Dl <- cbind(Dl, prho)
  }
  # nolint end

  colnames(Dl) <- likelihood_variables(object)

  Dl
})
