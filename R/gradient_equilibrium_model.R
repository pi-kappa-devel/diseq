#' @include system_fiml.R

setMethod("calculate_system_scores", signature(object = "system_fiml"),
          function(object) {
  # nolint start
  ad <- object@demand@alpha
  as <- object@supply@alpha
  bd <- object@demand@beta
  bs <- object@supply@beta
  sd <- object@demand@sigma
  ss <- object@supply@sigma
  vd <- object@demand@var
  vs <- object@supply@var
  xd <- object@demand@control_matrix
  xs <- object@supply@control_matrix
  dl <- object@delta
  r <- object@rho
  muP <- object@mu_P
  vP <- object@var_P
  muQ <- object@mu_Q
  vQ <- object@var_Q
  sP <- object@sigma_P
  sQ <- object@sigma_Q
  rQP <- object@rho_QP
  r1QP <- object@rho_1QP
  r2QP <- object@rho_2QP
  hP <- object@h_P
  hQ <- object@h_Q
  zPQ <- object@z_PQ
  zQP <- object@z_QP
  xbd <- xd %*% bd
  xbs <- xs %*% bs

  palpha_d <- (dl*muP*r1QP*vQ*zPQ - dl*r1QP*sP*sQ*zQP*(xbs - muQ) + dl*sP*vQ*(hP*r1QP*zPQ - 1) - r1QP**2*ss*(ad*rQP*sP*ss - as*r*rQP*sP*sd + r*sQ*sd - sQ*ss)*(-hP**2*r1QP**2*rQP + hP*hQ*r1QP**2 + hP*hQ*r2QP**2 - hQ**2*r1QP**2*rQP + rQP) + sP*(dl*vQ + ss*(ad*ss - as*r*sd))*(hQ*r1QP*zQP - 1))/(dl**2*sP*vQ)
  pbeta_d1 <- sweep(xd, MARGIN = 1, r1QP*(as*sP*zQP + sQ*zPQ)/(dl*sP*sQ), `*`)
  palpha_s <- (-dl*muP*r1QP*vQ*zPQ + dl*r1QP*sP*sQ*zQP*(xbd - muQ) + dl*sP*vQ*(-hP*r1QP*zPQ + 1) + r1QP**2*sd*(ad*r*rQP*sP*ss - as*rQP*sP*sd - r*sQ*ss + sQ*sd)*(-hP**2*r1QP**2*rQP + hP*hQ*r1QP**2 + hP*hQ*r2QP**2 - hQ**2*r1QP**2*rQP + rQP) - sP*(dl*vQ + sd*(ad*r*ss - as*sd))*(hQ*r1QP*zQP - 1))/(dl**2*sP*vQ)
  pbeta_s1 <- sweep(-xs, MARGIN = 1, r1QP*(ad*sP*zQP + sQ*zPQ)/(dl*sP*sQ), `*`)
  pvar_d <- (-as*vP*(ad*r*ss - as*sd)*(hQ*r1QP*zQP - 1) + r1QP**2*(rQP*(as*vP*(ad*r*ss - as*sd) + vQ*(r*ss - sd)) + sP*sQ*(2*as*sd - r*ss*(ad + as)))*(-hP**2*r1QP**2*rQP + hP*hQ*r1QP**2 + hP*hQ*r2QP**2 - hQ**2*r1QP**2*rQP + rQP) - vQ*(r*ss - sd)*(hP*r1QP*zPQ - 1))/(2*dl**2*vP*vQ*sd)
  pvar_s <- (ad*vP*(ad*ss - as*r*sd)*(hQ*r1QP*zQP - 1) - r1QP**2*(rQP*(ad*vP*(ad*ss - as*r*sd) + vQ*(-r*sd + ss)) - sP*sQ*(2*ad*ss - r*sd*(ad + as)))*(-hP**2*r1QP**2*rQP + hP*hQ*r1QP**2 + hP*hQ*r2QP**2 - hQ**2*r1QP**2*rQP + rQP) - vQ*(r*sd - ss)*(hP*r1QP*zPQ - 1))/(2*dl**2*vP*vQ*ss)
  Dl <- cbind(palpha_d, pbeta_d1, palpha_s, pbeta_s1, pvar_d, pvar_s)
  if (object@correlated_shocks) {
      prho <- sd*ss*(-ad*as*vP*(hQ*r1QP*zQP - 1) + r1QP**2*(rQP*(ad*as*vP + vQ) - sP*sQ*(ad + as))*(-hP**2*r1QP**2*rQP + hP*hQ*r1QP**2 + hP*hQ*r2QP**2 - hQ**2*r1QP**2*rQP + rQP) - vQ*(hP*r1QP*zPQ - 1))/(dl**2*vP*vQ)
    Dl <- cbind(Dl, prho)
  }
  # nolint end

  colnames(Dl) <- likelihood_variables(object)
  Dl
})
