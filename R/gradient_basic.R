#' @include system_basic.R

setMethod("calculate_system_scores", signature(object = "system_basic"),
          function(object) {
  # nolint start
  sd <- object@demand@sigma
  ss <- object@supply@sigma
  vd <- object@demand@var
  vs <- object@supply@var
  xd <- object@demand@independent_matrix
  xs <- object@supply@independent_matrix
  r <- object@rho
  r1 <- object@rho1
  hD <- object@demand@h
  hS <- object@supply@h
  zDS <- object@demand@z
  zSD <- object@supply@z
  psiD <- object@demand@psi
  psiS <- object@supply@psi
  PsiD <- object@demand@Psi
  PsiS <- object@supply@Psi

  pbeta_d1 <- sweep(xd, MARGIN = 1, (psiS*r1*sd + ss*(PsiD*hD - psiD*r*r1))/(vd*ss), `*`)
  pbeta_s1 <- sweep(xs, MARGIN = 1, (psiD*r1*ss + sd*(PsiS*hS - psiS*r*r1))/(sd*vs), `*`)
  pvar_d <- (hD*psiS*r1*sd - ss*(-PsiD*hD**2 + PsiD + hD*psiD*r*r1))/(2*sd**3*ss)
  pvar_s <- (hS*psiD*r1*ss - sd*(-PsiS*hS**2 + PsiS + hS*psiS*r*r1))/(2*sd*ss**3)
  Dl <- cbind(pbeta_d1, pbeta_s1, pvar_d, pvar_s)
  if (object@correlated_shocks) {
    prho <- r1*(psiD*ss*(hD - r*r1*zSD) + psiS*sd*(hS - r*r1*zDS))/(sd*ss)
    Dl <- cbind(Dl, prho)
  }
  Dl <- Dl / c(object@lh)
  # nolint end

  colnames(Dl) <- likelihood_variables(object)
  Dl
})
