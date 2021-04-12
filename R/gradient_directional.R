#' @include system_directional.R

setMethod("calculate_system_scores", signature(object = "system_directional"),
          function(object) {
  # nolint start
  sd <- object@demand@sigma
  ss <- object@supply@sigma
  vd <- object@demand@var
  vs <- object@supply@var
  xd <- object@demand@independent_matrix
  xs <- object@supply@independent_matrix
  Id <- object@demand@separation_subset
  Is <- object@supply@separation_subset
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

  pbeta_d1 <- sweep(xd, MARGIN = 1, (Id*PsiD*PsiS*hD - Id*PsiS*psiD*r*r1 + Is*PsiD*psiS*r1)/(PsiD*PsiS*sd), `*`)
  pbeta_s1 <- sweep(xs, MARGIN = 1, (Id*PsiS*psiD*r1 + Is*PsiD*PsiS*hS - Is*PsiD*psiS*r*r1)/(PsiD*PsiS*ss), `*`)
  pvar_d <- (Id*PsiD*PsiS*(hD**2 - 1) - Id*PsiS*hD*psiD*r*r1 + Is*PsiD*hD*psiS*r1)/(2*PsiD*PsiS*vd)
  pvar_s <- (Id*PsiS*hS*psiD*r1 + Is*PsiD*PsiS*(hS**2 - 1) - Is*PsiD*hS*psiS*r*r1)/(2*PsiD*PsiS*vs)
  Dl <- cbind(pbeta_d1, pbeta_s1, pvar_d, pvar_s)
  if (object@correlated_shocks) {
    prho <- r1*(Id*PsiS*psiD*(hD - r*r1*zSD) + Is*PsiD*psiS*(hS - r*r1*zDS))/(PsiD*PsiS)
    Dl <- cbind(Dl, prho)
  }
  # nolint end

  colnames(Dl) <- likelihood_variables(object)
  Dl
})
