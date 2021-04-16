#' @include diseq_basic.R

#' @rdname hessian
setMethod("hessian", signature(object = "diseq_basic"), function(object, parameters) {
  object@system <- set_parameters(object@system, parameters)
  object <- object@system

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

  pbeta_d1 <- sweep(xd, MARGIN = 1, (psiS*r1*sd + ss*(PsiD*hD - psiD*r*r1))/(vd*ss)/object@lh, `*`)
  pbeta_s1 <- sweep(xs, MARGIN = 1, (psiD*r1*ss + sd*(PsiS*hS - psiS*r*r1))/(sd*vs)/object@lh, `*`)
  pvar_d <- (hD*psiS*r1*sd - ss*(-PsiD*hD**2 + PsiD + hD*psiD*r*r1))/(2*sd**3*ss)/object@lh
  pvar_s <- (hS*psiD*r1*ss - sd*(-PsiS*hS**2 + PsiS + hS*psiS*r*r1))/(2*sd*ss**3)/object@lh
  pbeta_d1beta_d1 <- t(xd)%*%sweep(xd, MARGIN = 1, ((psiS*r1**2*sd*zDS + ss*(PsiD*hD**2 - PsiD - 2*hD*psiD*r*r1 + psiD*r**2*r1**2*zSD))/(sd**3*ss)/object@lh), `*`)
  pbeta_d1beta_s1 <- t(xd)%*%sweep(xs, MARGIN = 1, (r1*(psiD*ss*(hD - r*r1*zSD) + psiS*sd*(hS - r*r1*zDS))/(vd*vs)/object@lh), `*`)
  pbeta_d1var_d <- colSums(sweep(xd, MARGIN = 1, (psiS*r1*sd*(hD*r1*zDS - 1) + ss*(PsiD*hD**3 - 3*PsiD*hD - 2*hD**2*psiD*r*r1 + hD*psiD*r**2*r1**2*zSD + 2*psiD*r*r1))/(2*sd**4*ss)/object@lh, `*`))
  pbeta_d1var_s <- colSums(-sweep(xd, MARGIN = 1, r1*(hS*psiD*ss*(-hD + r*r1*zSD) + psiS*sd*(-hS**2 + hS*r*r1*zDS + 1))/(2*vd*ss**3)/object@lh, `*`))
  pbeta_s1beta_s1 <- t(xs)%*%sweep(xs, MARGIN = 1, ((psiD*r1**2*ss*zSD + sd*(PsiS*hS**2 - PsiS - 2*hS*psiS*r*r1 + psiS*r**2*r1**2*zDS))/(sd*ss**3)/object@lh), `*`)
  pbeta_s1var_d <- colSums(-sweep(xs, MARGIN = 1, r1*(hD*psiS*sd*(-hS + r*r1*zDS) + psiD*ss*(-hD**2 + hD*r*r1*zSD + 1))/(2*sd**3*vs)/object@lh, `*`))
  pbeta_s1var_s <- colSums(sweep(xs, MARGIN = 1, (psiD*r1*ss*(hS*r1*zSD - 1) + sd*(PsiS*hS**3 - 3*PsiS*hS - 2*hS**2*psiS*r*r1 + hS*psiS*r**2*r1**2*zDS + 2*psiS*r*r1))/(2*sd*ss**4)/object@lh, `*`))
  pvar_dvar_d <- colSums((hD*psiS*r1*sd*(hD*r1*zDS - 3) + ss*(PsiD*hD**4 - 6*PsiD*hD**2 + 3*PsiD - 2*hD**3*psiD*r*r1 + hD**2*psiD*r**2*r1**2*zSD + 5*hD*psiD*r*r1))/(4*sd**5*ss)/object@lh)
  pvar_dvar_s <- colSums(-r1*(hD*psiS*sd*(-hS**2 + hS*r*r1*zDS + 1) + hS*psiD*ss*(-hD**2 + hD*r*r1*zSD + 1))/(4*sd**3*ss**3)/object@lh)
  pvar_svar_s <- colSums((hS*psiD*r1*ss*(hS*r1*zSD - 3) + sd*(PsiS*hS**4 - 6*PsiS*hS**2 + 3*PsiS - 2*hS**3*psiS*r*r1 + hS**2*psiS*r**2*r1**2*zDS + 5*hS*psiS*r*r1))/(4*sd*ss**5)/object@lh)

  hbeta_d1beta_d1 <- pbeta_d1beta_d1 - t(pbeta_d1) %*% pbeta_d1
  hbeta_d1beta_s1 <- pbeta_d1beta_s1 - t(pbeta_d1) %*% pbeta_s1
  hbeta_d1var_d <- pbeta_d1var_d - t(pbeta_d1) %*% pvar_d
  hbeta_d1var_s <- pbeta_d1var_s - t(pbeta_d1) %*% pvar_s
  hbeta_s1beta_s1 <- pbeta_s1beta_s1 - t(pbeta_s1) %*% pbeta_s1
  hbeta_s1var_d <- pbeta_s1var_d - t(pbeta_s1) %*% pvar_d
  hbeta_s1var_s <- pbeta_s1var_s - t(pbeta_s1) %*% pvar_s
  hvar_dvar_d <- pvar_dvar_d - t(pvar_d) %*% pvar_d
  hvar_dvar_s <- pvar_dvar_s - t(pvar_d) %*% pvar_s
  hvar_svar_s <- pvar_svar_s - t(pvar_s) %*% pvar_s
  Dl <- rbind(
     cbind(hbeta_d1beta_d1, hbeta_d1beta_s1, hbeta_d1var_d, hbeta_d1var_s),
     cbind(t(hbeta_d1beta_s1), hbeta_s1beta_s1, hbeta_s1var_d, hbeta_s1var_s),
     cbind(t(hbeta_d1var_d), t(hbeta_s1var_d), hvar_dvar_d, hvar_dvar_s),
     cbind(t(hbeta_d1var_s), t(hbeta_s1var_s), hvar_dvar_s, hvar_svar_s)
  )

  if (object@correlated_shocks) {
    prho <- r1*(psiD*ss*(hD - r*r1*zSD) + psiS*sd*(hS - r*r1*zDS))/(sd*ss)/object@lh
    pbeta_d1rho <- colSums(sweep(xd, MARGIN = 1, r1*(psiD*ss*(hD**2 - 2*hD*r*r1*zSD + r**2*r1**2*zSD**2 - r**2*r1**2 - 1) + psiS*r1*sd*(hS*zDS - r*r1*zDS**2 + r*r1))/(vd*ss)/object@lh, `*`))
    pbeta_s1rho <- colSums(sweep(xs, MARGIN = 1, r1*(psiD*r1*ss*(hD*zSD - r*r1*zSD**2 + r*r1) + psiS*sd*(hS**2 - 2*hS*r*r1*zDS + r**2*r1**2*zDS**2 - r**2*r1**2 - 1))/(sd*vs)/object@lh, `*`))
    pvar_drho <- colSums(r1*(hD*psiS*r1*sd*(hS*zDS - r*r1*zDS**2 + r*r1) + psiD*ss*(hD**3 - 2*hD**2*r*r1*zSD + hD*r**2*r1**2*zSD**2 - hD*r**2*r1**2 - 2*hD + r*r1*zSD))/(2*sd**3*ss)/object@lh)
    pvar_srho <- colSums(r1*(hS*psiD*r1*ss*(hD*zSD - r*r1*zSD**2 + r*r1) + psiS*sd*(hS**3 - 2*hS**2*r*r1*zDS + hS*r**2*r1**2*zDS**2 - hS*r**2*r1**2 - 2*hS + r*r1*zDS))/(2*sd*ss**3)/object@lh)
    prhorho <- colSums(r1**2*(psiD*ss*(hD**2*zSD - 2*hD*r*r1*zSD**2 + 2*hD*r*r1 + r**2*r1**2*zSD**3 - 3*r**2*r1**2*zSD - zSD) + psiS*sd*(hS**2*zDS - 2*hS*r*r1*zDS**2 + 2*hS*r*r1 + r**2*r1**2*zDS**3 - 3*r**2*r1**2*zDS - zDS))/(sd*ss)/object@lh)

    hbeta_d1rho <- pbeta_d1rho - t(pbeta_d1) %*% prho
    hbeta_s1rho <- pbeta_s1rho - t(pbeta_s1) %*% prho
    hvar_drho <- pvar_drho - t(pvar_d) %*% prho
    hvar_srho <- pvar_srho - t(pvar_s) %*% prho
    hrhorho <- prhorho - t(prho) %*% prho
    rho_col <- rbind(hbeta_d1rho, hbeta_s1rho, hvar_drho, hvar_srho)
    Dl <- rbind(
     cbind(Dl, rho_col),
     cbind(t(rho_col), hrhorho)
    )
  }
  # nolint end

  rownames(Dl) <- likelihood_variables(object)
  colnames(Dl) <- likelihood_variables(object)

  -Dl
})
