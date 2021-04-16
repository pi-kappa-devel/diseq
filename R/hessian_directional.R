#' @include diseq_directional.R

#' @rdname hessian
setMethod("hessian", signature(object = "diseq_directional"),
          function(object, parameters) {
  object@system <- set_parameters(object@system, parameters)
  object <- object@system

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

  pbeta_d1 <- sweep(xd, MARGIN = 1,PsiD**Id*PsiS**Is* sd**(-Id)*ss**(-Is)*(Id*PsiD*PsiS*hD - Id*PsiS*psiD*r*r1 + Is*PsiD*psiS*r1)/(PsiD*PsiS*sd)/object@lh, `*`)
  pbeta_s1 <- sweep(xs, MARGIN = 1,PsiD**Id*PsiS**Is* sd**(-Id)*ss**(-Is)*(Id*PsiS*psiD*r1 + Is*PsiD*PsiS*hS - Is*PsiD*psiS*r*r1)/(PsiD*PsiS*ss)/object@lh, `*`)
  pvar_d <- PsiD**(Id - 1)*PsiS**(Is - 1)*sd**(-Id - 2)*ss**(-Is)*(Id*PsiD*PsiS*hD**2 - Id*PsiD*PsiS - Id*PsiS*hD*psiD*r*r1 + Is*PsiD*hD*psiS*r1)/2/object@lh
  pvar_s <- PsiD**(Id - 1)*PsiS**(Is - 1)*sd**(-Id)*ss**(-Is - 2)*(Id*PsiS*hS*psiD*r1 + Is*PsiD*PsiS*hS**2 - Is*PsiD*PsiS - Is*PsiD*hS*psiS*r*r1)/2/object@lh
  pbeta_d1beta_d1 <- t(xd)%*%sweep(xd, MARGIN = 1, (sd**(-29*Id - 47)*sd**(28*Id + 45)*ss**(-29*Is - 18)*ss**(28*Is + 18)*(-2*Id*Is*PsiD**(Id + 3)*PsiS**(Is + 3)*psiD*psiS*r*r1**2 + Id*PsiD**(Id + 2)*PsiS**(Is + 4)*psiD**2*r**2*r1**2*(Id - 1) + Id*PsiD**(Id + 3)*PsiS**(Is + 4)*psiD*r*r1*(-2*Id*hD + r*r1*zSD) + Id*PsiD**(Id + 4)*PsiS**(Is + 4)*(Id*hD**2 - 1) + Is*PsiD**(Id + 4)*PsiS**(Is + 2)*psiS**2*r1**2*(Is - 1) + Is*PsiD**(Id + 4)*PsiS**(Is + 3)*psiS*r1*(2*Id*hD + r1*zDS))/(PsiD**4*PsiS**4)/object@lh), `*`)
  pbeta_d1beta_s1 <- t(xd)%*%sweep(xs, MARGIN = 1, (sd**(-29*Id - 47)*sd**(28*Id + 46)*ss**(-29*Is - 18)*ss**(28*Is + 17)*(Id*Is*PsiD**(Id + 3)*PsiS**(Is + 3)*psiD*psiS*r1**2*(r**2 + 1) + Id*Is*PsiD**(Id + 4)*PsiS**(Is + 4)*hD*hS + Id*PsiD**(Id + 2)*PsiS**(Is + 4)*psiD**2*r*r1**2*(1 - Id) + Id*PsiD**(Id + 3)*PsiS**(Is + 4)*psiD*r1*(Id*hD - Is*hS*r - r*r1*zSD) + Is*PsiD**(Id + 4)*PsiS**(Is + 2)*psiS**2*r*r1**2*(1 - Is) + Is*PsiD**(Id + 4)*PsiS**(Is + 3)*psiS*r1*(-Id*hD*r + Is*hS - r*r1*zDS))/(PsiD**4*PsiS**4)/object@lh), `*`)
  pbeta_d1var_d <- colSums(sweep(xd, MARGIN = 1, sd**(-29*Id - 47)*sd**(28*Id + 44)*ss**(-29*Is - 18)*ss**(28*Is + 18)*(-2*Id*Is*PsiD**(Id + 3)*PsiS**(Is + 3)*hD*psiD*psiS*r*r1**2 + Id*PsiD**(Id + 2)*PsiS**(Is + 4)*hD*psiD**2*r**2*r1**2*(Id - 1) + Id*PsiD**(Id + 3)*PsiS**(Is + 4)*psiD*r*r1*(-2*Id*hD**2 + Id + hD*r*r1*zSD + 1) + Id*PsiD**(Id + 4)*PsiS**(Is + 4)*hD*(Id*hD**2 - Id - 2) + Is*PsiD**(Id + 4)*PsiS**(Is + 2)*hD*psiS**2*r1**2*(Is - 1) + Is*PsiD**(Id + 4)*PsiS**(Is + 3)*psiS*r1*(2*Id*hD**2 - Id + hD*r1*zDS - 1))/(2*PsiD**4*PsiS**4)/object@lh, `*`))
  pbeta_d1var_s <- colSums(sweep(xd, MARGIN = 1,PsiD**(Id + 2)*PsiS**(Is + 2)* sd**(-29*Id - 47)*sd**(28*Id + 46)*ss**(-29*Is - 18)*ss**(28*Is + 16)*(Id*Is*PsiD**2*PsiS**2*hD*(hS**2 - 1) + Id*Is*PsiD*PsiS*hS*psiD*psiS*r1**2*(r**2 + 1) + Id*PsiD*PsiS**2*psiD*r1*(Id*hD*hS - Is*hS**2*r + Is*r - hS*r*r1*zSD) + Id*PsiS**2*hS*psiD**2*r*r1**2*(1 - Id) + Is*PsiD**2*PsiS*psiS*r1*(-Id*hD*hS*r + Is*hS**2 - Is - hS*r*r1*zDS) + Is*PsiD**2*hS*psiS**2*r*r1**2*(1 - Is))/(2*PsiD**4*PsiS**4)/object@lh, `*`))
  pbeta_s1beta_s1 <- t(xs)%*%sweep(xs, MARGIN = 1, (sd**(-29*Id - 18)*sd**(28*Id + 18)*ss**(-29*Is - 47)*ss**(28*Is + 45)*(-2*Id*Is*PsiD**(Id + 3)*PsiS**(Is + 3)*psiD*psiS*r*r1**2 + Id*PsiD**(Id + 2)*PsiS**(Is + 4)*psiD**2*r1**2*(Id - 1) + Id*PsiD**(Id + 3)*PsiS**(Is + 4)*psiD*r1*(2*Is*hS + r1*zSD) + Is*PsiD**(Id + 4)*PsiS**(Is + 2)*psiS**2*r**2*r1**2*(Is - 1) + Is*PsiD**(Id + 4)*PsiS**(Is + 3)*psiS*r*r1*(-2*Is*hS + r*r1*zDS) + Is*PsiD**(Id + 4)*PsiS**(Is + 4)*(Is*hS**2 - 1))/(PsiD**4*PsiS**4)/object@lh), `*`)
  pbeta_s1var_d <- colSums(sweep(xs, MARGIN = 1,PsiD**(Id + 2)*PsiS**(Is + 2)* sd**(-29*Id - 18)*sd**(28*Id + 16)*ss**(-29*Is - 47)*ss**(28*Is + 46)*(Id*Is*PsiD**2*PsiS**2*hS*(hD**2 - 1) + Id*Is*PsiD*PsiS*hD*psiD*psiS*r1**2*(r**2 + 1) + Id*PsiD*PsiS**2*psiD*r1*(Id*hD**2 - Id - Is*hD*hS*r - hD*r*r1*zSD) + Id*PsiS**2*hD*psiD**2*r*r1**2*(1 - Id) + Is*PsiD**2*PsiS*psiS*r1*(-Id*hD**2*r + Id*r + Is*hD*hS - hD*r*r1*zDS) + Is*PsiD**2*hD*psiS**2*r*r1**2*(1 - Is))/(2*PsiD**4*PsiS**4)/object@lh, `*`))
  pbeta_s1var_s <- colSums(sweep(xs, MARGIN = 1, sd**(-29*Id - 18)*sd**(28*Id + 18)*ss**(-29*Is - 47)*ss**(28*Is + 44)*(-2*Id*Is*PsiD**(Id + 3)*PsiS**(Is + 3)*hS*psiD*psiS*r*r1**2 + Id*PsiD**(Id + 2)*PsiS**(Is + 4)*hS*psiD**2*r1**2*(Id - 1) + Id*PsiD**(Id + 3)*PsiS**(Is + 4)*psiD*r1*(2*Is*hS**2 - Is + hS*r1*zSD - 1) + Is*PsiD**(Id + 4)*PsiS**(Is + 2)*hS*psiS**2*r**2*r1**2*(Is - 1) + Is*PsiD**(Id + 4)*PsiS**(Is + 3)*psiS*r*r1*(-2*Is*hS**2 + Is + hS*r*r1*zDS + 1) + Is*PsiD**(Id + 4)*PsiS**(Is + 4)*hS*(Is*hS**2 - Is - 2))/(2*PsiD**4*PsiS**4)/object@lh, `*`))
  pvar_dvar_d <- colSums(sd**(-29*Id - 76)*sd**(28*Id + 72)*ss**(-29*Is - 18)*ss**(28*Is + 18)*(-2*Id*Is*PsiD**(Id + 3)*PsiS**(Is + 3)*hD**2*psiD*psiS*r*r1**2 + Id*PsiD**(Id + 2)*PsiS**(Is + 4)*hD**2*psiD**2*r**2*r1**2*(Id - 1) + Id*PsiD**(Id + 3)*PsiS**(Is + 4)*hD*psiD*r*r1*(-2*Id*hD**2 + 2*Id + hD*r*r1*zSD + 3) + Id*PsiD**(Id + 4)*PsiS**(Is + 4)*(Id*hD**4 - 2*Id*hD**2 + Id - 4*hD**2 + 2) + Is*PsiD**(Id + 4)*PsiS**(Is + 2)*hD**2*psiS**2*r1**2*(Is - 1) + Is*PsiD**(Id + 4)*PsiS**(Is + 3)*hD*psiS*r1*(2*Id*hD**2 - 2*Id + hD*r1*zDS - 3))/(4*PsiD**4*PsiS**4)/object@lh)
  pvar_dvar_s <- colSums(PsiD**(Id + 2)*PsiS**(Is + 2)*sd**(-29*Id - 76)*sd**(28*Id + 74)*ss**(-29*Is - 18)*ss**(28*Is + 16)*(Id*Is*PsiD**2*PsiS**2*(hD**2*hS**2 - hD**2 - hS**2 + 1) + Id*Is*PsiD*PsiS*hD*hS*psiD*psiS*r1**2*(r**2 + 1) + Id*PsiD*PsiS**2*psiD*r1*(Id*hD**2*hS - Id*hS - Is*hD*hS**2*r + Is*hD*r - hD*hS*r*r1*zSD) + Id*PsiS**2*hD*hS*psiD**2*r*r1**2*(1 - Id) + Is*PsiD**2*PsiS*psiS*r1*(-Id*hD**2*hS*r + Id*hS*r + Is*hD*hS**2 - Is*hD - hD*hS*r*r1*zDS) + Is*PsiD**2*hD*hS*psiS**2*r*r1**2*(1 - Is))/(4*PsiD**4*PsiS**4)/object@lh)
  pvar_svar_s <- colSums(sd**(-29*Id - 18)*sd**(28*Id + 18)*ss**(-29*Is - 76)*ss**(28*Is + 72)*(-2*Id*Is*PsiD**(Id + 3)*PsiS**(Is + 3)*hS**2*psiD*psiS*r*r1**2 + Id*PsiD**(Id + 2)*PsiS**(Is + 4)*hS**2*psiD**2*r1**2*(Id - 1) + Id*PsiD**(Id + 3)*PsiS**(Is + 4)*hS*psiD*r1*(2*Is*hS**2 - 2*Is + hS*r1*zSD - 3) + Is*PsiD**(Id + 4)*PsiS**(Is + 2)*hS**2*psiS**2*r**2*r1**2*(Is - 1) + Is*PsiD**(Id + 4)*PsiS**(Is + 3)*hS*psiS*r*r1*(-2*Is*hS**2 + 2*Is + hS*r*r1*zDS + 3) + Is*PsiD**(Id + 4)*PsiS**(Is + 4)*(Is*hS**4 - 2*Is*hS**2 + Is - 4*hS**2 + 2))/(4*PsiD**4*PsiS**4)/object@lh)

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
    prho <- PsiD**(Id - 1)*PsiS**(Is - 1)*r1*sd**(-Id)*ss**(-Is)*(Id*PsiS*hD*psiD - Id*PsiS*psiD*r*r1*zSD + Is*PsiD*hS*psiS - Is*PsiD*psiS*r*r1*zDS)/object@lh
    pbeta_d1rho <- colSums(-sweep(xd, MARGIN = 1,PsiD**(Id + 2)*PsiS**(Is + 2)* r1*sd**(-29*Id - 47)*sd**(28*Id + 46)*ss**(-29*Is - 18)*ss**(28*Is + 18)*(Id*Is*PsiD*PsiS*psiD*psiS*r1*(-hD + hS*r - r**2*r1*zDS + r*r1*zSD) + Id*PsiD*PsiS**2*psiD*(-Id*hD**2 + Id*hD*r*r1*zSD + hD*r*r1*zSD - r**2*r1**2*zSD**2 + r**2*r1**2 + 1) + Id*PsiS**2*psiD**2*r*r1*(Id*hD - Id*r*r1*zSD - hD + r*r1*zSD) + Is*PsiD**2*PsiS*psiS*(-Id*hD*hS + Id*hD*r*r1*zDS - hS*r1*zDS + r*r1**2*zDS**2 - r*r1**2) + Is*PsiD**2*psiS**2*r1*(-Is*hS + Is*r*r1*zDS + hS - r*r1*zDS))/(PsiD**4*PsiS**4)/object@lh, `*`))
    pbeta_s1rho <- colSums(-sweep(xs, MARGIN = 1,PsiD**(Id + 2)*PsiS**(Is + 2)* r1*sd**(-29*Id - 18)*sd**(28*Id + 18)*ss**(-29*Is - 47)*ss**(28*Is + 46)*(Id*Is*PsiD*PsiS*psiD*psiS*r1*(hD*r - hS - r**2*r1*zSD + r*r1*zDS) + Id*PsiD*PsiS**2*psiD*(-Is*hD*hS + Is*hS*r*r1*zSD - hD*r1*zSD + r*r1**2*zSD**2 - r*r1**2) + Id*PsiS**2*psiD**2*r1*(-Id*hD + Id*r*r1*zSD + hD - r*r1*zSD) + Is*PsiD**2*PsiS*psiS*(-Is*hS**2 + Is*hS*r*r1*zDS + hS*r*r1*zDS - r**2*r1**2*zDS**2 + r**2*r1**2 + 1) + Is*PsiD**2*psiS**2*r*r1*(Is*hS - Is*r*r1*zDS - hS + r*r1*zDS))/(PsiD**4*PsiS**4)/object@lh, `*`))
    pvar_drho <- colSums(-PsiD**(Id + 2)*PsiS**(Is + 2)*r1*sd**(-29*Id - 76)*sd**(28*Id + 74)*ss**(-29*Is - 18)*ss**(28*Is + 18)*(Id*Is*PsiD*PsiS*hD*psiD*psiS*r1*(-hD + hS*r - r**2*r1*zDS + r*r1*zSD) + Id*PsiD*PsiS**2*psiD*(-Id*hD**3 + Id*hD**2*r*r1*zSD + Id*hD - Id*r*r1*zSD + hD**2*r*r1*zSD - hD*r**2*r1**2*zSD**2 + hD*r**2*r1**2 + hD) + Id*PsiS**2*hD*psiD**2*r*r1*(Id*hD - Id*r*r1*zSD - hD + r*r1*zSD) + Is*PsiD**2*PsiS*psiS*(-Id*hD**2*hS + Id*hD**2*r*r1*zDS + Id*hS - Id*r*r1*zDS - hD*hS*r1*zDS + hD*r*r1**2*zDS**2 - hD*r*r1**2) + Is*PsiD**2*hD*psiS**2*r1*(-Is*hS + Is*r*r1*zDS + hS - r*r1*zDS))/(2*PsiD**4*PsiS**4)/object@lh)
    pvar_srho <- colSums(-PsiD**(Id + 2)*PsiS**(Is + 2)*r1*sd**(-29*Id - 18)*sd**(28*Id + 18)*ss**(-29*Is - 76)*ss**(28*Is + 74)*(Id*Is*PsiD*PsiS*hS*psiD*psiS*r1*(hD*r - hS - r**2*r1*zSD + r*r1*zDS) + Id*PsiD*PsiS**2*psiD*(-Is*hD*hS**2 + Is*hD + Is*hS**2*r*r1*zSD - Is*r*r1*zSD - hD*hS*r1*zSD + hS*r*r1**2*zSD**2 - hS*r*r1**2) + Id*PsiS**2*hS*psiD**2*r1*(-Id*hD + Id*r*r1*zSD + hD - r*r1*zSD) + Is*PsiD**2*PsiS*psiS*(-Is*hS**3 + Is*hS**2*r*r1*zDS + Is*hS - Is*r*r1*zDS + hS**2*r*r1*zDS - hS*r**2*r1**2*zDS**2 + hS*r**2*r1**2 + hS) + Is*PsiD**2*hS*psiS**2*r*r1*(Is*hS - Is*r*r1*zDS - hS + r*r1*zDS))/(2*PsiD**4*PsiS**4)/object@lh)
    prhorho <- colSums(PsiD**(Id - 2)*PsiS**(Is - 2)*r1**2*sd**(-25*Id - 15)*sd**(24*Id + 15)*ss**(-25*Is - 15)*ss**(24*Is + 15)*(2*Id*Is*PsiD*PsiS*psiD*psiS*(hD*hS - hD*r*r1*zDS - hS*r*r1*zSD + r**2*r1**2*zDS*zSD) + Id*PsiD*PsiS**2*psiD*(hD**2*zSD - 2*hD*r*r1*zSD**2 + 2*hD*r*r1 + r**2*r1**2*zSD**3 - 3*r**2*r1**2*zSD - zSD) + Id*PsiS**2*psiD**2*(Id*hD**2 - 2*Id*hD*r*r1*zSD + Id*r**2*r1**2*zSD**2 - hD**2 + 2*hD*r*r1*zSD - r**2*r1**2*zSD**2) + Is*PsiD**2*PsiS*psiS*(hS**2*zDS - 2*hS*r*r1*zDS**2 + 2*hS*r*r1 + r**2*r1**2*zDS**3 - 3*r**2*r1**2*zDS - zDS) + Is*PsiD**2*psiS**2*(Is*hS**2 - 2*Is*hS*r*r1*zDS + Is*r**2*r1**2*zDS**2 - hS**2 + 2*hS*r*r1*zDS - r**2*r1**2*zDS**2))/object@lh)

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
