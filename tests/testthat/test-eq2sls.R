context("Equilibrium 2SLS Model's Tests\n")

skip_if("2sls" %in% skipped_tests, message = "Focus on developing parts")

mdl <- simulate_model(
  "eq_2sls", nobs, tobs,
  alpha_d, beta_d0, beta_d, eta_d,
  alpha_s, beta_s0, beta_s, eta_s,
  gamma, beta_p0, beta_p,
  sigma_d = sigma_d, sigma_s = sigma_s, sigma_p = sigma_p, rho_ds = rho_ds, rho_dp = rho_dp, rho_sp = rho_sp,
  seed = seed, verbose = verbose
)

# Estimate
est <- estimate(mdl)
