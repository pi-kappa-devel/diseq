context("Deterministic Adjustment Model's Tests\n")

skip_if("deterministic_adjustment" %in% skipped_tests, message = "Focus on developing parts")

mdl <- simulate_model(
  "diseq_deterministic_adjustment", nobs, tobs,
  alpha_d, beta_d0, beta_d, eta_d,
  alpha_s, beta_s0, beta_s, eta_s,
  gamma, beta_p0, beta_p,
  sigma_d = sigma_d, sigma_s = sigma_s, sigma_p = sigma_p, rho_ds = rho_ds, rho_dp = rho_dp, rho_sp = rho_sp,
  seed = seed, verbose = verbose
)

# Estimation setup
reltol <- 1e-0
optimization_method <- "BFGS"

# Estimate
optimization_controls <- list(REPORT = 10, maxit = 50000, reltol = reltol)
est <- estimate(mdl, control = optimization_controls, method = optimization_method, use_numerical_hessian = TRUE)


test_that(paste0("Calcualted gradient of '", get_model_description(mdl), "' matches the numerical approximation"), {
  test_calculated_gradient(mdl, est@coef, 1e-5)
})

test_that(paste0("Mean marginal effect are calculatable"), {
  test_marginal_effect(get_mean_marginal_effect, mdl, est, "P")
  test_marginal_effect(get_mean_marginal_effect, mdl, est, "Xd1")
  test_marginal_effect(get_mean_marginal_effect, mdl, est, "X2")
})


test_that(paste0("Aggregation is calculatable"), {
  test_aggregation(get_aggregate_demand, mdl, est)
  test_aggregation(get_aggregate_supply, mdl, est)
})
