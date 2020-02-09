context("Directional Model's Tests\n")

skip_if("directional" %in% skipped_tests, message = "Focus on developing parts")

parameters <- list(
  nobs = 3000, tobs = 4,
  alpha_d = -0.2, beta_d0 = 4.3, beta_d = c(0.03, 0.02), eta_d = c(0.03, 0.01),
  beta_s0 = 4.0, beta_s = c(0.03), eta_s  = c(0.05, 0.02),
  sigma_d  = 1.0, sigma_s  = 1.0, rho_ds  = 0.0
)

mdl <- simulate_directional_model(parameters, seed = seed, verbose = verbose)

# Estimation setup
reltol <- 1e-4
optimization_method <- "BFGS"

# Estimate
optimization_controls <- list(REPORT = 10, maxit = 50000, reltol = reltol)
est <- estimate(mdl, control = optimization_controls, method = optimization_method)

test_that(paste0("Estimates of '", get_model_description(mdl), "' are accurate"), {
  test_estimation_accuracy(est@coef, unlist(parameters[-c(1,2)]), 1e-0)
})

test_that(paste0("Calcualted gradient of '", get_model_description(mdl), "' matches the numerical approximation"), {
  test_calculated_gradient(mdl, est@coef, 1e-5)
})

test_that(paste0("Calcualted hessian of '", get_model_description(mdl), "' matches the numerical approximation"), {
  test_calculated_hessian(mdl, est@coef, 1e-3)
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
