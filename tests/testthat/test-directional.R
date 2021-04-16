context("Directional Model's Tests\n")

skip_on_cran()

# Estimation setup
parameters <- list(
  nobs = 3000, tobs = 4,
  alpha_d = -0.2, beta_d0 = 4.3, beta_d = c(0.03, 0.02), eta_d = c(0.03, 0.01),
  alpha_s = 0.0, beta_s0 = 4.0, beta_s = c(0.03), eta_s = c(0.05, 0.02),
  sigma_d = 1.0, sigma_s = 1.0, rho_ds = 0.0
)

# Optimization setup
reltol <- 1e-4
optimization_method <- "BFGS"
optimization_control <- list(REPORT = 10, maxit = 50000, reltol = reltol)

# Tests
mdl <- NULL
test_that(paste0("Model can be simulated"), {
  mdl <<- load_or_simulate_model("diseq_directional", parameters)
  expect_is(mdl, "diseq_directional")
})

est <- NULL
test_that(paste0(model_name(mdl), " can be estimated"), {
  est <<- estimate(mdl, control = optimization_control, method = optimization_method)
  expect_is(est, "mle2")
})

test_that(paste0("Estimates of '", model_name(mdl), "' are accurate"), {
  test_estimation_accuracy(est@coef, unlist(parameters[-c(1, 2, 7)]), 1e-0)
})

test_that(paste0("Mean marginal effect can be calculated"), {
  test_marginal_effect(shortage_probability_marginal, mdl, est, "P", "mean")
  test_marginal_effect(shortage_probability_marginal, mdl, est, "Xd1", "mean")
  test_marginal_effect(shortage_probability_marginal, mdl, est, "X2", "mean")
  test_marginal_effect(shortage_probability_marginal, mdl, est, "P", "at_the_mean")
  test_marginal_effect(shortage_probability_marginal, mdl, est, "Xs1", "at_the_mean")
  test_marginal_effect(shortage_probability_marginal, mdl, est, "X2", "at_the_mean")
})

test_that(paste0("Aggregation can be calculated"), {
  test_aggregation(aggregate_demand, mdl, est@coef)
  test_aggregation(aggregate_supply, mdl, est@coef)
})

test_that(paste0("Shortages can be calculated"), {
  test_shortages(relative_shortages, mdl, est@coef)
  test_shortages(shortage_probabilities, mdl, est@coef)
})

test_that(paste0("Scores can be calculated"), {
  test_scores(mdl, est@coef)
})

test_that(paste0(
  "Calculated gradient of '",
  model_name(mdl), "' matches the numerical approximation"
), {
  test_calculated_gradient(mdl, est@coef, 1e-5)
})

test_that(paste0(
  "Calculated hessian of '",
  model_name(mdl), "' matches the numerical approximation"
), {
  test_calculated_hessian(mdl, est@coef, 1e-3)
})
