context("Stochastic Adjustment Model's Tests\n")

skip_on_cran()

# Estimation setup
parameters <- list(
  nobs = 2000, tobs = 4,
  alpha_d = -0.3, beta_d0 = 8.5, beta_d = c(0.3, -0.02), eta_d = c(0.6, -0.1),
  alpha_s = 0.1, beta_s0 = 7.3, beta_s = c(0.3), eta_s = c(-0.5, 0.2),
  gamma = 0.8, beta_p0 = 1.3, beta_p = c(0.3),
  sigma_d = 1.0, sigma_s = 1.0, sigma_p = 1.0, rho_ds = 0.0, rho_dp = 0.0, rho_sp = 0.0
)

# Optimization setup
reltol <- 1e-6
optimization_method <- "BFGS"
optimization_options <- list(REPORT = 10, maxit = 50000, reltol = reltol)

# Tests
mdl <- NULL
test_that(paste0("Model can be simulated"), {
  mdl <<- load_or_simulate_model("diseq_stochastic_adjustment", parameters)
  expect_is(mdl, "diseq_stochastic_adjustment")
})

est <- NULL
test_that(paste0(model_name(mdl), " can be estimated"), {
  est <<- diseq_stochastic_adjustment(
    formula(mdl), simulated_data,
    estimation_options = list(
      control = optimization_options, method = optimization_method
    )
  )
  expect_is(est@fit[[1]], "mle2")
})

test_that(paste0(model_name(mdl), " fit can be summarized"), {
  test_summary(est, 50)
})

test_that(paste0("Estimates of '", model_name(mdl), "' are accurate"), {
  test_estimation_accuracy(coef(est), unlist(parameters[-c(1, 2)]), 1e-0)
})

test_that(paste0(model_name(mdl), "' converges"), {
  test_convergence(est)
})

test_that(paste0("Mean marginal effect can be calculated"), {
  test_marginal_effect(shortage_probability_marginal, est, "P", "mean")
  test_marginal_effect(shortage_probability_marginal, est, "Xd1", "mean")
  test_marginal_effect(shortage_probability_marginal, est, "X2", "mean")
  test_marginal_effect(shortage_probability_marginal, est, "P", "at_the_mean")
  test_marginal_effect(shortage_probability_marginal, est, "Xs1", "at_the_mean")
  test_marginal_effect(shortage_probability_marginal, est, "X2", "at_the_mean")
})

test_that(paste0("Aggregation can be calculated"), {
  test_aggregation(aggregate_demand, est)
  test_aggregation(aggregate_supply, est)
})

test_that(paste0("Shortages can be calculated"), {
  test_shortages(relative_shortages, est)
  test_shortages(shortage_probabilities, est)
})

test_that(paste0("Scores can be calculated"), {
  test_scores(est)
})

test_that(paste0(
  "Calculated gradient of '",
  model_name(mdl), "' matches the numerical approximation"
), {
  test_calculated_gradient(mdl, coef(est), 1e-04)
})
