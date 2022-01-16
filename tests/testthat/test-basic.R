context("Basic Model's Tests\n")

# Estimation setup
parameters <- list(
  nobs = 1000, tobs = 3,
  alpha_d = -0.9, beta_d0 = 8.9, beta_d = c(0.3, -0.2), eta_d = c(-0.3, -0.1),
  alpha_s = 0.9, beta_s0 = 8.2, beta_s = c(0.3), eta_s = c(0.5, 0.2),
  sigma_d = 0.9, sigma_s = 1.2, rho_ds = 0.2
)

# Optimization setup
reltol <- 1e-4
optimization_method <- "BFGS"
optimization_options <- list(REPORT = 10, maxit = 50000, reltol = reltol)


# Tests
mdl <- NULL
test_that(paste0("Model can be simulated"), {
  mdl <<- load_or_simulate_model("diseq_basic", parameters)
  expect_is(mdl, "diseq_basic")
})

est <- NULL
test_that(paste0(model_name(mdl), " can be estimated"), {
  est <<- diseq_basic(
    formula(mdl), simulated_data,
    estimation_options = list(
      control = optimization_options, method = optimization_method,
      standard_errors = c("id")
    )
  )
  expect_is(est@fit[[1]], "mle2")
})

test_that(paste0(model_name(mdl), " fit can be summarized"), {
  test_summary(est, 41)
})

test_that(paste0("Estimates of '", model_name(mdl), "' are accurate"), {
  test_estimation_accuracy(coef(est), unlist(parameters[-c(1, 2)]), 1e-0)
})

test_that(paste0("Marginal effects can be calculated"), {
  test_marginal_effect(shortage_probability_marginal, est, "P", "mean")
  test_marginal_effect(shortage_probability_marginal, est, "Xs1", "mean")
  test_marginal_effect(shortage_probability_marginal, est, "P", "at_the_mean")
  test_marginal_effect(shortage_probability_marginal, est, "Xs1", "at_the_mean")
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

test_that(paste0("Coefficients can be accessed"), {
  test_coef(est)
})

test_that(paste0("Variance-covariance matrix can be accessed"), {
  test_vcov(est)
})

test_that(paste0("Log=likelihood object can be accessed"), {
  test_logLik(est)
})

test_that(paste0(
  "Calculated gradient of '", model_name(mdl),
  "' matches the numerical approximation"
), {
  test_calculated_gradient(mdl, coef(est), 1e-4)
})

skip_on_cran()

test_that(paste0(
  "Calculated hessian of '", model_name(mdl),
  "' matches the numerical approximation"
), {
  test_calculated_hessian(mdl, coef(est), 1e-4)
})
