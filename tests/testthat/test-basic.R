context("Basic Model's Tests\n")

# Estimation setup
parameters <- list(
  nobs = 1000, tobs = 3,
  alpha_d = -0.9, beta_d0 = 8.9, beta_d = c(0.03, -0.02), eta_d = c(-0.03, -0.01),
  alpha_s = 0.9, beta_s0 = 4.2, beta_s = c(0.03), eta_s = c(0.05, 0.02),
  sigma_d = 0.9, sigma_s = 1.2, rho_ds = 0.5
)

# Optimization setup
reltol <- 1e-6
optimization_method <- "BFGS"
optimization_control <- list(REPORT = 10, maxit = 50000, reltol = reltol)


# Tests
mdl <- NULL
test_that(paste0("Model can be simulated"), {
  mdl <<- load_or_simulate_model("diseq_basic", parameters)
  expect_is(mdl, "diseq_basic")
})

est <- NULL
test_that(paste0(model_description(mdl), " can be estimated"), {
  est <<- estimate(mdl,
    control = optimization_control, method = optimization_method,
    standard_errors = c("id")
  )
  expect_is(est, "mle2")
})

test_that(paste0("Estimates of '", model_description(mdl), "' are accurate"), {
  test_estimation_accuracy(est@coef, unlist(parameters[-c(1, 2)]), 1e-0)
})

test_that(paste0("Marginal effect can be calculated"), {
  test_marginal_effect(mean_marginal_effect, mdl, est, "P")
  test_marginal_effect(mean_marginal_effect, mdl, est, "Xs1")
  test_marginal_effect(marginal_effect_at_mean, mdl, est, "P")
  test_marginal_effect(marginal_effect_at_mean, mdl, est, "Xs1")
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
  "Calculated gradient of '", model_description(mdl),
  "' matches the numerical approximation"
), {
  test_calculated_gradient(mdl, est@coef, 1e-4)
})

skip_on_cran()

test_that(paste0(
  "Calculated hessian of '", model_description(mdl),
  "' matches the numerical approximation"
), {
  test_calculated_hessian(mdl, est@coef, 1e-4)
})
