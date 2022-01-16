context("Equilibrium Model's Tests\n")

# Estimation setup
parameters <- list(
  nobs = 1000, tobs = 3,
  alpha_d = -0.9, beta_d0 = 14.9, beta_d = c(0.3, -0.2), eta_d = c(-0.03, -0.01),
  alpha_s = 0.9, beta_s0 = 3.2, beta_s = c(0.3), eta_s = c(0.5, 0.02),
  sigma_d = 1.0, sigma_s = 1.0, rho_ds = 0.0
)

# Optimization setup
reltol <- 1e-8
optimization_method <- "BFGS"
optimization_options <- list(REPORT = 10, maxit = 50000, reltol = reltol)

# Tests
mdl <- NULL
test_that(paste0("Model can be simulated"), {
  mdl <<- load_or_simulate_model("equilibrium_model", parameters)
  expect_is(mdl, "equilibrium_model")
})

est <- NULL
test_that(paste0(model_name(mdl), " can be estimated"), {
  est <<- equilibrium_model(
    formula(mdl), simulated_data,
    estimation_options = list(
      control = optimization_options, method = optimization_method
    )
  )
  expect_is(est@fit[[1]], "mle2")
})

test_that(paste0(model_name(mdl), " fit can be summarized"), {
  test_summary(est, 41)
})

test_that(paste0(
  "Maximum likelihood estimates of '", model_name(mdl),
  "' are accurate"
), {
  test_estimation_accuracy(coef(est), unlist(parameters[-c(1, 2)]), 1e-0)
})


test_that(paste0("Aggregation can be calculated"), {
  test_aggregation(aggregate_demand, est)
  test_aggregation(aggregate_supply, est)
})

test_that(paste0("Scores can be calculated"), {
  test_scores(est)
})

reg <- NULL
test_that(paste0("First stage of '", model_name(mdl), "' can be estimated"), {
  reg <<- estimate(mdl, method = "2SLS")
  expect_is(reg@fit[[1]]$first_stage_model, "lm")
})

test_that(paste0("Second stage of '", model_name(mdl), "' can be estimated"), {
  expect_is(reg@fit[[1]]$system_model, "systemfit")
})

test_that(paste0(model_name(mdl), " regressions can be summarized"), {
  test_summary(reg, 75)
})

test_that(paste0(
  "Two-stage least squares estimates of '", model_name(mdl),
  "' are accurate"
), {
  test_estimation_accuracy(coef(reg), unlist(parameters[-c(1, 2)]), 1e-0)
})

test_that(paste0("Optimization of '", model_name(mdl), "' using GSL succeeds"), {
  mll <<- maximize_log_likelihood(mdl,
    start = NULL, step = 1e-5,
    objective_tolerance = 1e-4,
    gradient_tolerance = 1e-3,
    max_it = 1e+3
  )
  testthat::expect_length(mll, 8)
})

test_that(paste0(
  "Calculated gradient of '", model_name(mdl),
  "' matches the numerical approximation"
), {
  test_calculated_gradient(mdl, coef(est), 1e-3)
})
