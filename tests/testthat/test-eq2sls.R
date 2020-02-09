context("Equilibrium 2SLS Model's Tests\n")

skip_if("2sls" %in% skipped_tests, message = "Focus on developing parts")

parameters <- list(
  nobs = 1000, tobs = 9,
  beta_d0 = 14.9, alpha_d = -0.9, beta_d = c(0.3, -0.2), eta_d = c(-0.03, -0.01),
  beta_s0 = 3.2, alpha_s = 0.9, beta_s = c(0.3), eta_s  = c(0.5, 0.02)
)

mdl <- simulate_eq_2sls_model(parameters, seed = seed, verbose = verbose)

# Estimate
mdl <- estimate(mdl)

test_that(paste0("Estimates of '", get_model_description(mdl), "' are accurate"), {
  test_estimation_accuracy(mdl@system_model$coefficients, unlist(parameters[-c(1,2)]), 1e-0)
})

test_that(paste0("First stage of '", get_model_description(mdl), "' can be estimated"), {
  expect_is(mdl@first_stage_model, "lm")
})

test_that(paste0("Second stage of '", get_model_description(mdl), "' can be estimated"), {
  expect_is(mdl@system_model, "systemfit")
})
