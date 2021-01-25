context("Deterministic Adjustment Model's Tests\n")

parameters <- list(
    nobs = 2000, tobs = 3,
    alpha_d = -0.9, beta_d0 = 8.9, beta_d = c(0.03, -0.02), eta_d = c(-0.03, -0.01),
    alpha_s = 0.9, beta_s0 = 4.2, beta_s = c(0.03), eta_s = c(0.05, 0.02),
    gamma = 1.4,
    sigma_d = 0.9, sigma_s = 1.2, rho_ds = 0.5
)

mdl <- load_or_simulate_model("diseq_deterministic_adjustment", parameters)

# Estimation setup
reltol <- 1e-4
optimization_method <- "BFGS"

# Estimate
optimization_controls <- list(REPORT = 10, maxit = 50000, reltol = reltol)
est <- estimate(mdl,
    control = optimization_controls,
    method = optimization_method, use_numerical_hessian = TRUE
)

# Test
test_that(paste0("Estimates of '", get_model_description(mdl), "' are accurate"), {
    test_estimation_accuracy(est@coef, unlist(parameters[-c(1, 2)]), 1e-0)
})

test_that(paste0("Mean marginal effect can be calculated"), {
    test_marginal_effect(get_mean_marginal_effect, mdl, est, "P")
    test_marginal_effect(get_mean_marginal_effect, mdl, est, "Xd1")
    test_marginal_effect(get_mean_marginal_effect, mdl, est, "X2")
})

test_that(paste0("Aggregation can be calculated"), {
    test_aggregation(get_aggregate_demand, mdl, est@coef)
    test_aggregation(get_aggregate_supply, mdl, est@coef)
})

test_that(paste0("Scores can be calculated"), {
    test_scores(mdl, est@coef)
})

test_that(paste0(
    "Calculated gradient of '",
    get_model_description(mdl), "' matches the numerical approximation"
), {
    test_calculated_gradient(mdl, est@coef, 1e-5)
})
