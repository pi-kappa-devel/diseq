context("Stochastic Adjustment Model's Tests\n")

skip_on_cran()

# Estimation setup
parameters <- list(
    nobs = 2000, tobs = 4,
    alpha_d = -0.1, beta_d0 = 9.8, beta_d = c(0.3, -0.02), eta_d = c(0.6, -0.1),
    alpha_s = 0.1, beta_s0 = 6.1, beta_s = c(0.9), eta_s = c(-0.5, 0.2),
    gamma = 1.2, beta_p0 = 3.1, beta_p = c(0.8),
    sigma_d = 1.0, sigma_s = 1.0, sigma_p = 1.0, rho_ds = 0.0, rho_dp = 0.0, rho_sp = 0.0
)

# Optimization setup
reltol <- 1e-4
optimization_method <- "BFGS"
optimization_controls <- list(REPORT = 10, maxit = 50000, reltol = reltol)

# Tests
mdl <- NULL
test_that(paste0("Model can be simulated"), {
    mdl <<- load_or_simulate_model("diseq_stochastic_adjustment", parameters)
    expect_is(mdl, "diseq_stochastic_adjustment")
})

est <- NULL
test_that(paste0(get_model_description(mdl), " can be estimated"), {
    est <<- estimate(mdl, control = optimization_controls, method = optimization_method)
    expect_is(est, "mle2")
})

test_that(paste0("Estimates of '", get_model_description(mdl), "' are accurate"), {
    test_estimation_accuracy(est@coef, unlist(parameters[-c(1, 2)]), 1e-0)
})

test_that(paste0(get_model_description(mdl), "' converges"), {
    test_convergence(est)
})

test_that(paste0("Mean marginal effect can be calculated"), {
    test_marginal_effect(get_mean_marginal_effect, mdl, est, "P")
    test_marginal_effect(get_mean_marginal_effect, mdl, est, "Xd1")
    test_marginal_effect(get_mean_marginal_effect, mdl, est, "X2")
    test_marginal_effect(get_marginal_effect_at_mean, mdl, est, "P")
    test_marginal_effect(get_marginal_effect_at_mean, mdl, est, "Xs1")
    test_marginal_effect(get_marginal_effect_at_mean, mdl, est, "X2")
})

test_that(paste0("Aggregation can be calculated"), {
    test_aggregation(get_aggregate_demand, mdl, est@coef)
    test_aggregation(get_aggregate_supply, mdl, est@coef)
})

test_that(paste0("Shortages can be calculated"), {
    test_shortages(get_relative_shortages, mdl, est@coef)
    test_shortages(get_shortage_probabilities, mdl, est@coef)
})

test_that(paste0("Scores can be calculated"), {
    test_scores(mdl, est@coef)
})

test_that(paste0(
    "Calculated gradient of '",
    get_model_description(mdl), "' matches the numerical approximation"
), {
    test_calculated_gradient(mdl, est@coef, 1e-04)
})
