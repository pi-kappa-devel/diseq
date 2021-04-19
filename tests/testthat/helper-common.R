test_calculated_gradient <- function(mdl, params, tolerance) {
  cg <- as.matrix(diseq:::gradient(mdl, params))
  ng <- as.matrix(numDeriv::grad(
    function(p) minus_log_likelihood(mdl, p), params,
    method = "Richardson"
  ))
  rownames(ng) <- rownames(cg)

  pnames <- rownames(cg)
  max_diff <- 0

  act <- testthat::quasi_label(rlang::enquo(ng))

  for (row in seq_len(length(act$val))) {
    scale <- max(abs(ng[row]), abs(cg[row]))
    ad <- abs(act$val[row] - cg[row])
    rd <- ifelse(scale, ad / scale, 0)
    diff <- min(ad, rd)
    max_diff <- ifelse(max_diff < diff, diff, max_diff)
    testthat::expect(diff < tolerance, sprintf(
      "%s (numerical = %g, calculated = %g) failed with differences (%f, %f).",
      pnames[row], ng[row], cg[row], ad, rd
    ))
  }

  if (max_diff > tolerance) {
    cat("\nMaximum gradient difference: ", max_diff, "\n")
  }

  invisible(act$val)
}

test_convergence <- function(est) {
  testthat::expect(est@details$convergence == 0, sprintf("Failed to converge"))
}

test_calculated_hessian <- function(mdl, params, tolerance) {
  nh <- as.matrix(numDeriv::jacobian(function(p) diseq:::gradient(mdl, p),
    params,
    method = "Richardson"
  ))
  ch <- diseq:::hessian(mdl, params)
  pnames <- rownames(ch)
  max_diff <- 0

  act <- testthat::quasi_label(rlang::enquo(nh))

  for (row in seq_len(nrow(nh))) {
    for (col in seq_len(ncol(nh))) {
      scale <- max(abs(nh[row, col]), abs(ch[row, col]))
      ad <- abs(nh[row, col] - ch[row, col])
      rd <- ifelse(scale, ad / scale, 0)
      diff <- min(ad, rd)
      max_diff <- ifelse(max_diff < diff, diff, max_diff)
      testthat::expect(diff < tolerance, sprintf(
        "[%s, %s] (numerical = %g, calculated = %g) failed with differences (%f, %f).",
        pnames[row], pnames[col], nh[row, col], ch[row, col], ad, rd
      ))
    }
  }

  if (max_diff > tolerance) {
    cat("\nMaximum hessian difference: ", max_diff, "\n")
  }

  invisible(act$val)
}


test_marginal_effect <- function(effect, mdl, est, column, aggregate) {
  testthat::expect(
    !is.na(effect(mdl, est@coef, column, aggregate)),
    sprintf("Failed to calculate marginal effect of %s", column)
  )
}


test_aggregation <- function(aggregation, mdl, params) {
  result <- aggregation(mdl, params)
  testthat::expect(
    (is.null(dim(result)) && !is.na(result)) ||
      (!is.null(dim(result)) && !all(is.na(result[[2]]))),
    sprintf("Failed to calculate aggregate")
  )
}


test_shortages <- function(shortage_function, mdl, params) {
  testthat::expect(
    !any(is.na(shortage_function(mdl, params))),
    sprintf("Failed to calculate shortages")
  )
}


test_scores <- function(mdl, params) {
  scores <- scores(mdl, params)
  n <- diseq::number_of_observations(mdl)
  k <- length(diseq:::likelihood_variables(mdl@system))
  testthat::expect(any(dim(scores) == c(n, k)), sprintf("Score has wrong dimensions"))
  testthat::expect(
    !any(is.na(scores(mdl, params))),
    sprintf("Failed to calculate scores")
  )
}


test_estimation_accuracy <- function(estimation, parameters, tolerance) {
  errors <- abs(estimation - parameters)

  for (i in seq_len(length(errors))) {
    testthat::expect(errors[i] < tolerance, sprintf(
      "Accuracy test failed for %s (estimated = %g, actual = %g) with difference %f.",
      names(errors)[i], estimation[i], parameters[i], errors[i]
    ))
  }

  invisible()
}

## Simulation settings
seed <- 42
verbose <- 0

load_or_simulate_model <- function(model_string, parameters) {
  stored_data_filename <- paste0(model_string, "_", seed, ".rda")

  if (file.exists("devel-environment") & file.exists(stored_data_filename)) {
    load(stored_data_filename)
  }
  else {
    model_tibble <- do.call(simulate_data, c(model_string, parameters, seed = seed))
    if (file.exists("devel-environment")) {
      save(model_tibble, file = stored_data_filename)
    }
  }

  if (model_string %in% c("equilibrium_model", "diseq_basic")) {
    mdl <- new(
      model_string,
      c("id", "date"),
      "Q", "P", "P + Xd1 + Xd2 + X1 + X2", "P + Xs1 + X1 + X2",
      model_tibble,
      correlated_shocks = TRUE, verbose = verbose
    )
  }
  else if (model_string %in% c("diseq_directional")) {
    mdl <- new(
      model_string,
      c("id", "date"), "date",
      "Q", "P", "P + Xd1 + Xd2 + X1 + X2", "Xs1 + X1 + X2",
      model_tibble,
      correlated_shocks = TRUE, verbose = verbose
    )
  }
  else if (model_string %in% c("diseq_deterministic_adjustment")) {
    mdl <- new(
      model_string,
      c("id", "date"), "date",
      "Q", "P", "P + Xd1 + Xd2 + X1 + X2", "P + Xs1 + X1 + X2",
      model_tibble,
      correlated_shocks = TRUE, verbose = verbose
    )
  }
  else if (model_string %in% c("diseq_stochastic_adjustment")) {
    mdl <- new(
      model_string,
      c("id", "date"), "date",
      "Q", "P", "P + Xd1 + Xd2 + X1 + X2", "P + Xs1 + X1 + X2", "Xp1",
      model_tibble,
      correlated_shocks = TRUE, verbose = verbose
    )
  }
  else {
    stop("Unhandled model type.")
  }

  mdl
}
