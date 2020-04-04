skipped_tests <- c(
  ""
  # , "2sls"
  # , "fiml"
  # , "basic"
  # , "directional"
  # , "deterministic_adjustment"
  # , "stochastic_adjustment"
)
verbose <- 0

test_calculated_gradient <- function(mdl, params, tolerance) {
  cg <- as.matrix(gradient(mdl, params))
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
    expect(diff < tolerance, sprintf(
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
  nh <- as.matrix(numDeriv::jacobian(function(p) gradient(mdl, p), params, method = "Richardson"))
  ch <- hessian(mdl, params)
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
      expect(diff < tolerance, sprintf(
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


test_marginal_effect <- function(effect, mdl, params, column) {
  testthat::expect(
    !is.na(effect(mdl, params, column)),
    sprintf("Failed to calculate marginal effect of %s", column)
  )
}


test_aggregation <- function(aggregation, mdl, params) {
  testthat::expect(!is.na(aggregation(mdl, params)), sprintf("Failed to calculate aggregate"))
}


test_estimation_accuracy <- function(estimation, parameters, tolerance) {
  errors <- abs(estimation - parameters)

  for (i in seq_len(length(errors))) {
    expect(errors[i] < tolerance, sprintf(
      "Accuracy test failed for %s (estimated = %g, actual = %g) with difference %f.",
      names(errors)[i], estimation[i], parameters[i], errors[i]
    ))
  }

  invisible()
}

## Simulation settings
seed <- 42
verbose <- 0
