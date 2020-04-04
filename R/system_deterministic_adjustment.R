#' @include equation_deterministic_adjustment.R
#' @include system_base.R
setClass(
  "system_deterministic_adjustment",
  contains = "system_base",
  representation(
    gamma = "numeric",
    delta = "numeric",

    mu_P = "matrix",
    var_P = "numeric",
    sigma_P = "numeric",
    h_P = "matrix",

    gr_mu_P = "matrix",
    gr_var_P = "matrix",

    log_likelihood = "matrix",
    gradient = "matrix",

    lagged_price_vector = "matrix"
  ),
  prototype(
    gamma = NA_real_,
    delta = NA_real_,

    log_likelihood = matrix(NA_real_),
    gradient = matrix(NA_real_),

    lagged_price_vector = matrix(NA_real_)
  )
)

setMethod(
  "initialize", "system_deterministic_adjustment",
  function(
           .Object, quantity, price,
           demand_specification, supply_specification, data, correlated_shocks) {
    price_diff <- paste0(price, "_DIFF")

    demand_initializer <- function(...) {
      excess_supply_subset <- data[, price_diff] < 0
      new("equation_deterministic_adjustment", ..., excess_supply_subset)
    }
    supply_initializer <- function(...) {
      excess_demand_subset <- data[, price_diff] >= 0
      new("equation_deterministic_adjustment", ..., excess_demand_subset)
    }

    .Object <- callNextMethod(
      .Object, quantity, price,
      demand_specification, supply_specification, data, correlated_shocks,
      demand_initializer, supply_initializer
    )
    .Object@lagged_price_vector <- as.matrix(data[, get_lagged_price_variable(.Object)])

    .Object
  }
)

setMethod(
  "get_likelihood_variables", signature(object = "system_deterministic_adjustment"),
  function(object) {
    likelihood_variables <- callNextMethod(object)

    len <- length(likelihood_variables)
    pos <- len - ifelse(object@correlated_shocks, 3, 2)
    likelihood_variables <- c(
      likelihood_variables[1:pos],
      get_price_differences_variable(object), likelihood_variables[(pos + 1):len]
    )

    likelihood_variables
  }
)

setMethod(
  "set_parameters", signature(object = "system_deterministic_adjustment"),
  function(object, parameters) {
    object <- callNextMethod(object, parameters)

    object@gamma <- parameters[get_price_differences_variable(object)]
    object@delta <- object@gamma + object@supply@alpha - object@demand@alpha

    object <- calculate_system_moments(object)

    object
  }
)

setMethod(
  "calculate_system_moments", signature(object = "system_deterministic_adjustment"),
  function(object) {
    object@mu_P <- (
      +object@lagged_price_vector * object@gamma
        + object@demand@control_matrix %*% object@demand@beta
        - object@supply@control_matrix %*% object@supply@beta
    ) / (-object@demand@alpha + object@supply@alpha + object@gamma)
    object@var_P <- (
      -2 * object@rho * object@demand@sigma * object@supply@sigma
        + object@demand@sigma**2
        + object@supply@sigma**2
    ) / (-object@demand@alpha + object@supply@alpha + object@gamma)**2

    object@demand@mu_Q <-
      object@demand@control_matrix %*% object@demand@beta + object@demand@alpha * object@mu_P
    object@demand@var_Q <-
      object@demand@alpha**2 * object@var_P + 2 * object@demand@alpha * (
        -object@rho * object@demand@sigma * object@supply@sigma + object@demand@sigma**2
      ) / (-object@demand@alpha + object@supply@alpha + object@gamma) + object@demand@sigma**2

    object@supply@mu_Q <-
      object@supply@control_matrix %*% object@supply@beta + object@supply@alpha * object@mu_P
    object@supply@var_Q <-
      object@supply@alpha**2 * object@var_P - 2 * object@supply@alpha * (
        -object@rho * object@demand@sigma * object@supply@sigma + object@supply@sigma**2
      ) / (-object@demand@alpha + object@supply@alpha + object@gamma) + object@supply@sigma**2

    object@sigma_P <- sqrt(object@var_P)
    object@demand@sigma_Q <- sqrt(object@demand@var_Q)
    object@supply@sigma_Q <- sqrt(object@supply@var_Q)

    object@h_P <-
      (object@price_vector - object@mu_P) / object@sigma_P
    object@demand@h_Q <-
      (object@quantity_vector - object@demand@mu_Q) / object@demand@sigma_Q
    object@supply@h_Q <-
      (object@quantity_vector - object@supply@mu_Q) / object@supply@sigma_Q

    object@demand@cov_PQ <-
      object@demand@alpha * object@var_P + (
        -object@rho * object@demand@sigma * object@supply@sigma + object@demand@sigma**2
      ) / (-object@demand@alpha + object@supply@alpha + object@gamma)
    object@supply@cov_PQ <-
      object@supply@alpha * object@var_P + (
        object@rho * object@demand@sigma * object@supply@sigma - object@supply@sigma**2
      ) / (-object@demand@alpha + object@supply@alpha + object@gamma)

    object@demand@rho_PQ <- object@demand@cov_PQ / object@sigma_P / object@demand@sigma_Q
    if (!is.na(object@demand@rho_PQ) && abs(object@demand@rho_PQ) >= 1) {
      object@demand@rho_PQ <- NA_real_
    }
    object@demand@rho1_PQ <- 1 / sqrt(1 - object@demand@rho_PQ**2)
    object@demand@rho2_PQ <- object@demand@rho_PQ * object@demand@rho1_PQ

    object@supply@rho_PQ <- object@supply@cov_PQ / object@sigma_P / object@supply@sigma_Q
    if (!is.na(object@supply@rho_PQ) && abs(object@supply@rho_PQ) >= 1) {
      object@supply@rho_PQ <- NA_real_
    }
    object@supply@rho1_PQ <- 1 / sqrt(1 - object@supply@rho_PQ**2)
    object@supply@rho2_PQ <- object@supply@rho_PQ * object@supply@rho1_PQ

    object@demand@z_PQ <- (
      object@demand@rho1_PQ * object@h_P - object@demand@rho2_PQ * object@demand@h_Q)
    object@supply@z_PQ <- (
      object@supply@rho1_PQ * object@h_P - object@supply@rho2_PQ * object@supply@h_Q)

    object@demand@z_QP <- (
      object@demand@rho1_PQ * object@demand@h_Q - object@demand@rho2_PQ * object@h_P)
    object@supply@z_QP <- (
      object@supply@rho1_PQ * object@supply@h_Q - object@supply@rho2_PQ * object@h_P)

    names(object@sigma_P) <- c("sigma_P")
    names(object@demand@sigma_Q) <- c("sigma_D")
    names(object@supply@sigma_Q) <- c("sigma_S")

    names(object@demand@rho_PQ) <- c("rho_PD")
    names(object@demand@rho1_PQ) <- c("rho1_PD")
    names(object@demand@rho2_PQ) <- c("rho2_PD")

    names(object@supply@rho_PQ) <- c("rho_PS")
    names(object@supply@rho1_PQ) <- c("rho1_PS")
    names(object@supply@rho2_PQ) <- c("rho2_PS")

    object
  }
)

setMethod(
  "calculate_system_loglikelihood", signature(object = "system_deterministic_adjustment"),
  function(object) {
    lld <- -log(2 * pi) - log(object@sigma_P * object@demand@sigma_Q / object@demand@rho1_PQ) - (
      object@demand@rho1_PQ**2 * (
        object@h_P**2 - 2 * object@h_P * object@demand@h_Q * object@demand@rho_PQ +
          object@demand@h_Q**2
      )
    ) / 2

    lls <- -log(2 * pi) - log(object@sigma_P * object@supply@sigma_Q / object@supply@rho1_PQ) - (
      object@supply@rho1_PQ**2 * (
        object@h_P**2 - 2 * object@h_P * object@supply@h_Q * object@supply@rho_PQ +
          object@supply@h_Q**2
      )
    ) / 2

    lld[!object@demand@separation_subset] <- 0
    lls[!object@supply@separation_subset] <- 0

    object@log_likelihood <- lld + lls

    object
  }
)

setMethod(
  "calculate_system_gradient", signature(object = "system_deterministic_adjustment"),
  function(object) {
    pd_alpha_d <- partial_alpha_d_of_loglh_D(object)
    pd_beta_d <- partial_beta_d_of_loglh_D(object)
    pd_alpha_s <- partial_alpha_s_of_loglh_D(object)
    pd_beta_s <- partial_beta_s_of_loglh_D(object)
    pd_gamma <- partial_gamma_of_loglh_D(object)
    pd_sigma_d <- partial_sigma_d_of_loglh_D(object)
    pd_sigma_s <- partial_sigma_s_of_loglh_D(object)
    pd_rho <- partial_rho_of_loglh_D(object)
    ps_alpha_d <- partial_alpha_d_of_loglh_S(object)
    ps_beta_d <- partial_beta_d_of_loglh_S(object)
    ps_alpha_s <- partial_alpha_s_of_loglh_S(object)
    ps_beta_s <- partial_beta_s_of_loglh_S(object)
    ps_gamma <- partial_gamma_of_loglh_S(object)
    ps_sigma_d <- partial_sigma_d_of_loglh_S(object)
    ps_sigma_s <- partial_sigma_s_of_loglh_S(object)
    ps_rho <- partial_rho_of_loglh_S(object)

    object@gradient <- t(colSums(
      pd_beta_d * c(object@demand@separation_subset)
    ) + colSums(
      ps_beta_d * c(object@supply@separation_subset)
    ))
    if (!is.null(get_prefixed_price_variable(object@demand))) {
      object@gradient <- cbind(
        sum(pd_alpha_d * object@demand@separation_subset) +
          sum(ps_alpha_d * object@supply@separation_subset),
        object@gradient
      )
    }
    if (!is.null(get_prefixed_price_variable(object@supply))) {
      object@gradient <- cbind(
        object@gradient,
        sum(pd_alpha_s * object@demand@separation_subset) +
          sum(ps_alpha_s * object@supply@separation_subset)
      )
    }
    object@gradient <- cbind(
      object@gradient,
      t(colSums(
        pd_beta_s * c(object@demand@separation_subset)
      ) + colSums(
        ps_beta_s * c(object@supply@separation_subset)
      )),
      sum(pd_gamma * object@demand@separation_subset) +
        sum(ps_gamma * object@supply@separation_subset),
      sum(pd_sigma_d * object@demand@separation_subset) +
        sum(ps_sigma_d * object@supply@separation_subset),
      sum(pd_sigma_s * object@demand@separation_subset) +
        sum(ps_sigma_s * object@supply@separation_subset)
    )
    if (object@correlated_shocks) {
      object@gradient <- cbind(
        object@gradient,
        sum(pd_rho * object@demand@separation_subset) +
          sum(ps_rho * object@supply@separation_subset)
      )
    }
    object@gradient <- t(object@gradient)
    rownames(object@gradient) <- get_likelihood_variables(object)

    object
  }
)
