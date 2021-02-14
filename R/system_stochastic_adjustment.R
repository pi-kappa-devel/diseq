#' @include equation_stochastic_adjustment.R
#' @include system_base.R
setClass(
  "system_stochastic_adjustment",
  contains = "system_base",
  representation(
    price_equation = "equation_base",

    gamma = "numeric",
    delta = "numeric",
    zeta = "numeric",
    zeta_DD = "numeric",
    zeta_DS = "numeric",
    zeta_DP = "numeric",
    zeta_SS = "numeric",
    zeta_SP = "numeric",
    zeta_PP = "numeric",
    omega_D = "matrix",
    omega_S = "matrix",

    mu_P = "matrix",
    var_P = "numeric",
    sigma_P = "numeric",

    mu_D = "matrix",
    var_D = "numeric",
    sigma_D = "numeric",

    mu_S = "matrix",
    var_S = "numeric",
    sigma_S = "numeric",

    cov_DP = "numeric",
    cov_DS = "numeric",
    cov_SP = "numeric",

    rho_DS = "numeric",
    rho_DP = "numeric",
    rho_SP = "numeric",

    h_P = "matrix",
    h_D = "matrix",
    h_S = "matrix",

    z_DP = "matrix",
    z_PD = "matrix",

    z_SP = "matrix",
    z_PS = "matrix",

    g_D = "matrix",
    g_S = "matrix",

    rho_ds = "numeric",
    rho_dp = "numeric",
    rho_sp = "numeric",

    L_D = "matrix",
    L_S = "matrix",

    log_likelihood = "matrix",

    partial_own_price = "matrix",
    partial_own_controls = "matrix",
    partial_other_price = "matrix",
    partial_other_controls = "matrix",
    partial_lagged_price = "matrix",
    partial_price_controls = "matrix",
    partial_own_variance = "matrix",
    partial_other_variance = "matrix",
    partial_price_variance = "matrix",
    partial_own_other_correlation = "matrix",
    partial_own_price_correlation = "matrix",
    partial_other_price_correlation = "matrix",
    gradient = "matrix",

    lagged_price_vector = "matrix"
  ),
  prototype(
    gamma = NA_real_,

    rho_ds = 0,
    rho_dp = 0,
    rho_sp = 0,

    log_likelihood = matrix(NA_real_),
    gradient = matrix(NA_real_),

    lagged_price_vector = matrix(NA_real_)
  )
)

setMethod(
  "initialize", "system_stochastic_adjustment",
  function(
           .Object, quantity, price,
           demand_specification, supply_specification, price_specification,
           data, correlated_shocks) {
    demand_initializer <- function(...) {
      new("equation_stochastic_adjustment", ...)
    }
    supply_initializer <- function(...) {
      new("equation_stochastic_adjustment", ...)
    }

    .Object <- callNextMethod(
      .Object, quantity, price, demand_specification, supply_specification, data, correlated_shocks,
      demand_initializer, supply_initializer
    )
    .Object@price_equation <- new(
      "equation_stochastic_adjustment", quantity, price, price_specification, data,
      "Price Equation", "P_"
    )
    .Object@lagged_price_vector <- as.matrix(data[, get_lagged_price_variable(.Object)])

    .Object
  }
)

setMethod(
    "show_implementation", signature(object = "system_stochastic_adjustment"),
    function(object) {
        callNextMethod(object)
        show_implementation(object@price_equation)
    }
)

setMethod(
  "get_likelihood_variables", signature(object = "system_stochastic_adjustment"),
  function(object) {
    likelihood_variables <- callNextMethod(object)

    len <- length(likelihood_variables)
    pos <- len - ifelse(object@correlated_shocks, 3, 2)
    likelihood_variables <- c(
      likelihood_variables[1:pos],
      get_price_differences_variable(object), get_prefixed_control_variables(object@price_equation),
      likelihood_variables[(pos + 1):len]
    )

    len <- length(likelihood_variables)
    if (object@correlated_shocks) {
      likelihood_variables <- c(
        likelihood_variables[1:(len - 1)],
        get_prefixed_variance_variable(object@price_equation),
        paste0(likelihood_variables[len], c("_DS", "_DP", "_SP"))
      )
    }
    else {
      likelihood_variables <- c(
        likelihood_variables,
        get_prefixed_variance_variable(object@price_equation)
      )
    }

    likelihood_variables
  }
)

setMethod(
  "set_parameters", signature(object = "system_stochastic_adjustment"),
  function(object, parameters) {
    object@demand <- set_parameters(object@demand, parameters)
    object@supply <- set_parameters(object@supply, parameters)
    object@price_equation <- set_parameters(object@price_equation, parameters)
    if (object@correlated_shocks) {
      object@rho_ds <- parameters[paste0(get_correlation_variable(object), "_DS")]
      object@rho_dp <- parameters[paste0(get_correlation_variable(object), "_DP")]
      object@rho_sp <- parameters[paste0(get_correlation_variable(object), "_SP")]
    }
    object@gamma <- parameters[get_price_differences_variable(object)]
    object@delta <- object@gamma + object@supply@alpha - object@demand@alpha

    object <- calculate_system_moments(object)

    object
  }
)

setMethod(
  "calculate_system_moments", signature(object = "system_stochastic_adjustment"),
  function(object) {
    object@mu_P <-
      (
        object@lagged_price_vector * object@gamma +
          object@demand@control_matrix %*% object@demand@beta +
          object@price_equation@control_matrix %*% object@price_equation@beta * object@gamma -
          object@supply@control_matrix %*% object@supply@beta
      ) / object@delta
    object@var_P <-
      (
        object@gamma**2 * object@price_equation@sigma**2 + 2 * object@gamma * (
          object@rho_dp * object@demand@sigma * object@price_equation@sigma - object@rho_sp *
            object@price_equation@sigma * object@supply@sigma
        ) - 2 * object@rho_ds * object@demand@sigma * object@supply@sigma + object@demand@sigma**2 +
          object@supply@sigma**2
      ) / object@delta**2
    object@mu_D <-
      object@demand@control_matrix %*% object@demand@beta + object@demand@alpha * object@mu_P
    object@var_D <-
      (
        +object@demand@alpha**2 * object@delta * object@var_P
          + 2 * object@demand@alpha * object@gamma * object@rho_dp * object@demand@sigma *
            object@price_equation@sigma
          - 2 * object@demand@alpha * object@rho_ds * object@demand@sigma * object@supply@sigma
          + object@demand@sigma**2 * (object@demand@alpha + object@supply@alpha + object@gamma)
      ) / object@delta
    object@mu_S <-
      object@supply@control_matrix %*% object@supply@beta + object@supply@alpha * object@mu_P
    object@var_S <-
      (
        object@supply@alpha**2 * object@delta * object@var_P
          + 2 * object@supply@alpha * object@gamma * object@rho_sp * object@price_equation@sigma *
            object@supply@sigma
          + 2 * object@supply@alpha * object@rho_ds * object@demand@sigma * object@supply@sigma
          + object@supply@sigma**2 * (-object@demand@alpha - object@supply@alpha + object@gamma)
      ) / object@delta

    validate_variance <- function(var) {
      if (!is.na(var) && var < 0) {
        var <- NA_real_
      }
      var
    }

    object@var_P <- validate_variance(object@var_P)
    object@sigma_P <- sqrt(object@var_P)

    object@var_D <- validate_variance(object@var_D)
    object@sigma_D <- sqrt(object@var_D)

    object@var_S <- validate_variance(object@var_S)
    object@sigma_S <- sqrt(object@var_S)

    object@cov_DS <-
      (
        object@demand@alpha * object@supply@alpha * object@delta * object@var_P +
          object@demand@alpha * object@gamma *
            object@rho_sp * object@price_equation@sigma * object@supply@sigma -
          object@demand@alpha *
            object@supply@sigma**2 + object@supply@alpha * object@gamma * object@rho_dp *
            object@demand@sigma *
            object@price_equation@sigma + object@supply@alpha * object@demand@sigma**2 +
          object@gamma *
            object@rho_ds *
            object@demand@sigma * object@supply@sigma
      ) / object@delta

    object@cov_DP <-
      (
        object@demand@alpha * object@delta * object@var_P + object@gamma * object@rho_dp *
          object@demand@sigma *
          object@price_equation@sigma - object@rho_ds * object@demand@sigma * object@supply@sigma +
          object@demand@sigma**2
      ) / object@delta

    object@cov_SP <-
      (
        object@supply@alpha * object@delta * object@var_P + object@gamma * object@rho_sp *
          object@price_equation@sigma *
          object@supply@sigma + object@rho_ds * object@demand@sigma * object@supply@sigma -
          object@supply@sigma**2
      ) / object@delta

    validate_correlation <- function(rho) {
      if (!is.na(rho) && abs(rho) >= 1) {
        rho <- NA_real_
      }
      rho
    }

    object@rho_DS <- object@cov_DS / object@sigma_D / object@sigma_S
    object@rho_DS <- validate_correlation(object@rho_DS)

    object@rho_DP <- object@cov_DP / object@sigma_P / object@sigma_D
    object@rho_DP <- validate_correlation(object@rho_DP)

    object@rho_SP <- object@cov_SP / object@sigma_P / object@sigma_S
    object@rho_SP <- validate_correlation(object@rho_SP)

    zeta_sq <- (
      -object@rho_DP**2 + 2 * object@rho_DP * object@rho_DS * object@rho_SP - object@rho_DS**2 -
        object@rho_SP**2 + 1
    )
    if (!is.na(zeta_sq) && zeta_sq < 0) {
      zeta_sq <- NA_real_
    }
    object@zeta <- sqrt(zeta_sq)

    object@zeta_DD <- 1 - object@rho_SP**2
    object@zeta_DS <- -object@rho_DP * object@rho_SP + object@rho_DS
    object@zeta_DP <- object@rho_DP - object@rho_DS * object@rho_SP
    object@zeta_SS <- 1 - object@rho_DP**2
    object@zeta_SP <- -object@rho_DP * object@rho_DS + object@rho_SP
    object@zeta_PP <- 1 - object@rho_DS**2

    object@h_P <- (object@price_vector - object@mu_P) / object@sigma_P
    object@h_D <- (object@quantity_vector - object@mu_D) / object@sigma_D
    object@h_S <- (object@quantity_vector - object@mu_S) / object@sigma_S

    object@z_DP <- (object@h_D - object@h_P * object@rho_DP) / sqrt(object@zeta_SS)
    object@z_PD <- (-object@h_D * object@rho_DP + object@h_P) / sqrt(object@zeta_SS)
    object@z_SP <- (-object@h_P * object@rho_SP + object@h_S) / sqrt(object@zeta_DD)
    object@z_PS <- (object@h_P - object@h_S * object@rho_SP) / sqrt(object@zeta_DD)

    object@omega_D <- (
      -object@h_D * object@zeta_DS / sqrt(object@zeta_SS) - object@h_P * object@zeta_SP /
        sqrt(object@zeta_SS) +
        object@h_S * sqrt(object@zeta_SS)
    )
    object@omega_S <- (
      object@h_D * sqrt(object@zeta_DD) - object@h_P * object@zeta_DP / sqrt(object@zeta_DD) -
        object@h_S * object@zeta_DS / sqrt(object@zeta_DD)
    )

    object@L_D <- pnorm(object@omega_D / object@zeta, lower.tail = FALSE) * exp(
      (2 * object@h_D * object@h_P * object@rho_DP - object@h_D**2 - object@h_P**2) /
        (2 * object@zeta_SS)
    ) / (2 * pi * object@sigma_D * object@sigma_P * sqrt(object@zeta_SS))

    object@L_S <- pnorm(object@omega_S / object@zeta, lower.tail = FALSE) * exp(
      (2 * object@h_S * object@h_P * object@rho_SP - object@h_S**2 - object@h_P**2) /
        (2 * object@zeta_DD)
    ) / (2 * pi * object@sigma_S * object@sigma_P * sqrt(object@zeta_DD))

    object@g_D <- dnorm(object@omega_D / object@zeta) / pnorm(object@omega_D / object@zeta,
      lower.tail = FALSE
    )
    object@g_S <- dnorm(object@omega_S / object@zeta) / pnorm(object@omega_S / object@zeta,
      lower.tail = FALSE
    )

    names(object@sigma_P) <- c("sigma_P")
    names(object@sigma_D) <- c("sigma_D")
    names(object@sigma_S) <- c("sigma_S")

    names(object@rho_DS) <- c("rho_DS")
    names(object@rho_DP) <- c("rho_DP")
    names(object@rho_SP) <- c("rho_SP")

    object
  }
)

setMethod(
  "calculate_system_loglikelihood", signature(object = "system_stochastic_adjustment"),
  function(object) {
    object@log_likelihood <- log(object@L_D + object@L_S)
    object
  }
)

setMethod(
  "calculate_system_gradient", signature(object = "system_stochastic_adjustment"),
  function(object) {
    object@partial_own_price <- partial_own_price(object)
    object@partial_own_controls <- partial_own_controls(object)
    object@partial_other_price <- partial_other_price(object)
    object@partial_other_controls <- partial_other_controls(object)
    object@partial_lagged_price <- partial_lagged_price(object)
    object@partial_price_controls <- partial_price_controls(object)
    object@partial_own_variance <- partial_own_variance(object)
    object@partial_other_variance <- partial_other_variance(object)
    object@partial_price_variance <- partial_price_variance(object)
    object@partial_own_other_correlation <- partial_own_other_correlation(object)
    object@partial_own_price_correlation <- partial_own_price_correlation(object)
    object@partial_other_price_correlation <- partial_other_price_correlation(object)

    denominator <- c(object@L_D + object@L_S)

    if (!is.null(get_prefixed_price_variable(object@demand))) {
      p_dprice <- sum(object@partial_own_price / denominator)
    }
    else {
      p_dprice <- NULL
    }
    p_dcontrols <- colSums(object@partial_own_controls / denominator)
    if (!is.null(get_prefixed_price_variable(object@supply))) {
      p_sprice <- sum(object@partial_other_price / denominator)
    }
    else {
      p_sprice <- NULL
    }
    p_scontrols <- colSums(object@partial_other_controls / denominator)
    p_pdiff <- sum(object@partial_lagged_price / denominator)
    p_pcontrols <- colSums(object@partial_price_controls / denominator)
    p_vard <- sum(object@partial_own_variance / denominator)
    p_vars <- sum(object@partial_other_variance / denominator)
    p_varp <- sum(object@partial_price_variance / denominator)
    p_corrd <- sum(object@partial_own_other_correlation / denominator)
    p_corrs <- sum(object@partial_own_price_correlation / denominator)
    p_corrp <- sum(object@partial_other_price_correlation / denominator)

    object@gradient <- cbind(
      p_dprice, t(p_dcontrols), p_sprice, t(p_scontrols), p_pdiff, t(p_pcontrols),
      p_vard, p_vars, p_varp
    )
    if (object@correlated_shocks) {
      object@gradient <- cbind(object@gradient, p_corrd, p_corrs, p_corrp)
    }

    object@gradient <- t(object@gradient)
    rownames(object@gradient) <- get_likelihood_variables(object)

    object
  }
)

setMethod(
  "calculate_system_scores", signature(object = "system_stochastic_adjustment"),
  function(object) {
    object@partial_own_price <- partial_own_price(object)
    object@partial_own_controls <- partial_own_controls(object)
    object@partial_other_price <- partial_other_price(object)
    object@partial_other_controls <- partial_other_controls(object)
    object@partial_lagged_price <- partial_lagged_price(object)
    object@partial_price_controls <- partial_price_controls(object)
    object@partial_own_variance <- partial_own_variance(object)
    object@partial_other_variance <- partial_other_variance(object)
    object@partial_price_variance <- partial_price_variance(object)
    object@partial_own_other_correlation <- partial_own_other_correlation(object)
    object@partial_own_price_correlation <- partial_own_price_correlation(object)
    object@partial_other_price_correlation <- partial_other_price_correlation(object)

    denominator <- c(object@L_D + object@L_S)

    if (!is.null(get_prefixed_price_variable(object@demand))) {
      p_dprice <- object@partial_own_price
    }
    else {
      p_dprice <- NULL
    }
    p_dcontrols <- object@partial_own_controls
    if (!is.null(get_prefixed_price_variable(object@supply))) {
      p_sprice <- object@partial_other_price
    }
    else {
      p_sprice <- NULL
    }
    p_scontrols <- object@partial_other_controls
    p_pdiff <- object@partial_lagged_price
    p_pcontrols <- object@partial_price_controls
    p_vard <- object@partial_own_variance
    p_vars <- object@partial_other_variance
    p_varp <- object@partial_price_variance
    p_corrd <- object@partial_own_other_correlation
    p_corrs <- object@partial_own_price_correlation
    p_corrp <- object@partial_other_price_correlation

    scores <- cbind(
      p_dprice, p_dcontrols, p_sprice, p_scontrols, p_pdiff, p_pcontrols,
      p_vard, p_vars, p_varp
    )
    if (object@correlated_shocks) {
      scores <- cbind(scores, p_corrd, p_corrs, p_corrp)
    }

    scores <- scores / denominator
    colnames(scores) <- get_likelihood_variables(object)

    scores
  }
)
