#' @include model_logger.R
#' @include system_base.R
#' @importFrom bbmle parnames mle2
#' @importFrom stats formula lm model.matrix na.omit
#' @importFrom rlang :=
#' @import dplyr magrittr tibble

setClassUnion("characterOrNULL", c("character", "NULL"))
setOldClass(c("spec_tbl_df", "tbl_df", "tbl", "data.frame"))

#' Base S4 model class.
#'
#' @slot logger Logger object.
#' @slot key_columns Vector of column names that uniquely identify data records. For panel data this vector should
#' contain an entity and a time identifier.
#' @slot time_column Column name for the time-point data.
#' @slot explanatory_columns Vector of explanatory column names for all model's equations.
#' @slot data_columns Vector of model's data column names. This is the union of the quantity, price and
#' explanatory columns.
#' @slot columns Vector of primary key and data column names for all model's equations.
#' @slot model_tibble Model data tibble.
#' @slot model_type_string Model type string description.
#' @slot system Model system of equations.
setClass(
  "model_base",
  representation(
    ## Logging
    logger = "model_logger",

    ## Column fields
    key_columns = "vector",
    time_column = "characterOrNULL",
    explanatory_columns = "vector",
    data_columns = "vector",
    columns = "vector",

    ## Model data
    model_tibble = "tbl_df",
    model_type_string = "character",
    system = "system_base"

  )
)

setMethod("initialize", "model_base", function(
  .Object,
  model_type_string, verbose,
  key_columns, time_column,
  quantity_column, price_column, demand_specification, supply_specification, price_specification,
  use_correlated_shocks,
  data,
  system_initializer
) {

  ## Model assignments
  .Object@model_type_string <- model_type_string
  .Object@logger <- new("model_logger", verbose)
  .Object@system@correlated_shocks <- use_correlated_shocks
  print_info(.Object@logger, "This is '", get_model_description(.Object), "' model")


  .Object@key_columns <- key_columns
  .Object@time_column <- time_column

  .Object@explanatory_columns <- unique(c(
    all.vars(formula(paste0(quantity_column, " ~ ", demand_specification))),
    all.vars(formula(paste0(quantity_column, " ~ ", supply_specification)))
  ))
  if (.Object@model_type_string %in% c("Stochastic Adjustment")) {
    .Object@explanatory_columns <- unique(c(
      .Object@explanatory_columns, all.vars(formula(paste0(price_column, " ~ ", price_specification)))
    ))
  }
  .Object@data_columns <- unique(c(quantity_column, price_column, .Object@explanatory_columns))
  .Object@columns <- unique(c(.Object@key_columns, .Object@data_columns))

  ## Data assignment
  .Object@model_tibble <- data

  ## Create model tibble
  len <- nrow(.Object@model_tibble)
  .Object@model_tibble <- .Object@model_tibble %>%
    dplyr::select(!!!.Object@columns) %>%
    na.omit()
  drops <- len - nrow(.Object@model_tibble)
  if (drops) {
    print_warning(.Object@logger, "Dropping ", drops, " rows due to omitted values.")
  }

  .Object@model_tibble <- .Object@model_tibble %>%
    dplyr::mutate_if(is.factor, function(x) {
      initial_levels <- levels(x)
      x <- factor(x)
      remaining_levels <- levels(x)
      removed_levels <- initial_levels[!(initial_levels %in% remaining_levels)]
      if (length(removed_levels)) {
        print_warning(.Object@logger, "Removing unobserved '", paste0(removed_levels, collapse = ", "), "' level(s).")
      }
      x
    })

  ## Create primary key column
  key_columns_syms <- rlang::syms(.Object@key_columns)
  .Object@model_tibble <- .Object@model_tibble %>% dplyr::mutate(pk = as.integer(paste0(!!!key_columns_syms)))

  ## Do we need to use lags?
  if (.Object@model_type_string %in% c(
    "Directional", "Deterministic Adjustment", "Stochastic Adjustment"
  )) {
    ## Generate lags
    key_syms <- rlang::syms(.Object@key_columns[.Object@key_columns != .Object@time_column])
    price_sym <- rlang::sym(price_column)
    time_sym <- rlang::sym(.Object@time_column)
    lagged_price_column <- paste0("LAGGED_", price_column)
    lagged_price_sym <- rlang::sym(lagged_price_column)

    .Object@model_tibble <- .Object@model_tibble %>%
      dplyr::group_by(!!!key_syms) %>%
      dplyr::mutate(!!lagged_price_sym := dplyr::lag(!!price_sym, order_by = !!time_sym)) %>%
      dplyr::ungroup()

    drop_rows <- .Object@model_tibble %>%
      dplyr::select(!!lagged_price_sym) %>%
      is.na()
    .Object@model_tibble <- .Object@model_tibble[!drop_rows, ]
    print_info(.Object@logger, "Dropping ", sum(drop_rows), " rows by generating '", lagged_price_column, "'.")

    ## Do we need to use first differences?
    if (.Object@model_type_string %in% c("Directional", "Deterministic Adjustment")) {
      ## Generate first differences
      diff_column <- paste0(price_column, "_DIFF")
      diff_sym <- rlang::sym(diff_column)

      .Object@model_tibble <- .Object@model_tibble %>%
        dplyr::group_by(!!!key_syms) %>%
        dplyr::mutate(!!diff_sym := !!price_sym - !!lagged_price_sym) %>%
        dplyr::ungroup()
    }
  }

  if (.Object@model_type_string %in% c("Stochastic Adjustment")) {
    .Object@system <- system_initializer(
      quantity_column, price_column, demand_specification, supply_specification, price_specification,
      .Object@model_tibble, use_correlated_shocks
    )
  }
  else {
    .Object@system <- system_initializer(
      quantity_column, price_column, demand_specification, supply_specification,
      .Object@model_tibble, use_correlated_shocks
    )
  }

  print_verbose(.Object@logger, "Using columns ", paste0(.Object@columns, collapse = ", "), ".")

  .Object
})

#' Minus log-likelihood.
#'
#' Returns the opposite of the log-likelihood.
#' @param object A model object.
#' @param parameters A vector of parameters at which the function is to be evaluated.
#' @rdname minus_log_likelihood
#' @export
setGeneric("minus_log_likelihood", function(object, parameters) {
  standardGeneric("minus_log_likelihood")
})

#' Model estimation.
#'
#' With the exception of \code{\link{eq_2sls-class}} all model are estimated be maximum likelihood. The likelihood is
#' optimized using \code{\link[bbmle]{mle2}}. If no starting values are
#' provided, the function uses linear regression estimates as initializing values. The default optimization method is
#' BFGS. For other alternatives see \code{\link[bbmle]{mle2}}.
#'
#' The \code{\link{eq_2sls-class}} is estimated using linear least squares. The implementation is based on
#' \code{\link[systemfit]{systemfit}}.
#'
#' @param object A model object.
#' @param ... Named parameter used in the model's estimation. These are passed further down to the estimation call.
#' @rdname estimate
#' @export
setGeneric("estimate", function(object, ...) {
  standardGeneric("estimate")
})

#' Model description.
#'
#' Returns a unique string representation of the model.
#' @param object A model object.
#' @rdname get_model_description
#' @export
setGeneric("get_model_description", function(object) {
  standardGeneric("get_model_description")
})

#' Number of observations.
#'
#' Returns the number of observations that are used by an initialized model. The number of used observations may differ
#' from the numbers of observations of the data set that was passed to the model's initialization.
#' @param object A model object.
#' @rdname get_number_of_observations
#' @export
setGeneric("get_number_of_observations", function(object) {
  standardGeneric("get_number_of_observations")
})


setGeneric("get_descriptives", function(object, variables) {
  standardGeneric("get_descriptives")
})

#' Demand descriptives
#'
#' Calculates and returns basic descriptive statistics for the model's demand data. Factor variables are excluded from
#' the calculations.
#' @param object A model object.
#' @rdname get_demand_descriptives
#' @export
setGeneric("get_demand_descriptives", function(object) {
  standardGeneric("get_demand_descriptives")
})

#' Supply descriptives
#'
#' Calculates and returns basic descriptive statistics for the model's supply data. Factor variables are excluded from
#' the calculations.
#' @param object A model object.
#' @rdname get_supply_descriptives
#' @export
setGeneric("get_supply_descriptives", function(object) {
  standardGeneric("get_supply_descriptives")
})

#' @rdname get_model_description
setMethod("get_model_description", signature(object = "model_base"), function(object) {
  paste0(
    object@model_type_string, " with ",
    ifelse(object@system@correlated_shocks, "correlated", "independent"), " shocks"
  )
})

#' @rdname get_number_of_observations
setMethod("get_number_of_observations", signature(object = "model_base"), function(object) {
  nrow(object@model_tibble)
})

setMethod("get_descriptives", signature(object = "model_base"), function(object, variables = NULL) {
  if (is.null(variables)) {
    variables <- object@columns
  }
  variables <- variables[sapply(variables, function(c) !is.factor(object@model_tibble[, c]))]
  tibble::as_tibble(t(pastecs::stat.desc(object@model_tibble[, variables])), rownames = "col")
})

#' @rdname get_demand_descriptives
setMethod("get_demand_descriptives", signature(object = "model_base"), function(object) {
  get_descriptives(object, object@system@demand@independent_variables)
})

#' @rdname get_supply_descriptives
setMethod("get_supply_descriptives", signature(object = "model_base"), function(object) {
  get_descriptives(object, object@system@supply@independent_variables)
})

setGeneric("get_initializing_values", function(object) {
  standardGeneric("get_initializing_values")
})

setMethod("get_initializing_values", signature(object = "model_base"), function(object) {
  dlm <- object@system@demand@linear_model

  slm <- object@system@supply@linear_model

  ## Set demand initializing values
  varloc <- !(get_prefixed_independent_variables(object@system@demand) %in% names(dlm$coefficients))
  if (sum(varloc) > 0) {
    print_error(object@logger,
                "Misspecified model matrix. The matrix should contain all the variables except the variance."
    )
  }
  if (any(is.na(dlm$coefficients))) {
    print_warning(object@logger,
                  "Setting demand side NA initial values to zero: ",
                  paste0(names(dlm$coefficients[is.na(dlm$coefficients)]), collapse = ", "), "."
    )
    dlm$coefficients[is.na(dlm$coefficients)] <- 0
  }
  start_names <- c(
    get_prefixed_price_variable(object@system@demand),
    get_prefixed_control_variables(object@system@demand)
  )
  start <- c(dlm$coefficients[start_names])

  ## Set supply initializing values
  varloc <- !(get_prefixed_independent_variables(object@system@supply) %in% names(slm$coefficients))
  if (sum(varloc) > 0) {
    print_error(object@logger,
                "Misspecified model matrix. The matrix should contain all the variables except the variance."
    )
  }
  if (any(is.na(slm$coefficients))) {
    print_warning(object@logger,
                  "Setting supply side NA initial values to zero: ",
                  paste0(names(slm$coefficients[is.na(slm$coefficients)]), collapse = ", ")
    )
    slm$coefficients[is.na(slm$coefficients)] <- 0
  }
  start_names <- c(
    get_prefixed_price_variable(object@system@supply),
    get_prefixed_control_variables(object@system@supply)
  )
  start <- c(start, slm$coefficients[start_names])

  if (object@model_type_string %in% c("Deterministic Adjustment", "Stochastic Adjustment")) {
    start <- c(start, gamma = 1)
    names(start)[length(start)] <- get_price_differences_variable(object@system)
  }

  start <- c(start, 1, 1)
  names(start)[(length(start) - 1):length(start)] <- c(
    get_prefixed_variance_variable(object@system@demand), get_prefixed_variance_variable(object@system@supply)
  )

  if (object@system@correlated_shocks) {
    start <- c(start, rho = 0)
    names(start)[length(start)] <- get_correlation_variable(object@system)
  }

  start
})


#' Demand aggregation.
#'
#' Sets the model's parameters given in the estimation object and calculates the sample's aggregate demand.
#' @param object A model object.
#' @param estimation A model estimation object (i.e. a \code{\link[bbmle]{mle2}} object).
#' @rdname get_aggregate_demand
#' @seealso get_estimated_demanded_quantities
#' @export
setGeneric("get_aggregate_demand", function(object, estimation) {
  standardGeneric("get_aggregate_demand")
})

#' @rdname get_aggregate_demand
setMethod("get_aggregate_demand", signature(object = "model_base"), function(object, estimation) {
  object@system <- set_parameters(object@system, estimation@coef)
  get_aggregate(object@system@demand)
})

#' Estimated demanded quantities.
#'
#' Sets the model's parameters given in the estimation object and calculates the estimated demanded quantity for each
#' observation.
#' @param object A model object.
#' @param estimation A model estimation object (i.e. a \code{\link[bbmle]{mle2}} object).
#' @rdname get_estimated_demanded_quantities
#' @seealso get_aggregate_demand
#' @export
setGeneric("get_estimated_demanded_quantities", function(object, estimation) {
  standardGeneric("get_estimated_demanded_quantities")
})

#' @rdname get_estimated_demanded_quantities
setMethod("get_estimated_demanded_quantities", signature(object = "model_base"), function(object, estimation) {
  object@system <- set_parameters(object@system, estimation@coef)
  get_estimated_quantities(object@system@demand)
})

#' Supply aggregation.
#'
#' Sets the model's parameters given in the estimation object and calculates sample's aggregate supply.
#' @param object A model object.
#' @param estimation A model estimation object (i.e. a \code{\link[bbmle]{mle2}} object).
#' @rdname get_aggregate_supply
#' @seealso get_estimated_supplied_quantities
#' @export
setGeneric("get_aggregate_supply", function(object, estimation) {
  standardGeneric("get_aggregate_supply")
})

#' @rdname get_aggregate_supply
setMethod("get_aggregate_supply", signature(object = "model_base"), function(object, estimation) {
  object@system <- set_parameters(object@system, estimation@coef)
  get_aggregate(object@system@supply)
})

#' Estimated supplied quantities.
#'
#' Sets the model's parameters given in the estimation object and calculates the estimated supplied quantity for each
#' observation.
#' @param object A model object.
#' @param estimation A model estimation object (i.e. a \code{\link[bbmle]{mle2}} object).
#' @rdname get_estimated_supplied_quantities
#' @seealso get_aggregate_supply
#' @export
setGeneric("get_estimated_supplied_quantities", function(object, estimation) {
  standardGeneric("get_estimated_supplied_quantities")
})

#' @rdname get_estimated_supplied_quantities
setMethod("get_estimated_supplied_quantities", signature(object = "model_base"), function(object, estimation) {
  object@system <- set_parameters(object@system, estimation@coef)
  get_estimated_quantities(object@system@supply)
})

#' Simulate equilibrium model.
#'
#' Returns an initialized equilibrium model with simulated data. The simulated observations of the controls are in drawn
#' from a uniform distribution on the interval [2, 5]. Passing an NA price coefficient results into an error. Price
#' observations are not simulated. Instead, they are calculated so that the market clears. If the simulations results
#' into negative demanded quantities, supplied quantities or prices, the execution halts with an error.
#' @param model_string Equilibrium model type. It should be either 'eq_2sls' or 'eq_fiml'.
#' @param nobs Number of simulated entities.
#' @param tobs Number of simulated dates.
#' @param alpha_d Demand price coefficient.
#' @param beta_d0 Demand constant coefficient.
#' @param beta_d Demand exclusive controls' coefficients.
#' @param eta_d Demand common controls' coefficients.
#' @param alpha_s Supply price coefficient.
#' @param beta_s0 Supply constant coefficient.
#' @param beta_s Supply exclusive controls' coefficients.
#' @param eta_s Supply common controls' coefficients.
#' @param gamma Price adjustment coefficient.
#' @param beta_p0 Price equation's constant coefficient.
#' @param beta_p Price equation's control coefficients.
#' @param sigma_d Demand shock's standard deviation.
#' @param sigma_s Supply shock's standard deviation.
#' @param sigma_p Price equation shock's standard deviation.
#' @param rho_ds Demand and supply shocks' correlation coefficient.
#' @param rho_dp Demand and price shocks' correlation coefficient.
#' @param rho_sp Supply and price shocks' correlation coefficient.
#' @param seed Pseudo number generator seed.
#' @param verbose Verbosity level.
#' @rdname simulate_model
#' @export
setGeneric("simulate_model", function(
  model_string, nobs, tobs,
  alpha_d, beta_d0, beta_d, eta_d,
  alpha_s, beta_s0, beta_s, eta_s,
  gamma, beta_p0, beta_p,
  sigma_d = 1.0, sigma_s = 1.0, sigma_p = 1.0, rho_ds = 0.0, rho_dp = 0.0, rho_sp = 0.0,
  seed = NA, verbose = 0
) {
  standardGeneric("simulate_model")
})


#' @rdname simulate_model
setMethod("simulate_model", signature(), function(
  model_string, nobs, tobs,
  alpha_d, beta_d0, beta_d, eta_d,
  alpha_s, beta_s0, beta_s, eta_s,
  gamma, beta_p0, beta_p,
  sigma_d, sigma_s, sigma_p, rho_ds, rho_dp, rho_sp,
  seed, verbose
) {
  obs <- nobs * tobs

  if (!is.na(seed))  set.seed(seed)
  stopifnot(sigma_d >= 0)
  stopifnot(sigma_s >= 0)
  if (!is.na(sigma_p)) stopifnot(sigma_p >= 0)
  stopifnot(abs(rho_ds) <= 1)
  if (!is.na(rho_dp)) stopifnot(abs(rho_dp) <= 1)
  if (!is.na(rho_sp)) stopifnot(abs(rho_sp) <= 1)

  ## demand coefficients
  alpha_d_length <- length(alpha_d)
  stopifnot(alpha_d_length == 1)
  alpha_d <- matrix(alpha_d, 1)

  beta_d_length <- length(beta_d)
  if (beta_d_length > 0) beta_d <- matrix(beta_d, beta_d_length)

  eta_d_length <- length(eta_d)
  if (eta_d_length > 0) eta_d <- matrix(eta_d, eta_d_length)

  ## supply coefficients
  alpha_s_length <- length(alpha_s)
  stopifnot(alpha_s_length == 1)
  alpha_s <- matrix(alpha_s, 1)

  beta_s_length <- length(beta_s)
  if (beta_s_length > 0) beta_s <- matrix(beta_s, beta_s_length)

  eta_s_length <- length(eta_s)
  stopifnot(eta_d_length == eta_s_length)
  if (eta_s_length > 0) eta_s <- matrix(eta_s, eta_s_length)

  ## price dynamics coefficients
  gamma_length <- length(gamma)
  if (model_string %in% c("diseq_directional", "diseq_deterministic_adjustment", "diseq_stochastic_adjustment")) {
    stopifnot(gamma_length == 1)
  }
  if (gamma_length > 0) gamma <- matrix(gamma, gamma_length)

  beta_p_length <- length(beta_p)
  if (beta_p_length > 0) beta_p <- matrix(beta_p, beta_p_length)

  m <- beta_d_length + beta_s_length + eta_d_length + beta_p_length
  simulated_controls <- matrix(stats::runif(n = obs * m, min = 2, max = 5), obs, m)

  begin <- 1
  end <- begin + beta_d_length - 1
  x_d <- as.matrix(simulated_controls[, begin:end])
  colnames(x_d) <- paste0("Xd", 1:beta_d_length)

  begin <- end + 1
  end <- begin + beta_s_length - 1
  x_s <- as.matrix(simulated_controls[, begin:end])
  colnames(x_s) <- paste0("Xs", 1:beta_s_length)

  begin <- end + 1
  end <- begin + eta_d_length - 1
  x <- as.matrix(simulated_controls[, begin:end])
  colnames(x) <- paste0("X", 1:eta_d_length)

  begin <- end + 1
  end <- begin + beta_p_length - 1
  x_p <- as.matrix(simulated_controls[, begin:end])
  colnames(x_p) <- paste0("Xp", 1:beta_p_length)

  cov_ds <- sigma_d * sigma_s * rho_ds
  cov_dp <- sigma_d * sigma_p * rho_dp
  cov_sp <- sigma_s * sigma_p * rho_sp
  if (model_string == "diseq_stochastic_adjustment") {
    mu <- c(0, 0, 0)
    sigma <- matrix(c(sigma_d ** 2, cov_ds, cov_dp, cov_ds, sigma_s ** 2, cov_sp, cov_dp, cov_sp, sigma_p ** 2), 3, 3)
  }
  else {
    mu <- c(0, 0)
    sigma <- matrix(c(sigma_d ** 2, cov_ds, cov_ds, sigma_s ** 2), 2, 2)
  }
  disturbances <- MASS::mvrnorm(n = nobs, mu, sigma)
  u_d <- disturbances[, 1]
  u_s <- disturbances[, 2]
  if (model_string == "diseq_stochastic_adjustment") {
    u_p <- disturbances[, 3]
  }

  if (model_string %in% c("eq_2sls", "eq_fiml")) {
    scale <- (alpha_d[1, 1] - alpha_s[1, 1])
    p <- (x %*% (eta_s - eta_d) + x_s %*% beta_s - x_d %*% beta_d + beta_s0 - beta_d0 + u_s - u_d) / scale
    q_d <- p * alpha_d[1, 1] + beta_d0 + x_d %*% beta_d + x %*% eta_d + u_d
    q_s <- p * alpha_s[1, 1] + beta_s0 + x_s %*% beta_s + x %*% eta_s + u_s
    q <- q_d
  }
  else {
    p <- matrix(stats::runif(n = obs, min = 2, max = 5), obs, 1)
    q_d <- p * alpha_d[1, 1] + beta_d0 + x_d %*% beta_d + x %*% eta_d + u_d
    q_s <- p * alpha_s[1, 1] + beta_s0 + x_s %*% beta_s + x %*% eta_s + u_s
    q <- apply(cbind(q_d, q_s), 1, function(x) min(x[1], x[2]))
  }

  stopifnot(sum(p < 0) == 0)
  stopifnot(sum(q_d < 0) == 0)
  stopifnot(sum(q_s < 0) == 0)

  odt <- tibble::tibble(id = 1:nobs) %>%
    tidyr::crossing(tibble::tibble(date = as.factor(1:tobs))) %>%
    dplyr::mutate(Q = q) %>%
    dplyr::mutate(P = p) %>%
    dplyr::bind_cols(dplyr::as_tibble(x_d)) %>%
    dplyr::bind_cols(dplyr::as_tibble(x_s)) %>%
    dplyr::bind_cols(dplyr::as_tibble(x))
  if (model_string == "diseq_stochastic_adjustment") {
    odt <- odt %>%
      dplyr::bind_cols(dplyr::as_tibble(x_p))
  }

  key_columns <- c("id", "date")
  time_column <- c("date")
  quantity_column <- "Q"
  price_column <- "P"
  demand_specification <- paste0(price_column, " + Xd1 + Xd2 + X1 + X2")
  supply_specification <- "Xs1 + X1 + X2"
  if (model_string != "diseq_directional") {
    supply_specification <- paste0(price_column, " + ", supply_specification)
  }
  price_specification <- "Xp1"

  use_correlated_shocks <- TRUE

  if (model_string %in% c("eq_2sls", "eq_fiml", "diseq_basic")) {
    model <- new(
      model_string,
      key_columns,
      quantity_column, price_column, demand_specification, supply_specification,
      odt,
      use_correlated_shocks = use_correlated_shocks, verbose = verbose
    )
  }
  else if (model_string %in% c("diseq_directional", "diseq_deterministic_adjustment")) {
    model <- new(
      model_string,
      key_columns, time_column,
      quantity_column, price_column, demand_specification, supply_specification,
      odt,
      use_correlated_shocks = use_correlated_shocks, verbose = verbose
    )
  }
  else if (model_string %in% c("diseq_stochastic_adjustment")) {
    model <- new(
      model_string,
      key_columns, time_column,
      quantity_column, price_column, demand_specification, supply_specification, price_specification,
      odt,
      use_correlated_shocks = use_correlated_shocks, verbose = verbose
    )
  }
  else {
    stop("Unhandled model type.")
  }

  model
})
