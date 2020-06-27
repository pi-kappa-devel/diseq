#' @importFrom rlang .data
#' @importFrom tidyr crossing

setClass(
  "simulated_model",
  representation(
    ## Logging
    logger = "model_logger",

    ## simulated data sizes
    nobs = "numeric",
    tobs = "numeric",

    ## model parameters
    alpha_d = "numeric",
    beta_d0 = "numeric",
    beta_d = "vector",
    eta_d = "vector",
    alpha_s = "numeric",
    beta_s0 = "numeric",
    beta_s = "vector",
    eta_s = "vector",
    sigma_d = "numeric",
    sigma_s = "numeric",
    rho_ds = "numeric",

    mu = "vector",
    sigma = "matrix",

    ## simulation data
    seed = "numeric",
    price_generator = "function",
    control_generator = "function",
    simulation_tbl = "tbl_df"
  )
)

setMethod(
  "initialize", "simulated_model",
  function(
           .Object,
           nobs, tobs,
           alpha_d, beta_d0, beta_d, eta_d,
           alpha_s, beta_s0, beta_s, eta_s,
           sigma_d, sigma_s, rho_ds,
           seed, price_generator, control_generator) {
    .Object@logger <- new("model_logger", 2)

    .Object@nobs <- nobs
    .Object@tobs <- tobs

    .Object@alpha_d <- alpha_d
    .Object@beta_d0 <- beta_d0
    .Object@beta_d <- beta_d
    .Object@eta_d <- eta_d

    .Object@alpha_s <- alpha_s
    .Object@beta_s0 <- beta_s0
    .Object@beta_s <- beta_s
    .Object@eta_s <- eta_s

    stopifnot(sigma_d >= 0)
    .Object@sigma_d <- sigma_d
    stopifnot(sigma_s >= 0)
    .Object@sigma_s <- sigma_s
    stopifnot(abs(rho_ds) <= 1)
    .Object@rho_ds <- rho_ds

    .Object@seed <- seed
    if (!is.na(.Object@seed)) set.seed(.Object@seed)

    .Object@price_generator <- price_generator
    .Object@control_generator <- control_generator

    .Object@simulation_tbl <- tibble::tibble(id = 1:.Object@nobs) %>%
      tidyr::crossing(tibble::tibble(date = as.factor(1:.Object@tobs)))

    .Object <- simulate_controls(.Object)
    .Object <- simulate_shocks(.Object)
    .Object <- simulate_quantities_and_prices(.Object)

    .Object
  }
)

setGeneric("get_demand_controls", function(object) {
  standardGeneric("get_demand_controls")
})

setMethod("get_demand_controls", signature(object = "simulated_model"), function(object) {
  as.matrix(object@simulation_tbl[, grep("Xd", names(object@simulation_tbl))])
})

setGeneric("get_supply_controls", function(object) {
  standardGeneric("get_supply_controls")
})

setMethod("get_supply_controls", signature(object = "simulated_model"), function(object) {
  as.matrix(object@simulation_tbl[, grep("Xs", names(object@simulation_tbl))])
})

setGeneric("get_common_controls", function(object) {
  standardGeneric("get_common_controls")
})

setMethod("get_common_controls", signature(object = "simulated_model"), function(object) {
  as.matrix(object@simulation_tbl[, grep("X\\d", names(object@simulation_tbl))])
})

setGeneric("get_simulated_demanded_quantities", function(object, prices) {
  standardGeneric("get_simulated_demanded_quantities")
})

setMethod(
  "get_simulated_demanded_quantities", signature(object = "simulated_model"),
  function(object, prices) {
    as.vector(
      prices * object@alpha_d +
        object@beta_d0 + get_demand_controls(object) %*% object@beta_d +
        get_common_controls(object) %*% object@eta_d +
        object@simulation_tbl$u_d
    )
  }
)

setGeneric("get_simulated_supplied_quantities", function(object, prices) {
  standardGeneric("get_simulated_supplied_quantities")
})

setMethod(
  "get_simulated_supplied_quantities", signature(object = "simulated_model"),
  function(object, prices) {
    as.vector(
      prices * object@alpha_s +
        object@beta_s0 + get_supply_controls(object) %*% object@beta_s +
        get_common_controls(object) %*% object@eta_s +
        object@simulation_tbl$u_s
    )
  }
)

setGeneric("simulate_controls", function(object) {
  standardGeneric("simulate_controls")
})

setMethod("simulate_controls", signature(object = "simulated_model"), function(object) {
  object <- simulate_column(object, object@beta_d, "Xd", object@control_generator)
  object <- simulate_column(object, object@beta_s, "Xs", object@control_generator)
  object <- simulate_column(object, object@eta_d, "X", object@control_generator)

  object
})

setGeneric("simulate_column", function(object, coefficients, prefix, generator) {
  standardGeneric("simulate_column")
})

setMethod(
  "simulate_column", signature(object = "simulated_model"),
  function(
           object, coefficients, prefix, generator) {
    clen <- length(coefficients)
    if (clen > 0) {
      simn <- nrow(object@simulation_tbl)
      mat <- matrix(generator(simn * clen), simn, clen)
      colnames(mat) <- paste0(prefix, 1:clen)
      object@simulation_tbl <- object@simulation_tbl %>%
        dplyr::bind_cols(dplyr::as_tibble(mat))
    }

    object
  }
)

setGeneric("simulate_shocks", function(object) {
  standardGeneric("simulate_shocks")
})

setMethod("simulate_shocks", signature(object = "simulated_model"), function(object) {
  cov_ds <- object@sigma_d * object@sigma_s * object@rho_ds
  object@mu <- c(0, 0)
  object@sigma <- matrix(c(object@sigma_d**2, cov_ds, cov_ds, object@sigma_s**2), 2, 2)

  disturbances <- MASS::mvrnorm(n = nrow(object@simulation_tbl), object@mu, object@sigma)
  colnames(disturbances) <- c("u_d", "u_s")
  object@simulation_tbl <- object@simulation_tbl %>%
    dplyr::bind_cols(dplyr::as_tibble(disturbances))

  object
})


setGeneric(
  "simulate_quantities_and_prices",
  function(
           object,
           demanded_quantities = NA, supplied_quantities = NA,
           prices = NA, starting_prices = NA) {
    standardGeneric("simulate_quantities_and_prices")
  }
)

setMethod(
  "simulate_quantities_and_prices", signature(object = "simulated_model"),
  function(
           object, demanded_quantities, supplied_quantities, prices, starting_prices) {
    stopifnot(prices >= 0)
    stopifnot(demanded_quantities >= 0)
    stopifnot(supplied_quantities >= 0)

    object@simulation_tbl <- object@simulation_tbl %>%
      dplyr::mutate(D = demanded_quantities) %>%
      dplyr::mutate(S = supplied_quantities) %>%
      dplyr::mutate(P = prices) %>%
      dplyr::mutate(Q = pmin(.data$D, .data$S)) %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(LP = dplyr::lag(.data$P, order_by = date)) %>%
      dplyr::ungroup()
    object@simulation_tbl[is.na(object@simulation_tbl$LP), "LP"] <- starting_prices
    object@simulation_tbl <- object@simulation_tbl %>%
      dplyr::mutate(DP = .data$P - .data$LP) %>%
      dplyr::mutate(XD = .data$D - .data$S)

    object
  }
)

setClass(
  "simulated_equilibrium_model",
  contains = "simulated_model",
  representation()
)


setMethod(
  "simulate_quantities_and_prices", signature(object = "simulated_equilibrium_model"),
  function(
           object, demanded_quantities, supplied_quantities, prices, starting_prices) {
    scale <- (object@alpha_d - object@alpha_s)
    x_d <- get_demand_controls(object)
    x_s <- get_supply_controls(object)
    x <- get_common_controls(object)

    prices <- as.vector(
      (x %*% (object@eta_s - object@eta_d) +
        x_s %*% object@beta_s - x_d %*% object@beta_d +
        object@beta_s0 - object@beta_d0 +
        object@simulation_tbl$u_s - object@simulation_tbl$u_d
      ) / scale
    )
    demanded_quantities <- get_simulated_demanded_quantities(object, prices)
    supplied_quantities <- get_simulated_supplied_quantities(object, prices)

    callNextMethod(object, demanded_quantities, supplied_quantities, prices, starting_prices)
  }
)

setClass(
  "simulated_basic_model",
  contains = "simulated_model",
  representation()
)

setMethod(
  "simulate_quantities_and_prices", signature(object = "simulated_basic_model"),
  function(
           object, demanded_quantities, supplied_quantities, prices, starting_prices) {
    prices <- object@price_generator(nrow(object@simulation_tbl))
    demanded_quantities <- get_simulated_demanded_quantities(object, prices)
    supplied_quantities <- get_simulated_supplied_quantities(object, prices)

    callNextMethod(object, demanded_quantities, supplied_quantities, prices, starting_prices)
  }
)

setClass(
  "simulated_directional_model",
  contains = "simulated_model",
  representation()
)

setMethod(
  "simulate_quantities_and_prices", signature(object = "simulated_directional_model"),
  function(
           object, demanded_quantities, supplied_quantities, prices, starting_prices) {
    starting_prices <- object@price_generator(object@nobs)
    r_d <- get_simulated_demanded_quantities(object, 0)
    r_s <- get_simulated_supplied_quantities(object, 0)

    demand_columns <- grep("Xd", names(object@simulation_tbl))
    supply_columns <- grep("Xs", names(object@simulation_tbl))

    prices <- c()
    demanded_quantities <- c()
    supplied_quantities <- c()
    lagged_prices <- starting_prices
    for (i in 1:object@nobs) {
      i_offset <- (i - 1) * object@tobs
      for (t in 1:object@tobs) {
        count <- 0
        repeat {
          new_price <- object@price_generator(1)
          new_demand <- new_price * object@alpha_d + r_d[i_offset + t]
          new_supply <- r_s[i_offset + t]
          price_difference <- new_price - lagged_prices[i]
          shortage <- new_demand - new_supply
          count <- count + 1
          stopifnot(count < 1e+3)
          if (price_difference * shortage >= 0) {
            break
          }

          object@simulation_tbl[i_offset + t, demand_columns] <- as.list(
            object@control_generator(length(demand_columns))
          )
          object@simulation_tbl[i_offset + t, supply_columns] <- as.list(
            object@control_generator(length(supply_columns))
          )
          object@simulation_tbl[i_offset + t, c("u_d", "u_s")] <- as.list(
            MASS::mvrnorm(n = 1, object@mu, object@sigma)
          )
          r_d[i_offset + t] <- (
            object@beta_d0 + get_demand_controls(object)[i_offset + t, ] %*% object@beta_d +
              get_common_controls(object)[i_offset + t, ] %*% object@eta_d +
              object@simulation_tbl$u_d[i_offset + t]
          )
          r_s[i_offset + t] <- (
            object@beta_s0 + get_supply_controls(object)[i_offset + t, ] %*% object@beta_s +
              get_common_controls(object)[i_offset + t, ] %*% object@eta_s +
              object@simulation_tbl$u_s[i_offset + t]
          )
        }
        prices <- append(prices, new_price)
        demanded_quantities <- append(demanded_quantities, new_demand)
        supplied_quantities <- append(supplied_quantities, new_supply)
        lagged_prices[i] <- new_price
      }
    }

    callNextMethod(object, demanded_quantities, supplied_quantities, prices, starting_prices)
  }
)

setClass(
  "simulated_deterministic_adjustment_model",
  contains = "simulated_model",
  representation(
    ## model parameters
    gamma = "numeric"
  )
)

setMethod(
  "simulate_quantities_and_prices", signature(object = "simulated_deterministic_adjustment_model"),
  function(
           object, demanded_quantities, supplied_quantities, prices, starting_prices) {
    r_d <- get_simulated_demanded_quantities(object, 0)
    r_s <- get_simulated_supplied_quantities(object, 0)
    dr <- r_d - r_s

    if (class(object) == "simulated_stochastic_adjustment_model") {
      dr <- dr + object@gamma * (
        object@beta_p0 + get_price_controls(object) %*% object@beta_p + object@simulation_tbl$u_p
      )
    }

    starting_prices <- object@price_generator(object@nobs)
    scale <- object@gamma - object@alpha_d + object@alpha_s

    prices <- c()
    lagged_prices <- starting_prices
    for (i in 1:object@nobs) {
      i_offset <- (i - 1) * object@tobs
      for (t in 1:object@tobs) {
        lagged_prices[i] <- (object@gamma * lagged_prices[i] + dr[i_offset + t]) / scale
        prices <- append(prices, lagged_prices[i])
      }
    }

    demanded_quantities <- get_simulated_demanded_quantities(object, prices)
    supplied_quantities <- get_simulated_supplied_quantities(object, prices)

    callNextMethod(object, demanded_quantities, supplied_quantities, prices, starting_prices)
  }
)

setMethod(
  "initialize", "simulated_deterministic_adjustment_model",
  function(
           .Object,
           nobs, tobs,
           alpha_d, beta_d0, beta_d, eta_d,
           alpha_s, beta_s0, beta_s, eta_s,
           gamma,
           sigma_d, sigma_s, rho_ds,
           seed, price_generator, control_generator) {
    .Object@gamma <- gamma

    .Object <- callNextMethod(
      .Object,
      nobs, tobs,
      alpha_d, beta_d0, beta_d, eta_d,
      alpha_s, beta_s0, beta_s, eta_s,
      sigma_d, sigma_s, rho_ds,
      seed, price_generator, control_generator
    )

    .Object
  }
)


setClass(
  "simulated_stochastic_adjustment_model",
  contains = "simulated_deterministic_adjustment_model",
  representation(
    ## model parameters
    beta_p0 = "numeric",
    beta_p = "vector",
    sigma_p = "numeric",
    rho_dp = "numeric",
    rho_sp = "numeric"
  )
)

setMethod(
  "initialize", "simulated_stochastic_adjustment_model",
  function(
           .Object,
           nobs, tobs,
           alpha_d, beta_d0, beta_d, eta_d,
           alpha_s, beta_s0, beta_s, eta_s,
           gamma, beta_p0, beta_p,
           sigma_d, sigma_s, sigma_p, rho_ds, rho_dp, rho_sp,
           seed, price_generator, control_generator) {
    .Object@beta_p0 <- beta_p0
    .Object@beta_p <- beta_p

    stopifnot(sigma_p >= 0)
    .Object@sigma_p <- sigma_p
    stopifnot(abs(rho_dp) <= 1)
    .Object@rho_dp <- rho_dp
    stopifnot(abs(rho_sp) <= 1)
    .Object@rho_sp <- rho_sp

    .Object <- callNextMethod(
      .Object,
      nobs, tobs,
      alpha_d, beta_d0, beta_d, eta_d,
      alpha_s, beta_s0, beta_s, eta_s,
      gamma,
      sigma_d, sigma_s, rho_ds,
      seed, price_generator, control_generator
    )

    .Object
  }
)

setGeneric("get_price_controls", function(object) {
  standardGeneric("get_price_controls")
})

setMethod(
  "get_price_controls", signature(object = "simulated_stochastic_adjustment_model"),
  function(object) {
    as.matrix(object@simulation_tbl[, grep("Xp", names(object@simulation_tbl))])
  }
)

setMethod(
  "simulate_controls", signature(object = "simulated_stochastic_adjustment_model"),
  function(object) {
    object <- callNextMethod(object)
    object <- simulate_column(object, object@beta_p, "Xp", object@control_generator)

    object
  }
)

setMethod(
  "simulate_shocks", signature(object = "simulated_stochastic_adjustment_model"),
  function(object) {
    cov_ds <- object@sigma_d * object@sigma_s * object@rho_ds
    cov_dp <- object@sigma_d * object@sigma_p * object@rho_dp
    cov_sp <- object@sigma_s * object@sigma_p * object@rho_sp

    object@mu <- c(0, 0, 0)
    object@sigma <- matrix(
      c(
        object@sigma_d**2, cov_ds, cov_dp, cov_ds,
        object@sigma_s**2, cov_sp, cov_dp, cov_sp,
        object@sigma_p**2
      ),
      3, 3
    )

    disturbances <- MASS::mvrnorm(n = nrow(object@simulation_tbl), object@mu, object@sigma)
    colnames(disturbances) <- c("u_d", "u_s", "u_p")

    object@simulation_tbl <- object@simulation_tbl %>%
      dplyr::bind_cols(dplyr::as_tibble(disturbances))

    object
  }
)

#' Simulate model data.
#'
#' Returns a data \code{tibble} with simulated data from a generating process that matches the
#' passed model string. By default, the simulated observations of the controls are drawn from a
#' normal distribution.
#' @param model_string Model type. It should be among \code{eq_2sls}, \code{eq_fiml},
#'   \code{diseq_basic}, \code{diseq_directional}, \code{diseq_deterministic_adjustment}, and
#'   \code{diseq_stochastic_adjustment}.
#' @param nobs Number of simulated entities.
#' @param tobs Number of simulated dates.
#' @param alpha_d Price coefficient of demand.
#' @param beta_d0 Constant coefficient of demand.
#' @param beta_d Coefficients of exclusive demand controls.
#' @param eta_d Demand coefficients of common controls.
#' @param alpha_s Price coefficient of supply.
#' @param beta_s0 Constant coefficient of supply.
#' @param beta_s Coefficients of exclusive supply controls.
#' @param eta_s Supply coefficients of common controls.
#' @param gamma Price equation's stability factor.
#' @param beta_p0 Price equation's constant coefficient.
#' @param beta_p Price equation's control coefficients.
#' @param sigma_d Demand shock's standard deviation.
#' @param sigma_s Supply shock's standard deviation.
#' @param sigma_p Price equation shock's standard deviation.
#' @param rho_ds Demand and supply shocks' correlation coefficient.
#' @param rho_dp Demand and price shocks' correlation coefficient.
#' @param rho_sp Supply and price shocks' correlation coefficient.
#' @param seed Pseudo random number generator seed.
#' @param price_generator Pseudo random number generator callback for prices.
#' @param control_generator Pseudo random number generator callback for non-price controls.
#' @return The simulated data.
#' @rdname simulate_model_data
#' @export
setGeneric(
  "simulate_model_data",
  function(
           model_string, nobs, tobs,
           alpha_d, beta_d0, beta_d, eta_d,
           alpha_s, beta_s0, beta_s, eta_s,
           gamma, beta_p0, beta_p,
           sigma_d = 1.0, sigma_s = 1.0, sigma_p = 1.0, rho_ds = 0.0, rho_dp = 0.0, rho_sp = 0.0,
           seed = NA_integer_,
           price_generator = function(nobs) stats::rnorm(n = nobs, mean = 2.5, sd = 0.5),
           control_generator = function(nobs) stats::rnorm(n = nobs, mean = 2.5, sd = 0.5)) {
    standardGeneric("simulate_model_data")
  }
)

#' @rdname simulate_model_data
setMethod(
  "simulate_model_data", signature(),
  function(
           model_string, nobs, tobs,
           alpha_d, beta_d0, beta_d, eta_d,
           alpha_s, beta_s0, beta_s, eta_s,
           gamma, beta_p0, beta_p,
           sigma_d, sigma_s, sigma_p, rho_ds, rho_dp, rho_sp,
           seed,
           price_generator, control_generator) {
    if (model_string %in% c("eq_2sls", "eq_fiml")) {
      sim_mdl <- new(
        "simulated_equilibrium_model",
        nobs, tobs,
        alpha_d, beta_d0, beta_d, eta_d,
        alpha_s, beta_s0, beta_s, eta_s,
        sigma_d, sigma_s, rho_ds,
        seed,
        price_generator, control_generator
      )
      stopifnot(abs(sim_mdl@simulation_tbl$XD) < sqrt(.Machine$double.eps))
    }
    else if (model_string == "diseq_basic") {
      sim_mdl <- new(
        "simulated_basic_model",
        nobs, tobs,
        alpha_d, beta_d0, beta_d, eta_d,
        alpha_s, beta_s0, beta_s, eta_s,
        sigma_d, sigma_s, rho_ds,
        seed,
        price_generator, control_generator
      )
      xd_share <- sum(sim_mdl@simulation_tbl$XD > 0) / nrow(sim_mdl@simulation_tbl)
      stopifnot(xd_share < 0.9 && xd_share > 0.1)
    }
    else if (model_string == "diseq_directional") {
      sim_mdl <- new(
        "simulated_directional_model",
        nobs, tobs,
        alpha_d, beta_d0, beta_d, eta_d,
        alpha_s, beta_s0, beta_s, eta_s,
        sigma_d, sigma_s, rho_ds,
        seed,
        price_generator, control_generator
      )
      stopifnot(sim_mdl@simulation_tbl$DP * sim_mdl@simulation_tbl$XD >= 0)
      xd_share <- sum(sim_mdl@simulation_tbl$XD > 0) / nrow(sim_mdl@simulation_tbl)
      stopifnot(xd_share < 0.9 && xd_share > 0.1)
    }
    else if (model_string == "diseq_deterministic_adjustment") {
      sim_mdl <- new(
        "simulated_deterministic_adjustment_model",
        nobs, tobs,
        alpha_d, beta_d0, beta_d, eta_d,
        alpha_s, beta_s0, beta_s, eta_s,
        gamma,
        sigma_d, sigma_s, rho_ds,
        seed,
        price_generator, control_generator
      )
      stopifnot(abs(
        sim_mdl@gamma * sim_mdl@simulation_tbl$DP - sim_mdl@simulation_tbl$XD
      ) < sqrt(.Machine$double.eps))
      xd_share <- sum(sim_mdl@simulation_tbl$XD > 0) / nrow(sim_mdl@simulation_tbl)
      stopifnot(xd_share < 0.9 && xd_share > 0.1)
    }
    else if (model_string == "diseq_stochastic_adjustment") {
      sim_mdl <- new(
        "simulated_stochastic_adjustment_model",
        nobs, tobs,
        alpha_d, beta_d0, beta_d, eta_d,
        alpha_s, beta_s0, beta_s, eta_s,
        gamma, beta_p0, beta_p,
        sigma_d, sigma_s, sigma_p, rho_ds, rho_dp, rho_sp,
        seed,
        price_generator, control_generator
      )
      xd_share <- sum(sim_mdl@simulation_tbl$XD > 0) / nrow(sim_mdl@simulation_tbl)
      stopifnot(xd_share < 0.9 && xd_share > 0.1)
    }
    else {
      stop("Unhandled model type.")
    }

    sim_mdl@simulation_tbl
  }
)


setGeneric(
  "simulate_model",
  function(
           model_string, nobs, tobs,
           alpha_d, beta_d0, beta_d, eta_d,
           alpha_s, beta_s0, beta_s, eta_s,
           gamma, beta_p0, beta_p,
           sigma_d = 1.0, sigma_s = 1.0, sigma_p = 1.0, rho_ds = 0.0, rho_dp = 0.0, rho_sp = 0.0,
           seed = NA, verbose = 0) {
    standardGeneric("simulate_model")
  }
)


setMethod(
  "simulate_model", signature(),
  function(
           model_string, nobs, tobs,
           alpha_d, beta_d0, beta_d, eta_d,
           alpha_s, beta_s0, beta_s, eta_s,
           gamma, beta_p0, beta_p,
           sigma_d, sigma_s, sigma_p, rho_ds, rho_dp, rho_sp,
           seed, verbose) {
    sdt <- simulate_model_data(
      model_string, nobs, tobs,
      alpha_d, beta_d0, beta_d, eta_d,
      alpha_s, beta_s0, beta_s, eta_s,
      gamma, beta_p0, beta_p,
      sigma_d, sigma_s, sigma_p, rho_ds, rho_dp, rho_sp,
      seed
    )

    key_columns <- c("id", "date")
    time_column <- c("date")
    quantity_column <- "Q"
    price_column <- "P"
    demand_specification <- paste(
      price_column, paste("Xd", seq_along(beta_d), sep = "", collapse = " + "),
      paste("X", seq_along(eta_d), sep = "", collapse = " + "),
      sep = " + "
    )
    supply_specification <- paste(
      paste("Xs", seq_along(beta_s), sep = "", collapse = " + "),
      paste("X", seq_along(eta_s), sep = "", collapse = " + "),
      sep = " + "
    )
    if (model_string != "diseq_directional") {
      supply_specification <- paste0(price_column, " + ", supply_specification)
    }
    price_specification <- paste("Xp", seq_along(beta_p), sep = "", collapse = " + ")

    use_correlated_shocks <- TRUE


    if (model_string == "eq_2sls") {
      model <- new(
        model_string,
        key_columns,
        quantity_column, price_column, demand_specification, supply_specification,
        sdt,
        verbose = verbose
      )
    }
    else if (model_string %in% c("eq_fiml", "diseq_basic")) {
      model <- new(
        model_string,
        key_columns,
        quantity_column, price_column, demand_specification, supply_specification,
        sdt,
        use_correlated_shocks = use_correlated_shocks, verbose = verbose
      )
    }
    else if (model_string %in% c("diseq_directional", "diseq_deterministic_adjustment")) {
      model <- new(
        model_string,
        key_columns, time_column,
        quantity_column, price_column, demand_specification, supply_specification,
        sdt,
        use_correlated_shocks = use_correlated_shocks, verbose = verbose
      )
    }
    else if (model_string %in% c("diseq_stochastic_adjustment")) {
      model <- new(
        model_string,
        key_columns, time_column,
        quantity_column, price_column,
        demand_specification, supply_specification, price_specification,
        sdt,
        use_correlated_shocks = use_correlated_shocks, verbose = verbose
      )
    }
    else {
      stop("Unhandled model type.")
    }

    model
  }
)

simulate_eq_2sls_model <- function(parameters, seed, verbose) {
  simulate_model(
    "eq_2sls", parameters$nobs, parameters$tobs,
    parameters$alpha_d, parameters$beta_d0, parameters$beta_d, parameters$eta_d,
    parameters$alpha_s, parameters$beta_s0, parameters$beta_s, parameters$eta_s,
    NA, NA, c(NA),
    sigma_d = 1, sigma_s = 1, sigma_p = NA,
    rho_ds = 0, rho_dp = NA, rho_sp = NA,
    seed = seed, verbose = verbose
  )
}

simulate_eq_fiml_model <- function(parameters, seed, verbose) {
  simulate_model(
    "eq_fiml", parameters$nobs, parameters$tobs,
    parameters$alpha_d, parameters$beta_d0, parameters$beta_d, parameters$eta_d,
    parameters$alpha_s, parameters$beta_s0, parameters$beta_s, parameters$eta_s,
    NA, NA, c(NA),
    sigma_d = parameters$sigma_d, sigma_s = parameters$sigma_s, sigma_p = NA,
    rho_ds = parameters$rho_ds, rho_dp = NA, rho_sp = NA,
    seed = seed, verbose = verbose
  )
}

simulate_basic_model <- function(parameters, seed, verbose) {
  simulate_model(
    "diseq_basic", parameters$nobs, parameters$tobs,
    parameters$alpha_d, parameters$beta_d0, parameters$beta_d, parameters$eta_d,
    parameters$alpha_s, parameters$beta_s0, parameters$beta_s, parameters$eta_s,
    NA, NA, c(NA),
    sigma_d = parameters$sigma_d, sigma_s = parameters$sigma_s, sigma_p = NA,
    rho_ds = parameters$rho_ds, rho_dp = NA, rho_sp = NA,
    seed = seed, verbose = verbose
  )
}

simulate_directional_model <- function(parameters, seed, verbose) {
  simulate_model(
    "diseq_directional", parameters$nobs, parameters$tobs,
    parameters$alpha_d, parameters$beta_d0, parameters$beta_d, parameters$eta_d,
    0, parameters$beta_s0, parameters$beta_s, parameters$eta_s,
    NA, NA, c(NA),
    sigma_d = parameters$sigma_d, sigma_s = parameters$sigma_s, sigma_p = NA,
    rho_ds = parameters$rho_ds, rho_dp = NA, rho_sp = NA,
    seed = seed, verbose = verbose
  )
}

simulate_deterministic_adjustment_model <- function(parameters, seed, verbose) {
  simulate_model(
    "diseq_deterministic_adjustment", parameters$nobs, parameters$tobs,
    parameters$alpha_d, parameters$beta_d0, parameters$beta_d, parameters$eta_d,
    parameters$alpha_s, parameters$beta_s0, parameters$beta_s, parameters$eta_s,
    parameters$gamma, NA, c(NA),
    sigma_d = parameters$sigma_d, sigma_s = parameters$sigma_s, sigma_p = NA,
    rho_ds = parameters$rho_ds, rho_dp = NA, rho_sp = NA,
    seed = seed, verbose = verbose
  )
}

simulate_stochastic_adjustment_model <- function(parameters, seed, verbose) {
  simulate_model(
    "diseq_stochastic_adjustment", parameters$nobs, parameters$tobs,
    parameters$alpha_d, parameters$beta_d0, parameters$beta_d, parameters$eta_d,
    parameters$alpha_s, parameters$beta_s0, parameters$beta_s, parameters$eta_s,
    parameters$gamma, parameters$beta_p0, parameters$beta_p,
    sigma_d = parameters$sigma_d, sigma_s = parameters$sigma_s, sigma_p = parameters$sigma_p,
    rho_ds = parameters$rho_ds, rho_dp = parameters$rho_dp, rho_sp = parameters$rho_sp,
    seed = seed, verbose = verbose
  )
}
