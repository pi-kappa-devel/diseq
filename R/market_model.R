#' @include model_logger.R
#' @include system_base.R
#' @importFrom bbmle parnames mle2
#' @importFrom grid grid.raster
#' @importFrom png readPNG
#' @importFrom rlang :=
#' @importFrom stats formula lm model.matrix na.omit median qnorm sd var
#' @import dplyr magrittr tibble

setClassUnion("characterOrNULL", c("character", "NULL"))
setOldClass(c("spec_tbl_df", "tbl_df", "tbl", "data.frame"))

#' @title Model base class
#'
#' @slot logger Logger object.
#' @slot key_columns Vector of column names that uniquely identify data records. For panel data
#'   this vector should contain an entity and a time point identifier.
#' @slot time_column Column name for the time point data.
#' @slot explanatory_columns Vector of explanatory column names for all model's equations.
#' @slot data_columns Vector of model's data column names. This is the union of the quantity,
#'   price and explanatory columns.
#' @slot columns Vector of primary key and data column names for all model's equations.
#' @slot model_tibble Model data \code{tibble}.
#' @slot model_type_string Model type string description.
#' @slot system Model's system of equations.
setClass(
    "market_model",
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
        market_type_string = "character",
        system = "system_base"
    )
)

#' @title Model initialization
#'
#' @details
#' ## Common initialization
#'
#' ### Variable construction
#'
#'   The constructor prepares the model's variables using the passed specifications. The
#'   specification strings are expected to follow the
#'   syntax of \code{\link[stats]{formula}}. The construction of the model's data uses the variables
#'   that are extracted by these specification. The demand variables are extracted by a formula that
#'   uses the \code{quantity_column} on the left hand side and the \code{demand_specification} on
#'   the right hand side of the formula. The supply variables are constructed by the and the
#'   \code{quantity_column} and the  \code{supply_specification}. In the case of the
#'   \code{\linkS4class{diseq_stochastic_adjustment}} model, the price dynamics' variables are
#'   extracted using the \code{quantity_column} and the \code{price_specification}
#'
#' ### Data preparation
#'
#'   1. If the passed data set contains rows with NA values, they are dropped. If the verbosity
#'     level allows warnings, a warning is emitted reporting how many rows were dropped.
#'
#'   2. After dropping the rows, factor levels may be invalidated. If needed the constructor
#'     readjusts the factor variables by removing the unobserved levels. Factor indicators and
#'     interaction terms are automatically created.
#'
#'   3. The primary column is constructed by pasting the values of the key_columns.
#'
#'   4. In the case of the \code{\linkS4class{diseq_directional}},
#'     \code{\linkS4class{diseq_deterministic_adjustment}}, and
#'     the \code{\linkS4class{diseq_stochastic_adjustment}} models, a column with lagged prices
#'     is constructed. Since lagged prices are unavailable for the observation of the first
#'     time points, these observations are dropped. If the verbosity level allows the emission
#'     of information messages, the constructor prints the number of dropped observations.
#'
#'   5. In the case of the \code{\linkS4class{diseq_directional}},
#'     and the \code{\linkS4class{diseq_stochastic_adjustment}} models, a column with price
#'     differences is created.
#' @param .Object The object to be Constructed.
#' @param verbose Verbosity level.
#' @param key_columns Key columns of the data set.
#' @param time_column The time column of the data set.
#' @param quantity_column The quantity variable of the data set.
#' @param price_column The price variable of the data set.
#' @param demand_specification A formula representation of the right hand side of the
#'   demand equation.
#' @param supply_specification A formula representation of the right hand side of the
#'   supply equation.
#' @param price_specification A formula representation of the price equation.
#' @param use_correlated_shocks Should the model be estimated using correlated shocks?
#' @param data The data set.
#' @return The initialized model.
#' @name initialize_market_model
NULL

setMethod(
    "initialize", "market_model",
    function(
             .Object,
             model_type_string, verbose,
             key_columns, time_column,
             quantity_column, price_column,
             demand_specification, supply_specification, price_specification,
             use_correlated_shocks,
             data,
             system_initializer) {

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
                .Object@explanatory_columns,
                all.vars(formula(paste0(price_column, " ~ ", price_specification)))
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

        remove_unused_levels <- function(x) {
            if (is.factor(x)) {
                initial_levels <- levels(x)
                x <- factor(x)
                remaining_levels <- levels(x)
                removed_levels <- initial_levels[!(initial_levels %in% remaining_levels)]
                if (length(removed_levels)) {
                    print_warning(
                        .Object@logger, "Removing unobserved '",
                        paste0(removed_levels, collapse = ", "), "' level(s)."
                    )
                }
            }
            x
        }
        .Object@model_tibble <- tibble::as_tibble(sapply(.Object@model_tibble,
                                                         remove_unused_levels))

        ## Create primary key column
        key_columns_syms <- rlang::syms(.Object@key_columns)
        .Object@model_tibble <- .Object@model_tibble %>%
            dplyr::mutate(pk = as.integer(paste0(!!!key_columns_syms)))

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
                dplyr::mutate(
                    !!lagged_price_sym := dplyr::lag(!!price_sym, order_by = !!time_sym)
                ) %>%
                dplyr::ungroup()

            drop_rows <- .Object@model_tibble %>%
                dplyr::select(!!lagged_price_sym) %>%
                is.na() %>%
                c()
            .Object@model_tibble <- .Object@model_tibble[!drop_rows, ]
            print_info(
                .Object@logger, "Dropping ",
                sum(drop_rows), " rows by generating '", lagged_price_column, "'."
            )

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
                quantity_column, price_column,
                demand_specification, supply_specification, price_specification,
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
    }
)

#' Prints a short description of the model.
#'
#' Sends basic information about the model to standard output.
#' @param object A model object.
#' @examples
#' \donttest{
#' simulated_data <- simulate_model_data(
#'     "diseq_stochastic_adjustment", 500, 3, # model type, observed entities, observed time points
#'     -0.1, 9.8, c(0.3, -0.2), c(0.6, -0.1), # demand coefficients
#'     0.1, 5.1, c(0.9), c(-0.5, 0.2), # supply coefficients
#'     1.2, 3.1, c(0.8) # price equation
#' )
#'
#' # initialize the model
#' model <- new(
#'     "diseq_stochastic_adjustment", # model type
#'     c("id", "date"), "date", "Q", "P", # keys, time, quantity, and price variables
#'     "P + Xd1 + Xd2 + X1 + X2", "P + Xs1 + X1 + X2", # equation specifications
#'     "Xp1", # price dynamics specification
#'     simulated_data, # data
#'     use_correlated_shocks = TRUE # allow shocks to be correlated
#' )
#'
#' # print the model
#' show(model)
#' }
#' @rdname show
#' @export
setMethod("show", signature(object = "market_model"), function(object) {
    cat(sprintf(
        "\n%s Model for Markets in %s\n",
        object@model_type_string, object@market_type_string
    ))
    show_implementation(object@system)
    cat(sprintf(
        "  %-18s: %s\n", "Shocks",
        ifelse(object@system@correlated_shocks, "Correlated", "Independent")
    ))
})

#' Summarizes the model.
#'
#' Prints basic information about the passed model object. In addition to the output of
#' the \code{\link{show}} method, \code{summary} prints
#' - the number of observations,
#' - the number of observations in each equation for models with sample separation, and
#' - various categories of variables.
#' @param object A model object.
#' @examples
#' \donttest{
#' simulated_data <- simulate_model_data(
#'     "diseq_stochastic_adjustment", 500, 3, # model type, observed entities, observed time points
#'     -0.1, 9.8, c(0.3, -0.2), c(0.6, -0.1), # demand coefficients
#'     0.1, 5.1, c(0.9), c(-0.5, 0.2), # supply coefficients
#'     1.2, 3.1, c(0.8) # price equation
#' )
#'
#' # initialize the model
#' model <- new(
#'     "diseq_stochastic_adjustment", # model type
#'     c("id", "date"), "date", "Q", "P", # keys, time, quantity, and price variables
#'     "P + Xd1 + Xd2 + X1 + X2", "P + Xs1 + X1 + X2", # equation specifications
#'     "Xp1", # price dynamics specification
#'     simulated_data, # data
#'     use_correlated_shocks = TRUE # allow shocks to be correlated
#' )
#'
#' # print the model
#' summary(model)
#' }
#' @rdname summary
#' @export
setMethod("summary", signature(object = "market_model"), function(object) {
    show(object)
    cat(sprintf("  %-18s: %d\n", "Nobs", nrow(object@model_tibble)))
    summary_implementation(object@system)
    cat(sprintf(
        "  %-18s: %s\n", "Key Var(s)",
        paste0(object@key_columns, collapse = ", ")
    ))
    if (!is.null(object@time_column)) {
        cat(sprintf(
            "  %-18s: %s\n", "Time Var",
            paste0(object@time_column, collapse = ", ")
        ))
    }
})

#' Plots the model.
#'
#' Displays a graphical illustration of the passed model object.
#' @param x A model object.
#' @examples
#' \donttest{
#' simulated_data <- simulate_model_data(
#'     "diseq_basic", 500, 3, # model type, observed entities, observed time points
#'     -0.9, 8.9, c(0.03, -0.02), c(-0.03, -0.01), # demand coefficients
#'     0.9, 4.2, c(0.03), c(0.05, 0.02), # supply coefficients
#' )
#'
#' # initialize the model
#' model <- new(
#'     "diseq_basic", # model type
#'     c("id", "date"), "Q", "P", # keys, time, quantity, and price variables
#'     "P + Xd1 + Xd2 + X1 + X2", "P + Xs1 + X1 + X2", # equation specifications
#'     simulated_data, # data
#'     use_correlated_shocks = TRUE # allow shocks to be correlated
#' )
#'
#' # print the model
#' plot(model)
#' }
#' @rdname plot
#' @export
setMethod("plot", signature(x = "market_model"), function(x) {
    filename <- paste0(class(x)[1], ".png")
    path <- system.file("help", "figures", filename, package = "diseq")
    if (path == "") {
      path <- system.file("man", "figures", filename, package = "diseq")
    }
    grid::grid.raster(png::readPNG(path))
})

#' Minus log-likelihood.
#'
#' Returns the opposite of the log-likelihood. The likelihood functions are based on
#' Maddala and Nelson (1974) \doi{10.2307/1914215}. The likelihoods expressions
#' that the function uses are derived in
#' Karapanagiotis (2020) \doi{10.2139/ssrn.3525622}. The function calculates
#' the model's log likelihood by evaluating the log likelihood of each observation in the sample
#' and summing the evaluation results.
#' @param object A model object.
#' @param parameters A vector of parameters at which the function is to be evaluated.
#' @return The opposite of the sum of the likelihoods evaluated for each observation.
#' @rdname minus_log_likelihood
#' @export
setGeneric("minus_log_likelihood", function(object, parameters) {
  standardGeneric("minus_log_likelihood")
})

setGeneric("gradient", function(object, parameters) {
  standardGeneric("gradient")
})

setGeneric("hessian", function(object, parameters) {
    standardGeneric("hessian")
})

#' Model estimation.
#'
#' All models are estimated using full information maximum likelihood. The
#' \code{\linkS4class{equilibrium_model}} can also be estimated using two-stage least squares.
#' The maximum likelihood estimation is based on \code{\link[bbmle]{mle2}}. If no starting
#' values are provided, the function uses linear regression estimates as initializing values.
#' The default optimization method is
#' BFGS. For other alternatives see \code{\link[bbmle]{mle2}}. The implementation of the two-stage
#' least square estimation of the \code{\linkS4class{equilibrium_model}} is based on
#' \code{\link[systemfit]{systemfit}}.
#' @param object A model object.
#' @param ... Named parameter used in the model's estimation. These are passed further down
#'   to the estimation call. For the \code{\linkS4class{equilibrium_model}} model, the
#' parameters are passed
#'   to \code{\link[systemfit]{systemfit}}, if the method is set to \code{2SLS}, or to
#' \code{\link[bbmle]{mle2}} for any other method. For the rest of the models, the parameters
#' are passed to \code{\link[bbmle]{mle2}}.
#' @return The object that holds the estimation result.
#' @rdname estimate
#' @examples
#' \donttest{
#' simulated_data <- simulate_model_data(
#'   "diseq_basic", 500, 3, # model type, observed entities, observed time points
#'   -0.9, 8.9, c(0.3, -0.2), c(-0.03, -0.01), # demand coefficients
#'   0.9, 4.2, c(0.03), c(-0.05, 0.02) # supply coefficients
#' )
#'
#' # initialize the model
#' model <- new(
#'   "diseq_basic", # model type
#'   c("id", "date"), "Q", "P", # keys, quantity, and price variables
#'   "P + Xd1 + Xd2 + X1 + X2", "P + Xs1 + X1 + X2", # equation specifications
#'   simulated_data, # data
#'   use_correlated_shocks = TRUE # allow shocks to be correlated
#' )
#'
#' # estimate the model object (by default the maximum optimization is using BFGS)
#' est <- estimate(model)
#'
#' # estimate the model by specifying the optimization details that are passed to the optimizer.
#' est <- estimate(model, control = list(reltol = 1e-4), method = "BFGS")
#' }
#' @export
setGeneric("estimate", function(object, ...) {
  standardGeneric("estimate")
})

#' @describeIn estimate Full information maximum likelihood estimation.
#' @param use_numerical_gradient If true, the gradient is calculated numerically. By default,
#' all the models are estimated using the analytic expressions of their likelihoods'
#' gradients.
#' @param use_numerical_hessian If true, the variance-covariance matrix is calculated using
#' the numerically approximated Hessian. Calculated Hessians are only available for the basic
#' and directional models.
#' @param use_heteroscedastic_errors If true, the variance-covariance matrix is
#' calculated using heteroscedasticity adjusted (Huber-White) standard errors.
#' @param cluster_errors_by A vector with names of variables belonging in the data of the
#' model. If the vector is supplied, the variance-covariance matrix is calculated by
#' grouping the score matrix based on the passed variables.
setMethod(
    "estimate", signature(object = "market_model"),
    function(object, use_numerical_gradient = FALSE, use_numerical_hessian = TRUE,
             use_heteroscedastic_errors = FALSE, cluster_errors_by = NA, ...) {
        va_args <- list(...)

        va_args$skip.hessian <- !use_numerical_hessian

        va_args$start <- prepare_initializing_values(object, va_args$start)

        if (is.null(va_args$method)) {
            va_args$method <- "BFGS"
        }

        va_args$minuslogl <- function(...) minus_log_likelihood(object, ...)
        bbmle::parnames(va_args$minuslogl) <- get_likelihood_variables(object@system)
        if (!use_numerical_gradient) {
            va_args$gr <- function(...) gradient(object, ...)
            bbmle::parnames(va_args$gr) <- get_likelihood_variables(object@system)
        }

        est <- do.call(bbmle::mle2, va_args)
        est@call.orig <- call("bbmle::mle2", va_args)

        if ((object@model_type_string %in% c("Basic", "Directional")) && va_args$skip.hessian) {
            print_verbose(object@logger, "Calculating hessian and variance-covariance matrix.")
            est@details$hessian <- hessian(object, est@coef)
            tryCatch(
                est@vcov <- MASS::ginv(est@details$hessian),
                error = function(e) print_warning(object@logger, e$message)
            )
        }

        if (use_heteroscedastic_errors) {
            est <- set_heteroscedasticity_consistent_errors(object, est)
        }

        if (!is.na(cluster_errors_by)) {
            est <- set_clustered_errors(object, est, cluster_errors_by)
        }

        est
    }
)


#' Maximize the log-likelihood.
#'
#' Maximizes the log-likelihood using the
#' \href{https://www.gnu.org/software/gsl/doc/html/multimin.html}{\code{GSL}} implementation of
#' the BFGS algorithm. This function is primarily intended for advanced usage. The
#' \code{\link{estimate}} functionality is a fast, analysis-oriented alternative. If
#' the \href{https://www.gnu.org/software/gsl/doc/html/multimin.html}{\code{GSL}} is not
#' available, the function returns a trivial result list with status set equal to -1. If the
#' \href{https://en.cppreference.com/w/cpp/algorithm/execution_policy_tag_t}{C++17
#' execution policies}
#' are available, the implementation of the optimization is parallelized.
#' @param object A model object.
#' @param start Initializing vector.
#' @param step Optimization step.
#' @param objective_tolerance Objective optimization tolerance.
#' @param gradient_tolerance Gradient optimization tolerance.
#' @return A list with the optimization output.
#' @rdname maximize_log_likelihood
#' @seealso estimate
#' @examples
#' \donttest{
#' simulated_data <- simulate_model_data(
#'     "equilibrium_model", 500, 3, # model type, observed entities, observed time points
#'     -0.9, 14.9, c(0.3, -0.2), c(-0.03, -0.01), # demand coefficients
#'     0.9, 3.2, c(0.03), c(0.05, 0.02) # supply coefficients
#' )
#'
#' # initialize the model
#' model <- new(
#'     "equilibrium_model", # model type
#'     c("id", "date"), "Q", "P", # keys, quantity, and price variables
#'     "P + Xd1 + Xd2 + X1 + X2", "P + Xs1 + X1 + X2", # equation specifications
#'     simulated_data, # data
#'     use_correlated_shocks = TRUE # allow shocks to be correlated
#' )
#'
#' # maximize the model's log-likelihood
#' mll <- maximize_log_likelihood(
#'     model,
#'     start = NULL, step = 1e-5,
#'     objective_tolerance = 1e-4, gradient_tolerance = 1e-3
#' )
#' }
#' @export
setGeneric("maximize_log_likelihood", function(object, start, step, objective_tolerance,
                                               gradient_tolerance) {
  standardGeneric("maximize_log_likelihood")
})

#' Likelihood scores.
#'
#' It calculates the gradient of the likelihood at the given parameter point for each
#' observation in the sample. It, therefore, returns an n x k matrix, where n denotes
#' the number of observations in the sample and k the number of estimated parameters.
#' There order of the parameters is the same as the one that is used in the summary
#' of the results.
#' @param object A model object.
#' @param parameters A vector with model parameters.
#' @return The score matrix.
#' @rdname scores
#' @examples
#' \donttest{
#' simulated_data <- simulate_model_data(
#'   "diseq_basic", 500, 3, # model type, observed entities, observed time points
#'   -0.9, 8.9, c(0.3, -0.2), c(-0.03, -0.01), # demand coefficients
#'   0.9, 4.2, c(0.03), c(-0.05, 0.02) # supply coefficients
#' )
#'
#' # initialize the model
#' model <- new(
#'   "diseq_basic", # model type
#'   c("id", "date"), "Q", "P", # keys, quantity, and price variables
#'   "P + Xd1 + Xd2 + X1 + X2", "P + Xs1 + X1 + X2", # equation specifications
#'   simulated_data, # data
#'   use_correlated_shocks = TRUE # allow shocks to be correlated
#' )
#'
#' # estimate the model object (by default the maximum optimization is using BFGS)
#' est <- estimate(model)
#'
#' # Calculate the score matrix
#' scores <- scores(model, est@coef)
#' }
#' @export
setGeneric("scores", function(object, parameters) {
  standardGeneric("scores")
})

setGeneric("set_heteroscedasticity_consistent_errors", function(object, ...) {
  standardGeneric("set_heteroscedasticity_consistent_errors")
})

setGeneric("set_clustered_errors", function(object, ...) {
  standardGeneric("set_clustered_errors")
})

#' Model description.
#'
#' A unique identifying string for the model.
#' @param object A model object.
#' @return A string representation of the model.
#' @rdname get_model_description
#' @export
setGeneric("get_model_description", function(object) {
  standardGeneric("get_model_description")
})

#' Number of observations.
#'
#' Returns the number of observations that are used by an initialized model. The number of
#' used observations may differ
#' from the numbers of observations of the data set that was passed to the model's initialization.
#' @param object A model object.
#' @return The number of used observations.
#' @rdname get_number_of_observations
#' @export
setGeneric("get_number_of_observations", function(object) {
  standardGeneric("get_number_of_observations")
})


setGeneric("get_descriptives", function(object, variables) {
  standardGeneric("get_descriptives")
})

#' Demand descriptive statistics
#'
#' Calculates and returns basic descriptive statistics for the model's demand data. Factor
#' variables are excluded from the calculations.
#' @param object A model object.
#' @return A data \code{tibble} containing descriptive statistics.
#' @rdname get_demand_descriptives
#' @export
setGeneric("get_demand_descriptives", function(object) {
  standardGeneric("get_demand_descriptives")
})

#' Supply descriptive statistics
#'
#' Calculates and returns basic descriptive statistics for the model's supply data. Factor
#' variables are excluded from  the calculations.
#' @param object A model object.
#' @return A data \code{tibble} containing descriptive statistics.
#' @rdname get_supply_descriptives
#' @export
setGeneric("get_supply_descriptives", function(object) {
  standardGeneric("get_supply_descriptives")
})

setMethod(
    "set_heteroscedasticity_consistent_errors", signature(object = "market_model"),
    function(object, est) {
        est@details$original_hessian <- est@details$hessian
        scores <- scores(object, est@coef)
        nobs <- nrow(scores)
        adjustment <- MASS::ginv(t(scores) %*% scores) / nobs
        est@details$hessian <- est@details$hessian %*% adjustment %*% est@details$hessian
        est@vcov <- MASS::ginv(est@details$hessian)
        est
    }
)

setMethod(
    "set_clustered_errors", signature(object = "market_model"),
    function(object, est, cluster_errors_by) {
        if (!(cluster_errors_by %in% names(object@model_tibble))) {
            print_error(
                object@logger, "Cluster variable is not among model data variables."
            )
        }
        cluster_var <- rlang::syms(cluster_errors_by)
        est@details$original_hessian <- est@details$hessian
        clustered_scores <- tibble::tibble(
            object@model_tibble %>% dplyr::select(!!!cluster_var),
            tibble::as_tibble(scores(object, est@coef))
        ) %>%
            dplyr::group_by(!!!cluster_var) %>%
            dplyr::summarise_all(~ sum(.) / sqrt(length(.))) %>%
            dplyr::ungroup() %>%
            dplyr::select(!(!!!cluster_var)) %>%
            as.matrix()
        nobs <- nrow(object@model_tibble)
        npars <- ncol(clustered_scores)
        ncls <- object@model_tibble %>%
            dplyr::distinct(!!!cluster_var) %>%
            dplyr::count() %>%
            as.integer()
        adjustment <- MASS::ginv(t(clustered_scores) %*% clustered_scores) / ncls
        est@details$hessian <- est@details$hessian %*% adjustment %*% est@details$hessian
        est@vcov <- MASS::ginv(est@details$hessian)
        est
    }
)

#' @rdname get_model_description
setMethod("get_model_description", signature(object = "market_model"), function(object) {
    paste0(
        object@model_type_string, " with ",
        ifelse(object@system@correlated_shocks, "correlated", "independent"), " shocks"
    )
})

#' @rdname get_number_of_observations
setMethod("get_number_of_observations", signature(object = "market_model"), function(object) {
    nrow(object@model_tibble)
})

setMethod(
    "get_descriptives", signature(object = "market_model"),
    function(object, variables = NULL) {
        if (is.null(variables)) {
            variables <- object@columns
        }
        variables <- variables[sapply(variables, function(c) !is.factor(object@model_tibble[, c]))]

        tibble::as_tibble(apply(
            object@model_tibble[, variables], 2,
            function(x) {
                c(
                    nobs = length(x), nmval = sum(is.na(x)),
                    min = min(x), max = max(x), range = max(x) - min(x),
                    sum = sum(x), median = median(x), mean = mean(x),
                    mean_se = sqrt(var(x) / length(x)),
                    mean_ce = qnorm(0.975) * sqrt(var(x) / length(x)),
                    var = var(x), sd = sd(x), coef_var = sd(x) / mean(x)
                )
            }
        ), rownames = "col")
    }
)

#' @rdname get_demand_descriptives
setMethod("get_demand_descriptives", signature(object = "market_model"), function(object) {
    get_descriptives(object, object@system@demand@independent_variables)
})

#' @rdname get_supply_descriptives
setMethod("get_supply_descriptives", signature(object = "market_model"), function(object) {
    get_descriptives(object, object@system@supply@independent_variables)
})

setGeneric("calculate_initializing_values", function(object) {
    standardGeneric("calculate_initializing_values")
})

setMethod("calculate_initializing_values", signature(object = "market_model"), function(object) {
    dlm <- object@system@demand@linear_model

    slm <- object@system@supply@linear_model

    ## Set demand initializing values
    varloc <-
        !(get_prefixed_independent_variables(object@system@demand) %in% names(dlm$coefficients))
    if (sum(varloc) > 0) {
        print_error(
            object@logger,
            "Misspecified model matrix. ",
            "The matrix should contain all the variables except the variance."
        )
    }
    if (any(is.na(dlm$coefficients))) {
        print_warning(
            object@logger,
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
    varloc <-
        !(get_prefixed_independent_variables(object@system@supply) %in% names(slm$coefficients))
    if (sum(varloc) > 0) {
        print_error(
            object@logger,
            "Misspecified model matrix. ",
            "The matrix should contain all the variables except the variance."
        )
    }
    if (any(is.na(slm$coefficients))) {
        print_warning(
            object@logger,
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
        get_prefixed_variance_variable(object@system@demand),
        get_prefixed_variance_variable(object@system@supply)
    )

    if (object@system@correlated_shocks) {
        start <- c(start, rho = 0)
        names(start)[length(start)] <- get_correlation_variable(object@system)
    }

    start
})

setGeneric("prepare_initializing_values", function(object, initializing_vector) {
    standardGeneric("prepare_initializing_values")
})

setMethod(
    "prepare_initializing_values", signature(object = "market_model"),
    function(object, initializing_vector) {
        if (is.null(initializing_vector)) {
            print_verbose(object@logger, "Initializing using linear regression estimations.")
            initializing_vector <- calculate_initializing_values(object)
        }
        names(initializing_vector) <- get_likelihood_variables(object@system)
        print_debug(
            object@logger, "Using starting values: ",
            paste(names(initializing_vector), initializing_vector, sep = " = ", collapse = ", ")
        )

        initializing_vector
    }
)


#' Demand aggregation.
#'
#' Calculates the sample's aggregate demand at the passed set of parameters.
#' @param object A model object.
#' @param parameters A vector of model's parameters.
#' @return The sum of the demanded quantities evaluated at the given parameters.
#' @rdname get_aggregate_demand
#' @seealso get_demanded_quantities
#' @examples
#' \donttest{
#' simulated_data <- simulate_model_data(
#'   "diseq_basic", 500, 3, # model type, observed entities, observed time points
#'   -0.9, 8.9, c(0.3, -0.2), c(-0.03, -0.01), # demand coefficients
#'   0.9, 4.2, c(0.03), c(-0.05, 0.02) # supply coefficients
#' )
#'
#' # initialize the model
#' model <- new(
#'   "diseq_basic", # model type
#'   c("id", "date"), "Q", "P", # keys, quantity, and price variables
#'   "P + Xd1 + Xd2 + X1 + X2", "P + Xs1 + X1 + X2", # equation specifications
#'   simulated_data, # data
#'   use_correlated_shocks = TRUE # allow shocks to be correlated
#' )
#'
#' # estimate the model object
#' est <- estimate(model)
#'
#' # get estimated aggregate demand
#' get_aggregate_demand(model, est@coef)
#' }
#' @export
setGeneric("get_aggregate_demand", function(object, parameters) {
  standardGeneric("get_aggregate_demand")
})

#' @rdname get_aggregate_demand
setMethod("get_aggregate_demand", signature(object = "market_model"), function(object, parameters) {
    object@system <- set_parameters(object@system, parameters)
    get_aggregate(object@system@demand)
})

#' Demanded quantities.
#'
#' Calculates the demanded quantity for each observation.
#' @param object A model object.
#' @param parameters A vector of model's parameters.
#' @return A vector with the demanded quantities evaluated at the given parameter vector.
#' @rdname get_demanded_quantities
#' @seealso get_aggregate_demand
#' @examples
#' \donttest{
#' simulated_data <- simulate_model_data(
#'   "diseq_basic", 500, 3, # model type, observed entities, observed time points
#'   -0.9, 8.9, c(0.3, -0.2), c(-0.03, -0.01), # demand coefficients
#'   0.9, 4.2, c(0.03), c(-0.05, 0.02) # supply coefficients
#' )
#'
#' # initialize the model
#' model <- new(
#'   "diseq_basic", # model type
#'   c("id", "date"), "Q", "P", # keys, quantity, and price variables
#'   "P + Xd1 + Xd2 + X1 + X2", "P + Xs1 + X1 + X2", # equation specifications
#'   simulated_data, # data
#'   use_correlated_shocks = TRUE # allow shocks to be correlated
#' )
#'
#' # estimate the model object
#' est <- estimate(model)
#'
#' # get estimated demanded quantities
#' demq <- get_demanded_quantities(model, est@coef)
#' }
#' @export
setGeneric("get_demanded_quantities", function(object, parameters) {
  standardGeneric("get_demanded_quantities")
})

#' @rdname get_demanded_quantities
setMethod(
    "get_demanded_quantities", signature(object = "market_model"),
    function(object, parameters) {
        object@system <- set_parameters(object@system, parameters)
        get_quantities(object@system@demand)
    }
)

#' Supply aggregation.
#'
#' Calculates the sample's aggregate supply at the passed set of parameters.
#' @param object A model object.
#' @param parameters A vector of model's parameters.
#' @return The sum of the supplied quantities evaluated at the given parameters.
#' @rdname get_aggregate_supply
#' @seealso get_supplied_quantities
#' @examples
#' \donttest{
#' simulated_data <- simulate_model_data(
#'   "diseq_basic", 500, 3, # model type, observed entities, observed time points
#'   -0.9, 8.9, c(0.3, -0.2), c(-0.03, -0.01), # demand coefficients
#'   0.9, 4.2, c(0.03), c(-0.05, 0.02) # supply coefficients
#' )
#'
#' # initialize the model
#' model <- new(
#'   "diseq_basic", # model type
#'   c("id", "date"), "Q", "P", # keys, quantity, and price variables
#'   "P + Xd1 + Xd2 + X1 + X2", "P + Xs1 + X1 + X2", # equation specifications
#'   simulated_data, # data
#'   use_correlated_shocks = TRUE # allow shocks to be correlated
#' )
#'
#' # estimate the model object
#' est <- estimate(model)
#'
#' # get estimated aggregate supply
#' get_aggregate_supply(model, est@coef)
#' }
#' @export
setGeneric("get_aggregate_supply", function(object, parameters) {
  standardGeneric("get_aggregate_supply")
})

#' @rdname get_aggregate_supply
setMethod("get_aggregate_supply", signature(object = "market_model"), function(object, parameters) {
    object@system <- set_parameters(object@system, parameters)
    get_aggregate(object@system@supply)
})

#' Supplied quantities.
#'
#' Calculates the supplied quantity for each observation.
#' @param object A model object.
#' @param parameters A vector of model's parameters.
#' @return A vector with the supplied quantities evaluated at the given parameter vector.
#' @rdname get_supplied_quantities
#' @seealso get_aggregate_supply
#' @examples
#' \donttest{
#' simulated_data <- simulate_model_data(
#'   "diseq_basic", 500, 3, # model type, observed entities, observed time points
#'   -0.9, 8.9, c(0.3, -0.2), c(-0.03, -0.01), # demand coefficients
#'   0.9, 4.2, c(0.03), c(-0.05, 0.02) # supply coefficients
#' )
#'
#' # initialize the model
#' model <- new(
#'   "diseq_basic", # model type
#'   c("id", "date"), "Q", "P", # keys, quantity, and price variables
#'   "P + Xd1 + Xd2 + X1 + X2", "P + Xs1 + X1 + X2", # equation specifications
#'   simulated_data, # data
#'   use_correlated_shocks = TRUE # allow shocks to be correlated
#' )
#'
#' # estimate the model object
#' est <- estimate(model)
#'
#' # get estimated supplied quantities
#' supq <- get_supplied_quantities(model, est@coef)
#' }
#' @export
setGeneric("get_supplied_quantities", function(object, parameters) {
  standardGeneric("get_supplied_quantities")
})

#' @rdname get_supplied_quantities
setMethod(
    "get_supplied_quantities", signature(object = "market_model"),
    function(object, parameters) {
        object@system <- set_parameters(object@system, parameters)
        get_quantities(object@system@supply)
    }
)
