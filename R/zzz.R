.onLoad <- function(libname, pkgname) {
    message <- "Package diseq is deprecated. Please use package markets instead."
    rlang::warn(message, .frequency = "regularly",
        .frequency_id = "5df0d8b03677836d7b25325e42ea5270")

    invisible()
}
