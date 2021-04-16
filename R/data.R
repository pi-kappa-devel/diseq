#' @importFrom rlang .data

#' @title Credit market data for US housing starts
#'
#' @details
#' \subsection{The basic houses dataset (\code{houses})}{
#'
#' A dataset containing the monthly mortgage rates and other attributes of the US market
#' for new, non-farm houses from July 1958 to December 1969. The variables are as
#' follows:
#'
#' \itemize{
#'   \item \code{DATE} The date of the record.
#'   \item \code{HS} Private non-farm housing starts in thousands of units
#' (Not seasonally adjusted).
#'   \item \code{RM} FHA Mortgage rate series on new homes in units of 100 (
#' beginning-of-month Data).
#'   \item \code{DSLA} Savings capital (deposits) of savings and loan associations in
#' millions of dollars.
#'   \item \code{DMSB} Deposits of mutual savings banks in millions of dollars.
#'   \item \code{DHLB} Advances of the federal home loan bank to savings and loan
#' associations in million of dollars.
#'   \item \code{W} Number of working days in month.
#' }
#' }
#' @references \itemize{
#'   \item Fair, R. C. (1971). A short-run forecasting model of the United States
#' economy. Heath Lexington Books.
#'   \item Fair, R. C., & Jaffee, D. M. (1972). Methods of Estimation for Markets in
#' Disequilibrium. Econometrica, 40(3), 497. \doi{10.2307/1913181}
#'   \item Maddala, G. S., & Nelson, F. D. (1974). Maximum Likelihood Methods for
#' Models of Markets in Disequilibrium. Econometrica, 42(6), 1013. \doi{10.2307/1914215}
#'   \item Hwang, H. (1980). A test of a disequilibrium model. Journal of Econometrics,
#' 12(3), 319–333. \doi{10.1016/0304-4076(80)90059-7}
#' }
#'
#' @source \itemize{
#'   \item \code{HS} \href{https://fraser.stlouisfed.org/title/economic-report-president-45?browse=1940s}{Economic Reports of the President}
#'   \item \code{RM} \href{https://fairmodel.econ.yale.edu/RAYFAIR/pdf/1971EI.PDF}{Fair (1971)}
#'   \item \code{DSLA} \href{https://fraser.stlouisfed.org/title/federal-reserve-bulletin-62?browse=1910s}{Federal Reserve Bulletins}
#'   \item \code{DMSB} \href{https://fraser.stlouisfed.org/title/federal-reserve-bulletin-62?browse=1910s}{Federal Reserve Bulletins}
#'   \item \code{DHLB} \href{https://fraser.stlouisfed.org/title/federal-reserve-bulletin-62?browse=1910s}{Federal Reserve Bulletins}
#'   \item \code{W} \href{https://www.timeanddate.com/date/workdays.html}{Manually calculated}
#' }
#'
#' @examples
#' data(houses)
#' head(houses)
#' @docType data
#' @keywords datasets
#' @name houses
#' @usage data(houses)
#' @format A data frame with 138 rows and 7 columns
NULL

#' @details
#' \subsection{Generate the variables of the Fair & Jaffee (1972) dataset.
#' (\code{fair_houses})}{
#'
#' Loads the \code{\link{houses}} dataset and creates the additional variables used by
#' Fair & Jaffee (1972) \doi{10.2307/1913181}. These are
#' \itemize{
#'   \item \code{ID} A dummy entity identifier that is always equal to one since the
#' houses data have only a time series component.
#'   \item \code{DSF} Flow of deposits in savings and loan associations and mutual
#' savings banks in million of dollars. Equal to \deqn{DSLA_{t} + DMSB_{t} - (DSLA_{t-1}
#' + DMSB_{t-1}).}
#'   \item \code{DHF} Flow of advances of the federal home loan bank to savings and loan
#' associations in million of dollars. Equal to \deqn{DHLB_{t} - DHLB_{t-1}.}
#'   \item \code{MONTH} The month of the date of the observation.
#'   \item \code{L1RM} FHA Mortgage rate series on new homes in units of 100, lagged by
#' one date.
#'   \item \code{L2RM} FHA Mortgage rate series on new homes in units of 100, lagged by
#' two dates.
#'   \item \code{L1HS} Private non-farm housing starts in thousands of units (Not
#' seasonally adjusted), lagged by one date.
#'   \item \code{CSHS} The cumulative sum of past housing starts. Used to proxy the
#' stock of houses
#'   \item \code{MA6DSF} Moving average of order 6 of the flow of deposits in savings
#' associations and loan associations and  mutual savings banks.
#'   \item \code{MA3DHF} Moving average of order 3 of the flow of advances of the
#' federal home loan bank to savings and loan associations.
#'   \item \code{TREND} A time trend variable.
#' }
#'
#' Returns A modified version of the \code{houses} dataset.
#' }
#' @examples
#' head(fair_houses())
#' @describeIn houses Generate Fair & Jaffee (1972) dataset
#' @export
fair_houses <- function() {
  houses <- diseq::houses %>%
    dplyr::mutate(
      ID = 1,
      DSF = .data$DSLA + .data$DMSB - dplyr::lag(.data$DSLA + .data$DMSB),
      DHF = .data$DHLB - dplyr::lag(.data$DHLB),
      MONTH = as.factor(sub("[0-9]{2}-([0-9]{2})-[0-9]{2}", "\\1", .data$DATE)),
      L2RM = dplyr::lag(.data$RM, 2),
      L1RM = dplyr::lag(.data$RM),
      L1HS = dplyr::lag(.data$HS),
      CSHS = cumsum(ifelse(is.na(.data$L1HS), 0, .data$L1HS)),
      MA6DSF = (dplyr::lag(.data$DSF, 6) + dplyr::lag(.data$DSF, 5) +
                dplyr::lag(.data$DSF, 4) + dplyr::lag(.data$DSF, 3) +
                dplyr::lag(.data$DSF, 2) + dplyr::lag(.data$DSF)) / 6,
      MA3DHF = (dplyr::lag(.data$DHF, 3) + dplyr::lag(.data$DHF, 2) +
                dplyr::lag(.data$DHF)) / 3,
    ) %>%
    dplyr::arrange(.data$DATE) %>%
    dplyr::mutate(TREND = 1:dplyr::n())
  houses
}
