#' Credit market data for US housing starts
#'
#' A dataset containing the monthly mortgage rates and other attributes of the US market
#' for new, non-farm houses from July 1958 to December 1969. The variables are as follows:
#'
#' \itemize{
#'   \item \code{DATE} The date of the record.
#'   \item \code{HS} Private non-farm housing starts in thousands of units (Not seasonally
#' adjusted).
#'   \item \code{RM} FHA Mortgage rate series on new homes in units of 100 (beginning-of-month
#' Data).
#'   \item \code{DSLA} Savings capital (deposits) of savings and loan associations in millions
#' of Dollars.
#'   \item \code{DMSB} Deposits of mutual savings banks in millions of dollars.
#'   \item \code{DHLB} Advances of the federal home loan bank to savings and loan associations
#' in million of dollars.
#'   \item \code{W} Number of working days in month.
#' }
#'
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
#'
#' @docType data
#' @keywords datasets
#' @name houses
#' @usage data(houses)
#' @format A data frame with 138 rows and 7 columns
NULL
