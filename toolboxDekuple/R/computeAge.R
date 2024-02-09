#' @title Compute age
#'
#' @description Compute the age in years based on a start date and a end date
#'
#' @param start_date The origin date
#' @param end_date The end date.
#' @examples
#' \dontrun{
#'
#' library(toolboxDekuple)
#'
#' # With default end_date
#' compute_age(start_date = "2020-01-01")
#'
#' # With specific end_date
#' compute_age(start_date = "2020-01-01", end_date = "2024-01-01")
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom assertthat assert_that
#' @importFrom lubridate ymd month day interval
#' @export
#'
compute_age <- function(start_date, end_date = Sys.Date()) {

  start_date <- ymd(start_date)
  end_date <- ymd(end_date)

  assert_that(!is.na(start_date), msg = "Not a date format")
  assert_that(!is.na(end_date), msg = "Not a date format")
  assert_that(month(start_date) %in% seq(1,12), msg = "Not a valid month")
  assert_that(month(end_date) %in% seq(1,12), msg = "Not a valid month")
  assert_that(day(start_date) %in% seq(1,31), msg = "Not a valid day")
  assert_that(day(end_date) %in% seq(1,31), msg = "Not a valid day")

  if (start_date > end_date) {
    stop("start_date is supposed to be older than the end_date")
  }

  return(as.integer(interval(start_date, end_date)  %>% as.numeric('years')))
}
