#' Date To Integer
#'
#' Converts Dates to integers where
#' the 1st of Jan 2000 is equal to 1.
#'
#' @param x a Date.
#' @return An integer.
#' @examples
#' date2integer(integer2date(-1:3))
#' @export
date2integer <- function(x) {
  as.integer(as.Date(x)) - as.integer(as.Date("1999-12-31"))
}

#' Integer To Date
#'
#' Converts integers to Dates where
#' the 1st of Jan 2000 is equal to 1.
#'
#' @param x an integer
#' @return A Date.
#' @examples
#' integer2date(-1:3)
#' @export
integer2date <- function(x) {
  as.Date("1999-12-31") + as.integer(x)
}

