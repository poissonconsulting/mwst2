#' Predict Gonadal Somatic Index (GSI)
#'
#' Estimates GSI (with lower and upper 95\% CRIs)
#' by Sex and Dayte.
#'
#' @param x The \code{\link{jags_analysis}} model.
#' @return A data.frame of the estimates.
#' @seealso \code{\link{mwst2}}
#' @export
predict_gsi <- function (x) {
  assert_that(jaggernaut::is.jags_analysis(x))
  x <- predict(x, newdata = c("Dayte", "Sex"))
  dplyr::select_(x, ~Dayte, ~Sex, ~estimate, ~lower, ~upper)
}

#' Predict Spawners
#'
#' Estimates spawner abundance (with lower and upper 95\% CRIs) by Dayte.
#'
#' @param x The \code{\link{jags_analysis}} model.
#' @return A data.frame of the estimates.
#' @seealso \code{\link{mwst2}}
#' @export
predict_spawners <- function (x) {
  assert_that(jaggernaut::is.jags_analysis(x))
  x <- predict(x, newdata = c("Dayte"))
  dplyr::select_(x, ~Dayte, ~estimate, ~lower, ~upper)
}

#' Predict Spawn Timing
#'
#' Estimates the start (2.5\% of spawning completed), peak (50\% completed)
#' and end (97.5\% completed) of spawning (all with lower and upper 95\% CRIs).
#'
#' @param x The \code{\link{jags_analysis}} model.
#' @return A data.frame of the estimates.
#' @seealso \code{\link{mwst2}}
#' @export
predict_timing <- function (x) {

  assert_that(jaggernaut::is.jags_analysis(x))

  start <- predict(x, parm = "eStart", newdata = "")
  peak <- predict(x, parm = "ePeak", newdata = "")
  end <- predict(x, parm = "eEnd", newdata = "")

  start$Event <- "Start"
  peak$Event <- "Peak"
  end$Event <- "End"

  timing <- rbind(start, peak, end)
  timing %<>% dplyr::select_(~Event, ~estimate, ~lower, ~upper)
  timing$estimate %<>% datalist::integer2date()
  timing$lower %<>% datalist::integer2date()
  timing$upper %<>% datalist::integer2date()
  timing$Event %<>% factor(levels = c("Start", "Peak", "End"))
  dplyr::arrange_(timing, ~Event)
}
