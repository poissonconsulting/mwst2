#' Plot Gonadal Somatic Index (GSI)
#'
#' Plots GSI by Dayte.
#'
#' @param x A data.frame with GSI and Dayte values.
#' @return A \code{\link{ggplot}} scatterplot.
#' @seealso \code{\link{mwst2}}
#' @examples
#' data(gsi)
#' plot_gsi(gsi)
#' @export
plot_gsi <- function(x) {

  assert_that(is.data.frame(x))

  check_rows(x)
  check_columns(x, c("Dayte", "GSI"))

  check_class_columns(x, list("Dayte" = "Date",
                              "GSI" = "numeric"))

  ggplot2::ggplot(data = x, ggplot2::aes_string(x = "Dayte", y = "GSI")) +
    ggplot2::geom_point() +
    ggplot2::scale_x_date("Date", labels = scales::date_format("%b")) +
    ggplot2::scale_y_continuous("Gonadosomatic Index (%)", label = scales::percent) +
    ggplot2::expand_limits(y = 0)
}

#' Plot Spawners
#'
#' Plots Spawners by Dayte.
#'
#' @param x A data.frame with Spawner and Dayte values.
#' @return A \code{\link{ggplot}} scatterplot.
#' @seealso \code{\link{mwst2}}
#' @examples
#' data(spawners)
#' plot_spawners(spawners)
#' @export
plot_spawners <- function (x) {

  assert_that(is.data.frame(x))

  check_rows(x)
  check_columns(x, c("Dayte", "Spawners"))

  check_class_columns(x, list("Dayte" = "Date",
                              "Spawners" = "integer"))

  ggplot2::ggplot(data = x, ggplot2::aes_string(x = "Dayte", y = "Spawners")) +
    ggplot2::geom_point() +
    ggplot2::scale_x_date("Date", labels = scales::date_format("%b %d")) +
    ggplot2::scale_y_continuous("Spawner Count") +
    ggplot2::expand_limits(y = 0)
}

#' Plot Spawn Timing
#'
#' Plots spawn timing predictions (start, peak and end) by Year.
#'
#' @param x A data.frame with Event, estimate, lower, upper and Year values.
#' @return A \code{\link{ggplot}} point range plot.
#' @seealso \code{\link{mwst2}}
#' @export
plot_timing <- function(x) {
  assert_that(is.data.frame(x))

  x$Year %<>% factor

  ggplot2::ggplot(data = x, ggplot2::aes_string(x = "Year", y = "estimate")) +
    ggplot2::geom_pointrange(ggplot2::aes_string(ymin = "lower", ymax = "upper",
                                        color = "Event", shape = "Event"),
                             position = ggplot2::position_dodge(width = 0.10)) +
    ggplot2::scale_y_date("Date", labels = scales::date_format("%b %d")) +
    ggplot2::scale_color_manual(values = c("blue", "black", "red")) +
    ggplot2::guides(color = ggplot2::guide_legend(reverse = TRUE),
                    shape = ggplot2::guide_legend(reverse = TRUE))
}

#' Plot Residuals
#'
#' Plots standardised residuals by fitted values.
#'
#' @param x The \code{\link{jags_analysis}} model.
#' @return A \code{\link{ggplot}} residual plot.
#' @seealso \code{\link{mwst2}}
#' @export
plot_residuals <- function(x) {
  assert_that(jaggernaut::is.jags_analysis(x))

  x %<>% stats::residuals()

  ggplot2::ggplot(data = x, ggplot2::aes_string(x = "Dayte", y = "estimate")) +
    ggplot2::geom_point() + ggplot2::xlab("Date") + ggplot2::ylab("Standardised Residual")
}

#' Add Fit Lines
#'
#' Adds estimated and lower and upper fit lines to a \code{\link{ggplot}} scatterplot.
#'
#' @param x A \code{\link{ggplot}} scatterplot.
#' @param data A data.frame with Dayte, estimate, lower and upper values.
#' @return A \code{\link{ggplot}} scatterplot with fit lines.
#' @seealso \code{\link{mwst2}}
#' @export
add_fit_lines <- function(x, data) {
  assert_that(ggplot2::is.ggplot(x))
  assert_that(is.data.frame(data))
  x + ggplot2::geom_line(data = data, ggplot2::aes_string(y = "estimate")) +
  ggplot2::geom_line(data = data, ggplot2::aes_string(y = "lower"), linetype = "dotted") +
  ggplot2::geom_line(data = data, ggplot2::aes_string(y = "upper"), linetype = "dotted")
}

