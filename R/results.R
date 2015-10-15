#' Save Plot
#'
#' Saves the last plot displayed to plots as an .eps file.
#'
#' @param name A string indicating the file name (without .eps).
#' @param width A number indicating the width in inches.
#' @param height A number indicating the height in inches.
#' @export
save_plot <- function (name, width = 2.63, height = 2.63) {
  assert_that(is.string(name))
  assert_that(is.number(width))
  assert_that(is.number(height))

  if(grepl("[.]", name))
    stop("name should not include extension")

  base_family <- ggplot2::theme_get()$text$family
  file <- file.path(getOption("mwst2.dir", "results"), "plots", paste0(name, ".eps"))
  if(is.null(base_family) || base_family == "") {
    ggplot2::ggsave(file, width = width, height = height)
  } else {
    ggplot2::ggsave(file, width = width, height = height, family = base_family)
    extrafont::embed_fonts(file, options = "-dEPSCrop")
  }
  invisible()
}

#' Save PDF
#'
#' Plots the object to a .pdf file using plot_fun and saves to the pdf directory.
#'
#' @param x The object to plot.
#' @param name A string indicating the file name (without .pdf).
#' @param width A number indicating the width in inches.
#' @param height A number indicating the height in inches.
#' @param plot_fun A function to plot x.
#' @param ... Additional arguments passed to plot_fun.
#' @export
save_pdf <- function (x, name, width = 6, height = 6, plot_fun = plot, ...) {
  assert_that(is.string(name))
  assert_that(is.number(width))
  assert_that(is.number(height))
  assert_that(is.function(plot_fun))

  if(grepl("[.]", name))
    stop("name should not include extension")

  base_family <- ggplot2::theme_get()$text$family
  file <- file.path(getOption("mwst2.dir", "results"), "pdfs", paste0(name, ".pdf"))
  if(is.null(base_family) || base_family == "") {
    grDevices::pdf(file = file, width = width, height = height)
    plot_fun(x, ...)
    grDevices::dev.off()
  } else {
    grDevices::pdf(file = file, width = width, height = height, family = base_family)
    plot_fun(x, ...)
    grDevices::dev.off()
    extrafont::embed_fonts(file)
  }
  invisible()
}

#' Save RDS
#'
#' Saves the object to the rds dir as an .rds file.
#'
#' @param x The object to save.
#' @param name A string indicating the file name (without .rds).
#' @export
save_rds <- function(x, name) {

  if (missing(name))
    name <- deparse(substitute(x))

  assert_that(is.string(name))

  if(grepl("[.]", name))
    stop("name should not include extension")

  file <- file.path(getOption("mwst2.dir", "results"), "rds", paste0(name, ".rds"))

  saveRDS(x, file = file)
  invisible()
}

#' Create Directories
#'
#' Creates directory getOption("mwst2.dir") with the
#' subdirectories plots, rds and pdf to save plots and results.
#'
#' @export
create_dirs <- function () {
  dir.create(file.path(getOption("mwst2.dir", "results") ,"plots"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(getOption("mwst2.dir", "results") ,"pdfs"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(getOption("mwst2.dir", "results") ,"rds"), showWarnings = FALSE, recursive = TRUE)
  invisible()
}

#' Replicate Results
#'
#' Replicates the results by running the package demos.
#'
#' By default (\code{mode = "debug"}) the analyses are run in debug mode
#' which quickly generates non-convergent results (this is useful for testing
#' everything works before commiting to the full analyses).
#'
#' To run the analyses with sufficient iterations for convergence
#' with an R-hat value of 1.1. set \code{mode = "report"}. To run the
#' analyses with the same settings as used for the paper set
#' \code{mode = "paper"} and \code{base_family = "Arial"} after
#' executing \code{extrafont::font_import()}.
#'
#' @param mode A string specifying the mode for the analyses.
#' @param parallel A flag indicating whether to run the chains in parallel.
#' @param base_family A string specifying the font family.
#' @param dir A string specifying the directory to save the results.
#' @seealso \code{\link{mwst2}}
#' @export
replicate_results <- function(mode = "debug", parallel = FALSE, base_family = "", dir = getOption("mwst2.dir", "results")) {

  assert_that(is.string(mode))
  assert_that(is.flag(parallel))

  if (!mode %in% c("debug", "report", "paper"))
    stop("mode must be 'debug', 'report' or 'paper'")

  if(base_family != "") {
    extrafont::loadfonts(device = "pdf",  quiet = TRUE)
    extrafont::loadfonts(device = "postscript",  quiet = TRUE)
  }
  th <- ggplot2::theme_get()
  on.exit(ggplot2::theme_set(th), add = TRUE)
  ggplot2::theme_set(ggplot2::theme_bw(base_size = 8, base_family = base_family))
  ggplot2::theme_update(panel.grid = ggplot2::element_blank())

  op <- jaggernaut::opts_jagr(mode = mode)
  on.exit(jaggernaut::opts_jagr(op), add = TRUE)

  od <- getOption("mwst2.dir", "results")
  options(mwst2.dir = dir)
  on.exit(options(mwst2.dir = od), add = TRUE)

  if(parallel) {
    nworkers <- foreach::getDoParWorkers()
    if (nworkers < jaggernaut::opts_jagr()$nchains) {
      on.exit(doParallel::stopImplicitCluster(), add = TRUE)
      if (nworkers > 1) {
        on.exit(doParallel::registerDoParallel(nworkers), add = TRUE)
      }
      doParallel::stopImplicitCluster()
      doParallel::registerDoParallel(jaggernaut::opts_jagr()$nchains)
    }
  }
  jaggernaut::opts_jagr(parallel = parallel)

  demo("map", ask = FALSE)
  demo("timing", ask = FALSE)

  save_rds(mode, "mode")
  invisible()
}
