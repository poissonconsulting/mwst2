#' Yes or No
#'
#' Asks user a question and returns a TRUE or FALSE to indicate
#' whether user answered yes or no.
#' Modified from devtools package internal function so that yes returns TRUE.
#'
#' @param question a character scalar of the question to ask the user
#' @return A logical scalar indicating whether reponse was yes (TRUE) or no (FALSE).
yesno <- function(question) {
  yeses <- c("Yes", "Yup", "Yeah")
  nos <- c("No", "No way", "Nope")

  cat(question)
  qs <- c(sample(yeses, 1), sample(nos, 2))
  rand <- sample(length(qs))

  utils::menu(qs[rand]) == which(rand == 1)
}

#' Replicate Results
#'
#' Replicates the results by running the package demo. The results are saved
#' to a folder results in the working directory.
#'
#' By default (\code{mode = "debug"}) the analyses are run in debug mode
#' which quickly generates non-convergent results (this is useful for testing
#' everything works before commiting to the full analyses).
#'
#' To run the analyses with sufficient iterations for convergence
#' with an R-hat value of 1.1. set \code{mode = "report"}. To run the
#' analyses with the same settings as used for the paper set
#' \code{mode = "paper"}.
#'
#' @param mode A string specifying the mode for the analyses.
#' @param parallel A flag indicating whether to run the chains in parallel.
#' @param ask A flag indicating whether to ask before creating results directory.
#' @export
replicate_results <- function(mode = "debug", parallel = TRUE, ask = TRUE) {

  assert_that(is.string(mode))
  assert_that(is.flag(parallel))
  assert_that(is.flag(ask))

  requireNamespace("foreach")
  requireNamespace("doParallel")

  if (!mode %in% c("debug", "report", "paper"))
    stop("mode must be 'debug', 'report' or 'paper'")

  th <- ggplot2::theme_get()
  on.exit(ggplot2::theme_set(th), add = TRUE)
  ggplot2::theme_set(ggplot2::theme_bw(base_size = 8))
  ggplot2::theme_update(panel.grid = ggplot2::element_blank())

  op <- jaggernaut::opts_jagr(mode = mode)
  on.exit(jaggernaut::opts_jagr(op), add = TRUE)

  if (parallel) {
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

  if (!ask || yesno("Create a folder results in the working directory?")) {
    dir.create("results", showWarnings = FALSE, recursive = TRUE)

    saveRDS(mode, "results/mode.rds")

    utils::demo("map", ask = FALSE)
    utils::demo("timing", ask = FALSE)

  }
  invisible()
}
