#' GSI Model Code
#'
#' Returns a string of the JAGS model code
#' defining the GSI spawn timing model.
#'
#' @return A string of the JAGS model code.
#' @seealso \code{\link{mwst2}}
#' @examples
#' cat(gsi_model_code())
#' @export
gsi_model_code <- function () {
"model {
  sTiming ~ dunif(0, 30) # \\sigma_S
  bPeak ~ dnorm(0, 30^-2) # \\mu_S
  bPreGSIMale ~ dnorm(-3, 5^-2) # \\beta^M
  bPreGSIFemale ~ dnorm(-3, 5^-2) # \\beta^F
  bPreGSIFemaleDate ~ dnorm(0, 5^-2) # \\beta2^F
  bPostGSI ~ dnorm(-3, 5^-2) # \\alpha

  sGSI ~ dunif(0, 5)
  for (i in 1:length(Dayte)) {
    eSpawning[i] <- pnorm(Dayte[i], bPeak, sTiming^-2)

    eSpawnedMale[i] <- eSpawning[i]
    eSpawnedFemale[i] ~ dbern(eSpawning[i])

    ePreGSIMale[i] <- bPreGSIMale
    ePreGSIFemale[i] <- bPreGSIFemale + bPreGSIFemaleDate * Dayte[i]

    eSpawned[i] <- ifelse(Sex[i] == 1, eSpawnedMale[i], eSpawnedFemale[i])
    ePreGSI[i] <- ifelse(Sex[i] == 1, ePreGSIMale[i], ePreGSIFemale[i])

    eGSI[i] <- (1 - eSpawned[i]) * exp(ePreGSI[i]) + eSpawned[i] * exp(bPostGSI)
    GSI[i] ~ dlnorm(log(eGSI[i]) - sGSI^2 / 2, sGSI^-2)
  }
} "
}

#' Analyse GSI
#'
#' Analyses changes in GSI by Sex and Dayte using a Bayesian
#' spawn timing model.
#'
#' To view the JAGS model code see \code{\link{gsi_model_code}}.
#' For more information see the manuscript associated with this package.
#'
#' @param x A data.frame with GSI, Sex and Dayte values.
#' @param niter A count of the minimum number of MCMC iterations to
#' perform.
#' @return A jags_analysis object.
#' @seealso \code{\link{mwst2}}
#' @export
analyse_gsi <- function (x, niter = 10^4) {
  assert_that(is.data.frame(x))
  assert_that(is.count(niter) && noNA(niter))

  check_columns(x, c("Dayte", "Sex", "GSI"))

  check_class_columns(x, list("Dayte" = "Date",
                              "Sex" = "factor",
                              "GSI" = "numeric"))

  stopifnot(identical(levels(x$Sex), c("Male", "Female")))
  stopifnot(max(x$GSI, na.rm = TRUE) > 0.05)
  stopifnot(max(x$GSI, na.rm = TRUE) < 0.5)

  x <- dplyr::select_(x, ~Dayte, ~Sex, ~GSI)

  jmodels <- function () {
    jaggernaut::jags_model(
      gsi_model_code(),
      derived_code = "data {
    for (i in 1:length(Dayte)) {
      eProportion[i] <- pnorm(Dayte[i], bPeak, sTiming^-2)

      eSpawnedMale[i] <- eProportion[i]
      eSpawnedFemale[i] <- eProportion[i]

      ePreGSIMale[i] <- bPreGSIMale
      ePreGSIFemale[i] <- bPreGSIFemale + bPreGSIFemaleDate * Dayte[i]

      eSpawned[i] <- ifelse(Sex[i] == 1, eSpawnedMale[i], eSpawnedFemale[i])
      ePreGSI[i] <- ifelse(Sex[i] == 1, ePreGSIMale[i], ePreGSIFemale[i])

      eGSI[i] <- (1 - eSpawned[i]) * exp(ePreGSI[i]) + eSpawned[i] * exp(bPostGSI)

      ePeak[i] <- bPeak + Dayte_MU
      eStart[i] <- qnorm(0.025, bPeak, sTiming^-2) + Dayte_MU
      eEnd[i] <- qnorm(0.975, bPeak, sTiming^-2) + Dayte_MU
    }
    prediction <- eGSI
    residual <- (log(GSI) - log(eGSI) - sGSI^2 / 2) / sGSI
  } ",
      modify_data = function (data) {
        data$nSex <- NULL
        data
      },
    select_data = c("Dayte+", "Sex", "GSI")
  )
}
  jaggernaut::jags_analysis(jmodels(), x, niter = niter)
}

#' Spawners Model Code
#'
#' Returns a string of the JAGS model code
#' defining the spawner count spawn timing model.
#'
#' @return A string of the JAGS model code.
#' @seealso \code{\link{mwst2}}
#' @examples
#' cat(spawners_model_code())
#' @export
spawners_model_code <- function () {
"model {
  sTiming ~ dunif(0, 30) # \\sigma_S
  bPeak ~ dnorm(0, 30^-2) # \\mu_S
  bScaling ~ dnorm(10, 5^-2) # \\log(c)

  sSpawners ~ dunif(0, 5)
  for (i in 1:length(Dayte)) {
    eSpawning[i] <- dnorm(Dayte[i], bPeak, sTiming^-2)
    eSpawners[i] <- eSpawning[i] * exp(bScaling)
    Spawners[i] ~ dlnorm(log(eSpawners[i]) - sSpawners^2 / 2, sSpawners^-2)
  }
} "
}

#' Analyse Spawners
#'
#' Analyses changes in Spawners with Dayte using a Bayesian
#' spawn timing model.
#'
#' To view the JAGS model code see \code{\link{spawners_model_code}}
#' For more information see the manuscript associated with this package.
#'
#' @param x A data.frame with Spawners and Dayte values.
#' @param niter A count of the minimum number of MCMC iterations to
#' perform.
#' @return A jags_analysis object.
#' @seealso \code{\link{mwst2}}
#' @export
analyse_spawners <- function (x, niter = 10^4) {
  assert_that(is.data.frame(x))
  assert_that(is.count(niter) && noNA(niter))

  check_columns(x, c("Dayte", "Spawners"))

  check_class_columns(x, list("Dayte" = "Date",
                              "Spawners" = "integer"))

  x <- dplyr::select_(x, ~Dayte, ~Spawners)

  jmodels <- function () {
    jaggernaut::jags_model(
      spawners_model_code(),
      derived_code = "data {
    for (i in 1:length(Dayte)) {
      eSpawning[i] <- dnorm(Dayte[i], bPeak, sTiming^-2)

      ePeak[i] <- bPeak + Dayte_MU
      eStart[i] <- qnorm(0.025, bPeak, sTiming^-2) + Dayte_MU
      eEnd[i] <- qnorm(0.975, bPeak, sTiming^-2) + Dayte_MU
    }
    prediction <- eSpawning * exp(bScaling)
    residual <- (log(Spawners) - (log(prediction) - sSpawners^2 / 2)) / sSpawners
  } ",
    select_data = c("Dayte+", "Spawners")
  )
}
  jaggernaut::jags_analysis(jmodels(), x, niter = niter)
}
