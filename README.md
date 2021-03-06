
<!-- badges: start -->
[![DOI](https://zenodo.org/badge/44328995.svg)](https://zenodo.org/badge/latestdoi/44328995)
[![R build status](https://github.com/poissonconsulting/mwst2/workflows/R-CMD-check/badge.svg)](https://github.com/poissonconsulting/mwst2/actions)
<!-- badges: end -->

# Mountain Whitefish Spawn Timing R Analysis Package

`mwst2` is the companion R analysis package for Irvine et al.'s manuscript
*When Do Mountain Whitefish (Prosopium williamsoni) Spawn? A Comparison of Estimates Based on Gonadosomatic Indices and Spawner and Egg Counts*.

## Installation

To use the `mwst2` package first install 
[R](http://cran.r-project.org) (version 3.2.2 or greater) and
[JAGS](http://mcmc-jags.sourceforge.net) (version 3.4.0 or greater).

Then execute the following code at the R terminal:
```
install.packages("devtools") # if not already installed
devtools::install_github("poissonconsulting/mwst2")
library(mwst2)
```

## Usage

To quickly replicate the results with *unreliable* model estimates use:
```
replicate_results()
```
This option is useful for testing everything is installed correctly.

To replicate the results with **reliable** model estimates use:
```
replicate_results("report")
```

## Information

For more information type `?replicate_results` after loading the package.


## Code of Conduct

Please note that the mwst2 project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.

