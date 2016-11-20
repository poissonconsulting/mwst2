[![DOI](https://zenodo.org/badge/44328995.svg)](https://zenodo.org/badge/latestdoi/44328995)
[![Travis-CI Build Status](https://travis-ci.org/poissonconsulting/mwst2.svg?branch=master)](https://travis-ci.org/poissonconsulting/mwst2)

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
