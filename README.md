# Mountain Whitefish Spawn Timing R Analysis Package

`mwst2` is the companion R analysis package for Irvine et al.'s manuscript
*When Do Mountain Whitefish (Prosopium williamsoni) Spawn? A Comparison of Estimates Based on Gonadosomatic Indices and Spawner and Egg Counts*.

## Installation

To use the `mwst2` package first install 
[R](http://cran.r-project.org) (version 3.2.2 or greater) and
[JAGS](http://mcmc-jags.sourceforge.net) (version 3.4.0 or greater).

Then execute the following code at the R terminal:
```
library(devtools)
devtools::install_github(paste("poissonconsulting", sep = "/", c(
  "tulip@v0.0.13", "datalist@v0.5.0", "juggler@v0.1.5", "jaggernaut@v2.3.1",
  "mwstdatr", "mwst2")))
library(mwst2)
```
## Replication

To quickly replicate the results of the manuscript with *unreliable* model estimates use:
```
mwst2::replicate_results("debug")
```
This option is useful for testing everything is installed correctly.

To replicate the results with **reliable** model estimates use:
```
mwst2::replicate_results("report")
```

To replicate the results with the same analysis settings and
figure font family as the manuscript:
```
extrafont::font_import() # if you haven't done this already on your machine
mwst2::replicate_results("paper", parallel = TRUE, base_family = "Arial")
```

## Information

For more information type `?replicate_results` after loading the package.
