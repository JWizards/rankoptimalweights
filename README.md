
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rankoptimalweights

<!-- badges: start -->
<!-- badges: end -->

Rank Optimal Weights is an R package designed to take in a table of
indicators for competitors and then compute optimal weights for each
competitor to attain its best rank. This is done by calling SCIP which
can be installed with an academic license
[here](https://www.scipopt.org/index.php#download)

## Installation

You can install the development version of rankoptimalweights like so:
Ensure also that [SCIP](https://www.scipopt.org/index.php#download) is
installed and system callable.

``` r
library("devtools")
install()
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(rankoptimalweights)

countries <- read.csv("man/example_data/oecd_betterlife_2013.csv")
select_indicators <- countries[75:85]
solution_weights <- rank_best(select_indicators)

# loads example data (Countries by their indicators from oecd_betterlife index and selects a table of the relevent indicators)
```
