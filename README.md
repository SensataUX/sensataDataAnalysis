
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sensataDataAnalysis <img src="man/figures/sensata-small-logo-512x512.png" align="right" width="120" />

<!-- badges: start -->
<!-- badges: end -->

This package has the functions used for Sensata data analysis

Este paquete tiene las funciones necesarias para analizar los datos de
sensata

## Installation

``` r
# install.packages("devtools")
# install.packages("tidyverse")


devtools::install_github(repo = "https://github.com/SensataUx/sensataDataAnalysis", ref = "main", build_vignettes = T)
```

## Basic guide

Con estos comandos pueden acceder a una guía básica de R y de este
paquete

``` r
vignette("basicGuide", package = "sensataDataAnalysis")
```

## Example

TODO: Create example

``` r
library(sensataDataAnalysis)
# this automatically loads tidyverse and labelled
sensataExAnalysis <- sensataExAnalysis

# create graphData 5 options (likert)
createGraphData(df = sensataExAnalysis, originVar = "q_EA_IN_03", groupVar = "q_EA_CA_10")

# create graphData 4 options (text)
createGraphData(df = sensataExAnalysis, originVar = "q_EA_IN_03", groupVar = "q_EA_CA_10")
```

## TO-DO

-   Falta hacer todo!
-   Create vignettes
-   Create tests
