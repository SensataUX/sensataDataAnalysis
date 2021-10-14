
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sensataDataAnalysis

<!-- badges: start -->
<!-- badges: end -->

This package has the functions used for Sensata data analysis

Este paquete tiene las funciones necesarias para analizar los datos de
sensata

## Installation

It will not be publish on CRAN, you can only install it from
[GitHub](https://github.com/) with a Personal Access Token (create one
that expires soon):

No se va a publicar en CRAN, solo se puede instalar de
[GitHub](https://github.com/) con un token de acceso personal (PAT):

``` r
# install.packages("devtools")
# install.packages("credentials")

credentials::set_github_pat(force_new = T, validate = T)

# you will get a prompt here, enter your github PAT
library(devtools)
install_github(repo = "https://github.com/GaborioSensata/sensataDataAnalysis", ref = "main", auth_token = github_pat())
```

## Example

TODO: Create example

``` r
library(sensataDataAnalysis)
# this automatically loads tidyverse and labelled
```

## TO-DO

-   Falta hacer todo!
-   Create vignettes
-   Create tests
