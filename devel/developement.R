# script with mixed function calls to help building and documenting

library(devtools)
library(sinew)
library(roxygen2)
library(pkgdown)

makeOxygen()
devtools::document()

devtools::test()

# devtools::use_vignette("my-vignette")

# dump(function_name, file = "../pmxplore/R/file_name.R")

pkgdown::build_site()
