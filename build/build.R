library(testthat)
library(devtools)

roxygen2::roxygenize(clean=TRUE)
devtools::build()
devtools::build_vignettes()
devtools::install()
library(OriginPeriod)
test_dir("./inst/test/")

check_doc()
devtools::check(args = c('--as-cran'))
