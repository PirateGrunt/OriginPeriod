library(testthat)
library(devtools)
library(roxygen2)

roxygen2::roxygenize(clean=TRUE)
devtools::document(roclets=c('rd', 'collate', 'namespace'))
check_doc()

devtools::check(args = c('--as-cran'))

devtools::build()
devtools::build(vignettes = FALSE)
devtools::build_vignettes()
devtools::install()

test_dir("./inst/test/")
