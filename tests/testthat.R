library(testthat)
library(HyperG)

if (Sys.getenv("NOT_CRAN", "") == "true") {
	test_check("HyperG")
}
