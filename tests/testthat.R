library(testthat)
library(SummaryStats)

# Run all tests including those in subdirectories
test_check('SummaryStats', reporter = "summary")
