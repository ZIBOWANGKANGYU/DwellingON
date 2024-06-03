shinytest2::test_app()

# Run unit tests
source("utils.R")

testthat::test_dir("tests/unit_tests")
