# This file contains unit tests for the functions in utils.R
# It should use the testthat package to run the tests. 
# Refer to this website: https://testthat.r-lib.org/ for more information on how to write tests.

# Load necessary packages
library(DT)
library(dplyr)
# Now, write tests for the transform_to_proportions function.

test_that("transform_to_proportions function works correctly", {
  # Create a sample data frame for testing
  test_data <- data.frame(
    total = c(100, 200, 300),
    count1 = c(10, 20, 30),
    count2 = c(40, 50, 60),
    non_numeric = c("a", "b", "c")
  )
  
  # Test that the function returns a data frame
  expect_is(transform_to_proportions(test_data, "total"), "data.frame")
  
  # Test that the function calculates proportions correctly
  expected_output <- data.frame(
    count1 = c(0.1, 0.1, 0.1),
    count2 = c(0.4, 0.25, 0.2),
    non_numeric = c("a", "b", "c")
  )
  actual_output <- transform_to_proportions(test_data, "total")
  expect_equal(actual_output, expected_output)
  
  # Test that the total column is removed if remove_total is TRUE
  expect_false("total" %in% colnames(transform_to_proportions(test_data, "total")))
  
  # Test that the function handles non-numeric columns correctly
  expected_output <- data.frame(
    total = c(100, 200, 300),
    count1 = c(0.1, 0.1, 0.1),
    count2 = c(0.4, 0.25, 0.2),
    non_numeric = c("a", "b", "c")
  )
  actual_output <- transform_to_proportions(test_data, "total", remove_total = FALSE)
  expect_equal(actual_output, expected_output)
})

# Now, write tests for the DT_percentage_format function.

test_that("DT_percentage_format function works correctly", {
  # Create a sample DT object for testing
  test_DT <- datatable(
    data.frame(
      col1 = c(0.1, 0.2, 0.3),
      col2 = c(0.4, 0.5, 0.6),
      non_numeric = c("a", "b", "c")
    )
  )
  
  # Test that the function returns a DT object
  expect_is(DT_percentage_format(test_DT), "datatables")
  
  # Test that the function formats all numeric columns as percentages by default
  expected_output <- datatable(
    data.frame(
      col1 = c(0.1, 0.2, 0.3),
      col2 = c(0.4, 0.5, 0.6),
      non_numeric = c("a", "b", "c")
    ) 
  ) %>%
    formatPercentage(c("col1", "col2"), digits = 0)
  
  actual_output <- DT_percentage_format(test_DT)
  expect_equal(actual_output$x$data, expected_output$x$data)
  
  # Test that the function formats specified columns as percentages
  expected_output <- datatable(
    data.frame(
      col1 = c(0.1, 0.2, 0.3),
      col2 = c(0.4, 0.5, 0.6),
      non_numeric = c("a", "b", "c")
    ) 
  ) %>%
    formatPercentage("col1", digits = 2)
  
  actual_output <- DT_percentage_format(test_DT, percentage_cols = "col1", decimal_places = 2)
  expect_equal(actual_output$x$data, expected_output$x$data)
})

# The third function, change_font_size, is defined as following:

# Create a JS function that generates jQuery code to change the font size of all elements in a DT table.
# The function should have one parameter:
# - font_size: The font size to set for the elements in the DT table.
# The function should return a character string with the jQuery code to change the font size.
# The returned function should be used in the initComplete option of the datatable function.
# "function(settings, json) {"
# "$(this.api().table().container()).css('font-size', '10px');"
# "}"
change_font_size <- function(font_size) {
  # Check that font_size is a single numeric value
  stopifnot(is.numeric(font_size) && length(font_size) == 1)
  
  return(
    paste0(
      "function(settings, json) {",
      "$(this.api().table().container()).css('font-size', '", font_size, "px');",
      "}"
    )
  )}

# Now, create tests for the change_font_size function.

test_that("change_font_size function works correctly", {
  # Test that the function returns a character string
  expect_is(change_font_size(10), "character")
  
  # Test that the function generates the correct jQuery code
  expected_output <- "function(settings, json) {$(this.api().table().container()).css('font-size', '10px');}"
  actual_output <- change_font_size(10)
  expect_equal(actual_output, expected_output)
  
  # Test that the function throws an error if font_size is not numeric
  expect_error(change_font_size("10"))
  expect_error(change_font_size(c(10, 20)))
})