library(shiny)
test_that("multiplication works", {
  test_that("initial data load", {
    testServer(server, {
      print("test run")
      expect_equal(data_details, 344)
    })
  })
})
