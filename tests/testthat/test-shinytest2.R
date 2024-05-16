library(shinytest2)

test_that("{shinytest2} recording: DwellingON", {
  app <- AppDriver$new(name = "DwellingON", height = 698, width = 1139)
  app$set_inputs(subdivision = c("Toronto, City (C)", "Ottawa, City (CV)", "Vaughan, City (CY)"))
  app$expect_values()
})


test_that("{shinytest2} recording: DwellingON1", {
  app <- AppDriver$new(name = "DwellingON1", height = 698, width = 1139)
  app$set_inputs(subdivision = c("Toronto, City (C)", "Ottawa, City (CV)", "Kitchener, City (CY)"))
  app$expect_values()
})


test_that("{shinytest2} recording: DwellingON2", {
  app <- AppDriver$new(name = "DwellingON2", height = 698, width = 1139)
  app$set_inputs(subdivision = c("Toronto, City (C)", "Ottawa, City (CV)", "Markham, City (CY)"))
  app$expect_values(output = "dwellingsmain-table")
  app$expect_values(output = "dwellingsmain-table")
  app$expect_values(output = "dwellingsmain-table")
})


test_that("{shinytest2} recording: DwellingON3", {
  app <- AppDriver$new(name = "DwellingON3", height = 698, width = 1139)
  app$expect_values(output = "dwellingsmain-table")
  app$expect_values(output = "detailsmain-table_portrait")
})
