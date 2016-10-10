library(testthat)
library(BMLmodel)
context("Errors")

test_that("There should be an error", {
  expect_error(createBMLGrid(-6, 2, c(1,2)), "Dimensions of the grid should be positive") 
  expect_error(createBMLGrid(16, 20, c(-4, 2)), "Number of cars can not be negative") 
  expect_error(createBMLGrid(2, 5, c(10, 15)), "Total number of cars exceeds the maximum") 
})

