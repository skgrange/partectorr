context("Test the file reader")

test_that("Test `read_partectors_data` with no arguments", {
  
  # Get all example files included in the package
  file_list <- list.files(
    system.file("extdata", package = "partectorr"),
    full.names = TRUE
  )
  
  # Use function with no arguments
  df <- read_partectors_data(file_list)
  
  # Check return
  expect_equal(class(df), c("tbl_df", "tbl", "data.frame"))
  
})


test_that("Test `read_partectors_data` with no reshaping", {
  
  # Get all example files included in the package
  file_list <- list.files(
    system.file("extdata", package = "partectorr"),
    full.names = TRUE
  )
  
  # Use function with no arguments
  df <- read_partectors_data(file_list, as_long = FALSE)
  
  # Check return
  expect_equal(class(df), c("tbl_df", "tbl", "data.frame"))
  
})


test_that("Test `read_partectors_data` with minor arguments", {
  
  # Get all example files included in the package
  file_list <- list.files(
    system.file("extdata", package = "partectorr"),
    full.names = TRUE
  )
  
  # Use function with changed arguments
  expect_equal(
    class(read_partectors_data(file_list, as_long = FALSE)), 
    c("tbl_df", "tbl", "data.frame")
  )
  
  expect_equal(
    class(read_partectors_data(file_list, variable_switch = TRUE)), 
    c("tbl_df", "tbl", "data.frame")
  )
  
  expect_equal(
    class(read_partectors_data(file_list, variable_switch = TRUE, priority_variables = TRUE)), 
    c("tbl_df", "tbl", "data.frame")
  )
  
  expect_equal(
    class(read_partectors_data(file_list, serial_number_as_character = TRUE)),
    c("tbl_df", "tbl", "data.frame")
  )
  
  expect_equal(
    class(read_partectors_data(file_list, date_calibration_as_numeric = TRUE)), 
    c("tbl_df", "tbl", "data.frame")
  )
  
  expect_equal(
    class(read_partectors_data(file_list, names_to = "file")), 
    c("tbl_df", "tbl", "data.frame")
  )
  
})
