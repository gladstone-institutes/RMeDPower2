# Basic functionality tests to verify package loading

test_that("Package loads correctly", {
  expect_true("RMeDPower2" %in% loadedNamespaces())
})

test_that("Main functions are available", {
  expect_true(exists("calculatePower"))
  expect_true(exists("calculate_power"))
  expect_true(exists("get_model_and_data"))
  expect_true(exists("diagnoseDataModel"))
})

test_that("S4 classes are defined", {
  expect_no_error(getClass("RMeDesign"))
  expect_no_error(getClass("ProbabilityModel"))
  expect_no_error(getClass("PowerParams"))
})

test_that("Example data is available", {
  expect_true(exists("RMeDPower_data1"))
  data("RMeDPower_data1", envir = environment())
  expect_true(is.data.frame(RMeDPower_data1))
  expect_true(nrow(RMeDPower_data1) > 0)
})

test_that("Basic S4 class creation works", {
  # Create a minimal RMeDesign object using helper function
  design <- create_test_design("basic")
  
  expect_s4_class(design, "RMeDesign")
  expect_equal(design@response_column, "cell_size")
  expect_equal(design@condition_column, "treatment")
  expect_true(design@condition_is_categorical)
})

test_that("Basic function call works", {
  # Test that main exported functions exist and are callable
  test_data <- create_test_data()
  
  # Test that functions can be found
  expect_true(exists("get_model_and_data"))
  expect_true(exists("calculatePower"))
  expect_true(exists("diagnoseDataModel"))
  
  # Test basic functionality without complex model fitting
  expect_true(is.data.frame(test_data))
  expect_true(nrow(test_data) > 0)
  expect_true("treatment" %in% names(test_data))
  expect_true("cell_size" %in% names(test_data))
})