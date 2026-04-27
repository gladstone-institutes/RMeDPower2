# Basic functionality tests to verify package loading

test_that("Package loads correctly", {
  expect_true("RMeDPower2" %in% loadedNamespaces())
})

test_that("Main functions are available", {
  expect_true(exists("calculatePower"))
  expect_true(exists("getEstimatesOfInterest"))
  expect_true(exists("diagnoseDataModel"))
})

test_that("S4 classes are defined", {
  expect_no_error(getClass("RMeDesign"))
  expect_no_error(getClass("ProbabilityModel"))
  expect_no_error(getClass("PowerParams"))
})

test_that("Example data is available", {
  expect_true(exists("plate_assay_pilot_data"))
  data("plate_assay_pilot_data", envir = environment())
  expect_true(is.data.frame(plate_assay_pilot_data))
  expect_true(nrow(plate_assay_pilot_data) > 0)
})

test_that("Basic S4 class creation works", {
  # Create a minimal RMeDesign object using helper function
  design <- create_test_design("basic")

  expect_s4_class(design, "RMeDesign")
  expect_equal(design@response_column, "cell_size")
  expect_equal(design@condition_column, "treatment")
  expect_true(design@condition_is_categorical)
})

