# Tests for model building via diagnoseDataModel public API

test_that("diagnoseDataModel returns correct structure", {
  test_data <- create_test_data()
  design <- create_test_design("basic")
  model <- create_test_model("normal")

  result <- suppressWarnings(suppressMessages(
    diagnoseDataModel(data = test_data, design = design, model = model)
  ))

  expect_type(result, "list")
  expect_true("models" %in% names(result))
  expect_true("Data_updated" %in% names(result))
  expect_true("diagnostic_plots" %in% names(result))
  expect_true("cooks_result" %in% names(result))
})

test_that("diagnoseDataModel returns updated data frame", {
  test_data <- create_test_data()
  design <- create_test_design("basic")
  model <- create_test_model("normal")

  result <- suppressWarnings(suppressMessages(
    diagnoseDataModel(data = test_data, design = design, model = model)
  ))

  expect_true(is.data.frame(result$Data_updated))
  expect_true(nrow(result$Data_updated) > 0)
})

test_that("diagnoseDataModel handles covariates correctly", {
  test_data <- create_test_data()
  design <- create_test_design("with_covariate")
  model <- create_test_model("normal")

  result <- suppressWarnings(suppressMessages(
    diagnoseDataModel(data = test_data, design = design, model = model)
  ))

  expect_type(result, "list")
  expect_true("Data_updated" %in% names(result))
})

test_that("diagnoseDataModel handles interactions correctly", {
  test_data <- create_test_data()
  design <- create_test_design("with_covariate")
  design@include_interaction <- TRUE
  model <- create_test_model("normal")

  expect_no_error(suppressWarnings(suppressMessages(
    diagnoseDataModel(data = test_data, design = design, model = model)
  )))
})

test_that("diagnoseDataModel handles non-normal distributions", {
  test_data <- create_count_data()
  design <- create_test_design("count_data")
  model <- create_test_model("poisson")

  result <- suppressWarnings(suppressMessages(
    diagnoseDataModel(data = test_data, design = design, model = model)
  ))

  expect_type(result, "list")
  expect_true("models" %in% names(result))
})

test_that("diagnoseDataModel handles binomial data with totals", {
  test_data <- create_count_data()
  design <- create_test_design("count_data")
  model <- create_test_model("binomial")

  result <- suppressWarnings(suppressMessages(
    diagnoseDataModel(data = test_data, design = design, model = model)
  ))

  expect_type(result, "list")
  expect_true("Data_updated" %in% names(result))
})

test_that("diagnoseDataModel handles missing data correctly", {
  test_data_na <- create_test_data_with_na()
  design <- create_test_design("basic")
  model <- create_test_model("normal")

  result <- suppressWarnings(suppressMessages(
    diagnoseDataModel(data = test_data_na, design = design, model = model)
  ))

  expect_true(is.data.frame(result$Data_updated))
  expect_true(all(complete.cases(result$Data_updated[, c(design@condition_column, design@response_column)])))
})

test_that("diagnoseDataModel preserves data structure", {
  test_data <- create_test_data()
  design <- create_test_design("basic")
  model <- create_test_model("normal")

  result <- suppressWarnings(suppressMessages(
    diagnoseDataModel(data = test_data, design = design, model = model)
  ))

  updated_data <- result$Data_updated
  expect_true(design@condition_column %in% names(updated_data))
  expect_true(design@response_column %in% names(updated_data))
})
