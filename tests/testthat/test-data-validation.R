# Tests for input validation via public API

test_that("diagnoseDataModel catches missing condition column", {
  test_data <- create_test_data()
  design <- create_test_design("basic")
  design@condition_column <- "nonexistent_column"
  model <- create_test_model("normal")

  expect_error(suppressWarnings(suppressMessages(
    diagnoseDataModel(data = test_data, design = design, model = model)
  )))
})

test_that("diagnoseDataModel catches missing response column", {
  test_data <- create_test_data()
  design <- create_test_design("basic")
  design@response_column <- "nonexistent_response"
  model <- create_test_model("normal")

  expect_error(suppressWarnings(suppressMessages(
    diagnoseDataModel(data = test_data, design = design, model = model)
  )))
})

test_that("diagnoseDataModel catches missing experimental columns", {
  test_data <- create_test_data()
  design <- create_test_design("basic")
  design@experimental_columns <- c("experiment", "nonexistent_exp")
  model <- create_test_model("normal")

  expect_error(suppressWarnings(suppressMessages(
    diagnoseDataModel(data = test_data, design = design, model = model)
  )))
})

test_that("diagnoseDataModel catches missing covariate column", {
  test_data <- create_test_data()
  design <- create_test_design("basic")
  design@covariate <- "nonexistent_covariate"
  model <- create_test_model("normal")

  expect_error(suppressWarnings(suppressMessages(
    diagnoseDataModel(data = test_data, design = design, model = model)
  )))
})

test_that("diagnoseDataModel catches invalid crossed columns", {
  test_data <- create_test_data()
  design <- create_test_design("basic")
  design@crossed_columns <- "nonexistent_crossed"
  model <- create_test_model("normal")

  expect_error(suppressWarnings(suppressMessages(
    diagnoseDataModel(data = test_data, design = design, model = model)
  )))
})

test_that("diagnoseDataModel catches interaction without covariate", {
  test_data <- create_test_data()
  design <- create_test_design("basic")
  design@include_interaction <- TRUE
  model <- create_test_model("normal")

  expect_error(suppressWarnings(suppressMessages(
    diagnoseDataModel(data = test_data, design = design, model = model)
  )))
})

test_that("calculatePower catches missing condition column", {
  test_data <- create_test_data()
  design <- create_test_design("basic")
  design@condition_column <- "nonexistent_column"
  model <- create_test_model("normal")
  power_params <- create_test_power_params("basic")

  expect_null(suppressWarnings(suppressMessages(
    calculatePower(data = test_data, design = design, model = model, power_param = power_params)
  )))
})

test_that("calculatePower catches invalid nsimn", {
  test_data <- create_test_data()
  design <- create_test_design("basic")
  model <- create_test_model("normal")
  power_params <- create_test_power_params("basic")
  power_params@nsimn <- -10

  expect_null(suppressWarnings(suppressMessages(
    calculatePower(data = test_data, design = design, model = model, power_param = power_params)
  )))
})

test_that("calculatePower catches invalid levels", {
  test_data <- create_test_data()
  design <- create_test_design("basic")
  model <- create_test_model("normal")
  power_params <- create_test_power_params("basic")
  power_params@levels <- 2

  expect_null(suppressWarnings(suppressMessages(
    calculatePower(data = test_data, design = design, model = model, power_param = power_params)
  )))
})

test_that("calculatePower catches invalid power_curve", {
  test_data <- create_test_data()
  design <- create_test_design("basic")
  model <- create_test_model("normal")
  power_params <- create_test_power_params("basic")
  power_params@power_curve <- 2

  expect_null(suppressWarnings(suppressMessages(
    calculatePower(data = test_data, design = design, model = model, power_param = power_params)
  )))
})

test_that("calculatePower catches invalid target columns", {
  test_data <- create_test_data()
  design <- create_test_design("basic")
  model <- create_test_model("normal")
  power_params <- create_test_power_params("basic")
  power_params@target_columns <- "invalid_target"

  expect_null(suppressWarnings(suppressMessages(
    calculatePower(data = test_data, design = design, model = model, power_param = power_params)
  )))
})

