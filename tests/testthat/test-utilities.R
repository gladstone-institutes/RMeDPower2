# Tests for utility functions in RMeDPower2

test_that("readDesign function works correctly", {
  temp_file <- tempfile(fileext = ".json")

  test_json <- list(
    response_column = "outcome",
    condition_column = "treatment",
    condition_is_categorical = TRUE,
    experimental_columns = c("batch", "subject"),
    covariate = "age",
    covariate_is_categorical = FALSE,
    crossed_columns = NULL,
    include_interaction = FALSE,
    random_slope_variable = NULL,
    total_column = NULL,
    na_action = "complete"
  )

  jsonlite::write_json(test_json, temp_file, auto_unbox = TRUE)

  design <- readDesign(temp_file)

  expect_s4_class(design, "RMeDesign")
  expect_equal(design@response_column, "outcome")
  expect_equal(design@condition_column, "treatment")
  expect_true(design@condition_is_categorical)
  expect_equal(design@experimental_columns, c("batch", "subject"))
  expect_equal(design@covariate, "age")
  expect_false(design@covariate_is_categorical)

  unlink(temp_file)
})

test_that("readProbabilityModel function works correctly", {
  temp_file <- tempfile(fileext = ".json")

  test_json <- list(error_is_non_normal = FALSE, family_p = NULL)
  write(jsonlite::toJSON(test_json, pretty = TRUE, auto_unbox = TRUE, null = "null"), file = temp_file)

  model <- readProbabilityModel(temp_file)
  expect_s4_class(model, "ProbabilityModel")
  expect_false(model@error_is_non_normal)
  expect_null(model@family_p)

  test_json2 <- list(error_is_non_normal = TRUE, family_p = "poisson")
  write(jsonlite::toJSON(test_json2, pretty = TRUE, auto_unbox = TRUE, null = "null"), file = temp_file)

  model2 <- readProbabilityModel(temp_file)
  expect_true(model2@error_is_non_normal)
  expect_equal(model2@family_p, "poisson")

  unlink(temp_file)
})

test_that("readPowerParams function works correctly", {
  temp_file <- tempfile(fileext = ".json")

  test_json <- list(
    target_columns = "experiment",
    levels = 1,
    power_curve = 1,
    nsimn = 1000,
    alpha = 0.05,
    max_size = NULL,
    breaks = NULL,
    effect_size = NULL,
    icc = NULL
  )

  write(jsonlite::toJSON(test_json, pretty = TRUE, auto_unbox = TRUE, null = "null"), file = temp_file)

  power_params <- readPowerParams(temp_file)
  expect_s4_class(power_params, "PowerParams")
  expect_equal(power_params@target_columns, "experiment")
  expect_equal(power_params@levels, 1)
  expect_equal(power_params@power_curve, 1)
  expect_equal(power_params@nsimn, 1000)
  expect_equal(power_params@alpha, 0.05)

  test_json2 <- list(
    target_columns = c("experiment", "plate"),
    levels = c(1, 0),
    power_curve = 1,
    nsimn = 500,
    alpha = 0.01,
    max_size = c(10, 20),
    breaks = c(5, 10, 15),
    effect_size = 0.8,
    icc = c(0.1, 0.05)
  )

  write(jsonlite::toJSON(test_json2, pretty = TRUE, auto_unbox = TRUE, null = "null"), file = temp_file)

  power_params2 <- readPowerParams(temp_file)
  expect_equal(power_params2@target_columns, c("experiment", "plate"))
  expect_equal(power_params2@levels, c(1, 0))
  expect_equal(power_params2@max_size, c(10, 20))
  expect_equal(power_params2@effect_size, 0.8)

  unlink(temp_file)
})

test_that("getEstimatesOfInterest returns correct structure", {
  test_data <- create_test_data()
  design <- create_test_design("basic")
  model <- create_test_model("normal")

  result <- suppressWarnings(suppressMessages(
    getEstimatesOfInterest(data = test_data, design = design, model = model, print_plots = FALSE)
  ))

  expect_type(result, "list")
  expect_length(result, 2)
})

test_that("getEstimatesOfInterest handles covariates", {
  test_data <- create_test_data()
  design <- create_test_design("with_covariate")
  model <- create_test_model("normal")

  expect_no_error(suppressWarnings(suppressMessages(
    getEstimatesOfInterest(data = test_data, design = design, model = model, print_plots = FALSE)
  )))
})

test_that("getEstimatesOfInterest handles interactions", {
  test_data <- create_test_data()
  design <- create_test_design("with_covariate")
  design@include_interaction <- TRUE
  model <- create_test_model("normal")

  expect_no_error(suppressWarnings(suppressMessages(
    getEstimatesOfInterest(data = test_data, design = design, model = model, print_plots = FALSE)
  )))
})

test_that("diagnoseDataModel works correctly", {
  test_data <- create_test_data()
  design <- create_test_design("basic")
  model <- create_test_model("normal")

  expect_no_error(suppressWarnings(suppressMessages(
    diagnoseDataModel(data = test_data, design = design, model = model)
  )))
})

test_that("diagnoseDataModel handles different model types", {
  test_data <- create_count_data()
  design <- create_test_design("count_data")
  model <- create_test_model("poisson")

  expect_no_error(suppressWarnings(suppressMessages(
    diagnoseDataModel(data = test_data, design = design, model = model)
  )))
})

test_that("JSON reading functions handle missing files gracefully", {
  non_existent_file <- "/path/that/does/not/exist.json"

  expect_error(readDesign(non_existent_file))
  expect_error(readProbabilityModel(non_existent_file))
  expect_error(readPowerParams(non_existent_file))
})

test_that("JSON reading functions validate content", {
  temp_file <- tempfile(fileext = ".json")
  writeLines("{invalid json content", temp_file)

  expect_error(readDesign(temp_file))
  expect_error(readProbabilityModel(temp_file))
  expect_error(readPowerParams(temp_file))

  unlink(temp_file)
})
