# Tests for utility functions in RMeDPower2

test_that("readDesign function works correctly", {
  # Create a temporary JSON file for testing
  temp_file <- tempfile(fileext = ".json")

  # Create test JSON content
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

  # Write JSON file
  jsonlite::write_json(test_json, temp_file, auto_unbox = TRUE)

  # Test reading the design
  design <- readDesign(temp_file)

  expect_s4_class(design, "RMeDesign")
  expect_equal(design@response_column, "outcome")
  expect_equal(design@condition_column, "treatment")
  expect_true(design@condition_is_categorical)
  expect_equal(design@experimental_columns, c("batch", "subject"))
  expect_equal(design@covariate, "age")
  expect_false(design@covariate_is_categorical)

  # Clean up
  unlink(temp_file)
})

test_that("readProbabilityModel function works correctly", {
  # Create a temporary JSON file for testing
  temp_file <- tempfile(fileext = ".json")

  # Test normal distribution model
  test_json <- list(
    error_is_non_normal = FALSE,
    family_p = NULL
  )

  json_test <- jsonlite::toJSON(test_json, pretty = TRUE, auto_unbox = TRUE, null = "null")

  # Write JSON string to a file
  write(json_test, file = temp_file)

  model <- readProbabilityModel(temp_file)

  expect_s4_class(model, "ProbabilityModel")
  expect_false(model@error_is_non_normal)
  expect_null(model@family_p)

  # Test non-normal distribution model
  test_json2 <- list(
    error_is_non_normal = TRUE,
    family_p = "poisson"
  )

  json_test <- jsonlite::toJSON(test_json2, pretty = TRUE, auto_unbox = TRUE, null = "null")

  # Write JSON string to a file
  write(json_test, file = temp_file)

  model2 <- readProbabilityModel(temp_file)

  expect_true(model2@error_is_non_normal)
  expect_equal(model2@family_p, "poisson")

  # Clean up
  unlink(temp_file)
})

test_that("readPowerParams function works correctly", {
  # Create a temporary JSON file for testing
  temp_file <- tempfile(fileext = ".json")

  # Test basic power parameters
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

  json_test <- jsonlite::toJSON(test_json, pretty = TRUE, auto_unbox = TRUE, null = "null")

  # Write JSON string to a file
  write(json_test, file = temp_file)

  power_params <- readPowerParams(temp_file)

  expect_s4_class(power_params, "PowerParams")
  expect_equal(power_params@target_columns, "experiment")
  expect_equal(power_params@levels, 1)
  expect_equal(power_params@power_curve, 1)
  expect_equal(power_params@nsimn, 1000)
  expect_equal(power_params@alpha, 0.05)

  # Test with multiple target columns and additional parameters
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

  json_test <- jsonlite::toJSON(test_json2, pretty = TRUE, auto_unbox = TRUE, null = "null")

  # Write JSON string to a file
  write(json_test, file = temp_file)

  power_params2 <- readPowerParams(temp_file)

  expect_equal(power_params2@target_columns, c("experiment", "plate"))
  expect_equal(power_params2@levels, c(1, 0))
  expect_equal(power_params2@max_size, c(10, 20))
  expect_equal(power_params2@effect_size, 0.8)

  # Clean up
  unlink(temp_file)
})

test_that("check_normality function works correctly", {
  test_data <- create_test_data()

  # Test basic normality check
  expect_no_error({
    result <- suppressWarnings({
      check_normality(
        data = test_data,
        condition_column = "treatment",
        covariate = "batch",
        experimental_columns = c("experiment", "plate"),
        response_column = "cell_size",
        condition_is_categorical = TRUE,
        covariate_is_categorical = TRUE
      )
    })
  })
})

test_that("check_normality handles covariates", {
  test_data <- create_test_data()

  # Test with covariate
  expect_no_error({
    result <- suppressWarnings({
      check_normality(
        data = test_data,
        condition_column = "treatment",
        experimental_columns = c("experiment", "plate"),
        response_column = "cell_size",
        condition_is_categorical = TRUE,
        covariate = "age",
        covariate_is_categorical = FALSE
      )
    })
  })
})

test_that("get_residuals function works correctly", {
  test_data <- create_test_data()

  # Test basic residuals calculation
  result <- suppressWarnings({
    get_residuals(
      data = test_data,
      condition_column = "treatment",
      experimental_columns = c("experiment", "plate"),
      response_column = "cell_size",
      condition_is_categorical = TRUE,
      covariate_is_categorical = TRUE
    )
  })

  expect_type(result, "list")
  expect_true("residual" %in% names(result))
  expect_true(nrow(result) > 0)
})

test_that("get_residuals handles covariates", {
  test_data <- create_test_data()

  # Test with covariate
  result <- suppressWarnings({
    get_residuals(
      data = test_data,
      condition_column = "treatment",
      experimental_columns = c("experiment", "plate"),
      response_column = "cell_size",
      condition_is_categorical = TRUE,
      covariate = "age",
      covariate_is_categorical = FALSE
    )
  })

  expect_true("residual" %in% names(result))
  expect_true("covariate" %in% names(result))
})

test_that("get_residuals handles interactions", {
  test_data <- create_test_data()

  # Test with interaction
  expect_no_error({
    result <- suppressWarnings({
      get_residuals(
        data = test_data,
        condition_column = "treatment",
        experimental_columns = c("experiment", "plate"),
        response_column = "cell_size",
        condition_is_categorical = TRUE,
        covariate = "age",
        covariate_is_categorical = FALSE,
        include_interaction = TRUE
      )
    })
  })
})

test_that("diagnoseDataModel function works correctly", {
  test_data <- create_test_data()
  design <- create_test_design("basic")
  model <- create_test_model("normal")

  # Test basic diagnosis
  expect_no_error({
    result <- suppressMessages(suppressWarnings({
      diagnoseDataModel(
        data = test_data,
        design = design,
        model = model
      )
    }))
  })
})

test_that("diagnoseDataModel handles different model types", {
  test_data <- create_count_data()
  design <- create_test_design("count_data")
  model <- create_test_model("poisson")

  # Test with count data
  expect_no_error({
    result <- suppressMessages(suppressWarnings({
      diagnoseDataModel(
        data = test_data,
        design = design,
        model = model
      )
    }))
  })
})

test_that("transform_data function works correctly", {
  test_data <- create_test_data()

  # Test basic transformation
  expect_no_error({
    result <- suppressWarnings({
      transform_data(
        data = test_data,
        condition_column = "treatment",
        experimental_columns = c("experiment", "plate"),
        response_column = "cell_size",
        condition_is_categorical = TRUE
      )
    })
  })
})


test_that("JSON reading functions handle missing files gracefully", {
  non_existent_file <- "/path/that/does/not/exist.json"

  # Test that functions handle missing files appropriately
  expect_error(readDesign(non_existent_file))
  expect_error(readProbabilityModel(non_existent_file))
  expect_error(readPowerParams(non_existent_file))
})

test_that("JSON reading functions validate content", {
  # Create a temporary file with invalid JSON
  temp_file <- tempfile(fileext = ".json")
  writeLines("{invalid json content", temp_file)

  expect_error(readDesign(temp_file))
  expect_error(readProbabilityModel(temp_file))
  expect_error(readPowerParams(temp_file))

  # Clean up
  unlink(temp_file)
})

test_that("helper functions validate inputs appropriately", {
  test_data <- create_test_data()

  # Test that functions catch common input errors
  expect_null({
    get_residuals(
      data = test_data,
      condition_column = "nonexistent",
      experimental_columns = c("experiment", "plate"),
      response_column = "cell_size",
      condition_is_categorical = TRUE
    )
  })

  expect_error({
    check_normality(
      data = test_data,
      condition_column = "treatment",
      experimental_columns = c("experiment", "nonexistent"),
      response_column = "cell_size",
      condition_is_categorical = TRUE,
      covariate_is_categorical = TRUE
    )
  })
})
