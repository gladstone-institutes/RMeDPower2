# Tests for data validation functions in RMeDPower2

test_that("get_model_and_data validates column existence", {
  test_data <- create_test_data()
  
  # Test missing condition column
  expect_error(
    get_model_and_data(
      data = test_data,
      condition_column = "nonexistent_column",
      experimental_columns = c("experiment", "plate"),
      response_column = "cell_size",
      condition_is_categorical = TRUE,
      covariate_is_categorical = TRUE
    ),
    "condition_column should be one of the column names"
  )
  
  # Test missing response column
  expect_error(
    get_model_and_data(
      data = test_data,
      condition_column = "treatment",
      experimental_columns = c("experiment", "plate"),
      response_column = "nonexistent_response",
      condition_is_categorical = TRUE,
      covariate_is_categorical = TRUE
    ),
    "response_column should be one of the column names"
  )
  
  # Test missing experimental columns
  expect_error(
    get_model_and_data(
      data = test_data,
      condition_column = "treatment",
      experimental_columns = c("experiment", "nonexistent_exp"),
      response_column = "cell_size",
      condition_is_categorical = TRUE,
      covariate_is_categorical = TRUE
    ),
    "experimental_columns must match column names"
  )
})

test_that("get_model_and_data validates covariate column", {
  test_data <- create_test_data()
  
  # Test missing covariate column
  expect_error(
    get_model_and_data(
      data = test_data,
      condition_column = "treatment",
      experimental_columns = c("experiment", "plate"),
      response_column = "cell_size",
      condition_is_categorical = TRUE,
      covariate = "nonexistent_covariate",
      covariate_is_categorical = TRUE
    ),
    "covariate should be null or one of the column names"
  )
})

test_that("get_model_and_data validates crossed columns", {
  test_data <- create_test_data()
  
  # Test invalid crossed columns
  expect_error(
    get_model_and_data(
      data = test_data,
      condition_column = "treatment",
      experimental_columns = c("experiment", "plate"),
      response_column = "cell_size",
      condition_is_categorical = TRUE,
      crossed_columns = "nonexistent_crossed",
      covariate_is_categorical = TRUE
    ),
    "crossed_columns must match column names"
  )
})

test_that("get_model_and_data validates categorical parameters", {
  test_data <- create_test_data()
  
  # Test missing condition_is_categorical
  expect_error(
    get_model_and_data(
      data = test_data,
      condition_column = "treatment",
      experimental_columns = c("experiment", "plate"),
      response_column = "cell_size",
      condition_is_categorical = NULL,
      covariate_is_categorical = TRUE
    ),
    "condition_is_categorical must be TRUE or FALSE"
  )
  
  # Test missing covariate_is_categorical
  expect_error(
    get_model_and_data(
      data = test_data,
      condition_column = "treatment",
      experimental_columns = c("experiment", "plate"),
      response_column = "cell_size",
      condition_is_categorical = TRUE,
      covariate_is_categorical = NULL
    ),
    "covariate_is_categorical must be TRUE or FALSE"
  )
})

test_that("get_model_and_data validates interaction parameters", {
  test_data <- create_test_data()
  
  # Test interaction without covariate
  expect_error(
    get_model_and_data(
      data = test_data,
      condition_column = "treatment",
      experimental_columns = c("experiment", "plate"),
      response_column = "cell_size",
      condition_is_categorical = TRUE,
      covariate = NULL,
      include_interaction = TRUE,
      covariate_is_categorical = TRUE
    ),
    "Cannot include interaction when covariate is NULL"
  )
})

test_that("get_model_and_data validates random slope variable", {
  test_data <- create_test_data()
  
  # Test invalid random slope variable
  expect_error(
    get_model_and_data(
      data = test_data,
      condition_column = "treatment",
      experimental_columns = c("experiment", "plate"),
      response_column = "cell_size",
      condition_is_categorical = TRUE,
      random_slope_variable = "invalid_variable",
      covariate_is_categorical = TRUE
    ),
    "random_slope_variable should be"
  )
})

test_that("get_model_and_data handles missing data correctly", {
  test_data_na <- create_test_data_with_na()
  
  # Test complete case analysis
  result_complete <- suppress_known_warnings({
    get_model_and_data(
      data = test_data_na,
      condition_column = "treatment",
      experimental_columns = c("experiment", "plate"),
      response_column = "cell_size",
      condition_is_categorical = TRUE,
      na.action = "complete",
      covariate_is_categorical = TRUE
    )
  })
  
  expect_type(result_complete, "list")
  expect_length(result_complete, 4)
  
  # Check that rows with any NA values are removed
  processed_data <- result_complete[[2]]
  expect_true(all(complete.cases(processed_data)))
})

test_that("get_model_and_data processes data types correctly", {
  test_data <- create_test_data()
  
  result <- suppress_known_warnings({
    get_model_and_data(
      data = test_data,
      condition_column = "treatment",
      experimental_columns = c("experiment", "plate"),
      response_column = "cell_size",
      condition_is_categorical = TRUE,
      covariate = "age",
      covariate_is_categorical = FALSE,
      covariate_is_categorical = TRUE
    )
  })
  
  processed_data <- result[[2]]
  
  # Check that condition column is factor when categorical
  expect_true(is.factor(processed_data$condition_column))
  
  # Check that covariate is numeric when not categorical
  expect_true(is.numeric(processed_data$covariate))
  
  # Check that experimental columns are factors
  expect_true(is.factor(processed_data$experimental_column1))
  expect_true(is.factor(processed_data$experimental_column2))
})

test_that("calculate_power validates input parameters", {
  test_data <- create_test_data()
  
  # Test missing required columns in data
  incomplete_data <- test_data[, !names(test_data) %in% "treatment"]
  
  expect_error(
    calculate_power(
      data = incomplete_data,
      condition_column = "treatment",
      experimental_columns = c("experiment", "plate"),
      response_column = "cell_size",
      target_columns = "experiment",
      condition_is_categorical = TRUE,
      levels = 1,
      nsimn = 10,
      covariate_is_categorical = TRUE
    ),
    "condition_column should be one of the column names"
  )
})

test_that("calculate_power validates target columns", {
  test_data <- create_test_data()
  
  # Test invalid target column
  expect_error(
    calculate_power(
      data = test_data,
      condition_column = "treatment",
      experimental_columns = c("experiment", "plate"),
      response_column = "cell_size",
      target_columns = "invalid_target",
      condition_is_categorical = TRUE,
      levels = 1,
      nsimn = 10,
      covariate_is_categorical = TRUE
    ),
    "target_columns should be a subset of experimental_columns"
  )
})

test_that("calculate_power validates nsimn parameter", {
  test_data <- create_test_data()
  
  # Test invalid nsimn (negative)
  expect_error(
    calculate_power(
      data = test_data,
      condition_column = "treatment",
      experimental_columns = c("experiment", "plate"),
      response_column = "cell_size",
      target_columns = "experiment",
      condition_is_categorical = TRUE,
      levels = 1,
      nsimn = -10,
      covariate_is_categorical = TRUE
    ),
    "nsimn should be a positive integer"
  )
  
  # Test invalid nsimn (zero)
  expect_error(
    calculate_power(
      data = test_data,
      condition_column = "treatment",
      experimental_columns = c("experiment", "plate"),
      response_column = "cell_size",
      target_columns = "experiment",
      condition_is_categorical = TRUE,
      levels = 1,
      nsimn = 0,
      covariate_is_categorical = TRUE
    ),
    "nsimn should be a positive integer"
  )
})

test_that("calculate_power validates levels parameter", {
  test_data <- create_test_data()
  
  # Test invalid levels (not 0 or 1)
  expect_error(
    calculate_power(
      data = test_data,
      condition_column = "treatment",
      experimental_columns = c("experiment", "plate"),
      response_column = "cell_size",
      target_columns = "experiment",
      condition_is_categorical = TRUE,
      levels = 2,
      nsimn = 10,
      covariate_is_categorical = TRUE
    ),
    "levels should be 0 or 1"
  )
})

test_that("calculate_power validates power_curve parameter", {
  test_data <- create_test_data()
  
  # Test invalid power_curve (not 0 or 1)
  expect_error(
    calculate_power(
      data = test_data,
      condition_column = "treatment",
      experimental_columns = c("experiment", "plate"),
      response_column = "cell_size",
      target_columns = "experiment",
      condition_is_categorical = TRUE,
      levels = 1,
      power_curve = 2,
      nsimn = 10,
      covariate_is_categorical = TRUE
    ),
    "power_curve should be 0 or 1"
  )
})