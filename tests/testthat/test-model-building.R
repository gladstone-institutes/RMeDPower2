# Tests for model building functions in RMeDPower2

test_that("get_model_and_data returns correct structure", {
  test_data <- create_test_data()

  result <- suppress_known_warnings({
    get_model_and_data(
      data = test_data,
      condition_column = "treatment",
      experimental_columns = c("experiment", "plate"),
      response_column = "cell_size",
      condition_is_categorical = TRUE,
      covariate_is_categorical = TRUE
    )
  })

  expect_type(result, "list")
  expect_length(result, 4)

  # Check that the result contains the expected components
  expect_s4_class(result[[1]], "lmerMod")  # Model object
  expect_type(result[[2]], "list")         # Data frame
  expect_type(result[[3]], "character")    # Column names
  expect_type(result[[4]], "double")       # Residuals
})

test_that("get_model_and_data fits linear mixed model correctly", {
  test_data <- create_test_data()

  result <- suppress_known_warnings({
    get_model_and_data(
      data = test_data,
      condition_column = "treatment",
      experimental_columns = c("experiment", "plate"),
      response_column = "cell_size",
      condition_is_categorical = TRUE,
      error_is_non_normal = FALSE,
      covariate_is_categorical = TRUE
    )
  })

  model <- result[[1]]

  # Check that it's a linear mixed model
  expect_s4_class(model, "lmerMod")

  # Check that the model has the expected structure
  expect_true(any(grepl("condition_column", names(fixef(model)))))

  # Check that random effects are present
  random_effects <- names(ranef(model))
  expect_true(any(grepl("experimental_column", random_effects)))
})

test_that("get_model_and_data handles covariates correctly", {
  test_data <- create_test_data()

  result <- suppress_known_warnings({
    get_model_and_data(
      data = test_data,
      condition_column = "treatment",
      experimental_columns = c("experiment", "plate"),
      response_column = "cell_size",
      condition_is_categorical = TRUE,
      covariate = "age",
      covariate_is_categorical = FALSE
    )
  })

  model <- result[[1]]
  processed_data <- result[[2]]

  # Check that covariate is included in the model
  expect_true("covariate" %in% names(fixef(model)))

  # Check that covariate is processed correctly in data
  expect_true("covariate" %in% names(processed_data))
  expect_true(is.numeric(processed_data$covariate))
})

test_that("get_model_and_data handles interactions correctly", {
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
      include_interaction = TRUE
    )
  })

  model <- result[[1]]

  # Check that interaction term is included
  fixed_effects <- names(fixef(model))
  expect_true(any(grepl(":", fixed_effects)) )
})

test_that("get_model_and_data fits GLMM for non-normal data", {
  test_data <- create_count_data()

  result <- suppress_known_warnings({
    get_model_and_data(
      data = test_data,
      condition_column = "treatment",
      experimental_columns = c("experiment", "plate"),
      response_column = "count",
      condition_is_categorical = TRUE,
      error_is_non_normal = TRUE,
      family_p = "poisson",
      covariate_is_categorical = TRUE
    )
  })

  model <- result[[1]]

  # Check that it's a generalized linear mixed model
  expect_s4_class(model, "glmerMod")

  # Check family
  expect_equal(family(model)$family, "poisson")
})

test_that("get_model_and_data handles binomial data with totals", {
  test_data <- create_count_data()

  result <- suppress_known_warnings({
    get_model_and_data(
      data = test_data,
      condition_column = "treatment",
      experimental_columns = c("experiment", "plate"),
      response_column = "count",
      total_column = "total_count",
      condition_is_categorical = TRUE,
      error_is_non_normal = TRUE,
      family_p = "binomial",
      covariate_is_categorical = TRUE
    )
  })

  model <- result[[1]]

  # Check that it's a GLMM with binomial family
  expect_s4_class(model, "glmerMod")
  expect_equal(family(model)$family, "binomial")
})

test_that("build_fixed_formula works correctly", {
  # Test without covariate
  formula1 <- build_fixed_formula(covariate = NULL, include_interaction = FALSE)
  expect_equal(formula1, "condition_column")

  # Test with covariate, no interaction
  formula2 <- build_fixed_formula(covariate = "age", include_interaction = FALSE)
  expect_equal(formula2, "condition_column + covariate")

  # Test with covariate and interaction
  formula3 <- build_fixed_formula(covariate = "age", include_interaction = TRUE)
  expect_equal(formula3, "condition_column * covariate")
})

test_that("build_random_formula works correctly", {
  # Test basic random effects
  formula1 <- build_random_formula(experimental_columns = c("exp", "plate"))
  expect_equal(formula1, "(1 | experimental_column1) + (1 | experimental_column2)")

  # Test with random slope for condition
  formula2 <- build_random_formula(
    experimental_columns = c("exp", "plate"),
    random_slope_variable = "condition_column"
  )
  expect_true(grepl("condition_column.*experimental_column", formula2))

  # Test single experimental column
  formula3 <- build_random_formula(experimental_columns = "experiment")
  expect_equal(formula3, "(1 | experimental_column1)")
})

test_that("generate_model_fit handles different model types", {
  test_data <- create_test_data()

  # Prepare data as get_model_and_data would
  processed_data <- test_data
  names(processed_data)[names(processed_data) == "treatment"] <- "condition_column"
  names(processed_data)[names(processed_data) == "cell_size"] <- "response_column"
  names(processed_data)[names(processed_data) == "age"] <- "covariate"
  processed_data$experimental_column1 <- as.factor(processed_data$experiment)
  processed_data$experimental_column2 <- as.factor(processed_data$plate)
  processed_data$condition_column <- as.factor(processed_data$condition_column)

  # Test normal model
  fixed_formula <- "condition_column"
  random_formula <- "(1 | experimental_column1) + (1 | experimental_column2)"

  model <- suppress_known_warnings({
    generate_model_fit(
      data = processed_data,
      fixed_formula = fixed_formula,
      random_formula = random_formula,
      error_is_non_normal = FALSE
    )
  })

  expect_s4_class(model, "lmerMod")
})

test_that("model fitting preserves data structure", {
  test_data <- create_test_data()

  result <- suppress_known_warnings({
    get_model_and_data(
      data = test_data,
      condition_column = "treatment",
      experimental_columns = c("experiment", "plate"),
      response_column = "cell_size",
      condition_is_categorical = TRUE,
      covariate_is_categorical = TRUE
    )
  })

  processed_data <- result[[2]]

  # Check that essential columns are present and correctly named
  expected_columns <- c("condition_column", "response_column",
                       "experimental_column1", "experimental_column2")
  expect_true(all(expected_columns %in% names(processed_data)))

  # Check that sample size is preserved (after removing NAs)
  original_complete_cases <- sum(complete.cases(test_data))
  expect_equal(nrow(processed_data), original_complete_cases)
})

test_that("model residuals are calculated correctly", {
  test_data <- create_test_data()

  result <- suppress_known_warnings({
    get_model_and_data(
      data = test_data,
      condition_column = "treatment",
      experimental_columns = c("experiment", "plate"),
      response_column = "cell_size",
      condition_is_categorical = TRUE,
      covariate_is_categorical = TRUE
    )
  })

  model <- result[[1]]
  residuals <- result[[4]]
  processed_data <- result[[2]]

  # Check that residuals are numeric and have correct length
  expect_type(residuals, "double")
  expect_equal(length(residuals), nrow(processed_data))

  # Check that residuals are similar to model residuals
  model_residuals <- resid(model)
  expect_equal(length(residuals), length(model_residuals))
})
