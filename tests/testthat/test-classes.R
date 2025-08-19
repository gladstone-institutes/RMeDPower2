# Tests for S4 class definitions in RMeDPower2

test_that("RMeDesign class can be created with valid parameters", {
  # Test basic construction
  design <- new("RMeDesign",
    response_column = "cell_size",
    condition_column = "treatment",
    condition_is_categorical = TRUE,
    experimental_columns = c("experiment", "plate"),
    covariate = NULL,
    covariate_is_categorical = NA,
    crossed_columns = NULL,
    include_interaction = NA,
    random_slope_variable = NULL,
    total_column = NULL,
    na_action = "complete"
  )

  expect_s4_class(design, "RMeDesign")
  expect_equal(design@response_column, "cell_size")
  expect_equal(design@condition_column, "treatment")
  expect_true(design@condition_is_categorical)
  expect_equal(design@experimental_columns, c("experiment", "plate"))
  expect_null(design@covariate)
  expect_null(design@covariate_is_categorical)
  expect_false(design@include_interaction)
})

test_that("RMeDesign class handles covariate parameters correctly", {
  design <- new("RMeDesign",
    response_column = "outcome",
    condition_column = "treatment",
    condition_is_categorical = TRUE,
    experimental_columns = c("batch", "subject"),
    covariate = "age",
    covariate_is_categorical = FALSE,
    crossed_columns = NULL,
    include_interaction = TRUE,
    random_slope_variable = "condition_column",
    total_column = NULL,
    na_action = "unique"
  )

  expect_equal(design@covariate, "age")
  expect_false(design@covariate_is_categorical)
  expect_true(design@include_interaction)
  expect_equal(design@random_slope_variable, "condition_column")
  expect_equal(design@na_action, "unique")
})

test_that("RMeDesign class handles crossed columns correctly", {
  design <- new("RMeDesign",
    response_column = "expression",
    condition_column = "genotype",
    condition_is_categorical = TRUE,
    experimental_columns = c("experiment", "cell_line"),
    covariate = NULL,
    covariate_is_categorical = NULL,
    crossed_columns = "cell_line",
    include_interaction = FALSE,
    random_slope_variable = NULL,
    total_column = NULL,
    na_action = "complete"
  )

  expect_equal(design@crossed_columns, "cell_line")
})

test_that("ProbabilityModel class can be created for normal distribution", {
  model <- new("ProbabilityModel",
    error_is_non_normal = FALSE,
    family_p = NULL
  )

  expect_s4_class(model, "ProbabilityModel")
  expect_false(model@error_is_non_normal)
  expect_null(model@family_p)
})

test_that("ProbabilityModel class can be created for non-normal distributions", {
  # Test Poisson
  model_poisson <- new("ProbabilityModel",
    error_is_non_normal = TRUE,
    family_p = "poisson"
  )

  expect_true(model_poisson@error_is_non_normal)
  expect_equal(model_poisson@family_p, "poisson")

  # Test binomial
  model_binomial <- new("ProbabilityModel",
    error_is_non_normal = TRUE,
    family_p = "binomial"
  )

  expect_equal(model_binomial@family_p, "binomial")

  # Test negative binomial
  model_nb <- new("ProbabilityModel",
    error_is_non_normal = TRUE,
    family_p = "negative_binomial"
  )

  expect_equal(model_nb@family_p, "negative_binomial")
})

test_that("PowerParams class can be created with basic parameters", {
  power_params <- new("PowerParams",
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

  expect_s4_class(power_params, "PowerParams")
  expect_equal(power_params@target_columns, "experiment")
  expect_equal(power_params@levels, 1)
  expect_equal(power_params@power_curve, 1)
  expect_equal(power_params@nsimn, 1000)
  expect_equal(power_params@alpha, 0.05)
  expect_null(power_params@max_size)
})

test_that("PowerParams class can be created with multiple target columns", {
  power_params <- new("PowerParams",
    target_columns = c("experiment", "plate"),
    levels = c(1, 0),
    power_curve = 1,
    nsimn = 500,
    alpha = 0.01,
    max_size = c(10, 20),
    breaks = c(5, 10, 15),
    effect_size = 0.5,
    icc = c(0.1, 0.05)
  )

  expect_equal(power_params@target_columns, c("experiment", "plate"))
  expect_equal(power_params@levels, c(1, 0))
  expect_equal(power_params@nsimn, 500)
  expect_equal(power_params@alpha, 0.01)
  expect_equal(power_params@max_size, c(10, 20))
  expect_equal(power_params@breaks, c(5, 10, 15))
  expect_equal(power_params@effect_size, 0.5)
  expect_equal(power_params@icc, c(0.1, 0.05))
})

test_that("PowerParams class handles single values correctly", {
  power_params <- new("PowerParams",
    target_columns = "subject",
    levels = 0,
    power_curve = 0,
    nsimn = 100,
    alpha = 0.1,
    max_size = 50,
    breaks = NULL,
    effect_size = 1.2,
    icc = 0.2
  )

  expect_equal(power_params@target_columns, "subject")
  expect_equal(power_params@levels, 0)
  expect_equal(power_params@power_curve, 0)
  expect_equal(power_params@max_size, 50)
  expect_equal(power_params@effect_size, 1.2)
  expect_equal(power_params@icc, 0.2)
})

test_that("Class slots have correct types", {
  design <- create_test_design("basic")
  model <- create_test_model("normal")
  power_params <- create_test_power_params("basic")

  # Test RMeDesign slot types
  expect_type(design@response_column, "character")
  expect_type(design@condition_column, "character")
  expect_type(design@condition_is_categorical, "logical")
  expect_type(design@experimental_columns, "character")
  expect_type(design@covariate_is_categorical, "logical")
  expect_type(design@include_interaction, "logical")
  expect_type(design@na_action, "character")

  # Test ProbabilityModel slot types
  expect_type(model@error_is_non_normal, "logical")

  # Test PowerParams slot types
  expect_type(power_params@target_columns, "character")
  expect_type(power_params@levels, "double")
  expect_type(power_params@power_curve, "double")
  expect_type(power_params@nsimn, "double")
  expect_type(power_params@alpha, "double")
})
