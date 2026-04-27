# Tests for power calculation via calculatePower public API

test_that("calculatePower wrapper function works correctly", {
  test_data <- create_test_data()
  design <- create_test_design("basic")
  model <- create_test_model("normal")
  power_params <- create_test_power_params("basic")

  expect_no_error(suppressWarnings(suppressMessages(
    calculatePower(data = test_data, design = design, model = model, power_param = power_params)
  )))
})

test_that("calculatePower handles different target columns", {
  test_data <- create_test_data()
  design <- create_test_design("basic")
  model <- create_test_model("normal")
  power_params <- create_test_power_params("basic")
  power_params@target_columns <- "plate"

  expect_no_error(suppressWarnings(suppressMessages(
    calculatePower(data = test_data, design = design, model = model, power_param = power_params)
  )))
})

test_that("calculatePower handles covariates", {
  test_data <- create_test_data()
  design <- create_test_design("with_covariate")
  model <- create_test_model("normal")
  power_params <- create_test_power_params("basic")

  expect_no_error(suppressWarnings(suppressMessages(
    calculatePower(data = test_data, design = design, model = model, power_param = power_params)
  )))
})

test_that("calculatePower handles interactions", {
  test_data <- create_test_data()
  design <- create_test_design("with_covariate")
  design@include_interaction <- TRUE
  model <- create_test_model("normal")
  power_params <- create_test_power_params("basic")

  expect_no_error(suppressWarnings(suppressMessages(
    calculatePower(data = test_data, design = design, model = model, power_param = power_params)
  )))
})

test_that("calculatePower handles non-normal distributions", {
  test_data <- create_count_data()
  design <- create_test_design("count_data")
  model <- create_test_model("poisson")
  power_params <- create_test_power_params("basic")

  expect_no_error(suppressWarnings(suppressMessages(
    calculatePower(data = test_data, design = design, model = model, power_param = power_params)
  )))
})

test_that("calculatePower handles binomial data", {
  test_data <- create_count_data()
  design <- create_test_design("count_data")
  model <- create_test_model("binomial")
  power_params <- create_test_power_params("basic")

  expect_no_error(suppressWarnings(suppressMessages(
    calculatePower(data = test_data, design = design, model = model, power_param = power_params)
  )))
})

test_that("calculatePower handles multiple target columns", {
  test_data <- create_test_data()
  design <- create_test_design("basic")
  model <- create_test_model("normal")
  power_params <- create_test_power_params("multiple_targets")

  expect_no_error(suppressWarnings(suppressMessages(
    calculatePower(data = test_data, design = design, model = model, power_param = power_params)
  )))
})

test_that("calculatePower handles levels = 0 (within-group sample size)", {
  test_data <- create_test_data()
  design <- create_test_design("basic")
  model <- create_test_model("normal")
  power_params <- create_test_power_params("basic")
  power_params@levels <- 0

  expect_no_error(suppressWarnings(suppressMessages(
    calculatePower(data = test_data, design = design, model = model, power_param = power_params)
  )))
})

test_that("calculatePower handles power_curve = 0 (single calculation)", {
  test_data <- create_test_data()
  design <- create_test_design("basic")
  model <- create_test_model("normal")
  power_params <- create_test_power_params("basic")
  power_params@power_curve <- 0

  expect_no_error(suppressWarnings(suppressMessages(
    calculatePower(data = test_data, design = design, model = model, power_param = power_params)
  )))
})

test_that("calculatePower handles effect size specification", {
  test_data <- create_test_data()
  design <- create_test_design("basic")
  model <- create_test_model("normal")
  power_params <- create_test_power_params("basic")
  power_params@effect_size <- c(0.5, NA, NA)

  expect_no_error(suppressWarnings(suppressMessages(
    calculatePower(data = test_data, design = design, model = model, power_param = power_params)
  )))
})

test_that("calculatePower handles max_size parameter", {
  test_data <- create_test_data()
  design <- create_test_design("basic")
  model <- create_test_model("normal")
  power_params <- create_test_power_params("basic")
  power_params@max_size <- 5

  expect_no_error(suppressWarnings(suppressMessages(
    calculatePower(data = test_data, design = design, model = model, power_param = power_params)
  )))
})

test_that("calculatePower handles random slopes", {
  test_data <- create_test_data()
  design <- create_test_design("basic")
  design@random_slope_variable <- "treatment"
  model <- create_test_model("normal")
  power_params <- create_test_power_params("basic")

  expect_no_error(suppressWarnings(suppressMessages(
    calculatePower(data = test_data, design = design, model = model, power_param = power_params)
  )))
})

test_that("calculatePower handles crossed experimental factors", {
  test_data <- create_test_data()
  design <- create_test_design("basic")
  design@crossed_columns <- "plate"
  model <- create_test_model("normal")
  power_params <- create_test_power_params("basic")

  expect_no_error(suppressWarnings(suppressMessages(
    calculatePower(data = test_data, design = design, model = model, power_param = power_params)
  )))
})

test_that("calculatePower integrates S4 classes correctly", {
  test_data <- create_test_data()

  design <- new("RMeDesign",
    response_column = "cell_size",
    condition_column = "treatment",
    condition_is_categorical = TRUE,
    experimental_columns = c("experiment", "plate"),
    covariate = "age",
    covariate_is_categorical = FALSE,
    crossed_columns = NULL,
    include_interaction = FALSE,
    random_slope_variable = NULL,
    total_column = NULL,
    na_action = "complete"
  )

  model <- new("ProbabilityModel",
    error_is_non_normal = FALSE,
    family_p = NULL
  )

  power_params <- new("PowerParams",
    target_columns = "experiment",
    levels = 1,
    power_curve = 1,
    nsimn = 5,
    alpha = 0.05,
    max_size = NULL,
    breaks = NULL,
    effect_size = NULL,
    icc = NULL
  )

  expect_no_error(suppressWarnings(suppressMessages(
    calculatePower(data = test_data, design = design, model = model, power_param = power_params)
  )))
})
