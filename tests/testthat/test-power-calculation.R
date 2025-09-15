# Tests for power calculation functions in RMeDPower2

test_that("calculatePower wrapper function works correctly", {
  test_data <- create_test_data()
  design <- create_test_design("basic")
  model <- create_test_model("normal")
  power_params <- create_test_power_params("basic")

  # This is a basic integration test - just check it doesn't error
  expect_no_error({
    result <- suppressMessages(suppressWarnings({
      calculatePower(
        data = test_data,
        design = design,
        model = model,
        power_param = power_params
      )
    }))
  })
})

test_that("calculate_power basic functionality works", {
  test_data <- create_test_data()

  # Test basic power calculation
  expect_no_error({
    result <- suppressMessages(suppressWarnings({
      calculate_power(
        data = test_data,
        condition_column = "treatment",
        experimental_columns = c("experiment", "plate"),
        response_column = "cell_size",
        target_columns = "experiment",
        condition_is_categorical = TRUE,
        levels = 1,
        power_curve = 1,
        nsimn = 5,  # Very small for fast testing
        covariate_is_categorical = TRUE
      )
    }))
  })
})

test_that("calculate_power handles different target columns", {
  test_data <- create_test_data()

  # Test with different target column
  expect_no_error({
    result <- suppressMessages(suppressWarnings({
      calculate_power(
        data = test_data,
        condition_column = "treatment",
        experimental_columns = c("experiment", "plate"),
        response_column = "cell_size",
        target_columns = "plate",
        condition_is_categorical = TRUE,
        levels = 1,
        power_curve = 1,
        nsimn = 5,
        covariate_is_categorical = TRUE
      )
    }))
  })
})

test_that("calculate_power handles covariates", {
  test_data <- create_test_data()

  # Test with covariate
  expect_no_error({
    result <- suppressMessages(suppressWarnings({
      calculate_power(
        data = test_data,
        condition_column = "treatment",
        experimental_columns = c("experiment", "plate"),
        response_column = "cell_size",
        target_columns = "experiment",
        condition_is_categorical = TRUE,
        covariate = "age",
        covariate_is_categorical = FALSE,
        levels = 1,
        power_curve = 1,
        nsimn = 5
      )
    }))
  })
})

test_that("calculate_power handles interactions", {
  test_data <- create_test_data()

  # Test with interaction
  expect_no_error({
    result <- suppressMessages(suppressWarnings({
      calculate_power(
        data = test_data,
        condition_column = "treatment",
        experimental_columns = c("experiment", "plate"),
        response_column = "cell_size",
        target_columns = "experiment",
        condition_is_categorical = TRUE,
        covariate = "age",
        covariate_is_categorical = FALSE,
        include_interaction = TRUE,
        levels = 1,
        power_curve = 1,
        nsimn = 5
      )
    }))
  })
})

test_that("calculate_power handles non-normal distributions", {
  test_data <- create_count_data()

  # Test with Poisson distribution
  expect_no_error({
    result <- suppressMessages(suppressWarnings({
      calculate_power(
        data = test_data,
        condition_column = "treatment",
        experimental_columns = c("experiment", "plate"),
        response_column = "count",
        target_columns = "experiment",
        condition_is_categorical = TRUE,
        error_is_non_normal = TRUE,
        family_p = "poisson",
        levels = 1,
        power_curve = 1,
        nsimn = 5,
        covariate_is_categorical = TRUE
      )
    }))
  })
})

test_that("calculate_power handles binomial data", {
  test_data <- create_count_data()

  # Test with binomial distribution
  expect_no_error({
    result <- suppressMessages(suppressWarnings({
      calculate_power(
        data = test_data,
        condition_column = "treatment",
        experimental_columns = c("experiment", "plate"),
        response_column = "count",
        total_column = "total_count",
        target_columns = "experiment",
        condition_is_categorical = TRUE,
        error_is_non_normal = TRUE,
        family_p = "binomial",
        levels = 1,
        power_curve = 1,
        nsimn = 5,
        covariate_is_categorical = TRUE
      )
    }))
  })
})

test_that("calculate_power handles multiple target columns", {
  test_data <- create_test_data()

  # Test with multiple target columns
  expect_no_error({
    result <- suppressMessages(suppressWarnings({
      calculate_power(
        data = test_data,
        condition_column = "treatment",
        experimental_columns = c("experiment", "plate"),
        response_column = "cell_size",
        target_columns = c("experiment", "plate"),
        condition_is_categorical = TRUE,
        levels = c(1, 1),
        power_curve = 1,
        nsimn = 5,
        covariate_is_categorical = TRUE
      )
    }))
  })
})

test_that("calculate_power respects levels parameter", {
  test_data <- create_test_data()

  # Test with levels = 0 (increase within-group sample size)
  expect_no_error({
    result <- suppressMessages(suppressWarnings({
      calculate_power(
        data = test_data,
        condition_column = "treatment",
        experimental_columns = c("experiment", "plate"),
        response_column = "cell_size",
        target_columns = "experiment",
        condition_is_categorical = TRUE,
        levels = 0,
        power_curve = 1,
        nsimn = 5,
        covariate_is_categorical = TRUE
      )
    }))
  })
})

test_that("calculate_power respects power_curve parameter", {
  test_data <- create_test_data()

  # Test with power_curve = 0 (single power calculation)
  expect_no_error({
    result <- suppressMessages(suppressWarnings({
      calculate_power(
        data = test_data,
        condition_column = "treatment",
        experimental_columns = c("experiment", "plate"),
        response_column = "cell_size",
        target_columns = "experiment",
        condition_is_categorical = TRUE,
        levels = 1,
        power_curve = 0,
        nsimn = 5,
        covariate_is_categorical = TRUE
      )
    }))
  })
})

test_that("calculate_power handles effect size specification", {
  test_data <- create_test_data()

  # Test with specified effect size
  expect_no_error({
    result <- suppressMessages(suppressWarnings({
      calculate_power(
        data = test_data,
        condition_column = "treatment",
        experimental_columns = c("experiment", "plate"),
        response_column = "cell_size",
        target_columns = "experiment",
        condition_is_categorical = TRUE,
        levels = 1,
        power_curve = 1,
        effect_size = 0.5,
        nsimn = 5,
        covariate_is_categorical = TRUE
      )
    }))
  })
})

test_that("calculate_power handles max_size parameter", {
  test_data <- create_test_data()

  # Test with max_size specification
  expect_no_error({
    result <- suppressMessages(suppressWarnings({
      calculate_power(
        data = test_data,
        condition_column = "treatment",
        experimental_columns = c("experiment", "plate"),
        response_column = "cell_size",
        target_columns = "experiment",
        condition_is_categorical = TRUE,
        levels = 1,
        power_curve = 1,
        max_size = 5,
        nsimn = 5,
        covariate_is_categorical = TRUE
      )
    }))
  })
})

test_that("calculate_power handles random slopes", {
  test_data <- create_test_data()

  # Test with random slopes for condition
  expect_no_error({
    result <- suppressMessages(suppressWarnings({
      calculate_power(
        data = test_data,
        condition_column = "treatment",
        experimental_columns = c("experiment", "plate"),
        response_column = "cell_size",
        target_columns = "experiment",
        condition_is_categorical = TRUE,
        random_slope_variable = "condition_column",
        levels = 1,
        power_curve = 1,
        nsimn = 5,
        covariate_is_categorical = TRUE
      )
    }))
  })
})

test_that("calculate_power handles crossed experimental factors", {
  test_data <- create_test_data()

  # Test with crossed columns
  expect_no_error({
    result <- suppressMessages(suppressWarnings({
      calculate_power(
        data = test_data,
        condition_column = "treatment",
        experimental_columns = c("experiment", "plate"),
        response_column = "cell_size",
        target_columns = "experiment",
        condition_is_categorical = TRUE,
        crossed_columns = "plate",
        levels = 1,
        power_curve = 1,
        nsimn = 5,
        covariate_is_categorical = TRUE
      )
    }))
  })
})

test_that("calculate_power validates parameter combinations", {
  test_data <- create_test_data()

  # Test mismatched levels and target_columns lengths
  expect_null({
    calculate_power(
      data = test_data,
      condition_column = "treatment",
      experimental_columns = c("experiment", "plate"),
      response_column = "cell_size",
      target_columns = c("experiment", "plate"),
      condition_is_categorical = TRUE,
      levels = 1,  # Should be c(1, 1) for two target columns
      power_curve = 1,
      nsimn = 5,
      covariate_is_categorical = TRUE
    )
  })
})

test_that("calculatePower integrates S4 classes correctly", {
  test_data <- create_test_data()

  # Test full S4 class integration
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

  expect_no_error({
    result <- suppressMessages(suppressWarnings({
      calculatePower(
        data = test_data,
        design = design,
        model = model,
        power_param = power_params
      )
    }))
  })
})
