# Helper functions and test data for RMeDPower2 tests

# Create simple test dataset
create_test_data <- function(n_experiments = 2, n_plates = 3, n_wells = 10) {
  set.seed(123)  # For reproducible tests

  n_total <- n_experiments * n_plates * n_wells

  data.frame(
    experiment = rep(1:n_experiments, each = n_plates * n_wells),
    plate = rep(rep(1:n_plates, each = n_wells), n_experiments),
    well = rep(1:n_wells, n_experiments * n_plates),
    treatment = rep(c("control", "treated"), length.out = n_total),
    cell_size = rnorm(n_total, mean = 100, sd = 15) +
                rep(c(0, 10), length.out = n_total), # treatment effect
    batch = rep(c("A", "B"), length.out = n_total),
    age = runif(n_total, min = 20, max = 60),
    stringsAsFactors = FALSE
  )
}

# Create test data with missing values
create_test_data_with_na <- function() {
  data <- create_test_data(n_experiments = 2, n_plates = 2, n_wells = 5)
  # Introduce some missing values
  data$cell_size[c(5, 10, 15)] <- NA
  data$treatment[20] <- NA
  data$age[c(3, 8)] <- NA
  return(data)
}

# Create count data for testing non-normal distributions
create_count_data <- function(n_experiments = 2, n_plates = 3, n_wells = 10) {
  set.seed(123)

  n_total <- n_experiments * n_plates * n_wells

  data.frame(
    experiment = rep(1:n_experiments, each = n_plates * n_wells),
    plate = rep(rep(1:n_plates, each = n_wells), n_experiments),
    well = rep(1:n_wells, n_experiments * n_plates),
    treatment = rep(c("control", "treated"), length.out = n_total),
    count = rpois(n_total, lambda = rep(c(5, 8), length.out = n_total)),
    total_count = rep(100, n_total),
    stringsAsFactors = FALSE
  )
}

# Create valid design objects for testing
create_test_design <- function(type = "basic") {
  switch(type,
    "basic" = new("RMeDesign",
      response_column = "cell_size",
      condition_column = "treatment",
      condition_is_categorical = TRUE,
      experimental_columns = c("experiment", "plate"),
      covariate = NULL,
      covariate_is_categorical = NA,
      crossed_columns = NULL,
      include_interaction = FALSE,
      random_slope_variable = NULL,
      total_column = NULL,
      na_action = "complete"
    ),
    "with_covariate" = new("RMeDesign",
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
    ),
    "count_data" = new("RMeDesign",
      response_column = "count",
      condition_column = "treatment",
      condition_is_categorical = TRUE,
      experimental_columns = c("experiment", "plate"),
      covariate = NULL,
      covariate_is_categorical = TRUE,
      crossed_columns = NULL,
      include_interaction = FALSE,
      random_slope_variable = NULL,
      total_column = "total_count",
      na_action = "complete"
    )
  )
}

# Create valid probability model objects
create_test_model <- function(type = "normal") {
  switch(type,
    "normal" = new("ProbabilityModel",
      error_is_non_normal = FALSE,
      family_p = NULL
    ),
    "poisson" = new("ProbabilityModel",
      error_is_non_normal = TRUE,
      family_p = "poisson"
    ),
    "binomial" = new("ProbabilityModel",
      error_is_non_normal = TRUE,
      family_p = "binomial"
    )
  )
}

# Create valid power parameter objects
create_test_power_params <- function(type = "basic") {
  switch(type,
    "basic" = new("PowerParams",
      target_columns = "experiment",
      levels = 1,
      power_curve = 1,
      nsimn = 10,  # Small number for fast testing
      alpha = 0.05,
      max_size = 5,
      breaks = NULL,
      effect_size = NULL,
      icc = NULL
    ),
    "multiple_targets" = new("PowerParams",
      target_columns = c("experiment", "plate"),
      levels = c(1, 1),
      power_curve = 1,
      nsimn = 10,
      alpha = 0.05,
      max_size = c(5, 10),
      breaks = NULL,
      effect_size = NULL,
      icc = NULL
    )
  )
}

# Helper function to check if an object is approximately equal (for numeric comparisons)
expect_approximately_equal <- function(object, expected, tolerance = 1e-6) {
  expect_true(abs(object - expected) < tolerance)
}

# Helper function to suppress warnings for known issues during testing
suppress_known_warnings <- function(expr) {
  suppressWarnings({
    # Suppress known warnings about lme4::getData vs simr::getData masking
    expr
  })
}
