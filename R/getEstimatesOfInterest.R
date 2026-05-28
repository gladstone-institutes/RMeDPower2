#' @title getEstimatesOfInterest
#'
#' @description This function performs the estimations of interest and also visualizes the resulting association
#'
#'
#' @param data Input data frame with columns having all the necessary information regarding the dependent and independent variables of interest
#' @param design an object of class RMeDesign with the necessary design information about the data
#' @param model an object of class ProbabilityModel giving the error distribution of the data
#' @param print_plots Whether or not to print the plots, irrespective of this argument ggplot versions of evaluated association between the response_column and the condition_column. TRUE - print the plot, FALSE - do not print the plot
#'
#' @return a list with three elements - 1. an object of class summary.merMod and
#' 2. the output from the get_residuals functions. This output consists of a list
#' with 3 elements. 1. The updated input data with an additional column with the model residuals of the individual observations. 2. A plot representing the purported association between the response column and the condition column. 3. The corresponding caption for this figure.
#' 3. an object of class merMod
#'
#' @export
#'
#' @examples
#' \donttest{
#' template_dir <- system.file("input_templates/cell_assay_data", package = "RMeDPower2")
#' data <- plate_assay_pilot_data
#' design <- readDesign(file.path(template_dir,"design_cell_assay.json"))
#' model <- readProbabilityModel(file.path(template_dir,"prob_model.json"))
#' res <- getEstimatesOfInterest(data, design, model)
#' }

getEstimatesOfInterest <- function(data, design, model,print_plots=TRUE) {

  ##assign input arguments
  condition_column = design@condition_column
  experimental_columns = design@experimental_columns
  response_column = design@response_column
  total_column = design@total_column
  condition_is_categorical = design@condition_is_categorical
  covariate= design@covariate
  crossed_columns = design@crossed_columns
  error_is_non_normal=model@error_is_non_normal
  family_p=model@family_p
  alpha=design@outlier_alpha
  na.action=design@na_action

  # New enhanced parameters
  include_interaction = design@include_interaction
  random_slope_variable = design@random_slope_variable
  covariate_is_categorical = design@covariate_is_categorical

  # cat("\n=== Enhanced Estimates of Interest Analysis ===\n")
  # cat("Model specifications:\n")
  # cat("- Interaction effects:", ifelse(include_interaction, "Yes", "No"), "\n")
  # cat("- Random slopes:", ifelse(!is.null(random_slope_variable), "Yes", "No"), "\n")

  res <- list()
  ##get estimates
  res[[1]] <- calculate_lmer_estimates(data, condition_column, experimental_columns, response_column, total_column, condition_is_categorical, covariate,
                                                 crossed_columns, error_is_non_normal, family_p, na.action, include_interaction, random_slope_variable, covariate_is_categorical)
  ##visualize estimates
  res[[2]] <- get_residuals(data, condition_column, experimental_columns, response_column, condition_is_categorical, covariate,
                                                crossed_columns, total_column, error_is_non_normal, family_p, na.action, include_interaction, random_slope_variable, covariate_is_categorical, print_plots)

  temp_model_and_data <- get_model_and_data(data, condition_column, experimental_columns, response_column, total_column, condition_is_categorical, covariate,
                                       crossed_columns, error_is_non_normal, family_p, na.action, include_interaction, random_slope_variable, covariate_is_categorical)

  lms=get_model_and_data(data=data, condition_column, experimental_columns,
                         response_column, total_column, condition_is_categorical, covariate,
                         crossed_columns, error_is_non_normal, family_p, na.action=na.action,include_interaction, random_slope_variable, covariate_is_categorical)
  res[[3]] <- lms[[1]]

  return(res)

}
