#' @title getEstimatesOfInterest
#'
#' @description This function performs the estimations of interest and also visualizes the resulting association
#'
#'
#' @param data Input data frame with columns having all the necessary information regarding the dependent and independent variables of interest
#' @param design an object of class RMeDesign with the necessary design information about the data
#' @param model an object of class ProbabilityModel giving the error distribution of the data
#'
#' @return an object of class summary.merMod and a data frame with the residuals for each observation that can be used to generate other visualizations
#'
#' @export
#'
#' @examples result=diagnoseDataModel(data=data, design=design, model=model)

getEstimatesOfInterest <- function(data, design, model) {

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
                                                crossed_columns, total_column, error_is_non_normal, family_p, na.action, include_interaction, random_slope_variable, covariate_is_categorical)

  return(res)

}
