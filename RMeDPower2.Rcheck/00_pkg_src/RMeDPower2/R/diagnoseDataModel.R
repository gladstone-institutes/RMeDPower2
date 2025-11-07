#' @title diagnoseDataModel
#'
#' @description This function can be used to generate diagnostic QC plots for given model assumptions related to the input data, identify potential outlier observations and/or outlier experimental units
#'
#'
#' @param data Input data frame with columns having all the necessary information regarding the dependent and independent variables of interest
#' @param design an object of class RMeDesign with the necessary design information about the data
#' @param model an object of class ProbabilityModel giving the error distribution of the data

#' @return A list with four elements. 1) models: representing the names of the models
#' evaluated based on differnt modifications of the response column.
#' The models would include one called natural_scale,
#' another model called natural_scale_wo_outliers if outliers had beeen identified,
#' another model called log_scale if the respose column is continuous
#' and the model on the log-transformed values of the responses are what was evaluated
#' and finally log_scale_wo_outliers model if there were outliers identified in the log_scale model.
#' 2) Data_updated representing the updated data frame with additional columns for the modified response column corresponding to each of the models evaluated.
#' 3) cooks_result: cooks distance of each of the experimental columns for each of the models evaluated.
#' For models based on the binomial probability distribution, cooks distance is only
#' reported for the first experimental column on account the increased computation time
#' for evaluating this metric for the other experimental columns.
#' 4) plots_info: is a list with two elements plots and captions. plots is a named list and captions is a character vector,
#' both of the same length as the number of models evaluated. Each element of the plots list is yet another
#' list of QC/diagnostic plots related to the corresponding model fit, while the captions is a vector of captions for each of the
#' QC plots output
#'
#' @export
#'
#' @examples result=diagnoseDataModel(data=data, design=design, model=model)

diagnoseDataModel <- function(data, design, model) {
  transform_data(data,
                  condition_column = design@condition_column,
                  experimental_columns = design@experimental_columns,
                  response_column = design@response_column,
                  total_column = design@total_column,
                  condition_is_categorical = design@condition_is_categorical,
                  covariate= design@covariate,
                  crossed_columns = design@crossed_columns,
                  error_is_non_normal=model@error_is_non_normal,
                  family_p=model@family_p,
                  alpha=design@outlier_alpha,
                  na.action=design@na_action,
                 include_interaction = design@include_interaction,
                 random_slope_variable = design@random_slope_variable,
                 covariate_is_categorical = design@covariate_is_categorical
                 )

}
