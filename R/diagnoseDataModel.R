#' @title diagnoseDataModel
#'
#' @description This functions makes quantile-quanitle (qq) plots of i) raw residual values ii) log-transformed residual values iii) raw residual values after removing outliers, and iv) log-transformed residual values after removing outliers. To detect outliers, the function uses Rosner's test.
#'
#'
#' @param data Input data frame with columns having all the necessary information regarding the dependent and independent variables of interest
#' @param design an object of class RMeDesign with the necessary design information about the data
#' @param model an object of class ProbabilityModel giving the error distribution of the data
#'
#' @return For continuous data, the function returns quantile-quanitle (qq) plots of i) raw residual values ii) log-transformed residual values iii) raw residual values after removing outliers, and iv) log-transformed residual values. For discrete data, it returns a histogram. If "rosner" is chosen, a matrix with updated feature values after transformation will be returned. If "cook" is choose, a list with  a matrix with updated feature values after transformation will be returned,
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
                  na.action=design@na_action)

}
