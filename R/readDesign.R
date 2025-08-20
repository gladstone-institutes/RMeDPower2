#' @title readDesign
#'
#' @description This functions reads the underlying design for the data
#'
#'
#' @param jsonfile the jsonfile with the necessary design parameters: condition_column, experimental_columns, response_column, total_column, condition_is_categorical, covariate, method, crossed_columns, error_is_non_normal, family_p, outlier_alpha, na.action
#'
#' @return an object of class RMeDesign
#' @export
#'
#' @examples design=readDesign(jsonfile)

readDesign <- function(jsonfile) {
  design_data <- jsonlite::fromJSON(jsonfile)

  design <- new("RMeDesign")

  design@response_column = design_data$response_column

  design@condition_column = design_data$condition_column
  design@condition_is_categorical = design_data$condition_is_categorical
  design@experimental_columns = design_data$experimental_columns
  if("crossed_columns" %in% names(design_data) && !is.null(design_data$crossed_columns)) design@crossed_columns = design_data$crossed_columns
  if("total_column" %in% names(design_data) && !is.null(design_data$total_column)) design@total_column = design_data$total_column
  if("covariate" %in% names(design_data) && !is.null(design_data$covariate)) design@covariate = design_data$covariate
  if("covariate_is_categorical" %in% names(design_data)) design@covariate_is_categorical = design_data$covariate_is_categorical
  if("include_interaction" %in% names(design_data)) design@include_interaction = design_data$include_interaction
  if("random_slope_variable" %in% names(design_data) && !is.null(design_data$random_slope_variable)) design@random_slope_variable = design_data$random_slope_variable
  if("outlier_alpha" %in% names(design_data) && is.numeric(design_data$outlier_alpha)) design@outlier_alpha = design_data$outlier_alpha
  return(design)
}
