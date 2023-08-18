#' @title calculatePower
#'
#' @description This functions makes statistical power estimates given the data, the underlying design for it and the assumed probability model of the error distribution
#'
#'
#' @param data Input data frame with columns having all the necessary information regarding the dependent and independent variables of interest
#' @param design an object of class RMeDesign with the necessary design information about the data
#' @param model an object of class ProbabilityModel giving the error distribution of the data
#' @param power_param an object of class PowerParams giving the target parameter of interest and the other necessary parameter to perform the power estimation
#'
#' @return A power curve image or a power calculation result printed in a text fileosner" is chosen, a matrix with updated feature values after transformation will be returned. If "cook" is choose, a list with  a matrix with updated feature values after transformation will be returned,
#'
#' @export
#'
#' @examples result=calculatePower(data=data, design=design, model=model, power_param=power_param)

calculatePower <- function(data, design, model, power_param) {
  calculate_power(data = data,
                            condition_column = design@condition_column,
                            experimental_columns = design@experimental_columns,
                            response_column = design@response_column,
                            total_column = design@total_column,
                            target_columns = power_param@target_columns,
                            power_curve = power_param@power_curve,
                            condition_is_categorical = design@condition_is_categorical,
                            covariate=design@covariate,
                            crossed_columns = design@crossed_columns,
                            error_is_non_normal=model@error_is_non_normal,
                            nsimn=power_param@nsimn,
                            family_p=model@family_p,
                            levels=power_param@levels,
                            max_size=power_param@max_size,
                            breaks=power_param@breaks,
                            effect_size=power_param@effect_size,
                            ICC=power_param@icc,
                            na.action=design@na_action,
                            output=NULL,
                            alpha = power_param@alpha)

}
