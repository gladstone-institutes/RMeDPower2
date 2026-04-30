#' @title RMeDesign-class
#'
#' @description Objects of RMeDesign class store information on the relevant repeated measures design for the given data
#'
#'
#' @param data Input data
#' @param condition_column Name of the condition variable (ex variable with values such as control/case). The input file has to have a corresponding column name
#' @param experimental_columns Name of the variable related to experimental design such as "experiment", "plate", and "cell_line". They should be in order, for example, "experiment" should always come first .
#' @param response_column Name of the variable observed by performing the experiment. ex) intensity.
#' @param total_column Set this column only when family_p="binomial" and it is equal to the total number of observations (number of cases plus number of controls) for a given number of cases
#' @param outlier_alpha numeric scalar between 0 and 1 indicating the Type I error associated with the test of outliers
#' @param condition_is_categorical Specify whether the condition variable is categorical. TRUE: Categorical, FALSE: Continuous.
#' @param covariate The name of the covariate to control in the regression model
#' @param method The method used to detect outliers. "rosner" (default) runs Rosner's test and "cook" runs Cook's distance.
#' @param crossed_columns Name of experimental variables that may appear repeatedly with the same ID. For example, cell_line C1 may appear in multiple experiments, but plate P1 cannot appear in more than one experiment
#' @param error_is_non_normal Default: the observed variable is continuous Categorical response variable will be implemented in the future. TRUE: Categorical , FALSE: Continuous (default).
#' @param family_p The type of distribution family to specify when the response is categorical. If family is "binary" then binary(link="log") is used, if family is "poisson" then poisson(link="logit") is used, if family is "poisson_log" then poisson(link=") log") is used.
#' @param na.action "complete": missing data is not allowed in all columns (default), "unique": missing data is not allowed only in condition, experimental, and response columns. Selecting "complete" removes an entire row when there is one or more missing values, which may affect the distribution of other features.
#' @param include_interaction logical - TRUE or FALSE - Whether to include condition * covariate interaction
#' @param random_slope_variable Variable for random slopes (typically one of "condition_column" or "covariate" and assuming that they are numeric variables). A random slope term is added for each of the variables specified in the experimental columns in addition to their corresponding random intercept terms. The random slope and intercept terms for each experimental_columns variable are assumed to be uncorrelated.
#' @param covariate_is_categorical Specify whether the covariate variable is categorical. TRUE: Categorical, FALSE: Continuous.
#'
#' @return an object of class RMeDesign
#'
#' @export
#'
#' @examples design=new("RMeDesign")

setClass("RMeDesign",
         slots = list(
           response_column = "character",
           covariate = "ANY",
           condition_column = "character",
           condition_is_categorical = "logical",
           experimental_columns = "character",
           crossed_columns = "ANY",
           total_column = "ANY",
           outlier_alpha = "numeric",
           na_action = "character",
           # NEW SLOTS FOR ENHANCED FUNCTIONALITY
           include_interaction = "logical",    # Whether to include condition * covariate interaction
           random_slope_variable = "ANY", # Variable for random slopes (typically condition_column)
           covariate_is_categorical = "logical" # Specify whether the covariate variable is categorical. TRUE: Categorical, FALSE: Continuous.
         ),
         prototype = list(
           response_column = "response_column",
           covariate = NULL,
           condition_column = "condition_column",
           condition_is_categorical = TRUE,
           experimental_columns = "experimental_column",
           crossed_columns = NULL,
           total_column = NULL,
           outlier_alpha = 0.05,
           na_action = "complete",
           # NEW DEFAULTS
           include_interaction = NA,        # Default: no interaction
           random_slope_variable = NULL,        # Default: no random slopes
           covariate_is_categorical = NA
         ))

#' @title ProbabilityModel-class
#'
#' @description Objects of ProbabilityModel class store information on the assumed probability distribution for the model
#'
#'
#' @param error_is_non_normal Default: the observed variable is continuous Categorical response variable will be implemented in the future. TRUE: Categorical , FALSE: Continuous (default).
#' @param family_p The type of distribution family to specify when the response is categorical. If family is "binary" then binary(link="log") is used, if family is "poisson" then poisson(link="logit") is used, if family is "poisson_log" then poisson(link=") log") is used.
#'
#' @return an object of class ProbabilityModel
#'
#' @export
#'
#' @examples model=new("ProbabilityModel")


setClass("ProbabilityModel",
         slots = list(
           error_is_non_normal = "logical",
           family_p = "ANY"
         ),
         prototype = list(
           error_is_non_normal = FALSE,
           family_p = NULL
         ))

#' @title PowerParams-class
#'
#' @description Objects of PowerParams class store information required for sample size estimation for given data
#'
#'
#' @param power_curve 1: Power simulation over a range of sample sizes or levels. 0: Power calculation over a single sample size or a level.
#' @param nsimn The number of simulations to run. Default=1000
#' @param target_columns Name of the experimental parameters to use for the power calculation.
#' @param levels 1: Amplify the number of corresponding target parameter. 0: Amplify the number of samples from the corresponding target parameter, ex) If target_columns = c("experiment","cell_line") and if you want to expand the number of experiment and sample more cells from each cell line, set levels = c(1,0).
#' @param max_size Maximum levels or sample sizes to test. Default: the current level or the current sample size x 5. ex) If max_levels = c(10,5), it will test upto 10 experiments and 5 cell lines.
#' @param breaks Levels /sample sizes of the variable to be specified along the power curve. Default: max(1, round( the number of current levels / 5 ))
#' @param  effect_size If you know the effect size of your condition variable, the effect size can be provided as a parameter. If the effect size is not provided, it will be estimated from your data
#' @param  alpha Threshold for Type I error
#' @param  ICC Intra-Class Coefficients (ICC) for each parameter
#'
#' @return an object of class ProbabilityModel
#'
#' @export
#'
#' @examples power_param=new("PowerParams")

setClass("PowerParams",
         slots = list(
           target_columns = "character",
           power_curve = "numeric",
           nsimn = "numeric",
           levels = "numeric",
           max_size = "ANY",
           alpha = "numeric",
           breaks = "ANY",
           effect_size = "ANY",
           icc = "ANY"
         ),
         prototype = list(
           target_columns = "experimental_column",
           power_curve = 1,
           nsimn = 20,
           levels = 1,
           max_size = NULL,
           alpha = 0.05,
           breaks = NULL,
           effect_size = NULL,
           icc = NULL
         ))



