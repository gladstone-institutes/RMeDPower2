#' @title get_model_and_data
#'
#'
#' @description This function performs a linear mixed model analysis using lmer.
#'
#'
#' @import multtest
#' @import simr
#' @import lme4
#' @import lmerTest
#' @import readxl
#'
#' @param data Input data
#' @param condition_column Name of the condition variable (ex variable with values such as control/case). The input file has to have a corresponding column name
#' @param experimental_columns Name of variables related to experimental design such as "experiment", "plate", and "cell_line". They should be in order, for example, "experiment" should always come first .
#' @param response_column Name of the variable observed by performing the experiment. ex) intensity.
#' @param total_column Set this column only when family_p="binomial" and it is equal to the total number of observations (number of cases plus number of controls) for a given number of cases
#' @param condition_is_categorical Specify whether the condition variable is categorical. TRUE: Categorical, FALSE: Continuous.
#' @param covariate The name of the covariate to control in the regression model
#' @param crossed_columns Name of experimental variables that may appear repeatedly with the same ID. For example, cell_line C1 may appear in multiple experiments, but plate P1 cannot appear in more than one experiment
#' @param error_is_non_normal Default: the observed variable is continuous Categorical response variable will be implemented in the future. TRUE: Categorical , FALSE: Continuous (default).
#' @param family_p The type of distribution family to specify when the response is categorical. If family is "binary" then binary(link="log") is used, if family is "poisson" then poisson(link="logit") is used, if family is "poisson_log" then poisson(link=") log") is used.
#' @param na.action "complete": missing data is not allowed in all columns (default), "unique": missing data is not allowed only in condition, experimental, and response columns. Selecting "complete" removes an entire row when there is one or more missing values, which may affect the distribution of other features.
#' @param include_interaction Whether to include condition * covariate interaction
#' @param random_slope_variable Variable for random slopes (typically "condition_column")
#' @param covariate_is_categorical Specify whether the covariate variable is categorical. TRUE: Categorical, FALSE: Continuous.
#'
#' @return A list of the linear mixed model result, original data, experimental column names, and residual values
#'
#' @export
#' @examples



get_model_and_data <- function(data, condition_column, experimental_columns, response_column, total_column = NULL, condition_is_categorical,  covariate=NULL,
                                     crossed_columns=NULL, error_is_non_normal=FALSE, family_p=NULL, na.action="complete",
                               include_interaction = FALSE,
                               random_slope_variable = NULL,
                               covariate_is_categorical = TRUE){



  ######input error handler
  if(!is.null(covariate) )
    if(!covariate%in%colnames(data))
      { print("covariate should be null or one of the column names");return(NULL) }
  if(!condition_column%in%colnames(data)){ print("condition_column should be one of the column names");return(NULL) }
  if(sum(experimental_columns%in%colnames(data))!=length(experimental_columns) ){ print("experimental_columns must match column names");return(NULL) }
  if(!response_column%in%colnames(data)){  print("response_column should be one of the column names");return(NULL) }
  if(!is.na(condition_is_categorical) && !condition_is_categorical%in%c(TRUE,FALSE)){ print("condition_is_categorical must be TRUE or FALSE");return(NULL) }
  if(!is.null(crossed_columns)){if(sum(crossed_columns%in%colnames(data))!=length(crossed_columns) ){ print("crossed_columns must match column names");return(NULL) }}
  if(!is.na(covariate_is_categorical) && !covariate_is_categorical%in%c(TRUE,FALSE)){ print("covariate_is_categorical must be TRUE or FALSE");return(NULL) }

  # Validation for new parameters
  if(!is.na(include_interaction)) {
    if (include_interaction && is.null(covariate)) {
      print("Cannot include interaction when covariate is NULL")
      return(NULL)
    }
  }

  if (!is.null(random_slope_variable) &&
      !random_slope_variable %in% c("condition_column", condition_column, "covariate", covariate)) {
    print("random_slope_variable should be 'condition_column', 'covariate' or the actual condition column name or covariate column name")
    return(NULL)
  }

  if(error_is_non_normal==TRUE){
    if(family_p != "negative_binomial")
      family_p=switch(family_p,
                      "poisson" = poisson(link="log"),
                      "binomial" = binomial(link="logit"),
                      "bionomial_log" = binomial(link="log"),
                      "Gamma_log" = Gamma(link = "log"),
                      "Gamma" = Gamma(link = "inverse"))
    else{
      family_p = list(family = "negative_binomial")
    }
  }

  family_p<<-family_p

  if(na.action=="complete"){

    notNAindex=which( rowSums(is.na(data)) == 0 )

  }else if(na.action=="unique"){

    if(is.null(covariate)) notNAindex=which( rowSums(is.na(data[,c(condition_column, experimental_columns, response_column, covariate)])) == 0 )
    else notNAindex=which( rowSums(is.na(data[,c(condition_column, experimental_columns, response_column)])) == 0 )


  }



  fixed_global_variable_data=data[notNAindex,]


  # cat("\n")
  # print("__________________________________________________________________Summary of data:")
  # print(summary(fixed_global_variable_data))
  # cat("\n")

  colnames_original=colnames(fixed_global_variable_data)
  experimental_columns_index=NULL
  ####### assign categorical variables
  if(condition_is_categorical==TRUE)
    fixed_global_variable_data[,condition_column]=as.factor(fixed_global_variable_data[,condition_column])
  else
    fixed_global_variable_data[,condition_column]=as.numeric(fixed_global_variable_data[,condition_column])

  if(!is.null(covariate)) {
    if(covariate_is_categorical==TRUE)
      fixed_global_variable_data[,covariate]=as.factor(fixed_global_variable_data[,covariate])
    else
      fixed_global_variable_data[,covariate]=as.numeric(fixed_global_variable_data[,covariate])
  }
  # random slope should be allowed only with a continuous variable
  if(!is.null(random_slope_variable)) {
    if(class(fixed_global_variable_data[,random_slope_variable]) != "numeric") {
      print("random_slope_variable should be a numeric variable")
      return(NULL)
    }
  }

  #cat("\n")


  noncrossed_columns=NULL

  for(i in 1:length(experimental_columns)){
    fixed_global_variable_data[,experimental_columns[i]]=as.factor(fixed_global_variable_data[,experimental_columns[i]])
    experimental_columns_index=c(experimental_columns_index,which(colnames(fixed_global_variable_data)==experimental_columns[i]))
    colnames(fixed_global_variable_data)[experimental_columns_index[i]]=paste("experimental_column",i,sep="")

    if(i!=1&&!experimental_columns[i]%in%crossed_columns){
      noncrossed_columns=c(noncrossed_columns, paste("experimental_column",i,sep=""))
    }


    #cat("\n")
    # print(paste("_________________________________",experimental_columns[i]," is assigned to experimental_column",i,sep=""))
    #cat("\n")
  }



  if(length(experimental_columns)>=2){
      for(r in 2:length(experimental_columns)){
      if(colnames(fixed_global_variable_data)[experimental_columns_index[r]]%in%noncrossed_columns){
        fixed_global_variable_data[,experimental_columns_index[r]]=paste(fixed_global_variable_data[,experimental_columns_index[r-1]], fixed_global_variable_data[,experimental_columns_index[r]],sep="_")
        fixed_global_variable_data[,experimental_columns_index[r]]=as.factor(fixed_global_variable_data[,experimental_columns_index[r]])
      }
    }

  }


  # Rename columns to standard names
  colnames(fixed_global_variable_data)[which(colnames(fixed_global_variable_data)==condition_column)]="condition_column"
  colnames(fixed_global_variable_data)[which(colnames(fixed_global_variable_data)==response_column)]="response_column"
  if(!is.null(covariate)) colnames(fixed_global_variable_data)[which(colnames(fixed_global_variable_data)==covariate)]="covariate"
  if(!is.null(random_slope_variable)) {
    if(random_slope_variable == condition_column)
      random_slope_variable = "condition_column"
    else if(random_slope_variable == covariate)
      random_slope_variable = "covariate"
    else{
      print("random_slope_variable can only be 'condition_column', 'covariate' or the actual condition column name or covariate column name")
      return(NULL)
    }

  }

  if(!is.null(total_column))
    colnames(fixed_global_variable_data)[which(colnames(fixed_global_variable_data)==total_column)]="total_column"

  # Build formula components
  fixed_formula <- build_fixed_formula(covariate, include_interaction)
  random_formula <- build_random_formula(experimental_columns, random_slope_variable)
  lmerFit <- generate_model_fit(data=fixed_global_variable_data,
                                fixed_formula,
                                random_formula,
                                error_is_non_normal,
                                family_p,
                                total_column)


  # ####### run the formula
  # if(is.null(covariate)){
  #   if(error_is_non_normal==FALSE){
  #     if(length(experimental_columns)==1){
  #       lmerFit <- lmerTest::lmer(response_column ~ condition_column + (1 | experimental_column1), data=fixed_global_variable_data)
  #     }else if(length(experimental_columns)==2){
  #       lmerFit <- lmerTest::lmer(response_column ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2), data=fixed_global_variable_data)
  #     }else if(length(experimental_columns)==3){
  #       lmerFit <- lmerTest::lmer(response_column ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3), data=fixed_global_variable_data)
  #     }else if(length(experimental_columns)==4){
  #       lmerFit <- lmerTest::lmer(response_column ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4), data=fixed_global_variable_data)
  #     }else if(length(experimental_columns)==5){
  #       lmerFit <- lmerTest::lmer(response_column ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + (1 | experimental_column5), data=fixed_global_variable_data)
  #     }
  #   }else if(family_p$family == "binomial" & !is.null(total_column)){
  #     if(length(experimental_columns)==1){
  #       lmerFit <- lme4::glmer(cbind(response_column, (total_column - response_column)) ~ condition_column + (1 | experimental_column1), data=fixed_global_variable_data, family=family_p)
  #     }else if(length(experimental_columns)==2){
  #       lmerFit <- lme4::glmer(cbind(response_column, (total_column - response_column)) ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2), data=fixed_global_variable_data, family=family_p)
  #     }else if(length(experimental_columns)==3){
  #       lmerFit <- lme4::glmer(cbind(response_column, total_column - response_column) ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3), data=fixed_global_variable_data, family=family_p)
  #     }else if(length(experimental_columns)==4){
  #       lmerFit <- lme4::glmer(cbind(response_column, total_column - response_column) ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4), data=fixed_global_variable_data, family=family_p)
  #     }else if(length(experimental_columns)==5){
  #       lmerFit <- lme4::glmer(cbind(response_column, total_column - response_column) ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + (1 | experimental_column5), data=fixed_global_variable_data, family=family_p)
  #     }
  #   }else if(family_p$family == "negative_binomial" & !is.null(total_column)){
  #     if(length(experimental_columns)==1){
  #       lmerFit <- lme4::glmer.nb(response_column ~ condition_column + (1 | experimental_column1) + offset(log(total_column)), data=fixed_global_variable_data, family=family_p)
  #     }else if(length(experimental_columns)==2){
  #       lmerFit <- lme4::glmer.nb(response_column ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2) + offset(log(total_column)), data=fixed_global_variable_data, family=family_p)
  #     }else if(length(experimental_columns)==3){
  #       lmerFit <- lme4::glmer.nb(response_column ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + offset(log(total_column)) , data=fixed_global_variable_data, family=family_p)
  #     }else if(length(experimental_columns)==4){
  #       lmerFit <- lme4::glmer.nb(response_column ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + offset(log(total_column)), data=fixed_global_variable_data, family=family_p)
  #     }else if(length(experimental_columns)==5){
  #       lmerFit <- lme4::glmer.nb(response_column ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + (1 | experimental_column5) + offset(log(total_column)) , data=fixed_global_variable_data, family=family_p)
  #     }
  #   }else{
  #     if(length(experimental_columns)==1){
  #       lmerFit <- lme4::glmer(response_column ~ condition_column + (1 | experimental_column1), data=fixed_global_variable_data, family=family_p)
  #     }else if(length(experimental_columns)==2){
  #       lmerFit <- lme4::glmer(response_column ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2), data=fixed_global_variable_data, family=family_p)
  #     }else if(length(experimental_columns)==3){
  #       lmerFit <- lme4::glmer(response_column ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3), data=fixed_global_variable_data, family=family_p)
  #     }else if(length(experimental_columns)==4){
  #       lmerFit <- lme4::glmer(response_column ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4), data=fixed_global_variable_data, family=family_p)
  #     }else if(length(experimental_columns)==5){
  #       lmerFit <- lme4::glmer(response_column ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + (1 | experimental_column5), data=fixed_global_variable_data, family=family_p)
  #     }
  #   }
  # }else{
  #   if(error_is_non_normal==FALSE){
  #     if(length(experimental_columns)==1){
  #       lmerFit <- lmerTest::lmer(response_column ~ condition_column + covariate + (1 | experimental_column1), data=fixed_global_variable_data)
  #     }else if(length(experimental_columns)==2){
  #       lmerFit <- lmerTest::lmer(response_column ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2), data=fixed_global_variable_data)
  #     }else if(length(experimental_columns)==3){
  #       lmerFit <- lmerTest::lmer(response_column ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3), data=fixed_global_variable_data)
  #     }else if(length(experimental_columns)==4){
  #       lmerFit <- lmerTest::lmer(response_column ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4), data=fixed_global_variable_data)
  #     }else if(length(experimental_columns)==5){
  #       lmerFit <- lmerTest::lmer(response_column ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + (1 | experimental_column5), data=fixed_global_variable_data)
  #     }
  #   }else if(family_p$family == "binomial" & !is.null(total_column)){
  #     if(length(experimental_columns)==1){
  #       lmerFit <- lme4::glmer(cbind(response_column, (total_column - response_column)) ~ condition_column + covariate + (1 | experimental_column1), data=fixed_global_variable_data, family=family_p)
  #     }else if(length(experimental_columns)==2){
  #       lmerFit <- lme4::glmer(cbind(response_column, (total_column - response_column)) ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2), data=fixed_global_variable_data, family=family_p)
  #     }else if(length(experimental_columns)==3){
  #       lmerFit <- lme4::glmer(cbind(response_column, total_column - response_column) ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3), data=fixed_global_variable_data, family=family_p)
  #     }else if(length(experimental_columns)==4){
  #       lmerFit <- lme4::glmer(cbind(response_column, total_column - response_column) ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4), data=fixed_global_variable_data, family=family_p)
  #     }else if(length(experimental_columns)==5){
  #       lmerFit <- lme4::glmer(cbind(response_column, total_column - response_column) ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + (1 | experimental_column5), data=fixed_global_variable_data, family=family_p)
  #     }
  #   }else if(family_p$family == "negative_binomial" & !is.null(total_column)){
  #     if(length(experimental_columns)==1){
  #       lmerFit <- lme4::glmer.nb(response_column ~ condition_column + covariate + (1 | experimental_column1) + offset(log(total_column)), data=fixed_global_variable_data, family=family_p)
  #     }else if(length(experimental_columns)==2){
  #       lmerFit <- lme4::glmer.nb(response_column ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2) + offset(log(total_column)), data=fixed_global_variable_data, family=family_p)
  #     }else if(length(experimental_columns)==3){
  #       lmerFit <- lme4::glmer.nb(response_column ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + offset(log(total_column)) , data=fixed_global_variable_data, family=family_p)
  #     }else if(length(experimental_columns)==4){
  #       lmerFit <- lme4::glmer.nb(response_column ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + offset(log(total_column)), data=fixed_global_variable_data, family=family_p)
  #     }else if(length(experimental_columns)==5){
  #       lmerFit <- lme4::glmer.nb(response_column ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + (1 | experimental_column5) + offset(log(total_column)) , data=fixed_global_variable_data, family=family_p)
  #     }
  #   }else{
  #     if(length(experimental_columns)==1){
  #       lmerFit <- lme4::glmer(response_column ~ condition_column + covariate + (1 | experimental_column1), data=fixed_global_variable_data, family=family_p)
  #     }else if(length(experimental_columns)==2){
  #       lmerFit <- lme4::glmer(response_column ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2), data=fixed_global_variable_data, family=family_p)
  #     }else if(length(experimental_columns)==3){
  #       lmerFit <- lme4::glmer(response_column ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3), data=fixed_global_variable_data, family=family_p)
  #     }else if(length(experimental_columns)==4){
  #       lmerFit <- lme4::glmer(response_column ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4), data=fixed_global_variable_data, family=family_p)
  #     }else if(length(experimental_columns)==5){
  #       lmerFit <- lme4::glmer(response_column ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + (1 | experimental_column5), data=fixed_global_variable_data, family=family_p)
  #     }
  #   }
  # }
  #
  #
  #




  slmerFit <- summary(lmerFit)

  #cat("\n")
  # print("__________________________________________________________________Model statistics:")
  # print(slmerFit)
  #cat("\n")



  return(list(lmerFit, fixed_global_variable_data, colnames(fixed_global_variable_data)[experimental_columns_index], slmerFit$residuals))
}

# Helper function to build fixed effects formula
build_fixed_formula <- function(covariate = NULL, include_interaction = FALSE) {
  if (is.null(covariate)) {
    return("condition_column")
  } else {
    if (include_interaction) {
      return("condition_column * covariate")  # This expands to: condition_column + covariate + condition_column:covariate
    } else {
      return("condition_column + covariate")
    }
  }
}

# Helper function to build random effects formula
build_random_formula <- function(experimental_columns, random_slope_variable = NULL) {
  random_parts <- c()

  for (i in 1:length(experimental_columns)) {
    exp_col <- paste0("experimental_column", i)

    if (!is.null(random_slope_variable) && random_slope_variable == "condition_column") {
      # Add random slope for condition_column
      random_parts <- c(random_parts, paste0("(condition_column || ", exp_col, ")"))
    } else if (!is.null(random_slope_variable) && random_slope_variable == "covariate") {
      # Add random slope for condition_column
      random_parts <- c(random_parts, paste0("(covariate || ", exp_col, ")"))
    } else {
      # Standard random intercept only
      random_parts <- c(random_parts, paste0("(1 | ", exp_col, ")"))
    }
  }

  return(paste(random_parts, collapse = " + "))
}

build_random_formula0 <- function(experimental_columns) {
  random_parts <- c()

  for (i in 1:length(experimental_columns)) {
    exp_col <- paste0("experimental_column", i)
    # Standard random intercept only
    random_parts <- c(random_parts, paste0("(1 | ", exp_col, ")"))
  }

  return(paste(random_parts, collapse = " + "))
}


generate_model_fit <- function(data, fixed_formula, random_formula,
                                    error_is_non_normal = FALSE, family_p = NULL,
                                    total_column = NULL) {

  if (error_is_non_normal == FALSE) {
    # Linear mixed effects model
    formula_str <<- as.formula(paste("response_column ~", fixed_formula, "+", random_formula))
    lmerFit <- lmerTest::lmer(formula_str, data = data)
  } else if (!is.null(family_p) && family_p$family == "binomial" && !is.null(total_column)) {
    # Binomial with total column
    formula_str <<- as.formula(paste("cbind(response_column, (total_column - response_column)) ~",
                         fixed_formula, "+", random_formula))
    lmerFit <- lme4::glmer(formula_str, data = data, family = family_p)
  } else if (!is.null(family_p) && family_p$family == "negative_binomial" && !is.null(total_column)) {
    # Negative binomial with offset
    formula_str <<- as.formula(paste("response_column ~", fixed_formula, "+", random_formula,
                         "+ offset(log(total_column))"))
    lmerFit <- lme4::glmer.nb(formula_str, data = data, family = family_p)
  } else if (!is.null(family_p) && family_p$family == "poisson" && !is.null(total_column)) {
    # poisson with offset
    formula_str <<- as.formula(paste("response_column ~", fixed_formula, "+", random_formula,
                         "+ offset(log(total_column))"))
    lmerFit <- lme4::glmer(formula_str, data = data, family = family_p)
  }
  else if (!is.null(family_p) && family_p$family == "negative_binomial" && is.null(total_column)) {
    # Negative binomial with offset
    formula_str <<- as.formula(paste("response_column ~", fixed_formula, "+", random_formula,
                                     ")"))
    lmerFit <- lme4::glmer.nb(formula_str, data = data, family = family_p)
  }
  else {
    # Other GLMMs
    formula_str <<- as.formula(paste("response_column ~", fixed_formula, "+", random_formula))
    lmerFit <- lme4::glmer(formula_str, data = data, family = family_p)
  }

  lmerFit@call$formula = formula_str
  lmerFit@call$data = as.name("fixed_global_variable_data")
  return(lmerFit)
}

generate_model_fit0 <- function(data,  random_formula,
                               error_is_non_normal = FALSE, family_p = NULL,
                               total_column = NULL) {

  if (error_is_non_normal == FALSE) {
    # Linear mixed effects model
    formula_str <- paste("response_column ~",  random_formula)
    lmerFit <- lmerTest::lmer(as.formula(formula_str), data = data)
  } else if (!is.null(family_p) && family_p$family == "binomial" && !is.null(total_column)) {
    # Binomial with total column
    formula_str <- paste("cbind(response_column, (total_column - response_column)) ~", random_formula)
    lmerFit <- lme4::glmer(as.formula(formula_str), data = data, family = family_p)
  } else if (!is.null(family_p) && family_p$family == "negative_binomial" && !is.null(total_column)) {
    # Negative binomial with offset
    formula_str <- paste("response_column ~", random_formula, "+ offset(log(total_column))")
    lmerFit <- lme4::glmer.nb(as.formula(formula_str), data = data, family = family_p)
  } else if (!is.null(family_p) && family_p$family == "poisson" && !is.null(total_column)) {
    # Poisson with offset
    formula_str <- paste("response_column ~",  random_formula,"+ offset(log(total_column))")
    lmerFit <- lme4::glmer(as.formula(formula_str), data = data, family = family_p)
  }
  else {
    # Other GLMMs
    formula_str <- paste("response_column ~",  random_formula)
    lmerFit <- lme4::glmer(as.formula(formula_str), data = data, family = family_p)
  }


  lmerFit@call$formula = formula_str
  lmerFit@call$data = as.name("fixed_global_variable_data")
  return(lmerFit)
}

# generate_model_fit_4_power_estimates <- function(data, fixed_formula, random_formula,
#                                error_is_non_normal = FALSE, family_p = NULL,
#                                total_column = NULL) {
#
#   if (error_is_non_normal == FALSE) {
#     # Linear mixed effects model
#     formula_str <<- as.formula(paste("response_column ~", fixed_formula, "+", random_formula))
#     lmerFit <- lme4::lmer(formula_str, data = data)
#   } else if (!is.null(family_p) && family_p$family == "binomial" && !is.null(total_column)) {
#     # Binomial with total column
#     formula_str <<- as.formula(paste("cbind(response_column, (total_column - response_column)) ~",
#                                      fixed_formula, "+", random_formula))
#     lmerFit <- lme4::glmer(formula_str, data = data, family = family_p)
#   } else if (!is.null(family_p) && family_p$family == "negative_binomial" && !is.null(total_column)) {
#     # Negative binomial with offset
#     formula_str <<- as.formula(paste("response_column ~", fixed_formula, "+", random_formula,
#                                      "+ offset(log(total_column))"))
#     lmerFit <- lme4::glmer.nb(formula_str, data = data, family = family_p)
#   } else if (!is.null(family_p) && family_p$family == "poisson" && !is.null(total_column)) {
#     # Negative binomial with offset
#     formula_str <<- as.formula(paste("response_column ~", fixed_formula, "+", random_formula,
#                                      "+ offset(log(total_column))"))
#     lmerFit <- lme4::glmer(formula_str, data = data, family = family_p)
#   }
#   else {
#     # Other GLMMs
#     formula_str <<- as.formula(paste("response_column ~", fixed_formula, "+", random_formula))
#     lmerFit <- lme4::glmer(formula_str, data = data, family = family_p)
#   }
#
#   lmerFit@call$formula = formula_str
#   lmerFit@call$data = as.name("Data")
#   return(lmerFit)
# }
