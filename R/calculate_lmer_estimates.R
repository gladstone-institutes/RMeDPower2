#' @title calculate_lmer_estimates
#' @description This function performs a (generalized) linear mixed model analysis using (g)lmer.
#'
#'
#' @param data Input data
#' @param condition_column Name of the condition variable (ex variable with values such as control/case). The input file has to have a corresponding column name
#' @param experimental_columns Name of variables related to experimental design such as "experiment", "plate", and "cell_line". They should be in order, for example, "experiment" should always come first .
#' @param response_column Name of the variable observed by performing the experiment. ex) intensity.
#' @param total_column Set this column only when family_p="binomial" and it is equal to the total number of observations (number of cases plus number of controls) for a given number of cases, when family_p="poisson" or "negative_binomial" and it is represents the total number of observations to be used as offset in the model
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
#' @return A linear mixed model result
#'
#' @keywords internal
#' @noRd



calculate_lmer_estimates <- function(data, condition_column, experimental_columns, response_column, total_column, condition_is_categorical, covariate = NULL,
                                     crossed_columns=NULL, error_is_non_normal=FALSE, family_p=NULL, na.action="complete",
                                     include_interaction = FALSE,
                                     random_slope_variable = NULL,
                                     covariate_is_categorical = TRUE){



  ######input error handler
  if(!is.null(covariate) )
    if(!covariate%in%colnames(data))
      stop("covariate should be null or one of the column names")
  if(!condition_column%in%colnames(data)) stop("condition_column should be one of the column names")
  if(sum(experimental_columns%in%colnames(data))!=length(experimental_columns)) stop("experimental_columns must match column names")
  if(!response_column%in%colnames(data)) stop("response_column should be one of the column names")
  if(!is.na(condition_is_categorical) && !condition_is_categorical%in%c(TRUE,FALSE)) stop("condition_is_categorical must be TRUE or FALSE")
  if(!is.null(crossed_columns)){if(sum(crossed_columns%in%colnames(data))!=length(crossed_columns)) stop("crossed_columns must match column names")}
  if(!is.na(covariate_is_categorical) && !covariate_is_categorical%in%c(TRUE,FALSE)) stop("covariate_is_categorical must be TRUE or FALSE")

  # Validation for new parameters
  if(!is.na(include_interaction)) {
    if (include_interaction && is.null(covariate)) {
      stop("Cannot include interaction when covariate is NULL")
    }
  }

  if (!is.null(random_slope_variable) &&
      !random_slope_variable %in% c("condition_column", condition_column, "covariate", covariate)) {
    stop("random_slope_variable should be 'condition_column', 'covariate' or the actual condition column name or covariate column name")
  }

  if(error_is_non_normal==TRUE){
    if(family_p != "negative_binomial")
      family_p=switch(family_p, "poisson" = poisson(link="log"), "binomial" = binomial(link="logit"), "bionomial_log" = binomial(link="log") )
    else
      family_p = list(family = "negative_binomial")
  }


  if(na.action=="complete"){

    notNAindex=which( rowSums(is.na(data)) == 0 )

  }else if(na.action=="unique"){

    if(is.null(covariate)) notNAindex=which( rowSums(is.na(data[,c(condition_column, experimental_columns, response_column, covariate)])) == 0 )
    else notNAindex=which( rowSums(is.na(data[,c(condition_column, experimental_columns, response_column)])) == 0 )


  }



  Data=data[notNAindex,]


  # cat("\n")
  # print("__________________________________________________________________Summary of data:")
  # print(summary(Data))
  # cat("\n")

  colnames_original=colnames(Data)
  experimental_columns_index=NULL
  ####### assign categorical variables
  if(condition_is_categorical==TRUE)
    Data[,condition_column]=as.factor(Data[,condition_column])
  else
    Data[,condition_column]=as.numeric(Data[,condition_column])

  if(!is.null(covariate)) {
    if(covariate_is_categorical==TRUE)
      Data[,covariate]=as.factor(Data[,covariate])
    else
      Data[,covariate]=as.numeric(Data[,covariate])
  }

  # random slope should be allowed only with a continuous variable
  if(!is.null(random_slope_variable)) {
    if(!inherits(Data[,random_slope_variable], "numeric")) {
      stop("random_slope_variable should be a numeric variable")
    }
  }

  # cat("\n")


  noncrossed_columns=NULL

  for(i in 1:length(experimental_columns)){
    Data[,experimental_columns[i]]=as.factor(Data[,experimental_columns[i]])
    experimental_columns_index=c(experimental_columns_index,which(colnames(Data)==experimental_columns[i]))
    colnames(Data)[experimental_columns_index[i]]=paste("experimental_column",i,sep="")

    if(i!=1&&!experimental_columns[i]%in%crossed_columns){
      noncrossed_columns=c(noncrossed_columns, paste("experimental_column",i,sep=""))
    }


    # cat("\n")
    # print(paste("_________________________________",experimental_columns[i]," is assigned to experimental_column",i,sep=""))
    # cat("\n")
  }



  if(length(experimental_columns)>=2){
      for(r in 2:length(experimental_columns)){
      if(colnames(Data)[experimental_columns_index[r]]%in%noncrossed_columns){
        Data[,experimental_columns_index[r]]=paste(Data[,experimental_columns_index[r-1]], Data[,experimental_columns_index[r]],sep="_")
      }
    }

  }



  colnames(Data)[which(colnames(Data)==condition_column)]="condition_column"
  colnames(Data)[which(colnames(Data)==response_column)]="response_column"
  if(!is.null(covariate)) colnames(Data)[which(colnames(Data)==covariate)]="covariate"
  if(!is.null(random_slope_variable)) {
    if(random_slope_variable == condition_column)
      random_slope_variable = "condition_column"
    else if(random_slope_variable == covariate)
      random_slope_variable = "covariate"
    else{
      stop("random_slope_variable can only be 'condition_column', 'covariate' or the actual condition column name or covariate column name")
    }
  }

  if(!is.null(total_column))
    colnames(Data)[which(colnames(Data)==total_column)]="total_column"

  # Build formula components
  fixed_formula <- build_fixed_formula(covariate, include_interaction)
  random_formula <- build_random_formula(experimental_columns, random_slope_variable)
  lmerFit <- generate_model_fit(data=Data,
                                fixed_formula,
                                random_formula,
                                error_is_non_normal,
                                family_p,
                                total_column)


  # ####### run the formula
  #
  # if(is.null(covariate)){
  #   if(error_is_non_normal==FALSE){
  #     if(length(experimental_columns)==1){
  #       lmerFit <- lmerTest::lmer(response_column ~ condition_column + (1 | experimental_column1), data=Data)
  #     }else if(length(experimental_columns)==2){
  #       lmerFit <- lmerTest::lmer(response_column ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2), data=Data)
  #     }else if(length(experimental_columns)==3){
  #       lmerFit <- lmerTest::lmer(response_column ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3), data=Data)
  #     }else if(length(experimental_columns)==4){
  #       lmerFit <- lmerTest::lmer(response_column ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4), data=Data)
  #     }else if(length(experimental_columns)==5){
  #       lmerFit <- lmerTest::lmer(response_column ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + (1 | experimental_column5), data=Data)
  #     }
  #   }else if(family_p$family == "binomial" & !is.null(total_column)){
  #     if(length(experimental_columns)==1){
  #       lmerFit <- lme4::glmer(cbind(response_column, (total_column - response_column)) ~ condition_column + (1 | experimental_column1), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==2){
  #       lmerFit <- lme4::glmer(cbind(response_column, total_column - response_column) ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==3){
  #       lmerFit <- lme4::glmer(cbind(response_column, total_column - response_column) ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==4){
  #       lmerFit <- lme4::glmer(cbind(response_column, total_column - response_column) ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==5){
  #       lmerFit <- lme4::glmer(cbind(response_column, total_column - response_column) ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + (1 | experimental_column5), data=Data, family=family_p)
  #     }
  #   }else if(family_p$family == "negative_binomial" & !is.null(total_column)){
  #     if(length(experimental_columns)==1){
  #       lmerFit <- lme4::glmer.nb(response_column ~ condition_column + (1 | experimental_column1) + offset(log(total_column)), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==2){
  #       lmerFit <- lme4::glmer.nb(response_column ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2) + offset(log(total_column)), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==3){
  #       lmerFit <- lme4::glmer.nb(response_column ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + offset(log(total_column)) , data=Data, family=family_p)
  #     }else if(length(experimental_columns)==4){
  #       lmerFit <- lme4::glmer.nb(response_column ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + offset(log(total_column)), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==5){
  #       lmerFit <- lme4::glmer.nb(response_column ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + (1 | experimental_column5) + offset(log(total_column)) , data=Data, family=family_p)
  #     }
  #   }else if(family_p$family == "poisson" & !is.null(total_column)){
  #     if(length(experimental_columns)==1){
  #       lmerFit <- lme4::glmer(response_column ~ condition_column + (1 | experimental_column1) + offset(log(total_column)), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==2){
  #       lmerFit <- lme4::glmer(response_column ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2) + offset(log(total_column)), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==3){
  #       lmerFit <- lme4::glmer(response_column ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + offset(log(total_column)) , data=Data, family=family_p)
  #     }else if(length(experimental_columns)==4){
  #       lmerFit <- lme4::glmer(response_column ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + offset(log(total_column)), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==5){
  #       lmerFit <- lme4::glmer(response_column ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + (1 | experimental_column5) + offset(log(total_column)) , data=Data, family=family_p)
  #     }
  #   }else{
  #     if(length(experimental_columns)==1){
  #       lmerFit <- lme4::glmer(response_column ~ condition_column + (1 | experimental_column1), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==2){
  #       lmerFit <- lme4::glmer(response_column ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==3){
  #       lmerFit <- lme4::glmer(response_column ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==4){
  #       lmerFit <- lme4::glmer(response_column ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==5){
  #       lmerFit <- lme4::glmer(response_column ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + (1 | experimental_column5), data=Data, family=family_p)
  #     }
  #   }
  # }else{
  #   if(error_is_non_normal==FALSE){
  #     if(length(experimental_columns)==1){
  #       lmerFit <- lmerTest::lmer(response_column ~ condition_column + covariate + (1 | experimental_column1), data=Data)
  #     }else if(length(experimental_columns)==2){
  #       lmerFit <- lmerTest::lmer(response_column ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2), data=Data)
  #     }else if(length(experimental_columns)==3){
  #       lmerFit <- lmerTest::lmer(response_column ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3), data=Data)
  #     }else if(length(experimental_columns)==4){
  #       lmerFit <- lmerTest::lmer(response_column ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4), data=Data)
  #     }else if(length(experimental_columns)==5){
  #       lmerFit <- lmerTest::lmer(response_column ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + (1 | experimental_column5), data=Data)
  #     }
  #   }else if(family_p$family == "binomial" & !is.null(total_column)){
  #     if(length(experimental_columns)==1){
  #       lmerFit <- lme4::glmer(cbind(response_column, (total_column - response_column)) ~ condition_column + covariate + (1 | experimental_column1), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==2){
  #       lmerFit <- lme4::glmer(cbind(response_column, total_column - response_column) ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==3){
  #       lmerFit <- lme4::glmer(cbind(response_column, total_column - response_column) ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==4){
  #       lmerFit <- lme4::glmer(cbind(response_column, total_column - response_column) ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==5){
  #       lmerFit <- lme4::glmer(cbind(response_column, total_column - response_column) ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + (1 | experimental_column5), data=Data, family=family_p)
  #     }
  #   }else if(family_p$family == "negative_binomial" & !is.null(total_column)){
  #     if(length(experimental_columns)==1){
  #       lmerFit <- lme4::glmer.nb(response_column ~ condition_column + covariate + (1 | experimental_column1) + offset(log(total_column)), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==2){
  #       lmerFit <- lme4::glmer.nb(response_column ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2) + offset(log(total_column)), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==3){
  #       lmerFit <- lme4::glmer.nb(response_column ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + offset(log(total_column)) , data=Data, family=family_p)
  #     }else if(length(experimental_columns)==4){
  #       lmerFit <- lme4::glmer.nb(response_column ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + offset(log(total_column)), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==5){
  #       lmerFit <- lme4::glmer.nb(response_column ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + (1 | experimental_column5) + offset(log(total_column)) , data=Data, family=family_p)
  #     }
  #   }else if(family_p$family == "poisson" & !is.null(total_column)){
  #     if(length(experimental_columns)==1){
  #       lmerFit <- lme4::glmer(response_column ~ condition_column + covariate + (1 | experimental_column1) + offset(log(total_column)), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==2){
  #       lmerFit <- lme4::glmer(response_column ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2) + offset(log(total_column)), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==3){
  #       lmerFit <- lme4::glmer(response_column ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + offset(log(total_column)) , data=Data, family=family_p)
  #     }else if(length(experimental_columns)==4){
  #       lmerFit <- lme4::glmer(response_column ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + offset(log(total_column)), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==5){
  #       lmerFit <- lme4::glmer(response_column ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + (1 | experimental_column5) + offset(log(total_column)) , data=Data, family=family_p)
  #     }
  #   }else{
  #     if(length(experimental_columns)==1){
  #       lmerFit <- lme4::glmer(response_column ~ condition_column + covariate + (1 | experimental_column1), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==2){
  #       lmerFit <- lme4::glmer(response_column ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==3){
  #       lmerFit <- lme4::glmer(response_column ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==4){
  #       lmerFit <- lme4::glmer(response_column ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==5){
  #       lmerFit <- lme4::glmer(response_column ~ condition_column + covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + (1 | experimental_column5), data=Data, family=family_p)
  #     }
  #   }
  # }
  #
  #






  slmerFit <- summary(lmerFit)
  #cat("\n")
  # print("__________________________________________________________________Model statistics:")
  # print(slmerFit)
  #cat("\n")

  return(slmerFit)
}
