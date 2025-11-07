#' @title get_residuals_covariate
#'
#' @description This function retrieve residual values from an lmerFit summary object and plot residual values by condition_column
#'
#' Note: The current version does not accept categorical response variables, sample size parameters smaller than the observed samples size
#'
#' @import multtest
#' @import lme4
#' @import lmerTest
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
#' @param print_plots Whether or not to print the plots, irrespective of this argument ggplot versions of evaluated association between the response_column and the condition_column. TRUE - print the plot, FALSE - do not print the plot
#'
#' @return A list with 3 elements. 1. The updated input data with an additional column with the model residuals of the individual observations. 2. A plot representing the purported association between the response column and the condition column. 3. The corresponding caption for this figure.
#'
#' @export
#'
#' @examples result=get_residuals_covariate(data=plate_assay_pilot_data,
#' @examples condition_column="classification",
#' @examples experimental_columns=c("experiment", "line"),
#' @examples response_column="cell_size1",
#' @examples condition_is_categorical=TRUE,
#' @examples covariate="covariate",
#' @examples crossed_columns = "line",
#' @examples error_is_non_normal=FALSE)


get_residuals <- function(data, condition_column, experimental_columns, response_column,  condition_is_categorical, covariate=NULL,
                          crossed_columns=NULL, total_column=FALSE, error_is_non_normal=FALSE, family_p=NULL, na.action="complete", include_interaction=FALSE, random_slope_variable=NULL, covariate_is_categorical = TRUE, print_plots = TRUE){



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
    if(class(Data[,random_slope_variable]) != "numeric") {
      print("random_slope_variable should be a numeric variable")
      return(NULL)
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
      print("random_slope_variable can only be 'condition_column', 'covariate' or the actual condition column name or covariate column name")
      return(NULL)
    }
  }

  if(!is.null(total_column))
    colnames(Data)[which(colnames(Data)==total_column)]="total_column"

  # Build formula components
  random_formula <- build_random_formula0(experimental_columns)
  lmerFit <- generate_model_fit0(data=Data,
                                random_formula,
                                error_is_non_normal,
                                family_p,
                                total_column)

  # ####### run the formula
  #
  # if(is.null(covariate)){
  #   if(error_is_non_normal==FALSE){
  #     if(length(experimental_columns)==1){
  #       lmerFit <- lmerTest::lmer(response_column ~  (1 | experimental_column1), data=Data)
  #     }else if(length(experimental_columns)==2){
  #       lmerFit <- lmerTest::lmer(response_column ~  (1 | experimental_column1) + (1 | experimental_column2), data=Data)
  #     }else if(length(experimental_columns)==3){
  #       lmerFit <- lmerTest::lmer(response_column ~  (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3), data=Data)
  #     }else if(length(experimental_columns)==4){
  #       lmerFit <- lmerTest::lmer(response_column ~  (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4), data=Data)
  #     }else if(length(experimental_columns)==5){
  #       lmerFit <- lmerTest::lmer(response_column ~  (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + (1 | experimental_column5), data=Data)
  #     }
  #   }else if(family_p$family == "binomial" & !is.null(total_column)){
  #     if(length(experimental_columns)==1){
  #       lmerFit <- lme4::glmer(cbind(response_column, (total_column - response_column)) ~ (1 | experimental_column1), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==2){
  #       lmerFit <- lme4::glmer(cbind(response_column, total_column - response_column) ~ (1 | experimental_column1) + (1 | experimental_column2), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==3){
  #       lmerFit <- lme4::glmer(cbind(response_column, total_column - response_column) ~ (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==4){
  #       lmerFit <- lme4::glmer(cbind(response_column, total_column - response_column) ~ (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==5){
  #       lmerFit <- lme4::glmer(cbind(response_column, total_column - response_column) ~ (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + (1 | experimental_column5), data=Data, family=family_p)
  #     }
  #   }else if(family_p$family == "negative_binomial" & !is.null(total_column)){
  #     if(length(experimental_columns)==1){
  #       lmerFit <- lme4::glmer.nb(response_column ~  (1 | experimental_column1) + offset(log(total_column)), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==2){
  #       lmerFit <- lme4::glmer.nb(response_column ~  (1 | experimental_column1) + (1 | experimental_column2) + offset(log(total_column)), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==3){
  #       lmerFit <- lme4::glmer.nb(response_column ~  (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + offset(log(total_column)) , data=Data, family=family_p)
  #     }else if(length(experimental_columns)==4){
  #       lmerFit <- lme4::glmer.nb(response_column ~  (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + offset(log(total_column)), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==5){
  #       lmerFit <- lme4::glmer.nb(response_column ~ condition_column + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + (1 | experimental_column5) + offset(log(total_column)) , data=Data, family=family_p)
  #     }
  #   }else if(family_p$family == "poisson" & !is.null(total_column)){
  #     if(length(experimental_columns)==1){
  #       lmerFit <- lme4::glmer(response_column ~ (1 | experimental_column1) + offset(log(total_column)), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==2){
  #       lmerFit <- lme4::glmer(response_column ~ (1 | experimental_column1) + (1 | experimental_column2) + offset(log(total_column)), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==3){
  #       lmerFit <- lme4::glmer(response_column ~ (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + offset(log(total_column)) , data=Data, family=family_p)
  #     }else if(length(experimental_columns)==4){
  #       lmerFit <- lme4::glmer(response_column ~ (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + offset(log(total_column)), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==5){
  #       lmerFit <- lme4::glmer(response_column ~ (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + (1 | experimental_column5) + offset(log(total_column)) , data=Data, family=family_p)
  #     }
  #   }else{
  #     if(length(experimental_columns)==1){
  #       lmerFit <- lme4::glmer(response_column ~  (1 | experimental_column1), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==2){
  #       lmerFit <- lme4::glmer(response_column ~  (1 | experimental_column1) + (1 | experimental_column2), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==3){
  #       lmerFit <- lme4::glmer(response_column ~  (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==4){
  #       lmerFit <- lme4::glmer(response_column ~  (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==5){
  #       lmerFit <- lme4::glmer(response_column ~  (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + (1 | experimental_column5), data=Data, family=family_p)
  #     }
  #   }
  # }else{
  #   if(error_is_non_normal==FALSE){
  #     if(length(experimental_columns)==1){
  #       lmerFit <- lmerTest::lmer(response_column ~  covariate + (1 | experimental_column1), data=Data)
  #     }else if(length(experimental_columns)==2){
  #       lmerFit <- lmerTest::lmer(response_column ~  covariate + (1 | experimental_column1) + (1 | experimental_column2), data=Data)
  #     }else if(length(experimental_columns)==3){
  #       lmerFit <- lmerTest::lmer(response_column ~  covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3), data=Data)
  #     }else if(length(experimental_columns)==4){
  #       lmerFit <- lmerTest::lmer(response_column ~  covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4), data=Data)
  #     }else if(length(experimental_columns)==5){
  #       lmerFit <- lmerTest::lmer(response_column ~  covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + (1 | experimental_column5), data=Data)
  #     }
  #   }else if(family_p$family == "binomial" & !is.null(total_column)){
  #     if(length(experimental_columns)==1){
  #       lmerFit <- lme4::glmer(cbind(response_column, (total_column - response_column)) ~ covariate + (1 | experimental_column1), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==2){
  #       lmerFit <- lme4::glmer(cbind(response_column, total_column - response_column) ~ covariate + (1 | experimental_column1) + (1 | experimental_column2), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==3){
  #       lmerFit <- lme4::glmer(cbind(response_column, total_column - response_column) ~ covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==4){
  #       lmerFit <- lme4::glmer(cbind(response_column, total_column - response_column) ~ covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==5){
  #       lmerFit <- lme4::glmer(cbind(response_column, total_column - response_column) ~ covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + (1 | experimental_column5), data=Data, family=family_p)
  #     }
  #   }else if(family_p$family == "negative_binomial" & !is.null(total_column)){
  #     if(length(experimental_columns)==1){
  #       lmerFit <- lme4::glmer.nb(response_column ~ covariate + (1 | experimental_column1) + offset(log(total_column)), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==2){
  #       lmerFit <- lme4::glmer.nb(response_column ~ covariate + (1 | experimental_column1) + (1 | experimental_column2) + offset(log(total_column)), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==3){
  #       lmerFit <- lme4::glmer.nb(response_column ~ covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + offset(log(total_column)) , data=Data, family=family_p)
  #     }else if(length(experimental_columns)==4){
  #       lmerFit <- lme4::glmer.nb(response_column ~ covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + offset(log(total_column)), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==5){
  #       lmerFit <- lme4::glmer.nb(response_column ~ covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + (1 | experimental_column5) + offset(log(total_column)) , data=Data, family=family_p)
  #     }
  #   }else if(family_p$family == "poisson" & !is.null(total_column)){
  #     if(length(experimental_columns)==1){
  #       lmerFit <- lme4::glmer(response_column ~ covariate + (1 | experimental_column1) + offset(log(total_column)), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==2){
  #       lmerFit <- lme4::glmer(response_column ~ covariate + (1 | experimental_column1) + (1 | experimental_column2) + offset(log(total_column)), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==3){
  #       lmerFit <- lme4::glmer(response_column ~ covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + offset(log(total_column)) , data=Data, family=family_p)
  #     }else if(length(experimental_columns)==4){
  #       lmerFit <- lme4::glmer(response_column ~ covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + offset(log(total_column)), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==5){
  #       lmerFit <- lme4::glmer(response_column ~ covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + (1 | experimental_column5) + offset(log(total_column)) , data=Data, family=family_p)
  #     }
  #   }else{
  #     if(length(experimental_columns)==1){
  #       lmerFit <- lme4::glmer(response_column ~  covariate + (1 | experimental_column1), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==2){
  #       lmerFit <- lme4::glmer(response_column ~  covariate + (1 | experimental_column1) + (1 | experimental_column2), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==3){
  #       lmerFit <- lme4::glmer(response_column ~  covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==4){
  #       lmerFit <- lme4::glmer(response_column ~  covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4), data=Data, family=family_p)
  #     }else if(length(experimental_columns)==5){
  #       lmerFit <- lme4::glmer(response_column ~  covariate + (1 | experimental_column1) + (1 | experimental_column2) + (1 | experimental_column3) + (1 | experimental_column4) + (1 | experimental_column5), data=Data, family=family_p)
  #     }
  #   }
  # }







  slmerFit <- summary(lmerFit)
  # cat("\n")
  # print("__________________________________________________________________Model statistics:")
  # print(slmerFit)
  # cat("\n")

  residuals=slmerFit$residuals


  Data = cbind(residuals, Data)


  colnames(Data)[1] = "residual"
  #Data[,"condition_column"]=as.numeric(as.character(Data[,"condition_column"]))

  if(is.null(covariate)) {
    Data_sum <- Data %>%
      dplyr::group_by(condition_column, experimental_column1) %>%
      dplyr::summarise(mean_residual1 = mean(residual), med_residual1 = median(residual))
  }
  if(!is.null(covariate)){
    Data_sum <- Data %>%
      dplyr::group_by(covariate, condition_column, experimental_column1) %>%
      dplyr::summarise(mean_residual1 = mean(residual), med_residual1 = median(residual))
  }


  condition_var <- condition_column
  response_var <- response_column
  covariate_var <- covariate

  if(condition_is_categorical==TRUE){

    if(!is.null(covariate)) {
       if(class(Data[["covariate"]]) == "factor" | class(Data[["covariate"]]) == "character") {
        gp=ggplot2::ggplot(Data_sum, aes(x=condition_column, y=med_residual1, color = covariate)) +
          geom_boxplot(position = position_dodge(width = 0.7), outlier.shape = NA) +
          geom_point(position = position_jitterdodge(jitter.width = 0.1,dodge.width = 0.7)) +
          labs(title = "Association of interest",
               subtitle = paste0("Box plot of the median residuals across all observations within each level of experimental column = ", experimental_columns[1], " as a function of ", condition_column, " and separated by levels of ", covariate),
               y = paste(response_var, ": median residual"),
               x = condition_var) +
          theme_bw() +
          theme(axis.title   = element_text(face  = "bold"))+
          theme(plot.subtitle  = element_textbox_simple())
        captions = paste0("Box plot of the median residuals across all observations within each level of experimental column = ", experimental_columns[1], " as a function of ", condition_column, " and separated by levels of ", covariate)
      }
      else if(class(Data[["covariate"]]) == "numeric") {
        gp=ggplot2::ggplot(Data_sum, aes(x=covariate, y=med_residual1, color = condition_column)) +
          geom_smooth(method = "lm", se = T) +
          geom_point() +
          labs(title = "Association of interest",
               subtitle = paste0("Best linear fits of the median residuals across all observations within each level of experimental column = ", experimental_columns[1], " as a function of ", covariate, " for each level of ", condition_column),
               y = paste(response_var, ": median residual"),
               x = covariate_var) +
          theme_minimal() +
          theme(plot.title = element_text(size = 12, face = "bold")) +
          theme(plot.subtitle  = element_textbox_simple())
        captions = paste0("Best linear fits of the median residuals across all observations within each level of experimental column = ", experimental_columns[1], " as a function of ", covariate, " for each level of ", condition_column)

      }
    }else{
      gp=ggplot2::ggplot(Data_sum, aes(x=condition_column, y=med_residual1)) +
        geom_boxplot(position = position_dodge(width = 0.5), outlier.shape = NA) +
        geom_point() +
        labs(title = "Association of interest",
             subtitle = paste0("Boxplot of the median residuals across all observations within each level of experimental column = ", experimental_columns[1], " as a function of ", condition_column),
             y = paste(response_var, ": median residual"),
             x = condition_var) +
        theme_minimal() +
        theme(plot.title = element_text(size = 12, face = "bold")) +
        theme(plot.subtitle  = element_textbox_simple())
      captions = paste0("Boxplot of the median residuals across all observations within each level of experimental column = ", experimental_columns[1], " as a function of ", condition_column)

    }

  }else{


    # plot(Data[,"condition_column"], Data[,"residual"], xlab=condition_column, ylab=paste0(response_column, " Residual Value"), main=NULL)
    # abline(lm( as.formula( paste0( "residual ~  condition_column") ), data=Data),col='blue')

    gp <- ggplot2::ggplot(Data, aes(x=condition_column, y=residual, color=experimental_column1)) +
      geom_point() +
      geom_smooth(method = "lm", se = T) +
      labs(title = "Association of interest",
           subtitle = paste0("Best linear fits of the residuals across all observations separated for each level of experimental column = ", experimental_columns[1], " as a function of ", condition_column),
           y = paste(response_var, ": residual"),
           x = condition_var) +
      theme_minimal() +
      theme(plot.title = element_text(size = 12, face = "bold")) +
      theme(plot.subtitle  = element_textbox_simple())

    captions = paste0("Best linear fits of the residuals across all observations separated for each level of experimental column = ", experimental_columns[1], " as a function of ", condition_column)



  }

  if(print_plots)
    print(gp)

  return(list(Data = Data, plots = gp, captions = captions))
}
