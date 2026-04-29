#' @title calculate_power_covariate
#' @description This function uses simulation to perform power analysis. It is designed to explore the power of biological experiments and to suggest an optimal number of experimental variables with reasonable power. The backbone of the function is based on simr package, which fits a fixed effect or mixed effect model based on the observed data and simulates response variables. Users can test the power of different combinations of experimental variables and parameters.
#'
#'
#'
#'
#' @param data Input data
#' @param condition_column The name of the condition variable (ex a variable with values such as control/case). The input file has to have a corresponding column name
#' @param experimental_columns Names of variables related to the experimental design, such as "experiment", "plate", and "cell_line".vThey should be in order, for example, "experiment" should always come first .
#' @param response_column The name of the variable observed by performing the experiment. ex) intensity.
#' @param total_column Set this column only when family_p="binomial" and it is equal to the total number of observations (number of cases plus number of controls) for a given number of cases
#' @param power_curve 1: Power simulation over a range of sample sizes or levels. 0: Power calculation over a single sample size or a level.
#' @param condition_is_categorical Specify whether the condition variable is categorical. TRUE: Categorical, FALSE: Continuous.
#' @param covariate The name of the covariate to control in the regression model
#' @param crossed_columns Name of experimental variables that may appear repeatedly with the same ID. For example, cell_line C1 may appear in multiple experiments, but plate P1 cannot appear in more than one experiment
#' @param error_is_non_normal Default: Observed variable is continuous. Categorical response variable will be implemented in the future. TRUE: Categorical , FALSE: Continuous (default).
#' @param nsimn The number of simulations to run. Default=1000
#' @param family_p The type of distribution family to specify when the response is categorical. If family is "binary" then binary(link="log") is used, if family is "poisson" then poisson(link="logit") is used, if family is "poisson_log" then poisson(link=") log") is used.
#' @param target_columns Name of the experimental parameters to use for the power calculation.
#' @param levels 1: Amplify the number of corresponding target parameter. 0: Amplify the number of samples from the corresponding target parameter, ex) If target_columns = c("experiment","cell_line") and if you want to expand the number of experiment and sample more cells from each cell line, set levels = c(1,0).
#' @param max_size Maximum levels or sample sizes to test. Default: the current level or the current sample size x 5. ex) If max_levels = c(10,5), it will test upto 10 experiments and 5 cell lines.
#' @param breaks Levels /sample sizes of the variable to be specified along the power curve. Default: max(1, round( the number of current levels / 5 ))
#' @param na.action "complete": missing data is not allowed in all columns (default), "unique": missing data is not allowed only in condition, experimental, response, and target columns. Selecting "complete" removes an entire row when there is one or more missing values, which may affect the distribution of other features.
#' @param output Output file name
##### If variance estimates should be estimated from data
#' @param  effect_size A 3 dimensional numeric vector given the proposed effect sizes/parameter estimates for the condition_column, covariate and interaction term between these two variables, respectively in that order. Depending on the model not all elements of the effect_size would make sense. For example, in situations, where there is only the condition column, the second and third elements of effect_size should be equal to NA. If you know the effect size of your condition variable, the effect size can be provided as a parameter. If the effect size is not provided, it will be estimated from your data
##### If variance estimates are to be assigned by a user
#' @param  alpha Threshold for Type I error
#' @param  ICC Intra-Class Coefficients (ICC) for each parameter
#' @param include_interaction Whether to include condition * covariate interaction
#' @param random_slope_variable Variable for random slopes (typically "condition_column")
#' @param covariate_is_categorical Specify whether the covariate variable is categorical. TRUE: Categorical, FALSE: Continuous.
#'
#'
#' @return A power curve as a ggplot object or a power calculation result printed in a text file
#'
#' @keywords internal
#' @noRd



calculate_power <- function(data, condition_column, experimental_columns, response_column, total_column = NULL, target_columns, power_curve, condition_is_categorical, covariate=NULL,
                            crossed_columns = NULL, error_is_non_normal=FALSE, nsimn=1000, family_p=NULL,
                            levels=NULL, max_size=NULL, breaks=NULL, effect_size=NULL, ICC=NULL, na.action="complete", output=NULL, alpha =0.05,
                            include_interaction = NA,
                            random_slope_variable = NULL,
                            covariate_is_categorical = NA){






  ######input error handler
  if(length(levels)!=length(target_columns)) stop("User should specify levels of all target parameters")
  if(length(power_curve)==0 | !power_curve%in%c(0,1)) stop("power_curve must be 0 or 1")
  if(!condition_column %in% colnames(data)) stop("condition_column should be one of the column names")
  if(sum(experimental_columns%in%colnames(data))!=length(experimental_columns)) stop("experimental_columns must match column names")
  if(!is.null(crossed_columns)){if(sum(crossed_columns%in%colnames(data))!=length(crossed_columns)) stop("crossed_columns must match column names")}
  if(!response_column%in%colnames(data)) stop("response_column should be one of the column names")

  if(!is.na(condition_is_categorical) && !condition_is_categorical%in%c(TRUE,FALSE)) stop("condition_is_categorical must be TRUE or FALSE")
  if(!is.na(covariate_is_categorical) && !covariate_is_categorical%in%c(TRUE,FALSE)) stop("covariate_is_categorical must be TRUE or FALSE")

  if(!is.null(covariate)) {
    if(!covariate%in%colnames(data))
      stop("covariate should be NA or one of the column names")}
  if(! (is.numeric(nsimn)&&nsimn>0)) stop("nsimn should be a positive integer")
  if(sum(target_columns%in%colnames(data))!=length(target_columns)) stop("target_columns must match column names")
  if(sum(levels%in%c(0,1))!=length(levels)) stop("levels must be 0 or 1")
  if(!( is.null(max_size) | (is.numeric(max_size)&&sum(max_size>0)==length(max_size)) )) stop("max_size a positive integer")
  if(!( is.null(breaks) | (is.numeric(breaks)&&breaks>0) )) stop("breaks must be a positive integer")
  if(!(is.null(effect_size) | (is.numeric(effect_size) & length(effect_size) == 3))) stop("effect_size must be a 3 element numeric vector")
  if(!is.null(ICC) & error_is_non_normal==TRUE) stop("ICC-based simulations are not supported when the response is non-normal")
  if(!is.null(ICC)){if(length(ICC) != length(experimental_columns)) stop("The ICC vector should be of the same dimension as the number of experimental columns")}
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


  if(na.action=="complete"){

    notNAindex=which( rowSums(is.na(data)) == 0 )

  }else if(na.action=="unique"){

    if(!is.null(covariate)) notNAindex=which( rowSums(is.na(data[,c(condition_column, experimental_columns, response_column, covariate)])) == 0 )
    else notNAindex=which( rowSums(is.na(data[,c(condition_column, experimental_columns, response_column)])) == 0 )


  }

  Data <- data[notNAindex,]


  # cat("\n")
  # print("__________________________________________________________________Summary of data:")
  # print(summary(Data))
  # cat("\n")

  colnames_original=colnames(Data)
  experimental_columns_index=NULL
  ####### assign categorical variables
  if(condition_is_categorical==TRUE) Data[,condition_column]=as.factor(Data[,condition_column])

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
        Data[,experimental_columns_index[r]]=as.factor(Data[,experimental_columns_index[r]])
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


  ###### indices of target parameters in experimental variables
  target_i=NULL
  target_columns_renamed=NULL
  ###### match target parameters
  for(i in 1:length(target_columns)){
    cn=colnames(Data)[which(colnames_original==target_columns[i])]
    target_columns_renamed[i]=cn

    target_i=c(target_i,as.integer(substr(cn,nchar(cn),nchar(cn))))
  }


  multiple_levels_for_each_random_effect <- TRUE
  ####### Check to see if there is only one category
  for(i in 1:length(experimental_columns)){
    if(length(table(Data[,experimental_columns_index[i]]))==1 && multiple_levels_for_each_random_effect){
      multiple_levels_for_each_random_effect <- FALSE
    }
  }


  ####### run the formula
  if(multiple_levels_for_each_random_effect){


    # Build formula components
    fixed_formula <- build_fixed_formula(covariate, include_interaction)
    random_formula <- build_random_formula(experimental_columns, random_slope_variable)
    # lmerFit <- generate_model_fit_4_power_estimates(data=Data,
    #                               fixed_formula,
    #                               random_formula,
    #                               error_is_non_normal,
    #                               family_p,
    #                               total_column)


    if (error_is_non_normal == FALSE) {
      # Linear mixed effects model
      formula_str <- as.formula(paste("response_column ~", fixed_formula, "+", random_formula))
      lmerFit <- lmer(formula_str, data = Data)
      ##base model fixed formula when there is an interaction term
      if(!is.null(covariate)){
        formula_str0 <- as.formula(paste0("response_column ~ covariate"))
      }
    } else if (!is.null(family_p) && family_p$family == "binomial" && !is.null(total_column)) {
      # Binomial with total column
      formula_str <- as.formula(paste("cbind(response_column, (total_column - response_column)) ~",
                                       fixed_formula, "+", random_formula))
      lmerFit <- lme4::glmer(formula_str, data = Data, family = family_p)
      ##base model fixed formula when there is an interaction term
      if(!is.null(covariate)){
        formula_str0 <- as.formula(paste0("cbind(response_column, (total_column - response_column)) ~ covariate"))
      }

    } else if (!is.null(family_p) && family_p$family == "negative_binomial" && !is.null(total_column)) {
      # Negative binomial with offset
      formula_str <- as.formula(paste("response_column ~", fixed_formula, "+", random_formula,
                                       "+ offset(log(total_column))"))

      lmerFit <- lme4::glmer.nb(formula_str, data = Data, family = family_p)
      ##base model fixed formula when there is an interaction term
      if(!is.null(covariate)){
        formula_str0 <- as.formula(paste0("response_column ~ covariate"))

      }
    } else if (!is.null(family_p) && family_p$family == "poisson" && !is.null(total_column)) {
      # Poisson with offset
      formula_str <- as.formula(paste("response_column ~", fixed_formula, "+", random_formula,
                                       "+ offset(log(total_column))"))

      lmerFit <- lme4::glmer(formula_str, data = Data, family = family_p)
      ##base model fixed formula when there is an interaction term
      if(!is.null(covariate)){
        formula_str0 <- as.formula(paste0("response_column ~ covariate"))

      }
    }
    else if (!is.null(family_p) && family_p$family == "negative_binomial" && is.null(total_column)) {
      # Negative binomial with offset
      formula_str <- as.formula(paste("response_column ~", fixed_formula, "+", random_formula))
      lmerFit <- lme4::glmer.nb(formula_str, data = Data, family = family_p)
      ##base model fixed formula when there is an interaction term
      if(!is.null(covariate)){
        formula_str0 <- as.formula(paste0("response_column ~ covariate"))
      }
    }
    else {
      # Other GLMMs
      formula_str <- as.formula(paste("response_column ~", fixed_formula, "+", random_formula))
      lmerFit <- lme4::glmer(formula_str, data = Data, family = family_p)
      ##base model fixed formula when there is an interaction term
      if(!is.null(covariate)){
        formula_str0 <- as.formula(paste0("response_column ~ covariate"))
      }
    }

  }else if(!multiple_levels_for_each_random_effect && !error_is_non_normal){

      if(is.null(covariate)){
        lmerFit=stats::lm(response_column ~ condition_column, data=Data)
      }else if(!include_interaction){
        lmerFit=stats::lm(response_column ~ condition_column + covariate, data=Data)
      }else{
        lmerFit=stats::lm(response_column ~ condition_column*covariate, data=Data)
      }




  }


  if(error_is_non_normal == FALSE & multiple_levels_for_each_random_effect){
    lmerFit <- as(lmerFit, "lmerMod")
    lmerFit@call[[2]] <- formula_str
  }
  if(error_is_non_normal == TRUE) {
    if(family_p$family != "negative_binomial")
      lmerFit@call[[4]] <- family_p
  }
  slmerFit <- summary(lmerFit)
  # cat("\n")
  # print("__________________________________________________________________Model statistics:")
  # print(slmerFit)
  # cat("\n")

  fixed_effects=slmerFit$coefficients[,1]







  ##### ICC based variance estimation
  if(length(ICC)>0 && !error_is_non_normal){

    ####### If there is only one category, add one more
    for(i in 1:length(experimental_columns)){
      if(length(table(Data[,experimental_columns_index[i]]))==1){
        categroy_temp=paste0(Data[,experimental_columns_index[i]][1],"_2")
        levels(Data[,experimental_columns_index[i]])=c(levels(Data[,experimental_columns_index[i]]),categroy_temp)
        Data[,experimental_columns_index[i]][sample(1:length(Data[,1]),round(length(Data[,1])/2))]=rep(categroy_temp,round(length(Data[,1])/2))
      }
    }


    ##estimate varEs
    a = matrix(NA, length(experimental_columns), length(experimental_columns))
    b = matrix(NA, length(experimental_columns), 1)

    for(i in 1:length(experimental_columns)) {
      for(j in 1:length(experimental_columns)) {
        a[j,i] = -ICC[j]
      }
      a[i,i] = 1 - ICC[i]
      b[i,1] = ICC[i]*(slmerFit$sigma)^2
    }
    varEs=solve(a,b)

    # Build formula components
    fixed_formula <- build_fixed_formula(covariate, include_interaction)
    random_formula <- build_random_formula(experimental_columns, random_slope_variable)
    formula_str <- as.formula(paste("response_column ~", fixed_formula, "+", random_formula))

    artificial_lmer=simr::makeLmer(formula = formula_str,
                                   data=Data,
                                   VarCorr = as.list(varEs), sigma = slmerFit$sigma,
                                   fixef=fixed_effects )
    lmerFit=artificial_lmer

    ##base model fixed formula when there is an interaction term
    if(!is.null(covariate)){
      formula_str0 <- as.formula(paste0("response_column ~ covariate"))
    }


  }





  ####### print observed levels and sample sizes
  maxs=NULL
  mins=NULL
  lens=NULL
  for(i in 1:length(experimental_columns)){
    # cat("\n")
    # print(paste("__________________________________________________________________Levels and sample sizes of",experimental_columns[i]))
    # cat("\n")
    xtabs_s=stats::xtabs(~Data[,paste0("experimental_column",i)])


    # print(xtabs_s)
    maxs=c(maxs,max(xtabs_s))
    if(xtabs_s[1]==0){
      mins=c(mins,min(xtabs_s[-1]))
    }else{
      mins=c(mins,min(xtabs_s))
    }

    lens=c(lens,length(xtabs_s))
    # cat("\n")
    # print(paste("_________________________________Max sample size:",maxs[i]))
    # print(paste("_________________________________Min sample size:",mins[i]))
    # print(paste("_________________________________Count of levels:",lens[i]))
    # cat("\n")
  }







  ##### Assign known effect sizes
  if(length(effect_size)>0){

    if(!is.na(effect_size[1])){
      fixef(lmerFit)[2] <- effect_size[1]
      message("Effect size of the condition_column is now ", effect_size[1])
    }
    if(!is.na(effect_size[2]) & !is.null(covariate)){
      fixef(lmerFit)[3] <- effect_size[2]
      message("Effect size of the covariate is now ", effect_size[2])
    }
    if(!is.null(include_interaction)) {
      if(!is.na(effect_size[3]) & include_interaction){
        fixef(lmerFit)[4] <- effect_size[3]
        message("Effect size of the interaction term is now ", effect_size[3])
      }
    }

  }




  if(power_curve==0){




    ####### extend parameter levels and sample sizes


    if(length(max_size)==0){max_size=rep(0, length(target_columns))}
    if(length(breaks)==0){
      breaks=rep(0,length(target_columns))
    }


    for(i in 1:length(target_columns)){#target_columns_renamed, levels and max_size follow user input order but lens, mins, maxs don't
      if(levels[i]==1){ # increase levels

        if(max_size[i]==0){
          max_size[i]=lens[target_i[i]]*5
        }else if(max_size[i]<lens[target_i[i]]){
          # cat("\n")
          # print(paste("_________________________________Max size is set to ",max_size[i]," which is smaller than the observed max size ",lens[target_i[i]],". The observed max size will be used instead.",sep=""))
          # cat("\n")
          max_size[i]=lens[target_i[i]]
        }
        if(breaks[i]==0) breaks[i] = max(1, round( lens[target_i[i]] / 5 ))
        if(i==1){
          extended_target_columns=simr::extend(lmerFit,along=target_columns_renamed[i],n=max_size[i])
        }else{
          extended_target_columns=simr::extend(extended_target_columns,along=target_columns_renamed[i],n=max_size[i])
        }

      }else{ # increase sample sizes
        if(max_size[i]==0){
          max_size[i]=maxs[target_i[i]]*5
        }else if(max_size[i]<maxs[target_i[i]]){
          # cat("\n")
          # print(paste("_________________________________Max size is set to ",max_size[i]," which is smaller than the observed max size ",maxs[target_i[i]],". The observed max size will be used instead.",sep=""))
          # cat("\n")
          max_size[i]=maxs[target_i[i]]
        }
        if(breaks[i]==0) breaks[i] = max(1, round( maxs[target_i[i]] / 5 ))
        if(i==1){
          extended_target_columns=simr::extend(lmerFit,within=target_columns_renamed[i],n=max_size[i])
        }else{
          extended_target_columns=simr::extend(extended_target_columns,within=target_columns_renamed[i],n=max_size[i])
        }
      }
      # cat("\n")
      # print(paste("Input max size of",target_columns[i]))
      # print(max_size[i])
      # cat("\n")

    }

    #cat("\n")
    # print("__________________________________________________________________Extended parameters:")
    # for(i in 1:length(target_columns)){
    #   if(target_columns_renamed[i]=="experimental_column1"){
    #     # print(xtabs(~experimental_column1,data=attributes(extended_target_columns)$newData))
    #     cat("\n")
    #   }
    #   if(target_columns_renamed[i]=="experimental_column2"){
    #     # print(xtabs(~experimental_column2,data=attributes(extended_target_columns)$newData))
    #     cat("\n")
    #   }
    #   if(target_columns_renamed[i]=="experimental_column3"){
    #     # print(xtabs(~experimental_column3,data=attributes(extended_target_columns)$newData))
    #     cat("\n")
    #   }
    #   if(target_columns_renamed[i]=="experimental_column4"){
    #     # print(xtabs(~experimental_column4,data=attributes(extended_target_columns)$newData))
    #     cat("\n")
    #   }
    #   if(target_columns_renamed[i]=="experimental_column5"){
    #     # print(xtabs(~experimental_column5,data=attributes(extended_target_columns)$newData))
    #     cat("\n")
    #   }
    #
    # }

    if(length(experimental_columns)>=2){
            for(r in 2:length(experimental_columns)){
        if(colnames(Data)[experimental_columns_index[r]]%in%noncrossed_columns){
          attributes(extended_target_columns)$newData[,experimental_columns_index[r]]=paste(attributes(extended_target_columns)$newData[,experimental_columns_index[r-1]],attributes(extended_target_columns)$newData[,experimental_columns_index[r]],sep="_")
        }
      }
      # print(attributes(extended_target_columns)$newData)
    }



    ###### power simulation

    if(!is.null(covariate))
      ps=simr::powerSim(extended_target_columns, test=simr::fcompare(formula_str0), nsim=nsimn, progress = FALSE)
    else
      ps=simr::powerSim(extended_target_columns, test=simr::fixed("condition_column"), nsim=nsimn, progress = FALSE)
    # cat("\n")
    # print("__________________________________________________________________Power simulation result:")
    # print(ps)
    # cat("\n")
    if(length(output)!=0){
      sink(paste0(output,".txt"))
       print(ps)
      sink()
    }else{
       print(ps)
    }







  }else{#power curve =1


    ####### extend parameter levels and sample sizes
    extended_target_columns=list()
    if(length(max_size)==0){max_size=rep(0,length(target_columns))}
    if(length(breaks)==0){
      breaks=rep(0,length(target_columns))
    }





    for(i in 1:length(target_columns)){
      if(levels[i]==1){ # increase levels
        if(max_size[i]==0){
          max_size[i]=lens[target_i[i]]*5
        }else if(max_size[i]<lens[target_i[i]]){
          # cat("\n")
          # print(paste("_________________________________Max size is set to ",max_size[i]," which is smaller than the observed max size ",lens[target_i[i]],". The observed max size will be used instead.",sep=""))
          # cat("\n")
          max_size[i]=lens[target_i[i]]
        }
        if(breaks[i]==0) breaks[i] = max(1, round( lens[target_i[i]] / 5 ))
        extended_target_columns=c(extended_target_columns,list(simr::extend(lmerFit,along=target_columns_renamed[i],n=max_size[i])))
      }else{ # increase sample sizes
        if(max_size[i]==0){
          max_size[i]=maxs[target_i[i]]*5
        }
        if(breaks[i]==0) breaks[i] = max(1, round( maxs[target_i[i]] / 5 ))
        extended_target_columns=c(extended_target_columns,list(simr::extend(lmerFit,within=target_columns_renamed[i],n=max_size[i])))
      }
      # cat("\n")
      # print(paste("_________________________________Power simulation will be performed based on the max size of",target_columns[i],":"))
      # print(max_size[i])
      # cat("\n")

    }


    # cat("\n")
    # print("__________________________________________________________________Extended parameters:")
    # for(i in 1:length(target_columns)){
    #   if(target_columns_renamed[i]=="experimental_column1"){
    #     # print(xtabs(~experimental_column1,data=attributes(extended_target_columns[[i]])$newData))
    #     cat("\n")
    #   }
    #   if(target_columns_renamed[i]=="experimental_column2"){
    #     # print(xtabs(~experimental_column2,data=attributes(extended_target_columns[[i]])$newData))
    #     cat("\n")
    #   }
    #   if(target_columns_renamed[i]=="experimental_column3"){
    #     # print(xtabs(~experimental_column3,data=attributes(extended_target_columns[[i]])$newData))
    #     cat("\n")
    #   }
    #   if(target_columns_renamed[i]=="experimental_column4"){
    #     # print(xtabs(~experimental_column4,data=attributes(extended_target_columns[[i]])$newData))
    #     cat("\n")
    #   }
    #   if(target_columns_renamed[i]=="experimental_column5"){
    #     # print(xtabs(~experimental_column5,data=attributes(extended_target_columns[[i]])$newData))
    #     cat("\n")
    #   }
    #
    # }

    if(length(experimental_columns)>=2){
            for(r in 2:length(experimental_columns)){
        if(experimental_columns[r]%in%noncrossed_columns){
          attributes(extended_target_columns)$newData[noncrossed_columns[r]]=paste(attributes(extended_target_columns)$newData[noncrossed_columns[r-1]],
                                                                                      attributes(extended_target_columns)$newData[noncrossed_columns[r]],sep="_")
        }
      }
      # print(attributes(extended_target_columns)$newData)
    }


    plots <- list()
    captions <- vector(mode = "character")
    ###### power curve simulation
    for(i in 1:length(target_columns)){

      if(levels[i]==1){

          # pc=simr::powerCurve(extended_target_columns[[i]], test=simr::fixed("condition_column"),along=target_columns_renamed[i], nsim=nsimn,
          #                   breaks=seq(1,max_size[i],breaks[i]), progress = FALSE   )
          if(!is.null(covariate))
            pc=simr::powerCurve(extended_target_columns[[i]], test=simr::fcompare(formula_str0),along=target_columns_renamed[i], nsim=nsimn, breaks=seq(1,max_size[i],breaks[i]), progress = FALSE)
          else
            pc=simr::powerCurve(extended_target_columns[[i]], test=simr::fixed("condition_column"), along=target_columns_renamed[i], nsim=nsimn, breaks=seq(1,max_size[i],breaks[i]), progress = FALSE)

      }else{

          if(!is.null(covariate))
            pc=simr::powerCurve(extended_target_columns[[i]], test=simr::fcompare(formula_str0),within=target_columns_renamed[i], nsim=nsimn, breaks=seq(1,max_size[i],breaks[i]), progress = FALSE)
          else
            pc=simr::powerCurve(extended_target_columns[[i]], test=simr::fixed("condition_column"), within=target_columns_renamed[i], nsim=nsimn, breaks=seq(1,max_size[i],breaks[i]), progress = FALSE)

      }





      if(length(output)==0){
        #plot(pc)
        plot_data <- do.call("rbind", lapply(pc$ps, summary))
        plot_data <- plot_data[,-(1:2)]
        ##convert to percentage
        plot_data <- 100*plot_data
        plot_data <- data.frame(levels = pc$nlevels, plot_data)
        plots[[i]] <- ggplot(plot_data, aes(x=levels, y=mean)) +
          geom_point() +
          geom_line() +
          geom_errorbar(aes(ymin = lower, ymax = upper)) +
          geom_hline(yintercept = 80, lty=2) +
          labs(title = "Sample size calculations",
               subtitle = paste0("Statistical power estimates expressed as a function of different numbers of ", target_columns[i]),
               x = paste0("Number of ", target_columns[i]),
               y = "Statistical power") +
          theme_minimal() +
          theme(plot.title = element_text(size = 12, face = "bold"))+
          theme(plot.subtitle  = element_textbox_simple())

        captions[i] <- paste0("Statistical power estimates expressed as a function of different numbers of ", target_columns[i])
      }else{
        png(paste0(output[i],".png"))
        print(plots[[i]] + ggtitle(captions[i]) + theme(plot.title = element_textbox_simple()))
        print(mtext(response_column))
        dev.off()
      }



    }
    return(list(plots = plots, captions = captions))
  }


}

