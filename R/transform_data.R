#' @title transform_data
#'
#' @description This function can be used to generate diagnostic QC plots for given model assumptions related to the input data, identify potential outlier observations and/or outlier experimental units
#'
#'
#' @param data Input data
#' @param condition_column Name of the condition variable (ex variable with values such as control/case). The input file has to have a corresponding column name
#' @param experimental_columns Name of the variable related to experimental design such as "experiment", "plate", and "cell_line". They should be in order, for example, "experiment" should always come first .
#' @param response_column Name of the variable observed by performing the experiment. ex) intensity.
#' @param total_column Set this column only when family_p="binomial" and it is equal to the total number of observations (number of cases plus number of controls) for a given number of cases
#' @param condition_is_categorical Specify whether the condition variable is categorical. TRUE: Categorical, FALSE: Continuous.
#' @param covariate The name of the covariate to control in the regression model
#' @param crossed_columns Name of experimental variables that may appear repeatedly with the same ID. For example, cell_line C1 may appear in multiple experiments, but plate P1 cannot appear in more than one experiment
#' @param error_is_non_normal Default: the observed variable is continuous Categorical response variable will be implemented in the future. TRUE: Categorical , FALSE: Continuous (default).
#' @param family_p The type of distribution family to specify when the response is categorical. If family is "binary" then binary(link="log") is used, if family is "poisson" then poisson(link="logit") is used, if family is "poisson_log" then poisson(link=") log") is used.
#' @param alpha numeric scalar between 0 and 1 indicating the Type I error associated with the test of outliers
#' @param na.action "complete": missing data is not allowed in all columns (default), "unique": missing data is not allowed only in condition, experimental, and response columns. Selecting "complete" removes an entire row when there is one or more missing values, which may affect the distribution of other features.
#' @param include_interaction logical - TRUE or FALSE - Whether to include condition * covariate interaction
#' @param random_slope_variable Variable for random slopes (typically "condition_column")
#' @param covariate_is_categorical Specify whether the covariate variable is categorical. TRUE: Categorical, FALSE: Continuous.
#' @param print_plots Whether or not to print the plots, irrespective of this argument ggplot versions of the different QC figures generated are returned. TRUE - print the plots, FALSE - do not print the plots
#'
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
#' @examples result=transform_data2(data=data, condition_column="classif", experimental_columns=c("experiment","line"), response_column="feature", condition_is_categorical=TRUE, error_is_non_normal=FALSE, alpha=0.05, crossed_columns = "line", method="cook", na.action="complete")
#' @examples result=transform_data2(data=data, condition_column="classif", experimental_columns=c("experiment","line"), response_column="feature", condition_is_categorical=TRUE, error_is_non_normal=FALSE, alpha=0.05, crossed_columns = "line", method="cook", na.action="complete")
#' @examples result=transform_data2(data=data, condition_column="classif", experimental_columns=c("experiment","line"), response_column="feature", condition_is_categorical=TRUE, error_is_non_normal=TRUE, family_p="poisson", alpha=0.05, crossed_columns = "line", method="cook", na.action="complete")

transform_data<-function(data, condition_column, experimental_columns, response_column, total_column=NULL, condition_is_categorical=TRUE, covariate=NULL,
                         crossed_columns = NULL, error_is_non_normal=FALSE, family_p=NULL, alpha=0.05, na.action="complete", include_interaction = NA, random_slope_variable = NULL, covariate_is_categorical = NA, print_plots = TRUE){



  if(na.action=="complete"){

    notNAindex=which( rowSums(is.na(data)) == 0 )

  }else if(na.action=="unique"){

    if(is.null(covariate)) notNAindex=which( rowSums(is.na(data[,c(condition_column, experimental_columns, response_column, covariate)])) == 0 )
    else notNAindex=which( rowSums(is.na(data[,c(condition_column, experimental_columns, response_column)])) == 0 )


  }



  data=data[notNAindex,]




  Data_updated=data

  plots_info <- list()
  cooks_result <- list()
  model_no <- 1
  models <- c("Natural scale")

  family_p_temp = family_p
  lms=get_model_and_data(data=data, condition_column=condition_column, experimental_columns=experimental_columns,
                         response_column=response_column, total_column = total_column, condition_is_categorical=condition_is_categorical, covariate=covariate,
                         crossed_columns=crossed_columns, error_is_non_normal=error_is_non_normal, family_p=family_p_temp, na.action=na.action,include_interaction=include_interaction, random_slope_variable=random_slope_variable, covariate_is_categorical = covariate_is_categorical)

  plots_info[[model_no]] <- generate_qc_plots(lms = lms,
                                           error_is_non_normal = error_is_non_normal,
                                           description_suffix = "(natural scale)",
                                           experimental_columns,
                                           alpha)
  names(plots_info)[model_no] <- "natural_scale"

  residual <- plots_info[[model_no]]$residual
  #
  cutoffs=rosner_test(trait=residual, response_column=response_column, alpha=alpha, hist_text = "raw residual")

  #run cook
  fixed_global_variable_data<<-lms[[2]]

  choose_cols <- vector(mode = "character")
  temp_count <- 0
  for(c in 1:length(lms[[3]])) {
    if(length(unique(lms[[2]][[lms[[3]][c]]])) > 2) {
      temp_count <- temp_count + 1
      choose_cols[temp_count] <- lms[[3]][c]
    }
  }

  if(temp_count > 0) {
    cooks_result[[model_no]]=cooks_test(lms[[1]], lms[[2]], choose_cols, response_column="response_column", hist_text="raw")
    names(cooks_result[[model_no]]) <- paste0(names(cooks_result[[model_no]]), "_raw")

  }
  else {
    print(paste("_________________________________Not enough grouping levels to perform the cook analyses on the experimental factors", sep=""))
    return()
  }

  Data_noOutlier=Data_updated
  Data_noOutlier[is.infinite(residual),] <- NA
  if(sum(is.na(cutoffs))<2 | sum(is.infinite(residual)) > 0){

    model_no <- model_no + 1
    models[model_no] <- "Natural scale wo outliers"

    if(!is.na(cutoffs[1])){
      Data_noOutlier[residual<=cutoffs[1],]=NA
    }
    if(!is.na(cutoffs[2])){
      Data_noOutlier[residual>=cutoffs[2],]=NA
    }

    Data_updated=cbind(Data_noOutlier[,response_column], Data_updated)
    colnames(Data_updated)[1]=paste0(response_column,"_noOutlier")

    lms=get_model_and_data(data=Data_updated, condition_column=condition_column, experimental_columns=experimental_columns,
                           response_column=paste0(response_column,"_noOutlier"), total_column = total_column, condition_is_categorical=condition_is_categorical, covariate=covariate,
                           crossed_columns=crossed_columns, error_is_non_normal=error_is_non_normal, family_p=family_p_temp, na.action=na.action,include_interaction=include_interaction, random_slope_variable=random_slope_variable, covariate_is_categorical = covariate_is_categorical)

    plots_info[[model_no]] <- generate_qc_plots(lms = lms,
                                           error_is_non_normal = error_is_non_normal,
                                           description_suffix = " (natural scale wo outliers)",
                                           experimental_columns,
                                           alpha)
    names(plots_info)[model_no] <- "natural_scale_wo_outliers"

    #run cook
    fixed_global_variable_data<<-lms[[2]]
    #family_p<<-family_p


    if(length(choose_cols) > 0) {
      cooks_result[[model_no]]=cooks_test(lms[[1]], lms[[2]], choose_cols, response_column="response_column", hist_text="raw wo outliers")
      names(cooks_result[[model_no]]) <- paste0(names(cooks_result[[model_no]]), "_raw_wo_outliers")

    }
    else {
      print(paste("_________________________________Not enough grouping levels to perform the cook analyses on the experimental factors", sep=""))
      return()
    }


  }


  if(error_is_non_normal==FALSE){

    ########################do the same using log transformed values

    ###log transform

    model_no <- model_no + 1
    models[model_no] <- "Log scale"
    Data_log=data
    temp1=Data_log[, response_column]


    #seperate zero and negative
    if(min(Data_log[,response_column])<0){

      temp2 = abs(min(temp1))/10 - min(temp1)
      Data_log[,response_column]=log(Data_log[,response_column]+temp2)

    }else if(min(Data_log[,response_column])==0){

      temp2 = min(temp1[temp1>0])/10
      Data_log[,response_column]=log(Data_log[,response_column]+temp2)

    }else{

      Data_log[,response_column]=log(Data_log[,response_column])

    }


    Data_updated=cbind(Data_log[,response_column], Data_updated)
    colnames(Data_updated)[1]=paste0(response_column,"_logTransformed")



    lms=get_model_and_data(data=Data_updated, condition_column=condition_column, experimental_columns=experimental_columns,
                           response_column=paste0(response_column,"_logTransformed"), condition_is_categorical=condition_is_categorical, covariate=covariate,
                           crossed_columns=crossed_columns, error_is_non_normal=error_is_non_normal, family_p=family_p_temp, na.action=na.action, include_interaction=include_interaction, random_slope_variable=random_slope_variable, covariate_is_categorical = covariate_is_categorical)

    plots_info[[model_no]] <- generate_qc_plots(lms = lms,
                                            error_is_non_normal = FALSE,
                                            description_suffix = "(log scale)",
                                            experimental_columns,
                                            alpha)
    names(plots_info)[model_no] <- "log_scale"

    #run cook
    fixed_global_variable_data<<-lms[[2]]
    #family_p<<-family_p


    if(length(choose_cols) > 0) {
      cooks_result[[model_no]]=cooks_test(lms[[1]], lms[[2]], choose_cols, response_column="response_column", hist_text="log transform")
      names(cooks_result[[model_no]]) <- paste0(names(cooks_result[[model_no]]), "_logTransformed")
    }
    else {
      print(paste("_________________________________Not enough grouping levels to perform the cook analyses on the experimental factors", sep=""))
      return()
    }


    residual <- plots_info[[model_no]]$residual

    # trait = residual ###change the feature as you want
    #
    #
    #
    # #if(method=="rosner"){
    #
    cutoffs=rosner_test(trait=lms[[4]], response_column=response_column, alpha=alpha, hist_text = "log residual")

    if(sum(is.na(cutoffs))<2){

      model_no <- model_no + 1
      models[model_no] <- "Log scale wo outliers"
      Data_log_noOutlier=Data_log

      if(!is.na(cutoffs[1])){
        Data_log_noOutlier[residual<=cutoffs[1],]=NA
      }
      if(!is.na(cutoffs[2])){
        Data_log_noOutlier[residual>=cutoffs[2],]=NA
      }

      Data_updated=cbind(Data_log_noOutlier[,response_column], Data_updated)
      colnames(Data_updated)[1]=paste0(response_column,"_logTransformed_noOutlier")

      lms=get_model_and_data(data=Data_updated, condition_column=condition_column, experimental_columns=experimental_columns,
                             response_column=paste0(response_column,"_logTransformed_noOutlier"), condition_is_categorical=condition_is_categorical, covariate=covariate,
                             crossed_columns=crossed_columns, error_is_non_normal=error_is_non_normal, family_p=family_p_temp, na.action=na.action, include_interaction=include_interaction, random_slope_variable=random_slope_variable, covariate_is_categorical = covariate_is_categorical)

      plots_info[[model_no]] <- generate_qc_plots(lms = lms,
                                              error_is_non_normal = FALSE,
                                              description_suffix = "(log scale wo outlier)",
                                              experimental_columns,
                                              alpha)

      names(plots_info)[model_no] <- "log_scale_wo_outliers"

      #run cook
      fixed_global_variable_data<<-lms[[2]]
      #family_p<<-family_p


      if(length(choose_cols) > 0) {
        cooks_result[[model_no]]=cooks_test(lms[[1]], lms[[2]], choose_cols, response_column="response_column", hist_text="log transform wo outliers")
        names(cooks_result[[model_no]]) <- paste0(names(cooks_result[[model_no]]), "_logTransformed_wo_outliers")
      }
      else {
        print(paste("_________________________________Not enough grouping levels to perform the cook analyses on the experimental factors", sep=""))
        return()
      }


    }


  }
  result=list(Data_updated, cooks_result, plots_info, models)
  names(result)=c("Data_updated", "cooks_result", "plots_info", "models")

  if(print_plots) {
    for(m in 1:length(models)) {
      plots <- plots_info[[m]]$plots
      captions <- plots_info[[m]]$captions
      for (i in seq_along(plots)) {
        print(plots[[i]] + ggtitle(paste(models[m], captions[i]), sep =":") + theme(plot.title = element_textbox_simple()))
      }

    }
  }
  return(result)
}


generate_qc_plots <-  function(lms,
                               error_is_non_normal,
                               description_suffix,
                               experimental_columns,
                               alpha) {

  plot_index <- 0
  plots <- list()
  captions <- vector(mode = "character")

  if(error_is_non_normal==FALSE){

    residual <- lms[[4]]
    model_data <- data.frame(fitted = predict(lms[[1]], type = "link"),
                             model_residuals = lms[[4]])
    model_data$sqrt_abs_residuals = sqrt(abs(model_data$model_residuals))


    plots[[plot_index+1]] <- ggplot(model_data, aes(sample = model_residuals)) +
      stat_qq() +
      stat_qq_line(color = "red") +
      labs(title = "Q-Q Plot: Residuals",
           subtitle = paste0("Check normality of residuals",description_suffix),
           x = "Theoretical Quantiles",
           y = "Sample Quantiles") +
      theme_minimal() +
      theme(plot.title = element_text(size = 12, face = "bold"))

    names(plots)[plot_index+1] <- "residuals_QQ"

    captions[plot_index+1] <- paste0("Check normality of residuals ", description_suffix, ". Expectation is that the black points lie on or close to the solid red diaganol line")



    # 1a. Residuals vs Fitted Values
    plots[[plot_index+2]] <- ggplot(model_data, aes(x = fitted, y = model_residuals)) +
      geom_point(alpha = 0.6, size = 2) +
      geom_smooth(method = "loess", se = TRUE, color = "red") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
      labs(title = paste0("Residuals vs Fitted Values ", description_suffix),
           subtitle = "Check for linearity",
           x = "Fitted Values",
           y = "Residuals") +
      theme_minimal() +
      theme(plot.title = element_text(size = 12, face = "bold"))

    names(plots)[plot_index+2] <- "residuals_vs_fitted"
    captions[plot_index+2] <- paste0("Check for linearity ", description_suffix, ". Expectation is that the best fit solid line is horizontal or close to being horizontal")

    # 1b. Scale-Location Plot (Square root of standardized residuals vs fitted)
    plots[[plot_index+3]] <- ggplot(model_data, aes(x = fitted, y = sqrt_abs_residuals)) +
      geom_point(alpha = 0.6, size = 2) +
      geom_smooth(method = "loess", se = TRUE, color = "red") +
      labs(title = paste0("Scale-Location Plot ", description_suffix),
           subtitle = "Check for homoscedasticity",
           x = "Fitted Values",
           y = "√|Standardized Residuals|") +
      theme_minimal() +
      theme(plot.title = element_text(size = 12, face = "bold"))
    names(plots)[plot_index+3] <- "residuals_homoscedasticity"
    captions[plot_index+3] <- paste0("Check for homoscedasticity ",description_suffix, ". Expectation is that the best fit solid line is horizontal or close to being so")
    plot_index <- plot_index + 3

  }else{
    simulationOutput <- DHARMa::simulateResiduals(lms[[1]], plot = F)
    residual = residuals(simulationOutput, quantileFunction = qnorm)
    # plot(simulationOutput)
    # DHARMa::plotResiduals(simulationOutput, form =  lms[[2]]$condition_column)

    plots[[plot_index + 1]] <- ggplot_QQunif(simulationOutput)
    names(plots)[plot_index+1] <- "residuals_QQ"
    captions[plot_index + 1] <- paste0("Q-Q Plot for Uniform Distribution", ". Expectation is that the black points lie on or close to the solid red diaganol line")

    plots[[plot_index + 2]] <- ggplot_residuals_vs_predictor(simulationOutput)
    names(plots)[plot_index+2] <- "residuals_vs_predicted"
    captions[plot_index + 2] <- paste0("Residuals vs Predicted", ". Expectation is that the best fit blue lines at the three quartiles- 0.25, 0.50 and 0.75 - are close to the dashed horizontal lines at their respective quartiles")

    plot_index <- plot_index + 2
  }


  random_effects <- lme4::ranef(lms[[1]])

  temp_count <- 1
  for(c in (1:length(random_effects))) {
    exp_factor <- experimental_columns[as.integer(gsub("experimental_column", "", names(random_effects)[c]))]
    for(j in 1:ncol(random_effects[[c]])) {

      model_data <- data.frame(model_residuals = random_effects[[c]][,j])
      plots[[plot_index+1]] <- ggplot(model_data, aes(sample = model_residuals)) +
        stat_qq() +
        stat_qq_line(color = "red") +
        labs(title = paste0("Q-Q Plot: Random effects for ", names(random_effects)[c], "_", colnames(random_effects[[c]][j]), ": ", exp_factor),
             subtitle = paste0("Check normality of random effects ", description_suffix),
             x = "Theoretical Quantiles",
             y = "Sample Quantiles") +
        theme_minimal() +
        theme(plot.title = element_text(size = 12, face = "bold"))

      names(plots)[plot_index+1] <- paste0("random_effects_QQ_",temp_count)
      captions[plot_index+1] <- paste0("Check normality of random effects for ", names(random_effects)[c], "_", colnames(random_effects[[c]][j]), " ", description_suffix, ": ", exp_factor, ". Expectation is that the black points lie on or close to the solid red diaganol line")

      plot_index <- plot_index + 1
      temp_count <- temp_count + 1
    }
  }

  cutoffs=rosner_test(trait=residual, response_column=response_column, alpha=alpha, hist_text = "raw residual")
  if(sum(is.na(cutoffs))<2 | sum(is.infinite(residual)) > 0){
    residuals_df <- data.frame(residual)
    plots[[plot_index+1]] <- ggplot(residuals_df, aes(x=residual)) +
      geom_histogram() +
      labs(title = paste0("Histogram of Residuals Values"),
           subtitle = "Check for outliers",
           x = "Residuals") +

      theme_minimal() +
      theme(plot.title = element_text(size = 12, face = "bold"))
    if(!is.na(cutoffs[1])){
      plots[[plot_index+1]] <- plots[[plot_index+1]] + geom_vline(xintercept = cutoffs[1], color = "red")
    }
    if(!is.na(cutoffs[2])){
      plots[[plot_index+1]] <- plots[[plot_index+1]] + geom_vline(xintercept = cutoffs[2], color = "red")
    }

    names(plots)[plot_index+1] = "residuals_histogram"
    captions[plot_index+1] = "Histogram of residuals values. Red vertical lines (if present) are the location of the cutoffs to identify the outliers. No red vertical lines implies that the residual values that were estimated to be infinite were identified as outliers"
    plot_index <- plot_index + 1
  }
  return(list(plots = plots, captions = captions, residual = residual))
}

rosner_test<- function (trait, response_column, alpha, hist_text) {

  cutoff1=NA
  cutoff2=NA

  options(warn=-1)
  upper_bound <- median(trait) + 3 * mad(trait)
  upper_bound

  lower_bound <- median(trait) - 3 * mad(trait)
  lower_bound

  outlierC=sum(trait>upper_bound)+sum(trait<lower_bound)
  outlierC

  if( outlierC >0 ){

    ###rosner's test
    test <- EnvStats::rosnerTest(trait,
                                 k = outlierC, alpha=alpha)

    outliers=test$all.stats$Value[test$all.stats$Outlier]

    if(length(test$all.stats$Outlier) !=0  ){
      if(! ( length(test$all.stats$Outlier) ==1 &  sum(is.infinite(test$all.stats$Outlier)) ) ) {


        if(sum(outliers>median(trait)) >0) cutoff1=min( outliers[which(outliers>median(trait))] )
        if(sum(outliers<median(trait)) >0) cutoff2=max( outliers[which(outliers<median(trait))] )




        ############rosner's test end


      }



    }else{
      print("No outlier detected from the raw Data")
    }


  }else{
    print("No outlier detected from the raw Data")
  }

  return(c(cutoff2,cutoff1))
}

cooks_test<- function (model, fixed_global_variable_data, experimental_columns, response_column, hist_text) {

  n_cols_2_test <- length(experimental_columns)
  perform_test <- TRUE
  ##influence takes a very long time for testing influential observations higher than the first level
  if("glmerMod" %in% class(model)) {
    if(summary(model)$family == "binomial")
      n_cols_2_test <- 1
      if("experimental_column1" %in% experimental_columns)
        experimental_columns = "experimental_column1"
      else{
        print("Not enough levels to perform cooks test for experimental_column1 with the assumed binomial distribution")
        cooks_result <- NA
        perform_test <- FALSE
      }
  }



  if(perform_test) {
    cooks_result=lapply(1:n_cols_2_test,
                        function(i){

                          tb=table(fixed_global_variable_data[,"condition_column"], fixed_global_variable_data[,experimental_columns[i]])
                          tb=tb[rowSums(tb)>0,]
                          tb[tb>0]=1
                          condition_counts=rowSums(tb)


                          if(length(condition_counts)==2 & sum(condition_counts==1)==1){
                            not2exclude=fixed_global_variable_data[fixed_global_variable_data[,"condition_column"]==names(condition_counts[condition_counts==1]), experimental_columns[i]][1]
                            not2exclude=as.character(not2exclude)

                            cd=NULL

                            levels=setdiff(fixed_global_variable_data[,experimental_columns[i]],not2exclude)
                            for(level2test in levels ){
                              alt.est <- influence.ME::influence(model, group=experimental_columns[i], select=level2test)
                              cd=rbind(cd, cooks.distance(alt.est))
                            }
                              rownames(cd)=levels
                              cd

                          }else{
                            alt.est <- influence.ME::influence(model, group=experimental_columns[i] )
                            cooks.distance(alt.est)
                          }


                        }
    )


    names(cooks_result)=c(paste0("cooks_distance_",experimental_columns[1:n_cols_2_test]) )


  }

  return(cooks_result)

}

# Function to extract DHARMa residuals and create ggplot-ready data
extract_dharma_data <- function(dharma_obj) {
  data.frame(
    residuals = dharma_obj$scaledResiduals,
    predicted = dharma_obj$fittedPredictedResponse,
    observed = dharma_obj$observedResponse,
    outliers = as.logical(DHARMa::outliers(dharma_obj, return = "logical")),
    rank_predicted = rank(dharma_obj$fittedPredictedResponse),
    index = 1:length(dharma_obj$scaledResiduals)
  )
}

# ==============================================================================
# 1. GGPLOT VERSION OF plotQQunif()
# ==============================================================================

ggplot_QQunif <- function(dharma_obj,
                          title = "Q-Q Plot vs. Uniform Distribution",
                          test_uniformity = FALSE,
                          test_dispersion = FALSE,
                          test_outliers = FALSE) {

  residuals <- dharma_obj$scaledResiduals
  n <- length(residuals)

  # Create theoretical quantiles for uniform distribution
  theoretical <- (1:n - 0.5) / n
  observed <- sort(residuals)

  # Create data frame
  qq_data <- data.frame(
    theoretical = theoretical,
    observed = observed
  )

  # Base plot
  p <- ggplot(qq_data, aes(x = theoretical, y = observed)) +
    geom_point(alpha = 0.6, size = 1.5) +
    geom_abline(intercept = 0, slope = 1, color = "red",  linewidth = 1) +
    labs(title = title,
         x = "Expected (Uniform)",
         y = "Observed Residuals") +
    theme_minimal() +
    theme(plot.title = element_text(size = 12, face = "bold")) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))

  # Add test results as subtitle if requested
  test_results <- character()

  if (test_uniformity) {
    ks_test <- DHARMa::testUniformity(dharma_obj, plot = FALSE)
    test_results <- c(test_results, paste("KS test: p =", round(ks_test$p.value, 4)))
  }

  if (test_dispersion) {
    disp_test <- DHARMa::testDispersion(dharma_obj, plot = FALSE)
    test_results <- c(test_results, paste("Dispersion test: p =", round(disp_test$p.value, 4)))
  }

  if (test_outliers) {
    outlier_test <- DHARMa::testOutliers(dharma_obj, plot = FALSE)
    test_results <- c(test_results, paste("Outlier test: p =", round(outlier_test$p.value, 4)))
  }

  if (length(test_results) > 0) {
    p <- p + labs(subtitle = paste(test_results, collapse = " | "))
  }

  return(p)
}


# Plot residuals against custom predictor with DHARMa-style formatting
ggplot_residuals_vs_predictor <- function(dharma_obj,
                                          predictor = NULL,
                                          predictor_name = "predictions",   #or "predictions"
                                          condition_is_categorical = TRUE,
                                          rank = TRUE,
                                          quantreg = TRUE) {

  plot_data <- extract_dharma_data(dharma_obj)
  if(predictor_name == "predictions")
    plot_data$predictor <- plot_data$rank_predicted
  if(predictor_name == "condition_column" & !is.null(predictor))
    plot_data$predictor <- predictor

  if(predictor_name == "condition_column" & condition_is_categorical)
    plot_data$predictor = as.factor(plot_data$predictor)

  if (rank & !is.factor(plot_data$predictor)) {
    plot_data$predictor <- rank(plot_data$predictor)
    plot_data$predictor <- plot_data$predictor/max(plot_data$predictor)
    x_label <- paste(predictor_name, "(Rank Transformed)")
  } else {
    x_label <- predictor_name
  }

  if(is.factor(plot_data$predictor)){
    p <- ggplot(plot_data, aes(x = predictor, y = residuals)) +
      geom_boxplot() +
      theme_minimal()
  }
  else{
    p <- ggplot(plot_data, aes(x = predictor, y = residuals)) +
      geom_point(color = "black", alpha = 0.6, size = 1.5) +
      theme_minimal()

    if (quantreg && nrow(plot_data) > 10) {
      # Add quantile regression lines
      quantiles <- c(0.25, 0.5, 0.75)
      for (q in quantiles) {
        p <- p + geom_smooth(formula = y ~ poly(x,2), method = "rq", method.args = list(tau = q),
                             se = FALSE, color = "blue", linewidth = 0.8, alpha = 0.7)
      }
      # Add expected horizontal lines
      for (q in quantiles) {
        p <- p + geom_hline(yintercept = q, linetype = "dashed",
                            color = "black", alpha = 0.6)
      }
    } else {
      p <- p + geom_smooth(method = "gam", se = TRUE, color = "blue", alpha = 0.7) +
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", alpha = 0.6)
  }
}
  p <- p +
    labs(title = paste("DHARMa Residuals vs", predictor_name),
         x = x_label,
         y = "DHARMa Residuals") +
    theme_minimal() +
    theme(plot.title = element_text(size = 12, face = "bold")) +
    ylim(0, 1)

  return(p)
}

ensureDHARMa <- function(simulationOutput, convert = TRUE) {
  # Check if it's already a DHARMa object
  if (inherits(simulationOutput, "DHARMa")) {
    return(simulationOutput)
  }

  # Check if it's a numeric vector of residuals
  if (is.numeric(simulationOutput) && is.vector(simulationOutput)) {
    # Assume it's scaled residuals between 0 and 1
    if (any(simulationOutput < 0 | simulationOutput > 1, na.rm = TRUE)) {
      warning("Residuals not between 0 and 1. Are you sure these are DHARMa residuals?")
    }
    # Create a minimal DHARMa-like object
    out <- list(
      scaledResiduals = simulationOutput,
      fittedPredictedResponse = rep(0.5, length(simulationOutput))  # placeholder
    )
    class(out) <- "DHARMa"
    return(out)
  }

  # If convert is TRUE, try to extract from model object
  if (convert == TRUE || convert == "Model") {
    stop("DHARMa::plotResiduals > wrong argument to function, simulationOutput must be a DHARMa object or a numeric vector of quantile residuals! If you want to create DHARMa residuals from a model object, run simulateResiduals() first.")
  }

  stop("simulationOutput must be a DHARMa object, a numeric vector, or a supported model object")
}

# Helper function to ensure predictor
ensurePredictor <- function(simulationOutput, form = NULL) {
  if (is.null(form)) {
    # Use fitted predictions from DHARMa object
    if (!is.null(simulationOutput$fittedPredictedResponse)) {
      return(simulationOutput$fittedPredictedResponse)
    } else {
      stop("No predictor specified and no fitted predictions available in DHARMa object")
    }
  }

  # If form is provided directly as a vector
  if (is.numeric(form) || is.factor(form)) {
    if (length(form) != length(simulationOutput$scaledResiduals)) {
      stop("Length of form does not match number of residuals")
    }
    return(form)
  }

  # If form is a formula or expression, we can't evaluate it without the original data
  # This is a limitation of this standalone implementation
  stop("Form evaluation not supported in this implementation. Please provide predictor values directly as a numeric vector or factor.")
}

# Helper function to check dots (simplified version)
checkDots <- function(name, default, ...) {
  dots <- list(...)
  if (name %in% names(dots)) {
    return(dots[[name]])
  } else {
    return(default)
  }
}




# # ==============================================================================
# # 2. GGPLOT VERSION OF plotResiduals()
# # ==============================================================================
#
# ggplot_residuals <- function(dharma_obj,
#                              form = NULL,
#                              rank = TRUE,
#                              quantreg = NULL,
#                              title = "Residuals vs Predicted",
#                              quantiles = c(0.25, 0.5, 0.75),
#                              smooth_scatter = FALSE) {
#
#   # Extract data
#   plot_data <- extract_dharma_data(dharma_obj)
#
#   # Determine predictor variable
#   if (is.null(form)) {
#     x_var <- ifelse(rank, "rank_predicted", "predicted")
#     x_label <- ifelse(rank, "Predicted (Rank Transformed)", "Predicted")
#   } else {
#     # If form is provided, use it (this would need additional handling for custom predictors)
#     x_var <- ifelse(rank, "rank_predicted", "predicted")
#     x_label <- ifelse(rank, "Predicted (Rank Transformed)", "Predicted")
#   }
#
#   # Determine if quantile regression should be used
#   if (is.null(quantreg)) {
#     quantreg <- nrow(plot_data) < 2000
#   }
#
#   # Base plot
#   if (smooth_scatter && nrow(plot_data) > 10000) {
#     # Use density-based scatter plot for large datasets
#     p <- ggplot(plot_data, aes_string(x = x_var, y = "residuals")) +
#       geom_hex(bins = 30, alpha = 0.7) +
#       scale_fill_gradient(low = "lightblue", high = "darkblue")
#   } else {
#     # Regular scatter plot
#     p <- ggplot(plot_data, aes_string(x = x_var, y = "residuals")) +
#       geom_point(aes(color = outliers), alpha = 0.6, size = 1.5) +
#       scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"),
#                          guide = "none")
#   }
#
#   # Add quantile regression lines or smooth spline
#   if (quantreg) {
#     # Add quantile regression lines
#     for (q in quantiles) {
#       p <- p + geom_smooth(method = "rq", method.args = list(tau = q),
#                            se = FALSE, color = "blue", linewidth = 0.8, alpha = 0.7)
#     }
#     # Add theoretical expectation lines (should be horizontal at quantile values)
#     for (q in quantiles) {
#       p <- p + geom_hline(yintercept = q, linetype = "dashed",
#                           color = "black", alpha = 0.6)
#     }
#   } else {
#     # Add smooth spline around mean
#     p <- p + geom_smooth(method = "gam", se = TRUE, color = "blue", alpha = 0.7)
#     # Add horizontal line at 0.5 (expected mean for uniform distribution)
#     p <- p + geom_hline(yintercept = 0.5, linetype = "dashed",
#                         color = "black", alpha = 0.6)
#   }
#
#   p <- p +
#     labs(title = title,
#          x = x_label,
#          y = "DHARMa Residuals",
#          subtitle = ifelse(quantreg,
#                            paste("Blue lines = quantile regression (", paste(quantiles, collapse = ", "), "), Black dashed = expected"),
#                            "Blue line = GAM smooth, Black dashed = expected (0.5)")) +
#     theme_minimal() +
#     theme(plot.title = element_text(size = 12, face = "bold")) +
#     ylim(0, 1)
#
#   # Perform and display tests
#   if (quantreg) {
#     tryCatch({
#       quant_test <- DHARMa::testQuantiles(dharma_obj, plot = FALSE)
#       p <- p + labs(caption = paste("Quantile test p-value:", round(quant_test$p.value, 4)))
#     }, error = function(e) {
#       # If quantile test fails, just continue without it
#     })
#   }
#
#   return(p)
# }

# # ==============================================================================
# # 3. GGPLOT VERSION OF DHARMa HISTOGRAM
# # ==============================================================================
#
# ggplot_dharma_hist <- function(dharma_obj,
#                                title = "Distribution of DHARMa Residuals",
#                                bins = 20) {
#
#   residuals <- dharma_obj$scaledResiduals
#
#   # Create histogram with theoretical uniform distribution overlay
#   p <- ggplot(data.frame(residuals = residuals), aes(x = residuals)) +
#     geom_histogram(aes(y = ..density..), bins = bins,
#                    fill = "lightblue", color = "black", alpha = 0.7) +
#     # Add theoretical uniform distribution line
#     geom_hline(yintercept = 1, color = "red", linetype = "dashed", linewidth = 1) +
#     labs(title = title,
#          subtitle = "Red dashed line = theoretical uniform distribution (density = 1)",
#          x = "DHARMa Residuals",
#          y = "Density") +
#     theme_minimal() +
#     theme(plot.title = element_text(size = 12, face = "bold")) +
#     xlim(0, 1)
#
#   # Add test result
#   unif_test <- DHARMa::testUniformity(dharma_obj, plot = FALSE)
#   p <- p + labs(caption = paste("Uniformity test p-value:", round(unif_test$p.value, 4)))
#
#   return(p)
# }

# plotResiduals_ggplot <- function(simulationOutput, form = NULL, quantreg = NULL, rank = TRUE,
#                                  asFactor = NULL, smoothScatter = NULL, quantiles = c(0.25, 0.5, 0.75),
#                                  absoluteDeviation = FALSE, ...) {
#
#   # Handle additional arguments
#   dots <- list(...)
#
#   # Set up axis labels
#   yAxis <- ifelse(absoluteDeviation == TRUE, "Residual spread [2*abs(res - 0.5)]", "DHARMa residual")
#   ylab <- checkDots("ylab", yAxis, ...)
#   xlab <- checkDots("xlab", ifelse(is.null(form), "Model predictions",
#                                    gsub(".*[$]", "", deparse(substitute(form)))), ...)
#
#   if (rank == TRUE) {
#     xlab <- paste(xlab, "(rank transformed)")
#   }
#
#   # Ensure DHARMa object and extract residuals
#   simulationOutput <- ensureDHARMa(simulationOutput, convert = TRUE)
#   res <- simulationOutput$scaledResiduals
#
#   if (absoluteDeviation == TRUE) {
#     res <- 2 * abs(res - 0.5)
#   }
#
#   # Check form argument
#   if (inherits(form, "DHARMa")) {
#     stop("DHARMa::plotResiduals > argument form cannot be of class DHARMa. Note that the syntax of plotResiduals has changed since DHARMa 0.3.0. See ?plotResiduals.")
#   }
#
#   # Get predictor values
#   pred <- ensurePredictor(simulationOutput, form)
#
#   # Handle continuous predictors
#   if (!is.factor(pred)) {
#     if (rank == TRUE) {
#       pred <- rank(pred, ties.method = "average")
#       pred <- pred / max(pred)
#     }
#
#     nuniq <- length(unique(pred))
#     ndata <- length(pred)
#
#     if (is.null(asFactor)) {
#       asFactor <- (nuniq == 1) | (nuniq < 10 & ndata/nuniq > 10)
#     }
#
#     if (asFactor) {
#       pred <- factor(pred)
#     }
#   }
#
#   # Set quantreg default
#   if (is.null(quantreg)) {
#     quantreg <- ifelse(length(res) > 2000, FALSE, TRUE)
#   }
#
#   # Set smoothScatter default
#   switchScatter <- 10000
#   if (is.null(smoothScatter)) {
#     smoothScatter <- ifelse(length(res) > switchScatter, TRUE, FALSE)
#   }
#
#   # Create data frame for plotting
#   plot_data <- data.frame(
#     predictor = pred,
#     residuals = res,
#     is_extreme = (res == 0 | res == 1)
#   )
#
#   # Set up main title
#   main_title <- ifelse("main" %in% names(dots), dots$main,
#                        ifelse(is.null(form), paste(yAxis, "vs. predicted"),
#                               paste(yAxis, "Residual vs. predictor")))
#
#   # Create the base plot
#   if (is.factor(pred)) {
#     # Categorical predictor - boxplot style
#     p <- ggplot(plot_data, aes(x = predictor, y = residuals)) +
#       geom_boxplot(alpha = 0.7, outlier.shape = NA) +
#       geom_jitter(aes(color = is_extreme), width = 0.2, alpha = 0.6) +
#       scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"), guide = "none") +
#       scale_y_continuous(limits = c(0, 1), breaks = c(0, quantiles, 1)) +
#       labs(x = xlab, y = ylab, title = main_title) +
#       theme_minimal() +
#       theme(
#         panel.grid.minor = element_blank(),
#         plot.title = element_text(size = 10)
#       )
#
#     # Add quantile lines
#     for (q in quantiles) {
#       p <- p + geom_hline(yintercept = q, linetype = "dashed", alpha = 0.5)
#     }
#
#     # Note: testCategorical would need to be implemented separately
#     # For now, we'll just return NULL as the original function does for continuous predictors
#     out <- NULL
#
#   } else {
#     # Continuous predictor
#     alpha_val <- max(0.1, 1 - 3 * length(res) / switchScatter)
#
#     if (smoothScatter == TRUE) {
#       # Use density-based coloring for many points
#       p <- ggplot(plot_data, aes(x = predictor, y = residuals)) +
#         stat_density_2d_filled(alpha = 0.6, show.legend = FALSE) +
#         scale_fill_grey(start = 1, end = 0.3) +
#         geom_point(data = subset(plot_data, is_extreme),
#                    aes(color = is_extreme), size = 0.5) +
#         scale_color_manual(values = c("TRUE" = "red"), guide = "none") +
#         scale_y_continuous(limits = c(0, 1), breaks = c(0, quantiles, 1)) +
#         labs(x = xlab, y = ylab, title = main_title) +
#         theme_minimal() +
#         theme(
#           panel.grid.minor = element_blank(),
#           plot.title = element_text(size = 10)
#         )
#     } else {
#       # Regular scatter plot
#       p <- ggplot(plot_data, aes(x = predictor, y = residuals)) +
#         geom_point(aes(color = is_extreme, shape = is_extreme),
#                    alpha = alpha_val, size = 1) +
#         scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"), guide = "none") +
#         scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 8), guide = "none") +
#         scale_y_continuous(limits = c(0, 1), breaks = c(0, quantiles, 1)) +
#         labs(x = xlab, y = ylab, title = main_title) +
#         theme_minimal() +
#         theme(
#           panel.grid.minor = element_blank(),
#           plot.title = element_text(size = 10)
#         )
#     }
#
#     # Add quantile lines and smoothers
#     for (q in quantiles) {
#       p <- p + geom_hline(yintercept = q, linetype = "dashed", alpha = 0.5)
#     }
#
#     out <- NULL
#
#     if (quantreg == FALSE) {
#       # Add smooth spline and median line
#       p <- p +
#         geom_smooth(method = "loess", se = FALSE, color = "red", linewidth = 1) +
#         geom_hline(yintercept = 0.5, color = "red", linewidth = 1)
#
#     } else {
#       # Perform quantile regression tests
#       # Note: This would require the testQuantiles function from DHARMa
#       # For now, we'll create a placeholder that assumes no significant deviations
#       out <- list(
#         p.value = 0.5,  # placeholder
#         pvals = rep(0.5, length(quantiles)),  # placeholder
#         predictions = data.frame(
#           pred = seq(min(pred), max(pred), length.out = 50)
#         )
#       )
#       # Add placeholder quantile predictions
#       for (i in 1:length(quantiles)) {
#         out$predictions[, 2*i] <- quantiles[i]  # quantile line
#         out$predictions[, 2*i + 1] <- 0.05      # confidence interval width
#       }
#
#       if (is.na(out$p.value)) {
#         main_title <- paste(main_title, "Some quantile regressions failed", sep = "\n")
#         title_color <- "red"
#       } else {
#         if (any(out$pvals < 0.05, na.rm = TRUE)) {
#           main_title <- paste(main_title, "Quantile deviations detected (red curves)", sep = "\n")
#           if (out$p.value <= 0.05) {
#             main_title <- paste(main_title, "Combined adjusted quantile test significant", sep = "\n")
#           } else {
#             main_title <- paste(main_title, "Combined adjusted quantile test n.s.", sep = "\n")
#           }
#           title_color <- "red"
#         } else {
#           main_title <- paste(main_title, "No significant problems detected", sep = "\n")
#           title_color <- "black"
#         }
#       }
#
#       # Update plot with new title
#       p <- p + labs(title = main_title) +
#         theme(plot.title = element_text(color = title_color, size = 9))
#
#       # Add quantile regression lines and confidence bands
#       for (i in 1:length(quantiles)) {
#         line_color <- ifelse(out$pvals[i] <= 0.05 & !is.na(out$pvals[i]), "red", "black")
#         fill_color <- ifelse(out$pvals[i] <= 0.05 & !is.na(out$pvals[i]),
#                              alpha("red", 0.25), alpha("black", 0.125))
#
#         # Create ribbon data for confidence bands
#         ribbon_data <- data.frame(
#           x = out$predictions$pred,
#           y = out$predictions[, 2*i],
#           ymin = out$predictions[, 2*i] - out$predictions[, 2*i + 1],
#           ymax = out$predictions[, 2*i] + out$predictions[, 2*i + 1]
#         )
#
#         p <- p +
#           geom_ribbon(data = ribbon_data,
#                       aes(x = x, ymin = ymin, ymax = ymax),
#                       fill = fill_color, alpha = 0.5, inherit.aes = FALSE) +
#           geom_line(data = ribbon_data,
#                     aes(x = x, y = y),
#                     color = line_color, linewidth = 1, inherit.aes = FALSE)
#       }
#     }
#   }
#
#   # Apply x-axis limits if rank transformation was used
#   if (rank == TRUE && !is.factor(pred)) {
#     xlim <- ifelse("xlim" %in% names(dots), list(dots$xlim), list(c(0, 1)))[[1]]
#     p <- p + coord_cartesian(xlim = xlim)
#   }
#
#   # Return the plot and any test results
#   print(p)
#   invisible(out)
# }
#
