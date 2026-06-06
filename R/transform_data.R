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
#' @param random_slope_variable Variable for random slopes (typically one of "condition_column" or "covariate" and assuming that they are numeric variables). A random slope term is added for each of the variables specified in the experimental columns in addition to their corresponding random intercept terms. The random slope and intercept terms for each experimental_columns variable are assumed to be uncorrelated.
#' @param covariate_is_categorical Specify whether the covariate variable is categorical. TRUE: Categorical, FALSE: Continuous.
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
#' 4) diagnostic_plots: is a list with two elements plots and captions. plots is a named list and captions is a character vector,
#' both of the same length as the number of models evaluated. Each element of the plots list is yet another
#' list of QC/diagnostic plots related to the corresponding model fit, while the captions is a vector of captions for each of the
#' QC plots output
#' 5) cooks_plots: is a list of plots is named list of the same length as the number of models evaluated. Each element of the cooks_plot is another list of bar plots of cooks distance plots for each of the experimental columns. The title of the plot indicate the model and experimental factor while the subtitle indicates the identified outliers if any using the 4/n threshold
#'
#' @keywords internal
#' @noRd

transform_data<-function(data, condition_column, experimental_columns, response_column, total_column=NULL, condition_is_categorical=TRUE, covariate=NULL,
                         crossed_columns = NULL, error_is_non_normal=FALSE, family_p=NULL, alpha=0.05, na.action="complete", include_interaction = NA, random_slope_variable = NULL, covariate_is_categorical = NA){



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

  choose_cols <- vector(mode = "character")
  temp_count <- 0
  for(c in 1:length(lms[[3]])) {
    if(length(unique(lms[[2]][[lms[[3]][c]]])) > 2) {
      temp_count <- temp_count + 1
      choose_cols[temp_count] <- lms[[3]][c]
    }
  }

  if(temp_count > 0) {
    cooks_result[[model_no]]=cooks_test(lms[[1]], family_p, lms[[2]], choose_cols, response_column="response_column", hist_text="raw")
    names(cooks_result[[model_no]]) <- paste0(names(cooks_result[[model_no]]), "_raw")

  }
  else {
    message("Not enough grouping levels to perform the cook analyses on the experimental factors")
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


    if(length(choose_cols) > 0) {
      cooks_result[[model_no]]=cooks_test(lms[[1]], family_p, lms[[2]], choose_cols, response_column="response_column", hist_text="raw wo outliers")
      names(cooks_result[[model_no]]) <- paste0(names(cooks_result[[model_no]]), "_raw_wo_outliers")

    }
    else {
      message("Not enough grouping levels to perform the cook analyses on the experimental factors")
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


    if(length(choose_cols) > 0) {
      cooks_result[[model_no]]=cooks_test(lms[[1]], family_p, lms[[2]], choose_cols, response_column="response_column", hist_text="log transform")
      names(cooks_result[[model_no]]) <- paste0(names(cooks_result[[model_no]]), "_logTransformed")
    }
    else {
      message("Not enough grouping levels to perform the cook analyses on the experimental factors")
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


      if(length(choose_cols) > 0) {
        cooks_result[[model_no]]=cooks_test(lms[[1]], family_p, lms[[2]], choose_cols, response_column="response_column", hist_text="log transform wo outliers")
        names(cooks_result[[model_no]]) <- paste0(names(cooks_result[[model_no]]), "_logTransformed_wo_outliers")
      }
      else {
        message("Not enough grouping levels to perform the cook analyses on the experimental factors")
        return()
      }


    }


  }

  cooks_plots_plus_outliers <- generate_cooks_results_plots(cooks_result, names(plots_info), experimental_columns)

  result=list(models, plots_info, cooks_plots_plus_outliers$cooks_plots, cooks_plots_plus_outliers$inferred_outliers, cooks_result, Data_updated )
  names(result)=c("models", "diagnostic_plots", "cooks_plots", "inferred_outlier_groups","cooks_result", "Data_updated")

  return(result)
}

#' @keywords internal
#' @noRd
generate_cooks_results_plots <- function(cooks_result, models, experimental_columns) {
  cooks_plots <- list()
  inferred_outliers <- list()
  for(i in 1:length(models)) {
    cooks_plots[[i]] <- list()
    inferred_outliers[[i]] <- list()
    names(cooks_plots)[i] <- models[i]
    names(inferred_outliers)[i] <- models[i]
    #
    for(j in 1:length(cooks_result[[i]])){
      plot_title <- paste0("Cooks distance estimates for model ", models[i], " and experimental factor = ", experimental_columns[j])
      temp_data <- data.frame(cooks_distance = cooks_result[[i]][[j]])
      temp_data %<>% tibble::rownames_to_column("exp.factor")
      outliers <- temp_data %>% filter(cooks_distance > 4/nrow(.)) %>% .$exp.factor
      if(length(outliers) == 0)
        outliers <- "none"
      cooks_plots[[i]][[j]] <- ggplot(temp_data, aes(y=exp.factor, x=cooks_distance)) +
        geom_col() +
        theme_minimal() +
        geom_vline(xintercept = 4/nrow(temp_data), lty=2) +
        theme(axis.text.y = element_text(size = 8)) +
        ggtitle(plot_title) +
        labs(subtitle = paste0("Outliers are: ", paste(outliers, collapse = ","), " determined by the 4/n threshold as indicated by the vertical dashed line")) +
        theme(plot.title  = element_textbox_simple()) +
        theme(plot.subtitle  = element_textbox_simple())
      inferred_outliers[[i]][[j]] <- outliers
      names(inferred_outliers[[i]])[j] <- experimental_columns[j]
     }
  }

  return(list(cooks_plots=cooks_plots, inferred_outliers=inferred_outliers))
}

#' @keywords internal
#' @noRd
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
      labs(title = paste0("Q-Q Plot: Residuals", description_suffix),
           subtitle = paste0("Check normality of residuals", ". Expectation is that the black points lie on or close to the solid red diagonal line"),
           x = "Theoretical Quantiles",
           y = "Sample Quantiles") +
      theme_minimal() +
      theme(plot.title = element_text(size = 12, face = "bold")) +
      theme(plot.subtitle  = element_textbox_simple())

    names(plots)[plot_index+1] <- "residuals_QQ"

    captions[plot_index+1] <- paste0("Check normality of residuals ", description_suffix, ". Expectation is that the black points lie on or close to the solid red diagonal line. Check the Normal Q-Q plots for good and bad examples here: https://library.virginia.edu/data/articles/diagnostic-plots ")



    # 1a. Residuals vs Fitted Values
    plots[[plot_index+2]] <- ggplot(model_data, aes(x = fitted, y = model_residuals)) +
      geom_point(alpha = 0.6, size = 2) +
      geom_smooth(method = "loess", se = TRUE, color = "red") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
      labs(title = paste0("Residuals vs Fitted Values ", description_suffix),
           subtitle = paste0("Check for linearity", ". Expectation is that the best fit solid red line is horizontal or close to being horizontal"),
           x = "Fitted Values",
           y = "Residuals") +
      theme_minimal() +
      theme(plot.title = element_text(size = 12, face = "bold"))+
      theme(plot.subtitle  = element_textbox_simple())

    names(plots)[plot_index+2] <- "residuals_vs_fitted"
    captions[plot_index+2] <- paste0("Check for linearity ", description_suffix, ". Expectation is that the best fit solid red line is horizontal or close to being horizontal. Check the Residuals vs Fitted plots for good and bad examples here: https://library.virginia.edu/data/articles/diagnostic-plots")

    # 1b. Scale-Location Plot (Square root of standardized residuals vs fitted)
    plots[[plot_index+3]] <- ggplot(model_data, aes(x = fitted, y = sqrt_abs_residuals)) +
      geom_point(alpha = 0.6, size = 2) +
      geom_smooth(method = "loess", se = TRUE, color = "red") +
      labs(title = paste0("Scale-Location Plot ", description_suffix),
           subtitle = paste0("Check for homoscedasticity", ". Expectation is that the best fit solid red line is horizontal or close to being so"),
           x = "Fitted Values",
           y = "\u221a|Standardized Residuals|") +
      theme_minimal() +
      theme(plot.title = element_text(size = 12, face = "bold"))+
      theme(plot.subtitle  = element_textbox_simple())

    names(plots)[plot_index+3] <- "residuals_homoscedasticity"
    captions[plot_index+3] <- paste0("Check for homoscedasticity ",description_suffix, ". Expectation is that the best fit solid red line is horizontal or close to being so. Check the Scale-Location plots for good and bad examples here: https://library.virginia.edu/data/articles/diagnostic-plots")
    plot_index <- plot_index + 3

  }else{
    simulationOutput <- DHARMa::simulateResiduals(lms[[1]], simulateREs = "user-specified", plot = F)
    residual = residuals(simulationOutput, quantileFunction = qnorm)
    # plot(simulationOutput)
    # DHARMa::plotResiduals(simulationOutput, form =  lms[[2]]$condition_column)

    plots[[plot_index + 1]] <- ggplot_QQunif(simulationOutput, description_suffix = description_suffix)
    names(plots)[plot_index+1] <- "residuals_QQ"
    captions[plot_index + 1] <- paste0("Q-Q Plot for Uniform Distribution", ". Expectation is that the black points lie on or close to the solid red diagonal line. Check the Q-Q plots for good and bad examples here: https://library.virginia.edu/data/articles/diagnostic-plots ")

    plots[[plot_index + 2]] <- ggplot_residuals_vs_predictor(simulationOutput, description_suffix = description_suffix)
    names(plots)[plot_index+2] <- "residuals_vs_predicted"
    captions[plot_index + 2] <- paste0("Residuals vs Predicted", " Check the Residuals vs Fitted plots for good and bad examples here: https://library.virginia.edu/data/articles/diagnostic-plots ")

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
        labs(title = paste0("Q-Q Plot: Random effects for ", exp_factor),
             subtitle = paste0(description_suffix, ": Check normality of random effects for ", names(random_effects)[c], "_", colnames(random_effects[[c]][j]), " ", ": ", exp_factor, ". Expectation is that the black points lie on or close to the solid red diagonal line"),
             x = "Theoretical Quantiles",
             y = "Sample Quantiles") +
        theme_minimal() +
        theme(plot.title = element_text(size = 12, face = "bold"))+
        theme(plot.subtitle  = element_textbox_simple())

      names(plots)[plot_index+1] <- paste0("random_effects_QQ_",temp_count)
      captions[plot_index+1] <- paste0("Check normality of random effects for ", names(random_effects)[c], "_", colnames(random_effects[[c]][j]), " ", description_suffix, ": ", exp_factor, ". Expectation is that the black points lie on or close to the solid red diagonal line. Check the Normal Q-Q plots for good and bad examples here: https://library.virginia.edu/data/articles/diagnostic-plots ")

      plot_index <- plot_index + 1
      temp_count <- temp_count + 1
    }
  }

  cutoffs=rosner_test(trait=residual, response_column=response_column, alpha=alpha, hist_text = "raw residual")
  if(sum(is.na(cutoffs))<2 | sum(is.infinite(residual)) > 0){
    residuals_df <- data.frame(residual)
    plots[[plot_index+1]] <- ggplot(residuals_df, aes(x=residual)) +
      geom_histogram() +
      labs(title = "Histogram of Residuals Values",
           subtitle = "Histogram of residuals values. Red vertical lines (if present) are the location of the cutoffs to identify the outliers. No red vertical lines implies that the residual values that were estimated to be infinite were identified as outliers",
           x = "Residuals") +
      theme_minimal() +
      theme(plot.title = element_text(size = 12, face = "bold"))+
      theme(plot.subtitle  = element_textbox_simple())

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

#' @keywords internal
#' @noRd
rosner_test<- function (trait, response_column, alpha, hist_text) {

  cutoff1=NA
  cutoff2=NA


  upper_bound <- median(trait) + 3 * mad(trait)
  upper_bound

  lower_bound <- median(trait) - 3 * mad(trait)
  lower_bound

  outlierC=sum(trait>upper_bound)+sum(trait<lower_bound)
  outlierC

  if( outlierC >0 ){

    ###rosner's test
    suppressWarnings(
    test <- EnvStats::rosnerTest(trait,
                                 k = outlierC, alpha=alpha)
    )
    outliers=test$all.stats$Value[test$all.stats$Outlier]

    if(length(test$all.stats$Outlier) !=0  ){
      if(! ( length(test$all.stats$Outlier) ==1 &  sum(is.infinite(test$all.stats$Outlier)) ) ) {


        if(sum(outliers>median(trait)) >0) cutoff1=min( outliers[which(outliers>median(trait))] )
        if(sum(outliers<median(trait)) >0) cutoff2=max( outliers[which(outliers<median(trait))] )




        ############rosner's test end


      }



    }else{
      message("No outlier detected from the raw Data")
    }


  }else{
    message("No outlier detected from the raw Data")
  }

  return(c(cutoff2,cutoff1))
}

#' @keywords internal
#' @noRd
cooks_test<- function (model, family_p, fixed_global_variable_data, experimental_columns, response_column, hist_text) {

  n_cols_2_test <- length(experimental_columns)
  perform_test <- TRUE
  ##influence takes a very long time for testing influential observations higher than the first level
  if("glmerMod" %in% class(model)) {
    if(summary(model)$family == "binomial") {
      n_cols_2_test <- 1
      if("experimental_column1" %in% experimental_columns)
        experimental_columns = "experimental_column1"
      else{
        message("Not enough levels to perform cooks test for experimental_column1 with the assumed binomial distribution")
        cooks_result <- NA
        perform_test <- FALSE
      }
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
                              alt.est <- influence2(model, family_p, group=experimental_columns[i], select=level2test)
                              cd=rbind(cd, influence.ME::cooks.distance.estex(alt.est))
                            }
                            rownames(cd)=levels
                            cd

                          }else{
                            alt.est <- influence2(model, family_p, group=experimental_columns[i] )
                            influence.ME::cooks.distance.estex(alt.est)
                          }


                        }
    )


    names(cooks_result)=c(paste0("cooks_distance_",experimental_columns[1:n_cols_2_test]) )


  }

  return(cooks_result)

}

# Function to extract DHARMa residuals and create ggplot-ready data
#' @keywords internal
#' @noRd
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

#' @keywords internal
#' @noRd
ggplot_QQunif <- function(dharma_obj,
                          title = "Q-Q Plot vs. Uniform Distribution",
                          test_uniformity = FALSE,
                          test_dispersion = FALSE,
                          test_outliers = FALSE,
                          description_suffix = "") {

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
    labs(title = paste(title, description_suffix),
         subtitle = paste0("Q-Q Plot for Uniform Distribution", ". Expectation is that the black points lie on or close to the solid red diagonal line."),
         x = "Expected (Uniform)",
         y = "Observed Residuals") +
    theme_minimal() +
    theme(plot.title = element_text(size = 12, face = "bold")) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    theme(plot.subtitle  = element_textbox_simple())

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
#' @keywords internal
#' @noRd
ggplot_residuals_vs_predictor <- function(dharma_obj,
                                          predictor = NULL,
                                          predictor_name = "predictions",   #or "predictions"
                                          condition_is_categorical = TRUE,
                                          rank = TRUE,
                                          quantreg = TRUE,
                                          description_suffix = "") {

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

  if(is.factor(plot_data$predictor) | (length(unique(plot_data$predictor)) < 10) & (length(plot_data$predictor) >= 10)){
    plot_data$predictor <- as.factor(plot_data$predictor)
    p <- ggplot(plot_data, aes(x = predictor, y = residuals)) +
      geom_boxplot() +
      theme_minimal() +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", alpha = 0.6) +
      labs(title = paste("DHARMa Residuals vs", predictor_name, description_suffix),
           subtitle = paste0("Residuals vs Predicted", ". Expectation is that the median of the boxplots are at or close to 0.5"),
           x = x_label,
           y = "DHARMa Residuals")
  }
  else{
    p <- ggplot(plot_data, aes(x = predictor, y = residuals)) +
      geom_point(color = "black", alpha = 0.6, size = 1.5) +
      theme_minimal()

    if (quantreg && nrow(plot_data) > 10) {
      # Add quantile regression lines
      quantiles <- c(0.25, 0.5, 0.75)
      for (q in quantiles) {
        p <- p + geom_smooth(formula = y ~ poly(x,2), method = quantreg::rq, method.args = list(tau = q),
                             se = FALSE, color = "red", linewidth = 0.8, alpha = 0.7)
      }
      # Add expected horizontal lines
      for (q in quantiles) {
        p <- p + geom_hline(yintercept = q, linetype = "dashed",
                            color = "black", alpha = 0.6)
      }

      p <- p + labs(title = paste("DHARMa Residuals vs", predictor_name, description_suffix),
             subtitle = paste0("Residuals vs Predicted", ". Expectation is that the best fit red lines at the three quartiles- 0.25, 0.50 and 0.75 - are close to the dashed horizontal lines at their respective quartiles"),
             x = x_label,
             y = "DHARMa Residuals")
    } else {
      p <- p + geom_smooth(method = "lm", se = TRUE, color = "red", alpha = 0.7) +
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", alpha = 0.6) +
        labs(title = paste("DHARMa Residuals vs", predictor_name, description_suffix),
             subtitle = paste0("Residuals vs Predicted", ". Expectation is that the best fit solid red line(s) is horizontal or close to being so"),
             x = x_label,
             y = "DHARMa Residuals")
  }
}
  p <- p +
    theme_minimal() +
    theme(plot.title = element_text(size = 12, face = "bold")) +
    ylim(0, 1) +
    theme(plot.subtitle  = element_textbox_simple())

  return(p)
}

#' @keywords internal
#' @noRd
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
#' @keywords internal
#' @noRd
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
#' @keywords internal
#' @noRd
checkDots <- function(name, default, ...) {
  dots <- list(...)
  if (name %in% names(dots)) {
    return(dots[[name]])
  } else {
    return(default)
  }
}


## adapted from influence.ME to work with binomial glmer and avoid the need to declare global variables
## https://github.com/cran/influence.ME
#' @keywords internal
#' @noRd
influence2 <-
  function(model, family_p, group=NULL, select=NULL, obs=FALSE, gf="single", count = FALSE, delete=TRUE, ...)
  {

    fixef <- NA
    rm(fixef)

    ## Checks, errors, and warnings
    # obs=TRUE cannot be used with delete=FALSE, group, gf,  parameters

    if(is.null(group) & !obs)
    {
      stop("Please specify either the 'group' parameter, or specify 'obs=TRUE'")
    }

    if(!is.null(group) & obs)
    {
      stop("Either specify the 'group' parameter, or specify 'obs=TRUE', but not both.")
    }


    # Defining Internal Variables
    # Thanks to Kevin Darras for suggestion on how to extend functionality to binomial models and functions inside the model call

    ifelse(as.character(model@call)[3]=="data.update",
           data.adapted <- model.frame(model),
           data.adapted <- get(as.character(model@call)[3]))


    if("glmerMod" %in% class(model)) {
      if(summary(model)$family == "binomial") {
        success_failures <- data.adapted[,1]
        data.adapted[,1] <- success_failures[,1]
        colnames(data.adapted)[1] <- "response_column"
        data.adapted[,(ncol(data.adapted)+1)] <- success_failures[,1] + success_failures[,2]
        colnames(data.adapted)[ncol(data.adapted)] <- "total_column"
      }
      if((grepl("Negative",summary(model)$family) | grepl("poisson",summary(model)$family)) & any(grepl("offset", colnames(data.adapted)))) {
          offset_col <- grep("offset", colnames(data.adapted))
          data.adapted[, offset_col] <- exp(data.adapted[, offset_col])
          colnames(data.adapted)[offset_col] <- "total_column"
      }
      if(!grepl("Negative",summary(model)$family))
        family_p=switch(family_p,
                        "poisson" = poisson(link="log"),
                        "binomial" = binomial(link="logit"),
                        "bionomial_log" = binomial(link="log"),
                        "Gamma_log" = Gamma(link = "log"),
                        "Gamma" = Gamma(link = "inverse"))



    }


    original.no.estex <- which(substr(names(fixef(model)), 1,6) != "estex.")
    n.pred <- length(fixef(model)[original.no.estex])


    ####
    # Code kindly provided by Jennifer Bufford
    if("(weights)" %in% names(data.adapted)) {
      names(data.adapted)[names(data.adapted)=="(weights)"] <-
        as.character(model@call$weights)}
    if("(offset)" %in% names(data.adapted)) {
      names(data.adapted)[names(data.adapted)=="(offset)"] <-
        as.character(model@call$offset)}
    if(sum(grepl("offset", names(data.adapted)))>0) {
      names(data.adapted)[grep("offset", names(data.adapted))] <-
        gsub('offset\\(|\\)',"",names(data.adapted)[grep("offset", names(data.adapted))])}
    ####



    if(!obs)
    {
      grouping.names <- influence.ME::grouping.levels(model, group)
      n.groups <- length(grouping.names)
    }

    if(obs)
    {
      n.obs <- nrow(data.adapted)
    }

    ###
    # Defining and naming the output elements for the original models
    ###
    # These apply to all
    ###

    # Fixed Estimates of the original model
    or.fixed <- matrix(ncol = n.pred , nrow = 1, data = fixef(model)[original.no.estex])
    dimnames(or.fixed) <- list(NULL, names(fixef(model))[original.no.estex])

    # Standard Error of the original model
    or.se <- matrix(ncol = n.pred , nrow = 1, data = influence.ME::se.fixef(model)[original.no.estex])
    dimnames(or.se) <- list(NULL, names(fixef(model))[original.no.estex])

    # Variance / Covariance Matrix of the original model
    or.vcov <- as.matrix(vcov(model)[original.no.estex, original.no.estex])
    dimnames(or.vcov) <- list(
      names(fixef(model)[original.no.estex]),
      names(fixef(model)[original.no.estex]))

    # Test statistic of the original model
    or.test <- coef(summary(model))[original.no.estex,3]


    ###
    # Defining and naming the output elements for the adapted models
    ###
    # These procedures vary
    ###


    if(!obs)
    {

      if(is.null(select))
      {

        # Fixed Estimates of the modified model(s)
        alt.fixed <- matrix(ncol = n.pred, nrow = n.groups, data = NA)
        dimnames(alt.fixed) <- list(grouping.names, names(fixef(model))[original.no.estex])

        # Standard Error of the modified model(s)
        alt.se <- matrix(ncol = n.pred , nrow = n.groups, data = NA)
        dimnames(alt.se) <- list(grouping.names, names(fixef(model))[original.no.estex])

        # Variance / Covariance Matrix of the modified model(s)
        alt.vcov <- list()

        # Test statistic of the modified model(s)
        alt.test <- matrix(ncol = n.pred , nrow = n.groups, data = NA)
        dimnames(alt.test) <- list(grouping.names, names(fixef(model))[original.no.estex])

        for (i in 1:n.groups)
        {

          if(count == TRUE) {print(n.groups + 1 - i)}

          model.updated <- exclude.influence2(model=model, family_p, grouping=group, level=grouping.names[i], gf=gf, delete=delete)

          altered.no.estex <- which(substr(names(fixef(model.updated)), 1,6) != "estex.")

          alt.fixed[i,] 	<- as.matrix(fixef(model.updated)[altered.no.estex])
          alt.se[i,] 		<- as.matrix(influence.ME::se.fixef(model.updated)[altered.no.estex])
          alt.vcov[[i]] 	<- as.matrix(vcov(model.updated)[altered.no.estex, altered.no.estex])
          alt.test[i,] 		<- as.matrix(coef(summary(model.updated))[,3][altered.no.estex])
        }



      }

      if(!is.null(select))
      {

        model.updated <- exclude.influence2(model, family_p, group, select, gf=gf, delete=delete)
        altered.no.estex <- which(substr(names(fixef(model.updated)), 1,6) != "estex.")

        # Fixed Estimates of the modified model(s)
        alt.fixed <- matrix(ncol = n.pred, nrow = 1, data = fixef(model.updated)[altered.no.estex])
        dimnames(alt.fixed) <- list(
          "Altered model",
          names(fixef(model.updated))[altered.no.estex])

        # Standard Error of the modified model(s)
        alt.se <- matrix(ncol = n.pred , nrow = 1, data = influence.ME::se.fixef(model.updated)[altered.no.estex])
        dimnames(alt.se) <- list("Altered model", names(fixef(model.updated))[altered.no.estex])

        # Variance / Covariance Matrix of the modified model(s)
        alt.vcov <- list()
        alt.vcov[[1]] <- as.matrix(vcov(model.updated)[altered.no.estex, altered.no.estex])
        dimnames(alt.vcov[[1]]) <- list(
          names(fixef(model.updated)[altered.no.estex]),
          names(fixef(model.updated)[altered.no.estex]))

        # Test statistic of the modified model(s)
        alt.test <- matrix(ncol = n.pred , nrow = 1, data = coef(summary(model.updated))[,3][altered.no.estex])
        dimnames(alt.test) <- list("Altered model", names(fixef(model.updated))[altered.no.estex])

      }
    }


    if(obs)
    {

      if(is.null(select))
      {
        # Fixed Estimates of the modified model(s)
        alt.fixed <- matrix(ncol = n.pred, nrow = n.obs, data = NA)
        dimnames(alt.fixed) <- list(1:n.obs, names(fixef(model))[original.no.estex])

        # Standard Error of the modified model(s)
        alt.se <- matrix(ncol = n.pred , nrow = n.obs, data = NA)
        dimnames(alt.se) <- list(1:n.obs, names(fixef(model))[original.no.estex])

        # Variance / Covariance Matrix of the modified model(s)
        alt.vcov <- list()

        # Test statistic of the modified model(s)
        alt.test <- matrix(ncol = n.pred , nrow = n.obs, data = NA)
        dimnames(alt.test) <- list(1:n.obs, names(fixef(model))[original.no.estex])

        for (i in 1:n.obs)
        {

          if(count == TRUE) {print(n.obs + 1 - i)}

          model.updated <- exclude.influence2(model, family_p, obs=i)
          altered.no.estex <- which(substr(names(fixef(model.updated)), 1,6) != "estex.")

          alt.fixed[i,] 	<- as.matrix(fixef(model.updated)[altered.no.estex])
          alt.se[i,] 		<- as.matrix(influence.ME::se.fixef(model.updated)[altered.no.estex])
          alt.vcov[[i]] 	<- as.matrix(vcov(model.updated)[altered.no.estex, altered.no.estex])
          alt.test[i,]	 	<- as.matrix(coef(summary(model.updated))[,3][altered.no.estex])
        }


      }

      if(!is.null(select))
      {
        model.updated <- exclude.influence2(model, family_p, obs=select)
        altered.no.estex <- which(substr(names(fixef(model.updated)), 1,6) != "estex.")

        # Fixed Estimates of the modified model(s)
        alt.fixed <- matrix(ncol = n.pred, nrow = 1, data = fixef(model.updated)[altered.no.estex])
        dimnames(alt.fixed) <- list(
          "Altered model",
          names(fixef(model.updated))[altered.no.estex])

        # Standard Error of the modified model(s)
        alt.se <- matrix(ncol = n.pred , nrow = 1, data = influence.ME::se.fixef(model.updated)[altered.no.estex])
        dimnames(alt.se) <- list("Altered model", names(fixef(model.updated))[altered.no.estex])

        # Variance / Covariance Matrix of the modified model(s)
        alt.vcov <- list()
        alt.vcov[[1]] <- as.matrix(vcov(model.updated)[altered.no.estex, altered.no.estex])
        dimnames(alt.vcov[[1]]) <- list(
          names(fixef(model.updated)[altered.no.estex]),
          names(fixef(model.updated)[altered.no.estex]))

        # Test statistic of the modified model(s)
        alt.test <- matrix(ncol = n.pred , nrow = 1, data = coef(summary(model.updated))[,3][altered.no.estex])
        dimnames(alt.test) <- list("Altered model", names(fixef(model.updated))[altered.no.estex])

      }



    }


    estex <- list(
      or.fixed = or.fixed,
      or.se = or.se,
      or.vcov = or.vcov,
      or.test = or.test,
      alt.fixed = alt.fixed,
      alt.se = alt.se,
      alt.vcov = alt.vcov,
      alt.test = alt.test)

    class(estex) <- "estex"
    return(estex)

  }

## adapted from influence.ME to work with binomial glmer and avoid the need to declare global variables
## https://github.com/cran/influence.ME
#' @keywords internal
#' @noRd
exclude.influence2 <-
  function(model, family_p, grouping=NULL, level=NULL, obs=NULL, gf="single", delete=TRUE)
  {

    # Thanks to Kevin Darras for suggestion on how to extend functionality to binomial models and functions inside the model call

    ifelse(as.character(model@call)[3]=="data.update",
           data.adapted <- model.frame(model),
           data.adapted <- get(as.character(model@call)[3]))

    if("glmerMod" %in% class(model)) {
      if(summary(model)$family == "binomial") {
        success_failures <- data.adapted[,1]
        data.adapted[,1] <- success_failures[,1]
        colnames(data.adapted)[1] <- "response_column"
        data.adapted[,(ncol(data.adapted)+1)] <- success_failures[,1] + success_failures[,2]
        colnames(data.adapted)[ncol(data.adapted)] <- "total_column"
      }
      if((grepl("Negative",summary(model)$family) | grepl("poisson",summary(model)$family)) & any(grepl("offset", colnames(data.adapted)))) {
        offset_col <- grep("offset", colnames(data.adapted))
        data.adapted[, offset_col] <- exp(data.adapted[, offset_col])
        colnames(data.adapted)[offset_col] <- "total_column"
      }


    }

    added.variables <- character()
    ranef <- NA
    rm(ranef)

    ####
    # Code kindly provided by Jennifer Bufford
    if("(weights)" %in% names(data.adapted)) {
      names(data.adapted)[names(data.adapted)=="(weights)"] <-
        as.character(model@call$weights)}
    if("(offset)" %in% names(data.adapted)) {
      names(data.adapted)[names(data.adapted)=="(offset)"] <-
        as.character(model@call$offset)}
    if(sum(grepl("offset", names(data.adapted)))>0) {
      names(data.adapted)[grep("offset", names(data.adapted))] <-
        gsub('offset\\(|\\)',"",names(data.adapted)[grep("offset", names(data.adapted))])}
    ####


    if(!is.null(obs))
    {

      if(!is.null(grouping) | !is.null(level))
      {
        warning("Specification of the 'obs' parameter overrules specification of the 'grouping' and 'level' parameters.")
      }

      data.adapted <- data.adapted[-obs,]
      # For some reason, update() can not find the data.adapted within the function without the line below.
      data.update <- data.adapted
      # It can find data.update

      model.updated <- update(model, data=data.update)
      return(model.updated)
    }






    if(delete==TRUE)
    {

      ## Only works when length(level) == 1, this needs to be enhanced
      group.var <- which(names(data.adapted) == grouping)

      for (i in 1:length(level))
      {
        data.adapted <- subset(data.adapted, data.adapted[,group.var]!=level[i])
      }

      # For some reason, update() can not find the data.adapted within the function without the line below.
      data.update <- data.adapted
      # It can find data.update

      model.updated <- update(model, data=data.update)

      return(model.updated)

    }






    if(names(data.adapted)[2] != "intercept.alt")
    {

      data.adapted$intercept.alt <- ifelse(model@flist[[grouping]]==level[1], 0, 1)

      data.adapted[, ncol(data.adapted)+1] <-
        ifelse(model@flist[[grouping]]==level[1], 1, 0)

      added.variables <- make.names(paste("estex.", as.character(level[1]), sep=""))
      colnames(data.adapted)[ncol(data.adapted)] <- added.variables


      if(length(level) > 1)
      {
        for (i in 2:length(level))
        {

          data.adapted$intercept.alt[model@flist[[grouping]]==level[i]] <- 0

          data.adapted[, ncol(data.adapted)+1] <-
            ifelse(model@flist[[grouping]]==level[i], 1, 0)

          added.variables <- append(added.variables, values = make.names(paste("estex.", as.character(level[i]), sep="")))

          colnames(data.adapted)[ncol(data.adapted)] <- added.variables[length(added.variables)]

        }
      }

      if(gf=="single")
      {
        # grnr refers to "grouping number"
        grnr <- which(names(ranef(model))==grouping)

        if (length(names(ranef(model)[[grnr]])) == 1)
        {
          model.updated <- update(model,
                                  formula = as.formula(paste(". ~ 0 + intercept.alt +",
                                                             paste(added.variables, collapse="+"),
                                                             "+ .",
                                                             "- (1 |", grouping, ") + (0 + intercept.alt |", grouping, ")")),
                                  data = data.adapted)

        }

        if (length(names(ranef(model)[[grnr]])) > 1)
        {
          model.updated <- update(model,
                                  formula = as.formula(paste(". ~ 0 + intercept.alt + ",
                                                             paste(added.variables, collapse="+"),
                                                             " + .",
                                                             paste(" - (", paste(names(ranef(model)[[grnr]])[-1], collapse="+"), "|", grouping, ")"),
                                                             " + (0 + intercept.alt +", paste(names(ranef(model)[[grnr]])[-1], collapse="+"), "|", grouping, ")")),
                                  data = data.adapted)
        }
      }

      if(gf=="all")
      {
        delete.gf <- vector()
        for (i in 1:length(ranef(model)))
        {
          if(length(names(ranef(model)[[i]])) > 1)
          {
            delete.gf[i] <- paste(
              "- (",
              paste(names(ranef(model)[[i]][-1]), collapse="+"),
              "|",
              names(ranef(model))[i],
              ")")
          }

          if(length(names(ranef(model)[[i]])) == 1)
          {
            delete.gf[i] <- paste(
              "- ( 1 |",
              names(ranef(model))[i],
              ")")
          }
        }
        delete.gf <- paste(delete.gf, collapse=" ")

        new.gf <- vector()
        for (i in 1:length(ranef(model)))
        {
          if(length(names(ranef(model)[[i]])) > 1)
          {
            new.gf[i] <- paste(
              "+ (0 + intercept.alt +",
              paste(names(ranef(model)[[i]][-1]), collapse="+"),
              "|",
              names(ranef(model))[i],
              ")")
          }

          if(length(names(ranef(model)[[i]])) == 1)
          {
            new.gf[i] <- paste(
              "+ (0 + intercept.alt |",
              names(ranef(model))[i],
              ")")
          }
        }
        new.gf <- paste(new.gf, collapse=" ")

        model.updated <- update(model,
                                formula = as.formula(
                                  paste(
                                    ". ~ 0 + intercept.alt + ",
                                    paste(added.variables, collapse="+"),
                                    "+ . ",
                                    delete.gf,
                                    new.gf)),
                                data=data.adapted)

      }
    }


    if(names(data.adapted)[2] == "intercept.alt")
    {

      for (i in 1:length(level))
      {
        data.adapted$intercept.alt[model@flist[[grouping]]==level[i]] <- 0

        data.adapted[, ncol(data.adapted)+1] <-
          ifelse(model@flist[[grouping]]==level[i], 1, 0)

        added.variables <- append(added.variables, values = make.names(paste("estex.", as.character(level[i]), sep="")))

        colnames(data.adapted)[ncol(data.adapted)] <- added.variables[length(added.variables)]
      }

      model.updated <- update(model,
                              formula = as.formula(paste(
                                ". ~ 0 + intercept.alt + ",
                                paste(added.variables, collapse="+"),
                                "+ .")),
                              data = data.adapted)
    }

    return(model.updated)

  }
