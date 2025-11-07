pkgname <- "RMeDPower2"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('RMeDPower2')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("PowerParams-class")
### * PowerParams-class

flush(stderr()); flush(stdout())

### Name: PowerParams-class
### Title: PowerParams-class
### Aliases: PowerParams-class

### ** Examples

power_param=new("PowerParams")



cleanEx()
nameEx("ProbabilityModel-class")
### * ProbabilityModel-class

flush(stderr()); flush(stdout())

### Name: ProbabilityModel-class
### Title: ProbabilityModel-class
### Aliases: ProbabilityModel-class

### ** Examples

model=new("ProbabilityModel")



cleanEx()
nameEx("RMeDesign-class")
### * RMeDesign-class

flush(stderr()); flush(stdout())

### Name: RMeDesign-class
### Title: RMeDesign-class
### Aliases: RMeDesign-class

### ** Examples

design=new("RMeDesign")



cleanEx()
nameEx("calculatePower")
### * calculatePower

flush(stderr()); flush(stdout())

### Name: calculatePower
### Title: calculatePower
### Aliases: calculatePower

### ** Examples

result=calculatePower(data=data, design=design, model=model, power_param=power_param)



cleanEx()
nameEx("calculate_lmer_estimates")
### * calculate_lmer_estimates

flush(stderr()); flush(stdout())

### Name: calculate_lmer_estimates
### Title: calculate_lmer_estimates_covariate
### Aliases: calculate_lmer_estimates

### ** Examples

result=calculate_lmer_estimates(data=RMeDPower_data1,
condition_column="classification",
experimental_columns=c("experiment", "line"),
response_column="cell_size1",
condition_is_categorical=TRUE,
covariate="covariate",
crossed_columns = "line",
family_p=NULL,
error_is_non_normal=FALSE)



cleanEx()
nameEx("calculate_power")
### * calculate_power

flush(stderr()); flush(stdout())

### Name: calculate_power
### Title: calculate_power_covariate
### Aliases: calculate_power

### ** Examples

result=calculate_power(data=RMeDPower_data1,
condition_column="classification",
experimental_columns=c("experiment", "line"),
response_column="cell_size1",
target_columns="experiment",
power_curve=1,
condition_is_categorical=TRUE,
crossed_columns = "line",
error_is_non_normal=FALSE,
levels=1)



cleanEx()
nameEx("diagnoseDataModel")
### * diagnoseDataModel

flush(stderr()); flush(stdout())

### Name: diagnoseDataModel
### Title: diagnoseDataModel
### Aliases: diagnoseDataModel

### ** Examples

result=diagnoseDataModel(data=data, design=design, model=model)



cleanEx()
nameEx("getEstimatesOfInterest")
### * getEstimatesOfInterest

flush(stderr()); flush(stdout())

### Name: getEstimatesOfInterest
### Title: getEstimatesOfInterest
### Aliases: getEstimatesOfInterest

### ** Examples

result=diagnoseDataModel(data=data, design=design, model=model)



cleanEx()
nameEx("get_residuals")
### * get_residuals

flush(stderr()); flush(stdout())

### Name: get_residuals
### Title: get_residuals_covariate
### Aliases: get_residuals

### ** Examples

result=get_residuals_covariate(data=RMeDPower_data1,
condition_column="classification",
experimental_columns=c("experiment", "line"),
response_column="cell_size1",
condition_is_categorical=TRUE,
covariate="covariate",
crossed_columns = "line",
error_is_non_normal=FALSE)



cleanEx()
nameEx("readDesign")
### * readDesign

flush(stderr()); flush(stdout())

### Name: readDesign
### Title: readDesign
### Aliases: readDesign

### ** Examples

design=readDesign(jsonfile)



cleanEx()
nameEx("readPowerParams")
### * readPowerParams

flush(stderr()); flush(stdout())

### Name: readPowerParams
### Title: readPowerParams
### Aliases: readPowerParams

### ** Examples

design=readPowerParams(jsonfile)



cleanEx()
nameEx("readProbabilityModel")
### * readProbabilityModel

flush(stderr()); flush(stdout())

### Name: readProbabilityModel
### Title: readProbabilityModel
### Aliases: readProbabilityModel

### ** Examples

design=readPowerParams(jsonfile)



cleanEx()
nameEx("transform_data")
### * transform_data

flush(stderr()); flush(stdout())

### Name: transform_data
### Title: transform_data
### Aliases: transform_data

### ** Examples

result=transform_data2(data=data, condition_column="classif", experimental_columns=c("experiment","line"), response_column="feature", condition_is_categorical=TRUE, error_is_non_normal=FALSE, alpha=0.05, crossed_columns = "line", method="cook", na.action="complete")
result=transform_data2(data=data, condition_column="classif", experimental_columns=c("experiment","line"), response_column="feature", condition_is_categorical=TRUE, error_is_non_normal=FALSE, alpha=0.05, crossed_columns = "line", method="cook", na.action="complete")
result=transform_data2(data=data, condition_column="classif", experimental_columns=c("experiment","line"), response_column="feature", condition_is_categorical=TRUE, error_is_non_normal=TRUE, family_p="poisson", alpha=0.05, crossed_columns = "line", method="cook", na.action="complete")



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
