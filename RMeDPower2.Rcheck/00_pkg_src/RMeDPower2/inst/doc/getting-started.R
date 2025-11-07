## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)

## ----eval=FALSE---------------------------------------------------------------
# # Install devtools if you haven't already
# install.packages("devtools")
# 
# # Install RMeDPower2 from GitHub
# devtools::install_github("gladstone-institutes/RMeDPower2", build_vignettes = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# library(RMeDPower2)
# 
# # Load example data
# data(RMeDPower_data1)
# 
# # Define experimental design
# design <- new("RMeDesign",
#     response_column = "cell_size2",
#     condition_column = "classification",
#     experimental_columns = c("experiment", "line"),
#     condition_is_categorical = TRUE
# )
# 
# # Define probability model
# model <- new("ProbabilityModel",
#     error_is_non_normal = FALSE
# )
# 
# # Define power parameters
# power_param <- new("PowerParams",
#     target_columns = c("experiment"),
#     power_curve = 1,
#     nsimn = 100
# )
# 
# # Step 1: Diagnose data and model assumptions
# diagnose_res <- diagnoseDataModel(RMeDPower_data1, design, model)
# 
# # Step 2: Calculate power
# power_res <- calculatePower(RMeDPower_data1, design, model, power_param)
# 
# # Step 3: Get parameter estimates
# estimate_res <- getEstimatesOfInterest(RMeDPower_data1, design, model)

