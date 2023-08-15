#' @title readProbabilityModel
#'
#' @description This functions reads the underlying design for the data
#'
#'
#' @param jsonfile the jsonfile with the necessary parameters for probability model: error_is_non_normal, family_p
#'
#' @return an object of class ProbabilityModel
#' @export
#'
#' @examples design=readPowerParams(jsonfile)

readProbabilityModel <- function(jsonfile) {
  Prob_Model_data <- jsonlite::fromJSON(jsonfile)

  prob_model <- new("ProbabilityModel")

  prob_model@error_is_non_normal = Prob_Model_data$error_is_non_normal
  if(!is.null(Prob_Model_data$family_p)) prob_model@family_p = Prob_Model_data$family_p

  return(prob_model)
}
