#' @keywords internal
#' @import ggplot2 dplyr magrittr methods
#' @importFrom grDevices png dev.off
#' @importFrom graphics mtext
#' @importFrom EnvStats rosnerTest
#' @importFrom jsonlite fromJSON
#' @importFrom stats poisson binomial Gamma as.formula median qqnorm qqline mad qnorm model.frame update vcov fitted predict residuals coef
#' @importFrom influence.ME cooks.distance.estex grouping.levels se.fixef
#' @importFrom lme4 fixef ranef VarCorr glmer glmer.nb getME
#' @importFrom lmerTest lmer
#' @importFrom simr "fixef<-" extend fcompare fixed makeLmer powerCurve powerSim
#' @importFrom ggtext element_textbox_simple
#' @importFrom tibble rownames_to_column
#' @importFrom DHARMa simulateResiduals outliers
NULL

utils::globalVariables(c(
  ".", "cooks_distance", "exp.factor", "experimental_column1",
  "lower", "med_residual1", "model_residuals", "residual",
  "response_column", "sqrt_abs_residuals", "upper"
))
