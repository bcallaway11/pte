#' @title did_attgt
#'
#' @description Takes a "local" data.frame and computes
#'  an estimate of a group time average treatment effect
#'  and a corresponding influence function using a difference in differences
#'  approach.
#'
#'  The code relies on \code{gt_data} having certain variables defined. 
#'  In particular, there should be an \code{id} column (individual identifier),
#'  \code{D} (treated group identifier), \code{period} (time period), \code{name}
#'  (equal to "pre" for pre-treatment periods and equal to "post" for post
#'  treatment periods), \code{Y} (outcome).
#'
#'  In our case, we call \code{pte::two_by_two_subset} which sets up the
#'  data to have this format before the call to \code{did_attgt}.
#'
#' @param gt_data data that is "local" to a particular group-time average
#'  treatment effect
#' @param xformla one-sided formula for covariates used in the propensity score
#'  and outcome regression models
#' @param ... extra function arguments; not used here
#'
#' @return attgt_if
#'
#' @export
did_attgt <- function(gt_data, xformla, ...) {

  #-----------------------------------------------------------------------------
  # handle covariates
  #-----------------------------------------------------------------------------
  # for outcome regression, get pre-treatment values
  Xpre <- model.frame(xformla, data=subset(gt_data,name=="pre"))

  # convert two period panel into one period
  gt_data_outcomes <- tidyr::pivot_wider(gt_data[,c("D","id","period","name","Y")], id_cols=c(id, D),
                                           names_from=c(name),
                                           values_from=c(Y))

  # merge outcome and covariate data
  gt_dataX <- cbind.data.frame(gt_data_outcomes, Xpre)

  # treatment dummy variable
  D <- gt_dataX$D

  # pre- and post-treatment outcomes
  Y_post <- gt_dataX$post
  Y_pre <- gt_dataX$pre

  # call DRDID functions to make the computations;
  # just like in `did` package
  gt_dataX <- droplevels(gt_dataX)
  attgt <- DRDID::drdid_panel(y1=Y_post,
                              y0=Y_pre,
                              D=D,
                              covariates=model.matrix(xformla,
                                                      data=gt_dataX),
                              inffunc=TRUE)

  # return attgt
  attgt_if(attgt=attgt$ATT, inf_func=attgt$att.inf.func)
}
