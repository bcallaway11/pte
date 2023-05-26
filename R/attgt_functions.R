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


#' @title pte_attgt
#'
#' @description `pte_attgt` takes a "local" data.frame and computes
#'  an estimate of a group time average treatment effect
#'  and a corresponding influence function.  This function generalizes
#'  a number of existing methods.
#'
#'  The code relies on \code{this.data} having certain variables defined.
#'  In particular, there should be an \code{id} column (individual identifier),
#'  \code{G} (group identifier), \code{period} (time period), \code{name}
#'  (equal to "pre" for pre-treatment periods and equal to "post" for post
#'  treatment periods), \code{Y} (outcome).
#'
#'  In our case, we call \code{pte::two_by_two_subset} which sets up the
#'  data to have this format before the call to `pte_attgt`
#'
#' @param gt_data data that is "local" to a particular group-time average
#'  treatment effect
#' @param xformla one-sided formula for covariates used in the propensity score
#'  and outcome regression models
#' @param d_outcome Whether or not to take the first difference of the outcome.
#'  The default is FALSE.  To use difference-in-differences, set this to be TRUE.
#' @param d_covs_formula A formula for time varying covariates to enter the
#'  first estimation step models.  The default is not to include any, and, hence,
#'  to only include pre-treatment covariats.
#' @param lagged_outcome_cov Whether to include the lagged outcome as a covariate.
#'  Default is FALSE.
#' @param est_method Which type of estimation method to use. Default is "dr" for
#'  doubly robust.  The other option is "reg" for regression adjustment.
#' @param ... extra function arguments; not used here
#'
#' @return attgt_if
#'
#' @export
pte_attgt <- function(gt_data, xformla, d_outcome=FALSE, d_covs_formula=~-1, lagged_outcome_cov=FALSE, est_method="dr", ...) {

  #-----------------------------------------------------------------------------
  # handle covariates
  #-----------------------------------------------------------------------------
  
  # pre-treatment covariates
  Xpre <- model.frame(xformla, data=subset(gt_data,name=="pre"))

  # change in covariates
  dX <- model.frame(d_covs_formula, data=subset(gt_data,name=="post")) - model.frame(d_covs_formula, data=subset(gt_data,name=="pre"))
  if (ncol(dX) > 0) colnames(dX) <- paste0("d", colnames(dX))

  # lagged outcome
  if (lagged_outcome_cov) lagY_formula <- -1 + Y else lagY_formla <- ~ -1
  lagY <- model.frame(lagY_formula, data=subset(gt_data, name=="post"))
  

  # convert two period panel into one period
  gt_data_outcomes <- tidyr::pivot_wider(gt_data[,c("D","id","period","name","Y")], id_cols=c(id, D),
                                           names_from=c(name),
                                           values_from=c(Y))

  # merge outcome and covariate data
  gt_dataX <- cbind.data.frame(gt_data_outcomes, Xpre, dX, lagY)

  # treatment dummy variable
  D <- gt_dataX$D

  # post treatment outcome
  Y <- gt_dataX$post

  if (d_outcome) Y <- gt_dataX$post - gt_dataX$pre

  # estimate attgt
  # DRDID::drdid_panel is for panel data, but we can hack it
  # to work in levels by just setting outcomes in "first period"
  # to be equal to 0 for all units
  gt_dataX <- droplevels(gt_dataX)
  use_formula <- BMisc::toformula("", c(BMisc::rhs.vars(xformla), colnames(dX)))
  covmat <- model.matrix(use_formula, data=gt_dataX)
  covmat2 <- covmat[D==0,]
  #www <- gt_dataX[D==0,]$.w
  n_unt <- sum(1-D)
  precheck_reg <- qr(t(covmat2)%*%covmat2/n_unt)
  keep_covs <- precheck_reg$pivot[1:precheck_reg$rank]
  covmat <- covmat[,keep_covs]
  if (est_method == "dr") {
    attgt <- DRDID::drdid_panel(y1=Y,
                                y0=rep(0,length(Y)),
                                D=D,
                                covariates=covmat,      
                                inffunc=TRUE)
  } else if (est_method == "reg") {
    attgt <- DRDID::reg_did_panel(y1=Y,
                                y0=rep(0,length(Y)),
                                D=D,
                                covariates=covmat,      
                                inffunc=TRUE)
  } else {
    stop(paste0("est_method: ", est_method, " is not supported"))
  }
  # return attgt
  pte::attgt_if(attgt=attgt$ATT, inf_func=attgt$att.inf.func)
}
