#' @title process_att_gt
#'
#' @description process attgt results when influence function is available
#'
#' @param att_gt_results ATT(g,t)'s
#' @inheritParams pte_results
#'
#' @export
process_att_gt <- function(att_gt_results, ptep) {

  # extract ATT(g,t) and influence functions
  attgt.list <- att_gt_results$attgt.list
  inffunc <- att_gt_results$inffunc

  # process results
  attgt.results <- do.call("rbind.data.frame", attgt.list)
  att <- attgt.results$att
  group <- attgt.results$group
  time.period <- attgt.results$time.period

  #-----------------------------------------------------------------------------
  # analytical standard errors
  #   * estimate variance
  #     this is analogous to cluster robust standard errors that
  #     are clustered at the unit level
  #-----------------------------------------------------------------------------
  n <- nrow(inffunc)
  V <- Matrix::t(inffunc)%*%inffunc/n
  se <- sqrt(Matrix::diag(V)/n)
  alp <- ptep$alp

  # critical value from N(0,1), for pointwise
  cval <- qnorm(1-alp/2)

  # multiplier bootstrap results
  bout <- mboot2(inffunc, alp=alp)
  
  #-----------------------------------------------------------------------------
  # compute Wald pre-test
  #-----------------------------------------------------------------------------
  # select which periods are pre-treatment
  pre <- which(group > time.period)

  # pseudo-atts in pre-treatment periods
  preatt <- as.matrix(att[pre])

  # covariance matrix of pre-treatment atts
  preV <- as.matrix(V[pre,pre])

  # check if there are actually any pre-treatment periods
  W <- NULL
  Wpval <- NULL
  if (length(preV) == 0) {
    message("No pre-treatment periods to test")
  } else if(sum(is.na(preV))) {
    warning("Not returning pre-test Wald statistic due to NA pre-treatment values")
  } else if (rcond(preV) <= .Machine$double.eps) {
    # singluar covariance matrix for pre-treatment periods
    warning("Not returning pre-test Wald statistic due to singular covariance matrix")
  } else {
    # everything is working...
    W <- n*t(preatt)%*%solve(preV)%*%preatt
    q <- length(pre) # number of restrictions
    Wpval <- round(1-pchisq(W,q),5)
  }

  
  # Return list for ATT(g,t)
  return(group_time_att(group=group, time.period=time.period, att=att, V_analytical=V, se=bout$boot_se, crit_val=bout$crit_val, inf_func=inffunc, n=n, W=W, Wpval=Wpval, alp = alp, ptep=ptep))
}

#' @title mboot2
#'
#' @description function for using multiplier bootstrap to conduct
#'  inference
#'
#' @param inffunc influence function matrix
#' @inheritParams pte
#'
#' @export
mboot2 <- function(inffunc, biters=1000, alp=.05) {
  n <- nrow(as.matrix(inffunc))

  # run the multiplier bootstrap 
  bout <- lapply(1:biters, function(b) {
    # draw from -1,1 each with p=1/2
    Ub <- sample(c(-1,1), size=n, replace=TRUE)
    Rb <- sqrt(n)*(apply(Ub*inffunc, 2, mean))
    Rb
  })
  bres <- do.call("rbind", bout)

  # bootstrap compatible standard errors
  boot_se <- apply(bres, 2,
                  function(b) (quantile(b, .75, type=1, na.rm = T) -
                                 quantile(b, .25, type=1, na.rm = T))/(qnorm(.75) - qnorm(.25))) / sqrt(n)

  # bootstrap t-stat (i.e., sup-t)
  bT <- apply(bres, 1, function(b) max( abs(b/boot_se)) ) / sqrt(n)

  # new critical value for uniform confidence bands
  crit_val <- quantile(bT, 1-alp, type=1, na.rm = T)

  return(list(boot_se=boot_se, crit_val=crit_val))
}