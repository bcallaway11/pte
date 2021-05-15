process_att_gt <- function(att_gt_results, dp) {

  # extract ATT(g,t) and influence functions
  attgt.list <- att_gt_results$attgt.list
  inffunc <- att_gt_results$inffunc

  # process results
  # attgt.results <- did::process_attgt(results)
  # group <- unlist(BMisc::getListElement(attgt.list, "group"))
  # att <- attgt.results$att
  # tt <- attgt.results$tt
  # inffunc <- attgt.results$inf.func
  attgt.results <- do.call("rbind.data.frame", attgt.list)
  att <- attgt.results$att
  group <- attgt.results$group
  time.period <- attgt.results$time.period

  # analytical standard errors
  # estimate variance
  # this is analogous to cluster robust standard errors that
  # are clustered at the unit level

  # note to self: this def. won't work with unbalanced panel,
  # but it is always ignored b/c bstrap has to be true in that case
  n <- nrow(inffunc)
  V <- Matrix::t(inffunc)%*%inffunc/n
  se <- sqrt(Matrix::diag(V)/n)
  alp <- dp$alp

  # critical value from N(0,1), for pointwise
  cval <- qnorm(1-alp/2)
  bout <- mboot2(inffunc)
  
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
  if (length(preV) == 0) {
    message("No pre-treatment periods to test")
    W  <- NULL
    Wpval <- NULL
  } else if(sum(is.na(preV))) {
    warning("Not returning pre-test Wald statistic due to NA pre-treatment values")
    W <- NULL
    Wpval <- NULL
  } else if (rcond(preV) <= .Machine$double.eps) {
    # singluar covariance matrix for pre-treatment periods
    warning("Not returning pre-test Wald statistic due to singular covariance matrix")
    W <- NULL
    Wpval <- NULL
  } else {
    # everything is working...
    W <- n*t(preatt)%*%solve(preV)%*%preatt
    q <- length(pre) # number of restrictions
    Wpval <- round(1-pchisq(W,q),5)
  }

  #-----------------------------------------------------------------------------
  # compute confidence intervals / bands
  #-----------------------------------------------------------------------------
  
  # Return this list
  return(MP(group=group, t=time.period, att=att, V_analytical=V, se=bout$boot_se, c=bout$crit_val, inffunc=inffunc, n=n, W=W, Wpval=Wpval, alp = alp, DIDparams=dp))
}

mboot2 <- function(inffunc, biters=1000, alp=.05) {
  n <- nrow(as.matrix(inffunc))
  bout <- lapply(1:biters, function(b) {
    Ub <- sample(c(-1,1), size=n, replace=TRUE)
    Rb <- sqrt(n)*(apply(Ub*inffunc, 2, mean))
    Rb
  })
  bres <- do.call("rbind", bout)
  boot_se <- apply(bres, 2,
                  function(b) (quantile(b, .75, type=1, na.rm = T) -
                                 quantile(b, .25, type=1, na.rm = T))/(qnorm(.75) - qnorm(.25))) / sqrt(n)
  bT <- apply(bres, 1, function(b) max( abs(b/boot_se)) ) / sqrt(n)
  crit_val <- quantile(bT, 1-alp, type=1, na.rm = T)

  return(list(boot_se=boot_se, crit_val=crit_val))
}
