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

  # a bit of a hack way to get standard errors
  # with a universal base period when some ATT(g,t)'s
  # are 0 by construction and have no s.e.'s / inf. func.
  # if (isTRUE(ptep$base_period == "universal")) {
  #  set0cols <- sapply(1:ncol(inffunc), function(i) {
  #    (all(is.na(inffunc[,i])))
  #  })
  #  inffunc[,set0cols] <- 0
  # }

  # process results
  attgt.results <- do.call("rbind.data.frame", attgt.list)
  att <- attgt.results$att
  group <- attgt.results$group
  time.period <- attgt.results$time.period
  extra_gt_returns <- att_gt_results$extra_gt_returns

  #-----------------------------------------------------------------------------
  # analytical standard errors
  #   * estimate variance
  #     this is analogous to cluster robust standard errors that
  #     are clustered at the unit level
  #-----------------------------------------------------------------------------
  n <- nrow(inffunc)
  V <- Matrix::t(inffunc) %*% inffunc / n
  se <- sqrt(Matrix::diag(V) / n)
  cband <- ptep$cband
  alp <- ptep$alp

  # critical value from N(0,1), for pointwise
  cval <- qnorm(1 - alp / 2)

  # multiplier bootstrap results
  bout <- mboot2(inffunc, alp = alp)

  if (cband) cval <- bout$crit_val

  #-----------------------------------------------------------------------------
  # compute Wald pre-test
  #-----------------------------------------------------------------------------
  # select which periods are pre-treatment
  pre <- which(group > time.period)

  # pseudo-atts in pre-treatment periods
  preatt <- as.matrix(att[pre])

  # covariance matrix of pre-treatment atts
  preV <- as.matrix(V[pre, pre])

  # check if there are actually any pre-treatment periods
  W <- NULL
  Wpval <- NULL
  if (length(preV) == 0) {
    message("No pre-treatment periods to test")
  } else if (sum(is.na(preV))) {
    warning("Not returning pre-test Wald statistic due to NA pre-treatment values")
  } else if (rcond(preV) <= .Machine$double.eps) {
    # singluar covariance matrix for pre-treatment periods
    warning("Not returning pre-test Wald statistic due to singular covariance matrix")
  } else {
    # everything is working...
    W <- n * t(preatt) %*% solve(preV) %*% preatt
    q <- length(pre) # number of restrictions
    Wpval <- round(1 - pchisq(W, q), 5)
  }

  # convert tlist and glist to be compatible with did::aggte

  # from "new" time to "original" time
  original_time.periods <- sort(unique(ptep$data[, ptep$tname]))
  if (!all(ptep$tlist %in% original_time.periods)) {
    ptep$tlist <- sapply(ptep$tlist,
      BMisc::t2orig,
      original_time.periods = original_time.periods
    )
    ptep$glist <- sapply(ptep$glist,
      BMisc::t2orig,
      original_time.periods = original_time.periods
    )
    group <- sapply(group,
      BMisc::t2orig,
      original_time.periods = original_time.periods
    )
    time.period <- sapply(time.period,
      BMisc::t2orig,
      original_time.periods = original_time.periods
    )
    extra_gt_returns <- lapply(
      extra_gt_returns,
      function(egr) {
        egr$group <- BMisc::t2orig(egr$group, original_time.periods)
        egr$time.period <- BMisc::t2orig(egr$time.period, original_time.periods)
        egr
      }
    )
  }

  # set groups to be untreated so that we do not drop them later (would be better to have this in
  # aggte code, but no changes there.  The only place where I know this is used is for staggered_ife
  # ptep$data[,ptep$gname] <- ifelse( !(ptep$data[,ptep$gname] %in% ptep$glist), 0, ptep$data[,ptep$gname])
  if (is.null(ptep$weightsname)) {
    ptep$data$.w <- 1
  } else {
    ptep$data$.w <- ptep$data[, ptep$weightsname]
  }

  # Return list for ATT(g,t)
  return(group_time_att(group = group, time.period = time.period, att = att, V_analytical = V, se = bout$boot_se, crit_val = cval, inf_func = inffunc, n = n, W = W, Wpval = Wpval, cband = cband, alp = alp, ptep = ptep, extra_gt_returns = extra_gt_returns))
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
mboot2 <- function(inffunc, biters = 1000, alp = .05) {
  n <- nrow(as.matrix(inffunc))

  # run the multiplier bootstrap
  bout <- lapply(1:biters, function(b) {
    # draw from -1,1 each with p=1/2
    Ub <- sample(c(-1, 1), size = n, replace = TRUE)
    Rb <- sqrt(n) * (apply(Ub * inffunc, 2, mean))
    Rb
  })
  bres <- do.call("rbind", bout)

  # bootstrap compatible standard errors
  boot_se <- apply(
    bres, 2,
    function(b) {
      (quantile(b, .75, type = 1, na.rm = T) -
        quantile(b, .25, type = 1, na.rm = T)) / (qnorm(.75) - qnorm(.25))
    }
  ) / sqrt(n)

  # bootstrap t-stat (i.e., sup-t)
  bT <- apply(bres, 1, function(b) max(abs(b / boot_se), na.rm = TRUE)) / sqrt(n)

  # new critical value for uniform confidence bands
  crit_val <- quantile(bT, 1 - alp, type = 1, na.rm = T)
  if (crit_val < qnorm(1 - alp / 2)) {
    warning("critical value for uniform confidence band is somehow smaller than
            critical value for pointwise confidence interval...using pointwise
            confidence interal")
    crit_val <- cval <- qnorm(1 - alp / 2)
  }

  return(list(boot_se = boot_se, crit_val = crit_val))
}
