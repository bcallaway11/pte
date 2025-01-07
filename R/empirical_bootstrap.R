#' @title panel_empirical_bootstrap
#'
#' @description Computes empirical bootstrap pointwise standard errors
#'
#' @inheritParams compute.pte
#' @inheritParams pte2
#' @inheritParams attgt_if
#' @param attgt.list list of attgt results from \code{compute.pte}
#'
#' @return \code{pte_emp_boot} object
#'
#' @export
panel_empirical_bootstrap <- function(attgt.list,
                                      ptep,
                                      setup_pte_fun,
                                      subset_fun,
                                      attgt_fun,
                                      extra_gt_returns,
                                      ...) {
  # unpack ptep
  data <- ptep$data
  idname <- ptep$idname
  boot_type <- ptep$boot_type
  biters <- ptep$biters
  cl <- ptep$cl
  gt_type <- ptep$gt_type
  ret_quantile <- ptep$ret_quantile

  #-----------------------------------------------------------------------------
  # compute aggregations
  #-----------------------------------------------------------------------------

  # all results that return QTTs will go through empirical bootstrap code
  if (gt_type == "qtt") {
    aggte <- qtt_pte_aggregations(attgt.list, ptep, extra_gt_returns)
  } else if (gt_type == "qott") {
    aggte <- qott_pte_aggregations(attgt.list, ptep, extra_gt_returns)
  } else {
    aggte <- attgt_pte_aggregations(attgt.list, ptep)
  }

  # kind of hack...calls and returns of emprical and multiplier bootstrap
  # not matching exactly
  original_time.periods <- sort(unique(data[, ptep$tname]))
  extra_gt_returns <- lapply(
    extra_gt_returns,
    function(egr) {
      egr$group <- BMisc::t2orig(egr$group, original_time.periods)
      egr$time.period <- BMisc::t2orig(egr$time.period, original_time.periods)
      egr
    }
  )

  # bootstrap
  # list to store bootstrap results
  boot.res <- list()

  # loop for each nonparametric bootstrap iteration
  boot.res <- pbapply::pblapply(1:biters, function(b) {
    # draw a bootstrap sample; here, we'll call an outside function
    bdata <- BMisc::blockBootSample(data, idname)

    bptep <- setup_pte_fun(
      yname = ptep$yname,
      gname = ptep$gname,
      tname = ptep$tname,
      idname = ptep$idname,
      data = bdata,
      alp = ptep$alp,
      boot_type = boot_type,
      gt_type = gt_type,
      # ret_quantile=ret_quantile,
      biters = ptep$biters,
      cl = ptep$cl,
      ...
    )
    # bptep <- ptep
    # bptep$data <- bdata

    # call our function for estimating attgt on the
    # bootstrapped data
    bres_gt <- compute.pte(
      ptep = bptep,
      subset_fun = subset_fun,
      attgt_fun = attgt_fun,
      ...
    )[c("attgt.list", "extra_gt_returns")] # don't need to carry around ptep

    if (gt_type == "qtt") {
      bres <- qtt_pte_aggregations(bres_gt$attgt.list, bptep, bres_gt$extra_gt_returns)
    } else if (gt_type == "qott") {
      bres <- qott_pte_aggregations(bres_gt$attgt.list, bptep, bres_gt$extra_gt_returns)
    } else {
      bres <- attgt_pte_aggregations(bres_gt$attgt.list, bptep)
    }

    bres
  }, cl = cl)

  # attgt results
  attgt_results_inner <- bind_rows(BMisc::getListElement(boot.res, "attgt_results")) %>%
    group_by(group, time.period)
  attgt_results_se <- unlist(attgt_results_inner %>%
    group_map(~ sd(.x$att)))
  attgt_results <- aggte$attgt_results
  attgt_results$se <- attgt_results_se

  # dynamic results
  dyn_results_inner <- bind_rows(BMisc::getListElement(boot.res, "dyn_results")) %>%
    group_by(e) %>%
    mutate(length.e = length(e))
  original_elength <- length(unique(dyn_results_inner$e))
  dyn_results_inner <- subset(dyn_results_inner, length.e == biters)
  new_elength <- length(unique(dyn_results_inner$e))
  if (new_elength != original_elength) {
    warning("dropping some event times due to small groups")
  }

  dyn_results_se <- dyn_results_inner %>%
    transmute(se = sd(att.e))
  dyn_results_se <- dyn_results_se[1:new_elength, ]

  dyn_results <- aggte$dyn_results
  dyn_results <- inner_join(dyn_results, dyn_results_se, by = "e")

  # group results
  group_results <- aggte$group_results
  group_results_inner <- bind_rows(BMisc::getListElement(boot.res, "group_results")) %>%
    group_by(group) %>%
    mutate(length.group = length(group))
  original_glength <- length(unique(group_results_inner$group))
  group_results_inner <- subset(group_results_inner, length.group == biters)
  new_glength <- length(unique(group_results_inner$group))
  if (new_glength != original_glength) {
    warning("dropping some groups due to small groups")
  }

  group_results_se <- group_results_inner %>%
    transmute(se = sd(att.g))
  group_results_se <- group_results_se[1:new_glength, ]

  group_results <- inner_join(group_results, group_results_se, by = "group")

  # overall results
  overall_results <- aggte$overall_results
  overall_results_se <- sd(unlist(BMisc::getListElement(boot.res, "overall_results")))
  overall_results <- tibble(att = overall_results, se = overall_results_se)


  # return all results
  pte_emp_boot(
    attgt_results = attgt_results,
    overall_results = overall_results,
    group_results = group_results,
    dyn_results = dyn_results,
    extra_gt_returns = extra_gt_returns
  )
}


#' @title attgt_pte_aggregations
#'
#' @description Aggregate group-time average treatment effects into
#'  overall, group, and dynamic effects.  This function is only used
#'  for (i) computing standard errors using the empirical bootstrap,
#'  and (ii) combining distributions at the (g,t) level
#'
#' @inheritParams panel_empirical_bootstrap
#' @inheritParams attgt_if
#'
#' @return \code{pte_emp_boot} object
#'
#' @export
attgt_pte_aggregations <- function(attgt.list, ptep) {
  # pick up all time periods
  time.periods <- ptep$tlist
  groups <- ptep$glist

  original_time.periods <- sort(unique(ptep$data[, ptep$tname]))


  # data
  data <- ptep$data

  # turn results into a data.frame
  attgt.results <- do.call("rbind.data.frame", attgt.list)

  # this drop na att(g,t) due to violations of overlap conditions
  attgt.results <- attgt.results[complete.cases(attgt.results), ]

  # handle unequally spaced periods
  if (!(all(time.periods %in% original_time.periods))) {
    time.periods <- sapply(
      time.periods,
      BMisc::t2orig,
      original_time.periods
    )
    groups <- sapply(
      groups,
      BMisc::t2orig,
      original_time.periods
    )

    attgt.results$time.period <- sapply(
      attgt.results$time.period,
      BMisc::t2orig,
      original_time.periods
    )
    attgt.results$group <- sapply(
      attgt.results$group,
      BMisc::t2orig,
      original_time.periods
    )
  }

  # add event time to the results
  attgt.results$e <- attgt.results$time.period - attgt.results$group

  # calculate relative sizes of each group
  # (will be used as weights)
  n.group <- sapply(groups, function(gg) {
    nrow(subset(data, data[, ptep$gname] == gg & data[, ptep$tname] == time.periods[1]))
  })
  # merge in group sizes
  ngroup.mat <- cbind(groups, n.group)
  attgt.results <- merge(attgt.results, ngroup.mat, by.x = "group", by.y = "groups")

  # event times to calculate dynamic effects
  eseq <- unique(attgt.results$e)
  eseq <- sort(eseq)

  # calculate average effects by event time
  att.e <- c()
  weights.e <- list()
  counter <- 1
  for (this.e in eseq) {
    # get subset of results at this event time
    res.e <- subset(attgt.results, e == this.e)

    # calculate weights by group size
    res.e$weight <- res.e$n.group / sum(res.e$n.group)
    weights.e[[counter]] <- list()
    weights.e[[counter]]$e <- this.e
    weights.e[[counter]]$weights <- rep(0, nrow(attgt.results)) # start w/ all 0 weights
    weights.e[[counter]]$weights[attgt.results$e == this.e] <- res.e$weight # fill in some weights

    # calculate dynamic effect as weighted average
    att.e[counter] <- sum(res.e$att * res.e$weight)

    # on to the next one
    counter <- counter + 1
  }

  # calculate average effects by group
  att.g <- data.frame(group = integer(), att.g = double(), n.group = integer(), group_post_length = integer())
  weights.g <- list()
  counter <- 1
  for (this.g in groups) {
    # get subset of results at this event time
    res.g <- subset(attgt.results, group == this.g & time.period >= group)

    # calculate (g,t) weights
    weights.g[[counter]] <- list()
    weights.g[[counter]]$g <- this.g
    weights.g[[counter]]$weights <- rep(0, nrow(attgt.results)) # start w/ all 0 weights
    weights.g[[counter]]$weights[attgt.results$group == this.g & attgt.results$time.period >= attgt.results$group] <- 1 / nrow(res.g) # fill in weights

    # calculate group effect as weighted average
    att.g[counter, ] <- c(this.g, mean(res.g$att), mean(res.g$n.group), group_post_length = nrow(res.g))

    # on to the next one
    counter <- counter + 1
  }

  # drops any that are missing due to violations of overlap
  att.g <- att.g[complete.cases(att.g), ]


  # weighted average across groups to get overall att
  att.overall <- sum(att.g$att.g * (att.g$n.group / sum(att.g$n.group)))

  # att_gt weights
  # don't interpret this, this is just to put the weights back on ATT(g,t)
  att.g$g.overall.w <- (att.g$n.group / sum(att.g$n.group)) / att.g$group_post_length
  weights.overall <- dplyr::left_join(attgt.results, att.g[, c("group", "g.overall.w")], by = "group")$g.overall.w *
    (attgt.results$e >= 0)

  group.results <- att.g[, c(1, 2)] # drop group sizes

  # store dynamic effects results
  dyn.results <- cbind.data.frame(e = eseq, att.e = att.e)

  # return pte_emp_boot object
  pte_emp_boot(
    attgt_results = attgt.results[, c("group", "att", "time.period")],
    dyn_results = dyn.results,
    dyn_weights = weights.e,
    group_results = group.results,
    group_weights = weights.g,
    overall_results = att.overall,
    overall_weights = weights.overall
  )
}


#' @title qtt_pte_aggregations
#'
#' @description Aggregate group-time distributions into qtt versions of
#'  overall, group, and dynamic effects.
#'
#' @inheritParams attgt_pte_aggregations
#' @inheritParams attgt_if
#'
#' @return \code{pte_emp_boot} object
#'
#' @export
qtt_pte_aggregations <- function(attgt.list, ptep, extra_gt_returns) {
  ret_quantile <- ptep$ret_quantile

  # compute results for att_gt, but we are actually just interested in getting
  # the weights here.
  attgt_res <- attgt_pte_aggregations(attgt.list, ptep = ptep)
  F0_gt <- lapply(extra_gt_returns, function(egr) egr$extra_gt_returns$F0)
  F1_gt <- lapply(extra_gt_returns, function(egr) egr$extra_gt_returns$F1)
  qtt_gt <- unlist(lapply(1:length(F0_gt), function(j) {
    quantile(F1_gt[[j]], probs = ret_quantile, type = 1) - quantile(F0_gt[[j]], probs = ret_quantile, type = 1)
  }))
  groups <- unlist(BMisc::getListElement(attgt.list, "group"))
  time.periods <- unlist(BMisc::getListElement(attgt.list, "time.period"))
  yname <- ptep$yname
  y.seq <- quantile(ptep$data[, yname], probs = seq(0, 1, length.out = 1000))

  F0_overall <- BMisc::combineDfs(
    y.seq = y.seq,
    dflist = F0_gt,
    pstrat = attgt_res$overall_weights
  )
  F1_overall <- BMisc::combineDfs(
    y.seq = y.seq,
    dflist = F1_gt,
    pstrat = attgt_res$overall_weights
  )
  overall_qtt <- quantile(F1_overall, probs = ret_quantile, type = 1) - quantile(F0_overall, probs = ret_quantile, type = 1)

  dyn_qtt <- lapply(attgt_res$dyn_weights, function(dw) {
    F0_e <- BMisc::combineDfs(
      y.seq = y.seq,
      dflist = F0_gt,
      pstrat = dw$weights
    )
    F1_e <- BMisc::combineDfs(
      y.seq = y.seq,
      dflist = F1_gt,
      pstrat = dw$weights
    )
    list(e = dw$e, att.e = quantile(F1_e, probs = ret_quantile, type = 1) - quantile(F0_e, probs = ret_quantile, type = 1))
  })

  group_qtt <- lapply(attgt_res$group_weights, function(gw) {
    F0_g <- BMisc::combineDfs(
      y.seq = y.seq,
      dflist = F0_gt,
      pstrat = gw$weights
    )
    F1_g <- BMisc::combineDfs(
      y.seq = y.seq,
      dflist = F1_gt,
      pstrat = gw$weights
    )
    list(group = gw$g, att.g = quantile(F1_g, probs = ret_quantile, type = 1) - quantile(F0_g, probs = ret_quantile, type = 1))
  })

  pte_emp_boot(
    attgt_results = data.frame(
      group = groups,
      time.period = time.periods,
      att = qtt_gt
    ),
    dyn_results = do.call(rbind.data.frame, dyn_qtt),
    group_results = do.call(rbind.data.frame, group_qtt),
    overall_results = overall_qtt
  )
}



#' @title qott_pte_aggregations
#'
#' @description Aggregate group-time distribution of the treatment effect into
#'  overall, group, and dynamic effects.
#'
#' @inheritParams attgt_pte_aggregations
#' @inheritParams attgt_if
#'
#' @return \code{pte_emp_boot} object
#'
#' @export
qott_pte_aggregations <- function(attgt.list, ptep, extra_gt_returns) {
  ret_quantile <- ptep$ret_quantile

  # compute results for att_gt, but we are actually just interested in getting
  # the weights here.
  attgt_res <- attgt_pte_aggregations(attgt.list, ptep = ptep)
  Fte_gt <- lapply(extra_gt_returns, function(egr) egr$extra_gt_returns$Fte)
  qott_gt <- unlist(lapply(1:length(Fte_gt), function(j) {
    quantile(Fte_gt[[j]], probs = ret_quantile, type = 1)
  }))
  groups <- unlist(BMisc::getListElement(attgt.list, "group"))
  time.periods <- unlist(BMisc::getListElement(attgt.list, "time.period"))
  yname <- ptep$yname
  y.seq <- seq(-max(ptep$data[, yname]), max(ptep$data[, yname]), length.out = 1000)

  Fte_overall <- BMisc::combineDfs(
    y.seq = y.seq,
    dflist = Fte_gt,
    pstrat = attgt_res$overall_weights
  )
  overall_qott <- quantile(Fte_overall, probs = ret_quantile, type = 1)

  dyn_qott <- lapply(attgt_res$dyn_weights, function(dw) {
    Fte_e <- BMisc::combineDfs(
      y.seq = y.seq,
      dflist = Fte_gt,
      pstrat = dw$weights
    )
    list(e = dw$e, att.e = quantile(Fte_e, probs = ret_quantile, type = 1))
  })

  group_qott <- lapply(attgt_res$group_weights, function(gw) {
    Fte_g <- BMisc::combineDfs(
      y.seq = y.seq,
      dflist = Fte_gt,
      pstrat = gw$weights
    )
    list(group = gw$g, att.g = quantile(Fte_g, probs = ret_quantile, type = 1))
  })

  pte_emp_boot(
    attgt_results = data.frame(
      group = groups,
      time.period = time.periods,
      att = qott_gt
    ),
    dyn_results = do.call(rbind.data.frame, dyn_qott),
    group_results = do.call(rbind.data.frame, group_qott),
    overall_results = overall_qott
  )
}
