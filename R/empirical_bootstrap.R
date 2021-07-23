#' @title panel_empirical_bootstrap
#'
#' @description Computes empirical bootstrap pointwise standard errors
#'
#' @inheritParams compute.pte
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
  
  # compute aggregations
  aggte <- attgt_pte_aggregations(attgt.list, ptep)

  # kind of hack...calls and returns of emprical and multiplier bootstrap
  # not matching exactly
  original_time.periods <- sort(unique(data[,ptep$tname]))
  extra_gt_returns <- lapply(extra_gt_returns,
                             function(egr) {
                               egr$group <- t2orig(egr$group, original_time.periods)
                               egr$time.period <- t2orig(egr$time.period, original_time.periods)
                               egr
                             })

  # bootstrap
  # list to store bootstrap results
  boot.res <- list()

  # loop for each nonparametric bootstrap iteration
  boot.res <- pbapply::pblapply(1:biters, function(b) {
    # draw a bootstrap sample; here, we'll call an outside function
    bdata <- BMisc::blockBootSample(data, idname)

    bptep <- setup_pte_fun(yname=ptep$yname,
                           gname=ptep$gname,
                           tname=ptep$tname,
                           idname=ptep$idname,
                           data=bdata,
                           alp=ptep$alp,
                           boot_type=boot_type,
                           biters=ptep$biters,
                           cl=ptep$cl,
                           ...)
    # bptep <- ptep
    # bptep$data <- bdata

    # call our function for estimating attgt on the
    # bootstrapped data
    bres_attgt <- compute.pte(ptep=bptep,
                              subset_fun=subset_fun,
                              attgt_fun=attgt_fun,
                              ...)$attgt.list
    
    bres <- attgt_pte_aggregations(bres_attgt, bptep)
    bres
  }, cl=cl)

  # attgt results
  attgt_results_inner <- bind_rows(BMisc::getListElement(boot.res, "attgt_results")) %>%
    group_by(group, time.period)
  attgt_results_se <- unlist(attgt_results_inner %>%
                               group_map(~sd(.x$att)))
  attgt_results <- aggte$attgt_results
  attgt_results$se <- attgt_results_se
  
  # dynamic results
  dyn_results_inner <- bind_rows(BMisc::getListElement(boot.res, "dyn_results")) %>%
    group_by(e) %>%
    mutate(length.e=length(e))
  original_elength <- length(unique(dyn_results_inner$e))
  dyn_results_inner <- subset(dyn_results_inner, length.e==biters)
  new_elength <- length(unique(dyn_results_inner$e))
  if (new_elength != original_elength) {
    warning("dropping some event times due to small groups")
  }
  
  dyn_results_se <- dyn_results_inner %>%
    transmute(se=sd(att.e))
  dyn_results_se <- dyn_results_se[1:new_elength,]

  dyn_results <- aggte$dyn_results
  dyn_results <- inner_join(dyn_results, dyn_results_se, by="e")

  # group results
  group_results <- aggte$group_results
  group_results_inner <- bind_rows(BMisc::getListElement(boot.res, "group_results")) %>%
    group_by(group) %>%
    mutate(length.group=length(group))
  original_glength <- length(unique(group_results_inner$group))
  group_results_inner <- subset(group_results_inner, length.group==biters)
  new_glength <- length(unique(group_results_inner$group))
  if (new_glength != original_glength) {
    warning("dropping some groups due to small groups")
  }
  
  group_results_se <- group_results_inner %>%
    transmute(se=sd(att.g))
  group_results_se <- group_results_se[1:new_glength,]

  group_results <- inner_join(group_results, group_results_se, by="group")

  # overall results
  overall_results <- aggte$overall_results
  overall_results_se <- sd(unlist(BMisc::getListElement(boot.res, "overall_results")))
  overall_results <- tibble(att=overall_results, se=overall_results_se)


  # return all results
  pte_emp_boot(attgt_results=attgt_results,
               overall_results=overall_results,
               group_results=group_results,
               dyn_results=dyn_results,
               extra_gt_returns=extra_gt_returns)
}


#' @title attgt_pte_aggregations
#'
#' @description Aggregate group-time average treatment effects into
#'  overall, group, and dynamic effects.  This function is only used
#'  for computing standard errors using the empirical bootstrap.
#'
#' @inheritParams panel_empirical_bootstrap
#'
#' @return \code{pte_emp_boot} object
#'
#' @export
attgt_pte_aggregations <- function(attgt.list, ptep) {
  # pick up all time periods
  time.periods <- ptep$tlist
  groups <- ptep$glist
  
  original_time.periods <- sort(unique(ptep$data[,ptep$tname]))

  
  # data
  data <- ptep$data
  
  # turn results into a data.frame
  attgt.results <- do.call("rbind.data.frame", attgt.list)
  
  # this drop na att(g,t) due to violations of overlap conditions
  attgt.results <- attgt.results[complete.cases(attgt.results),]

  # handle unequally spaced periods
  if ( ! (all(time.periods %in% original_time.periods)) ) {
    time.periods <- sapply(time.periods,
                           t2orig, 
                           original_time.periods)
    groups <- sapply(groups,
                     t2orig,
                     original_time.periods)

    attgt.results$time.period <- sapply(attgt.results$time.period,
                                        t2orig,
                                        original_time.periods)
    attgt.results$group <- sapply(attgt.results$group,
                                  t2orig,
                                  original_time.periods)
  }
  
  # add event time to the results
  attgt.results$e <- attgt.results$time.period - attgt.results$group
  
  # calculate relative sizes of each group
  # (will be used as weights)
  n.group <- sapply(groups, function(gg) {
    nrow(subset(data, data[,ptep$gname]==gg & data[,ptep$tname]==time.periods[1]))
  })
  # merge in group sizes
  ngroup.mat <- cbind(groups, n.group)
  attgt.results <- merge(attgt.results, ngroup.mat, by.x = "group", by.y = "groups")

  # event times to calculate dynamic effects
  eseq <- unique(attgt.results$e) 
  eseq <- sort(eseq)

  # calculate average effects by event time
  att.e <- c()
  counter <- 1
  for (this.e in eseq) {
    # get subset of results at this event time
    res.e <- subset(attgt.results, e==this.e)

    # calculate weights by group size
    res.e$weight <- res.e$n.group / sum(res.e$n.group)

    # calculate dynamic effect as weighted average
    att.e[counter] <- sum(res.e$att * res.e$weight)

    # on to the next one
    counter <- counter+1
  }

  # calculate average effects by group
  att.g <- data.frame(group=integer(), att.g=double(), n.group=integer())
  counter <- 1
  for (this.g in groups) {
    # get subset of results at this event time
    res.g <- subset(attgt.results, group==this.g & time.period >= group)
    
    # calculate dynamic effect as weighted average
    att.g[counter,] <- c(this.g, mean(res.g$att), mean(res.g$n.group))

    # on to the next one
    counter <- counter+1
  }

  # drops any that are missing due to violations of overlap
  att.g <- att.g[complete.cases(att.g),]

  
  # weighted average across groups to get overall att
  att.overall <- sum(att.g$att.g * (att.g$n.group/sum(att.g$n.group)))

  group.results <- att.g[,c(1,2)] # drop group sizes

  # store dynamic effects results
  dyn.results <- cbind.data.frame(e = eseq, att.e = att.e)

  # return pte_emp_boot object
  pte_emp_boot(attgt_results=attgt.results[,c("group","att","time.period")],
               dyn_results=dyn.results,
               group_results=group.results,
               overall_results=att.overall)
}
