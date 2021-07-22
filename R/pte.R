#' @title compute.pte
#'
#' @description Function that actually computes panel treatment effects
#'
#' @inheritParams pte
#' @param ptep \code{pte_params} object
#'
#' @return list of attgt results and, sometimes, and influence function
#'
#' @export
compute.pte <- function(ptep,
                        subset_fun,
                        attgt_fun,
                        ...) {
  #-----------------------------------------------------------------------------
  # unpack ptep
  #-----------------------------------------------------------------------------
  data <- ptep$data
  yname <- ptep$yname
  gname <- ptep$gname
  idname <- ptep$idname
  tname <- ptep$tname
  
  
  data <- as.data.frame(data)
  
  # setup data
  # G <- data[,gname]
  # id <- data[,idname]
  # period <- data[,tname]
  G <- data$G
  id <- data$id
  period <- data$period
  original_groups <- sort(unique(data$original_group))[-1] # drops never treated
  original_time.periods <- sort(unique(data$original_period))
  
  #data$G <- G
  #data$id <- id
  n <- length(unique(data$id))
  #data$period <- period
  #data$Y <- data[,yname]

  # pick up all time periods
  time.periods <- ptep$tlist

  # sort the groups and drop the untreated group
  groups <- ptep$glist

  # list to store all group-time average treatment effects
  # that we calculate
  attgt.list <- list()
  counter <- 1
  nG <- length(groups)
  nT <- length(time.periods)
  inffunc <- matrix(data=NA, nrow=n, ncol=nG*(nT))
  
  # loop over all groups
  for (g in groups) {
        
    # loop over all time periods
    for (tp in time.periods) {
      
      #-----------------------------------------------------------------------------
      # code to get the right subset of the data
      #-----------------------------------------------------------------------------
      gt_subset <- subset_fun(data, g, tp, ... )
      gt_data <- gt_subset$gt_data
      n1 <- gt_subset$n1
      disidx <- gt_subset$disidx

      #-----------------------------------------------------------------------------
      # code to estimate attgt using correct relevant data
      #-----------------------------------------------------------------------------
      attgt <- attgt_fun(gt_data=gt_data, ...)

      #-----------------------------------------------------------------------------
      # If this is too generic...
      # an alternative idea is to just delete the call to attgt_fun above
      # and just write your own code in this spot
      #-----------------------------------------------------------------------------

      #-----------------------------------------------------------------------------
      # process attgt results
      #   - branch based on whether or not attgt_fun returned an influence
      #     function
      #-----------------------------------------------------------------------------

      # save results
      attgt.list[[counter]] <- list(att=attgt$attgt,
                                    group=t2orig(g,original_time.periods),
                                    time.period=t2orig(tp,original_time.periods))


      # code if influence function is available
      if ( !is.null(attgt$inf_func) ) {
        # adjust for relative sizes of overall data
        # and groups used for this attgt
        attgt$inf_func <- (n/n1)*attgt$inf_func

        this.inf_func <- rep(0,n)
        this.inf_func[disidx] <- attgt$inf_func
        inffunc[,counter] <- this.inf_func
      }

      #cat("counter: ", counter, "\n")
      counter <- counter+1
      #----------------------------------------------------

    }
  }

  return(list(attgt.list=attgt.list, inffunc=inffunc))

}


#' @title pte
#'
#' @description Main function for computing panel treatment effects
#'
#' @inheritParams pte_params
#' @param setup_pte_fun This is a function that should take in \code{data},
#'  \code{yname} (the name of the outcome variable in \code{data}),
#'  \code{gname} (the name of the group variable),
#'  \code{idname} (the name of the id variable),
#'  and possibly other arguments such as the significance level \code{alp},
#'  the number of bootstrap iterations \code{biters}, and how many clusters
#'  for parallel computing in the bootstrap \code{cl}.  The key thing that
#'  needs to be figured out in this function is which groups and time periods
#'  ATT(g,t) should be computed in.  The function should
#'  return a \code{pte_params} object which contains all of the parameters
#'  passed into the function as well as \code{glist} and \code{tlist} which
#'  should be ordered lists of groups and time periods for ATT(g,t) to be computed.
#'
#'  This function provides also provides a good place for error handling related
#'  to the types of data that can be handled.
#' 
#'  The \code{pte} package contains the function \code{setup_pte} that is
#'  a lightweight function that basically just takes the data, omits
#'  the never-treated group from \code{glist} but includes all other groups
#'  and drops the first time period.  This works in cases where ATT would
#'  be identified in the 2x2 case (i.e., where there are two time periods,
#'  no units are treated in the first period and the identification strategy
#'  "works" with access to a treated and untreated group and untreated
#'  potential outcomes for both groups in the first period) --- for example,
#'  this approach works if DID is the identification strategy.
#'  
#' @param subset_fun This is a function that should take in \code{data},
#'  \code{g} (for group), \code{tp} (for time period), and \code{...}
#'  and be able to return the appropriate \code{data.frame} that can be used
#'  by \code{attgt_fun} to produce ATT(g=g,t=tp).  The data frame should
#'  be constructed using \code{gt_data_frame} in order to guarantee that
#'  it has the appropriate columns that identify which group an observation
#'  belongs to, etc.
#' @param attgt_fun This is a function that should work in the case where
#'  there is a single group and the "right" number of time periods to
#'  recover an estimate of the ATT.  For example, in the contest of
#'  difference in differences, it would need to work for a single group,
#'  find the appropriate comparison group (untreated units), find the right
#'  time periods (pre- and post-treatment), and then recover an estimate
#'  of ATT for that group.  It will be called over and over separately
#'  by groups and by time periods to compute ATT(g,t)'s.
#'
#'  The function needs to work in a very specific way.  It should take in the
#'  arguments: \code{data}, \code{...}.  \code{data} should be constructed
#'  using the function \code{gt_data_frame} which checks to make sure
#'  that \code{data} has the correct columns defined.
#'  \code{...} are additional arguments (such as
#'  formulas for covariates) that \code{attgt_fun} needs.  From these arguments
#'  \code{attgt_fun} must return a list with element \code{ATT} containing the
#'  group-time average treatment effect for that group and that time period.
#'
#'  If \code{attgt_fun} returns an influence function (which should be provided
#'  in a list element named \code{inf_func}), then the code will use the
#'  multiplier bootstrap to compute standard errors for group-time average
#'  treatment effects, an overall treatment effect parameter, and a dynamic
#'  treatment effect parameter (i.e., event study parameter).  If
#'  \code{attgt_fun} does not return an influence function, then the same
#'  objects will be computed using the empirical bootstrap.  This is usually
#'  (perhaps substantially) easier to code, but also will usually be (perhaps
#'  substantially) computationally slower.
#'
#' @param ... extra arguments that can be passed to create the correct subsets
#'  of the data (depending on \code{subset_fun}), to estimate group time
#'  average treatment effects (depending on \code{attgt_fun}), or to
#'  aggregating treatment effects (particularly useful are \code{min_e},
#'  \code{max_e}, and \code{balance_e} arguments to event study aggregations)
#'
#' @return \code{pte_results} object
#'
#' @export
pte <- function(yname,
                gname,
                tname,
                idname,
                data,
                setup_pte_fun,
                subset_fun,
                attgt_fun,
                alp=0.05,
                biters=100,
                cl=1,
                ...) {


  ptep <- setup_pte_fun(yname=yname,
                        gname=gname,
                        tname=tname,
                        idname=idname,
                        data=data,
                        alp=alp,
                        biters=biters,
                        cl=cl,
                        ...)
  
  res <- compute.pte(ptep=ptep,
                     subset_fun=subset_fun,
                     attgt_fun=attgt_fun,
                     ...)

  # check if no influence function provided,
  # if yes, go to alternate code for empirical
  # bootstrap
  if (all(is.na(res$inffunc))) {
    return(panel_empirical_bootstrap(res$attgt.list,
                                     ptep,
                                     subset_fun,
                                     attgt_fun,
                                     ...))
  }  
  att_gt <- process_att_gt(res,ptep)

  #-----------------------------------------------------------------------------
  # aggregate ATT(g,t)'s
  #-----------------------------------------------------------------------------

  # overall
  overall_att <- did::aggte(att_gt, type="group", bstrap=TRUE, cband=TRUE)

  # event study
  # ... for max_e and min_e
  dots <- list(...)
  min_e <- ifelse(is.null(dots$min_e), -Inf, dots$min_e)
  max_e <- ifelse(is.null(dots$max_e), Inf, dots$max_e)
  balance_e <- dots$balance_e
  
  event_study <- did::aggte(att_gt, type="dynamic", bstrap=TRUE, cband=TRUE, min_e=min_e, max_e=max_e, balance_e=balance_e)

  # output
  out <- pte_results(att_gt=att_gt,
                     overall_att=overall_att,
                     event_study=event_study,
                     ptep=ptep)
  
  out
}

