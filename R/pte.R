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
  G <- data[,gname]
  id <- data[,idname]
  period <- data[,tname]

  data$G <- G
  data$id <- id
  n <- length(unique(data$id))
  data$period <- period
  data$Y <- data[,yname]

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
  inffunc <- matrix(data=0, nrow=n, ncol=nG*(nT))
  
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

      if ( !is.null(attgt$inf_func) ) {
        # adjust for relative sizes of overall data
        # and groups used for this attgt
        attgt$inf_func <- (n/n1)*attgt$inf_func
        # save results
        attgt.list[[counter]] <- list(att=attgt$attgt, group=g, time.period=tp)

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
#'  arguments: \code{data}, \code{group}, \code{time.periods}, \code{glist},
#'  \code{tlist}, and \code{...}.  \code{...} are additional arguments (such as
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
pte <- function(yname,
                gname,
                tname,
                idname,
                data,
                subset_fun,
                attgt_fun,
                alp=0.05,
                biters=100,
                cl=1,
                ...) {


  ptep <- setup_pte(yname=yname,
                    gname=gname,
                    tname=tname,
                    idname=idname,
                    data=data,
                    alp=alp,
                    biters=biters,
                    cl=cl)

  res <- compute.pte(ptep=ptep,
                     subset_fun=subset_fun,
                     attgt_fun=attgt_fun,
                     ...)


  
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
