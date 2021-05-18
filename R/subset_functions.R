#' @title two_by_two_subset
#'
#' @description A function for computing a 2x2 subset of original data.
#'   This is the subset with post treatment periods separately for the
#'   treated group and comparison group and pre-treatment periods in the period
#'   immediately before the treated group became treated.
#'
#' @param data the full dataset
#' @param g the current group
#' @param tp the current time period
#' @param control_group whether to use "notyettreated" (default) or
#'  "nevertreated"
#' @param ... extra arguments to get the subset correct
#'
#' @return list that contains correct subset of data, \code{n1}
#'  number of observations
#'  in this subset, and \code{disidx} a vector of the correct ids for this
#'  subset.
#'
#' @export
two_by_two_subset <- function(data, g, tp, control_group="notyettreated", ...) {
  
  # get the correct "base" period for this group
  # (subtract 2 to avoid anticipation)
  main.base.period <- g - 1

  #----------------------------------------------------
  # if it's a pre-treatment time period (used for the
  # pre-test, we need to adjust the base period)

  # group not treated yet
  if (tp < g) {
    # move two periods before
    base.period <- tp - 1
  } else {
    # this is a post-treatment period
    base.period <- main.base.period
  }
  #----------------------------------------------------

  #----------------------------------------------------
  # collect the right subset of the data 

  # get group g and not-yet-treated group
  if (control_group == "notyettreated") {
    this.data <- subset(data, G==g | G>tp | G==0)
  } else {
    # use never treated group
    this.data <- subset(data, G==g | G==0)
  }

  # get current period and base period data
  this.data <- subset(this.data, period==tp | period==base.period)
  
  # variable to keep track of pre/post periods
  this.data$name <- ifelse(this.data$period==tp, "post", "pre")

  # variable to indicate local treatment status
  this.data$D <- 1*(this.data$G==g)
  
  # make this.data into gt_data_frame object
  this.data <- gt_data_frame(this.data)
  
  # number of observations used for this (g,t)
  n1 <- length(unique(this.data$id))
  disidx <- unique(data$id) %in% unique(this.data$id)

  list(gt_data=this.data, n1=n1, disidx=disidx)
}
