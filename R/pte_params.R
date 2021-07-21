#' @title Setup panel treatment effects parameters
#'
#' @description This is a lightweight (example) function for how to setup
#' the data to be used in the \code{pte} package.
#'
#' \code{setup_pte} takes in information about the structure of \code{data}
#' and returns a \code{pte_params} object.  The key piece of information
#' that is computed by this function is the list of groups and list of
#' time periods where ATT(g,t) should be computed.  In particular, this function
#' omits the never-treated group but includes all other groups and drops the first
#' time period.  This setup is basically geared towards the 2x2 case ---
#' i.e., where ATT could be identified with two periods, a treated and
#' untreated group, and the first period being pre-treatment for both groups.
#' This is the relevant case for DID, but is also relevant for other cases as well.
#' However, for example, if more pre-treatment periods were needed, then this
#' function should be replaced by something else.
#'
#' For code that is written with the idea of being easy-to-use by other
#' researchers, this is a good place to do some error handling / checking
#' that the data is in the correct format, etc.
#'
#' @inheritParams pte_params
#'
#' @return \code{pte_params} object
#'
#' @export
setup_pte <- function(yname,
                      gname,
                      tname,
                      idname,
                      data,
                      alp=0.05,
                      biters=100,
                      cl=1) {
  
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
  
  time.periods <- unique(data$period)
  groups <- unique(data$G)

  # drop never treated group
  groups <- sort(groups)[-1]

  # sort the time periods and drop the first period
  time.periods <- sort(time.periods)[-1]

  params <- pte_params(yname=yname,
                       gname=gname,
                       tname=tname,
                       idname=idname,
                       data=data,
                       glist=groups,
                       tlist=time.periods,
                       alp=alp,
                       biters=biters,
                       cl=cl)

  params
}

#' @title pte_params
#'
#' @description Objects that contain pte parameters
#'
#' @param yname Name of outcome in \code{data}
#' @param gname Name of group in \code{data}
#' @param tname Name of time period in \code{data}
#' @param idname Name of id in \code{data}
#' @param data balanced panel data
#' @param glist list of groups to create group-time average treatment effects
#'  for
#' @param tlist list of time periods to create group-time average treatment
#'  effects for
#' @param alp significance level; default is 0.05
#' @param biters number of bootstrap iterations; default is 100
#' @param cl number of clusters to be used when bootstrapping; default is 1
#'
#' @export
pte_params <- function(yname,
                       gname,
                       tname,
                       idname,
                       data,
                       glist,
                       tlist,
                       alp,
                       biters,
                       cl) {

  obj <- list(yname=yname,
              gname=gname,
              tname=tname,
              idname=idname,
              data=data,
              glist=glist,
              tlist=tlist,
              alp=alp,
              biters=biters,
              cl=cl)

  class(obj) <- "pte_params"

  obj
}
