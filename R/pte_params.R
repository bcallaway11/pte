#' @title setup_pte
#'
#' @desc Setup panel treatment effects parameters
#'
#' @inheritParams pte_params
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
#' @desc Objects that contain pte parameters
#'
#' @param yname Name of outcome in \code{data}
#' @param gname Name of group in \code{data}
#' @param tname Name of time period in \code{data}
#' @param idname Name of id in \code{data}
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
