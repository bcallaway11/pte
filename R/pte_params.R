#' @title Example setup panel treatment effects parameters
#'
#' @description This is a lightweight (example) function for how to setup
#' the data to be used in the \code{pte} package.
#'
#' \code{setup_pte_basic} takes in information about the structure of \code{data}
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
setup_pte_basic <- function(yname,
                            gname,
                            tname,
                            idname,
                            data,
                            cband = TRUE,
                            alp = 0.05,
                            boot_type = "multiplier",
                            gt_type = "att",
                            ret_quantile = 0.5,
                            biters = 100,
                            cl = 1,
                            ...) {
  data <- as.data.frame(data)

  # setup data
  G <- data[, gname]
  id <- data[, idname]
  period <- data[, tname]

  data$G <- G
  data$id <- id
  n <- length(unique(data$id))
  data$period <- period
  data$Y <- data[, yname]

  time.periods <- unique(data$period)
  groups <- unique(data$G)

  # drop never treated group
  groups <- sort(groups)[-1]

  # sort the time periods and drop the first period
  time.periods <- sort(time.periods)[-1]

  params <- pte_params(
    yname = yname,
    gname = gname,
    tname = tname,
    idname = idname,
    data = data,
    glist = groups,
    tlist = time.periods,
    cband = cband,
    alp = alp,
    boot_type = boot_type,
    gt_type = gt_type,
    ret_quantile = ret_quantile,
    biters = biters,
    cl = cl
  )

  params
}


#' @title Setup panel treatment effects parameters
#'
#' @description This is a function for how to setup
#' the data to be used in the \code{pte} package.
#'
#' The \code{setup_pte} function builds on \code{setup_pte_basic} and
#' attempts to provide a general purpose function (with error handling)
#' to arrange the data in a way that can be processed by \code{subset_fun}
#' and \code{attgt_fun} in the next steps.
#'
#' @inheritParams setup_pte
#'
#' @return \code{pte_params} object
#'
#' @export
setup_pte <- function(yname,
                      gname,
                      tname,
                      idname,
                      data,
                      required_pre_periods = 1,
                      anticipation = 0,
                      base_period = "varying",
                      cband = TRUE,
                      alp = 0.05,
                      boot_type = "multiplier",
                      weightsname = NULL,
                      gt_type = "att",
                      ret_quantile = 0.5,
                      biters = 100,
                      cl = 1,
                      ...) {
  data <- as.data.frame(data)

  # setup data
  G <- data[, gname]
  id <- data[, idname]
  period <- data[, tname]
  if (is.null(weightsname)) {
    .w <- rep(1, nrow(data))
  } else {
    .w <- data[, weightsname]
  }

  data$G <- G
  data$id <- id
  n <- length(unique(data$id))
  # data$original_period <- period
  # data$original_group <- G
  data$Y <- data[, yname]
  data$.w <- .w

  time.periods <- unique(period)
  groups <- unique(data$G)

  # drop never treated group
  groups <- sort(groups)[-1]

  # do some recoding to make sure time periods are 1 unit apart
  # and then put these back together at the end
  # (this probably won't work for unequally spaced periods)
  original_time.periods <- time.periods
  original_groups <- groups

  # get new time periods / groups
  time.periods <- sapply(original_time.periods,
    BMisc::orig2t,
    original_time.periods = original_time.periods
  )
  groups <- sapply(original_groups,
    BMisc::orig2t,
    original_time.periods = original_time.periods
  )

  # put the period in new time scale
  data$period <- sapply(period,
    BMisc::orig2t,
    original_time.periods = original_time.periods
  )
  data$G <- sapply(G,
    BMisc::orig2t,
    original_time.periods = original_time.periods
  )

  t_adjust <- 0
  # sort the time periods and drop the first
  # \code{required_pre_periods} time periods
  # these are the ones we loop over below
  if (base_period == "universal") {
    time.periods <- sort(time.periods)
    groups <- groups[groups %in% time.periods[-1]]
    # account for anticipation
    groups <- groups[groups >= (min(time.periods + 1) + anticipation)]
    # drop early treated groups
    data <- data[!(data$G %in% seq(1, required_pre_periods + anticipation)), ]
  } else {
    time.periods <- sort(time.periods)[-seq(1, required_pre_periods)]
    groups <- groups[groups %in% time.periods]
    # account for anticipation
    groups <- groups[groups >= (min(time.periods) + anticipation)]
    # drop early treated groups
    data <- data[!(data$G %in% seq(1, required_pre_periods + anticipation)), ]
  }



  params <- pte_params(
    yname = yname,
    gname = gname,
    tname = tname,
    idname = idname,
    data = data,
    glist = groups,
    tlist = time.periods,
    cband = cband,
    alp = alp,
    boot_type = boot_type,
    anticipation = anticipation,
    base_period = base_period,
    weightsname = weightsname,
    gt_type = gt_type,
    ret_quantile = ret_quantile,
    biters = biters,
    cl = cl
  )

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
#' @param boot_type which type of bootstrap to use
#' @param anticipation how many periods before the treatment actually takes
#'  place that it can have an effect on outcomes
#' @param control_group Which group is used as the comparison group.  For
#'  backwards compatibility, the default choice is "notyettreated", but
#'  different estimation strategies can implement their own choices
#'  for the control group
#' @param gt_type which type of group-time effects are computed.
#'  For backward compatiblity, the default choice is "att"
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
                       cband,
                       alp,
                       boot_type,
                       anticipation = NULL,
                       base_period = NULL,
                       weightsname = NULL,
                       control_group = "notyettreated",
                       gt_type = "att",
                       ret_quantile = 0.5,
                       biters,
                       cl) {
  obj <- list(
    yname = yname,
    gname = gname,
    tname = tname,
    idname = idname,
    data = data,
    glist = glist,
    tlist = tlist,
    cband = cband,
    alp = alp,
    boot_type = boot_type,
    anticipation = anticipation,
    control_group = control_group,
    base_period = base_period,
    weightsname = weightsname,
    gt_type = gt_type,
    ret_quantile = ret_quantile,
    biters = biters,
    cl = cl
  )

  class(obj) <- "pte_params"

  obj
}
