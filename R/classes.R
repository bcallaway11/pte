#' @title group_time_att
#'
#' @description group_time_att class
#'
#' @return object of class \code{group_time_att}
#'
#' @export
group_time_att <- function(group,
                           time.period,
                           att,
                           V_analytical,
                           se,
                           crit_val,
                           inf_func,
                           n,
                           W,
                           Wpval,
                           alp,
                           ptep) {

  out <- list(group=group,
              t=time.period,
              att=att,
              V_analytical=V_analytical,
              se=se,
              crit_val=crit_val,
              inf_func=inf_func,
              n=n,
              W=W,
              Wpval=Wpval,
              alp=alp,
              ptep=ptep)

  class(out) <- "group_time_att"

  out
}
