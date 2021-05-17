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

#' @title summary.group_time_att
#'
#' @description prints a summary of a \code{group_time_att} object
#'
#' @param object an \code{group_time_att} object
#' @param ... extra arguments
#'
#' @export
summary.group_time_att <- function(object, ...) {
  group_time_att_obj <- object

  # group time average treatment effects
  cat("Group-Time Average Treatment Effects:\n")

  cband_text1a <- paste0(100*(1-group_time_att_obj$alp),"% ")
  cband_text1b <- ifelse(group_time_att_obj$DIDparams$bstrap,
                         ifelse(group_time_att_obj$DIDparams$cband, "Simult. ", "Pointwise "),
                         "Pointwise ")
  cband_text1 <- paste0("[", cband_text1a, cband_text1b)

  cband_lower <- group_time_att_obj$att - group_time_att_obj$c*group_time_att_obj$se
  cband_upper <- group_time_att_obj$att + group_time_att_obj$c*group_time_att_obj$se

  sig <- (cband_upper < 0) | (cband_lower > 0)
  sig[is.na(sig)] <- FALSE
  sig_text <- ifelse(sig, "*", "")

  out <- cbind.data.frame(group_time_att_obj$group, group_time_att_obj$t, group_time_att_obj$att, group_time_att_obj$se, cband_lower, cband_upper)
  out <- round(out,4)
  out <- cbind.data.frame(out, sig_text)


  colnames(out) <- c("Group", "Time", "ATT(g,t)","Std. Error", cband_text1, "Conf. Band]", "")
  print(out, row.names=FALSE)
  cat("---\n")
  cat("Signif. codes: `*' confidence band does not cover 0")
  cat("\n\n")

  # report pre-test
  if (!is.null(group_time_att_obj$Wpval)) {
    cat("P-value for pre-test of parallel trends assumption:  ")
    cat(as.character(group_time_att_obj$Wpval))
    cat("\n")
  }

}

#' @title print.group_time_att
#'
#' @description prints value of a \code{group_time_att} object
#'
#' @param x a \code{group_time_att} object
#' @param ... extra arguments
#'
#' @export
print.group_time_att <- function(x,...) {
  summary.group_time_att(x,...)
}
