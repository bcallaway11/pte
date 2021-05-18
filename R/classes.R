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

  # might eventually drop this, but add a few things to get access
  # to aggregations from did package
  out$DIDparams <- out$ptep
  out$DIDparams$panel <- TRUE
  out$DIDparams$data$w <- 1
  out$c <- out$crit_val
  out$inffunc <- out$inf_func


  class(out) <- c("group_time_att","MP")

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


#' @title pte_results
#'
#' @description class for holding pte results
#'
#' @param att_gt attgt results
#' @param overall_att overall_att results
#' @param event_study event_study results
#'
#' @export
pte_results <- function(att_gt,
                        overall_att,
                        event_study,
                        ptep) {

  out <- list(att_gt=att_gt,
              overall_att=overall_att,
              event_study=event_study,
              ptep=ptep)

  class(out) <- "pte_results"

  out
}


#' @title summary.pte_results
#'
#' @description A function to summarize \code{pte} results.
#'
#' @param object an \code{pte_results} object
#' @param ... other arguments
#'
#' @export
summary.pte_results <- function(object, ...) {

  overall_att <- object$overall_att$overall.att
  overall_se <- object$overall_att$overall.se

  event_study <- object$event_study
  event_study_att_egt <- object$event_study$att.egt
  event_study_se <- object$event_study$se.egt
  
  # overall estimates
  alp <- object$ptep$alp
  pointwise_cval <- qnorm(1-alp/2)
  overall_cband_upper <- overall_att + pointwise_cval*overall_se
  overall_cband_lower <- overall_att - pointwise_cval*overall_se
  out1 <- cbind.data.frame(overall_att, overall_se, overall_cband_lower, overall_cband_upper)
  out1 <- round(out1, 4)
  overall_sig <- (overall_cband_upper < 0) | (overall_cband_lower > 0)
  overall_sig[is.na(overall_sig)] <- FALSE
  overall_sig_text <- ifelse(overall_sig, "*", "")
  out1 <- cbind.data.frame(out1, overall_sig_text)
  

  # Event study
  # header
  bstrap <- object$DIDparams$bstrap
  cband <- object$DIDparams$cband
  cband_text1a <- paste0(100*(1-alp),"% ")
  cband_text1b <- ifelse(bstrap,
                         ifelse(cband, "Simult. ", "Pointwise "),
                         "Pointwise ")
  cband_text1 <- paste0("[", cband_text1a, cband_text1b)

  cband_lower <- event_study$att.egt - event_study$crit.val.egt*event_study$se.egt
  cband_upper <- event_study$att.egt + event_study$crit.val.egt*event_study$se.egt

  sig <- (cband_upper < 0) | (cband_lower > 0)
  sig[is.na(sig)] <- FALSE
  sig_text <- ifelse(sig, "*", "")

  out2 <- cbind.data.frame(event_study$egt, event_study$att.egt, event_study$se.egt, cband_lower, cband_upper)
  out2 <- round(out2, 4)
  out2 <- cbind.data.frame(out2, sig_text)

  c1name <- "Event Time"
  colnames(out2) <- c(c1name, "Estimate","Std. Error", cband_text1, "Conf. Band]", "")
  
  out <- list(overall_att=out1,
              event_study=out2,
              alp=alp,
              bstrap=bstrap,
              cband=cband)

  class(out) <- "summary.pte_results"

  out
}

#' @title print.pte_results
#'
#' @description prints value of a \code{pte_results} object
#'
#' @param x a \code{pte_results} object
#' @param ... extra arguments
#'
#' @export
print.pte_results <- function(x,...) {
  summary.pte_results(x,...)
}

print.summary.pte_results <- function(object,...) {
  out1 <- object$overall_att
  out2 <- object$event_study
  alp <- object$alp
  
  # overall att
  cat("\n")
  cat("Overall ATT:  \n")
  colnames(out1) <- c("ATT","   Std. Error", paste0("    [ ",100*(1-alp),"% "), "Conf. Int.]","")
  print(out1, row.names=FALSE)
  cat("\n\n")

  # event study
  c1name <- "Event time"; cat("Dynamic Effects:")
  cat("\n")

  print(out2, row.names=FALSE, justify = "centre")
  
  cat("---\n")
  cat("Signif. codes: `*' confidence band does not cover 0")
  cat("\n\n")
}

#' @title attgt_if
#'
#' @description Class for holding group-time average treatment effects
#'  along with their influence function
#'
#' @param attgt group-time average treatment effect
#' @param inf_func influence function
#'
#' @return attgt_if object
#'
#' @export
attgt_if <- function(attgt, inf_func) {

  out <- list(attgt=attgt$ATT, inf_func=attgt$att.inf.func)
  class(out) <- "attgt_if"
  out
}
