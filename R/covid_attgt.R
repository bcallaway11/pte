#' @title covid_attgt
#'
#' @description takes groups and time periods and turns it into
#'  an estimate of a group time average treatment effect
#'  and a corresponding influence function
covid_attgt <- function(g, tp, data, xformla) {
  
  this.data <- data
  # handle covariates
  # for outcome regression, get pre-treatment values
  Xor <- model.frame(xformla, data=subset(this.data,name=="pre"))
  # for pscore, get pre-treatment values
  Xpscore <- model.frame(xformla, data=subset(this.data,name=="pre"))

  # convert two period panel into one period
  this.data_outcomes <- tidyr::pivot_wider(this.data[,c("G","id","period","name","Y")], id_cols=c(id, G),
                                           names_from=c(name),
                                           values_from=c(Y))

  # merge outcome and covariate data
  this.dataOR <- cbind.data.frame(this.data_outcomes, Xor)
  this.dataPscore <- cbind.data.frame(this.data_outcomes, Xpscore)

  # create local treated variable
  this.dataPscore$D <- 1*(this.dataPscore$G==g)

  # run the outcome regression,
  # note: this is regression of change on covariates (which makes sense given structure of model in paper)
  or_formla <- BMisc::toformula("I(post-pre)", BMisc::rhs.vars(xformla))
  outcome_regression <- lm(or_formla, data=this.dataOR)#data=subset(this.dataOR, (G > g | G == 0)))
  # and get predicted values
  # note: add back in pre-treatment outcomes so that this
  # is prediction for the level of outcomes in post-treatment
  # period
  or_preds <- predict(outcome_regression, newdata=this.dataOR) + this.dataOR$pre

  # run the pscore regression
  pscore_formla <- BMisc::toformula("D", c(BMisc::rhs.vars(xformla)))

  # warn about units violating common support
  # keepers provides a way to drop problematic pscores,
  # but current version just warns
  keepers <- rep(TRUE, nrow(this.dataPscore))
  #if (!boot_iter) {
  ##  keepers <- pscore_preds < .99
  ##   droppers <- pscore_preds > .99
  ##   if (sum(droppers) > 0) {
  ##     warn_message <- paste0(sum(droppers), " observations have pscores greater than .99 with ids: ", paste0(this.dataPscore[droppers==1,]$id, collapse=", "), ". These likely indicate violations of the overlap condition and you should consider dropping these observations.")
  ##     warning(warn_message)
  ##   }
  ## }

  # treatment dummy variable
  D <- this.dataPscore$D[keepers]
  #pscore_preds <- pscore_preds[keepers]

  Y_post <- this.dataOR$post[keepers]

  
  this.dataPscore <- droplevels(this.dataPscore)
  attgt <- DRDID::drdid_panel(y1=Y_post,
                              y0=rep(0,length(Y_post)),
                              D=D,
                              covariates=model.matrix(pscore_formla,
                                                      data=this.dataPscore),
                              inffunc=TRUE)

  list(attgt=attgt$ATT, inf_func=attgt$att.inf.func)
}
