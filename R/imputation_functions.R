# in progress...not sure if we'll actually use this
#' @keywords internal
twfe_imputation <- function(data, ptep) {

  browser()
  yname <- ptep$yname
  idname <- ptep$idname
  tname <- ptep$tname
  gname <- ptep$gname
  
  formla <- paste0(yname, "~", 0)
  formla <- paste0(formla, " | ", idname, " + ", tname)
  formla <- as.formula(formla)

  pre_data <- data[ ( data[,tname] < data[,gname] ) | data[,gname]==0, ]

  twfe_est <- fixest::feols(formla, data=pre_data)

  y0 <- predict(twfe_est, newdata=data)

  y0
}
