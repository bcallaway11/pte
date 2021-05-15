compute.pte <- function(ptep,
                        attgt_fun,
                        ...) {

}



pte <- function(yname,
                gname,
                tname,
                idname,
                data,
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
                     attgt_fun=attgt_fun,
                     ...)
               
  
  
  att_gt <- process_att_gt(res,dp)
  
  return(att_gt)

}
