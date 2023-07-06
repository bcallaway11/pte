#------------------------------------------------------------------------
#  inference tests for did
#------------------------------------------------------------------------

library(did)
library(pbapply)

cl <- as.numeric(readline("how many clusters would like to use for the inference tests?  "))

test_that("tests for inference", {
  mc_sims <- 100
  rejs <- pblapply(1:mc_sims, function(mc) {
    sp <- did::reset.sim()
    data <- did::build_sim_dataset(sp)
    
    res <- pte(yname="Y",
               gname="G",
               tname="period",
               idname="id",
               data=data,
               setup_pte_fun=setup_pte,
               subset_fun=two_by_two_subset,
               attgt_fun=did_attgt,
               xformla=~X)
    # truth is that att = 1
    tstat <- (res$overall_att$overall.att-1) / res$overall_att$overall.se
    rej <- 1*( abs(tstat) > qnorm(.975))
    rej
  }, cl=cl)
  
  rej_frac <- mean(unlist(rejs))
  
  expect_equal(rej_frac, 0.06, tolerance=.05) # make test fail if reject 0
})