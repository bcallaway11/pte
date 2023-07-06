#------------------------------------------------------------------------
#  Some tests for `did`
#------------------------------------------------------------------------

library(did)

test_that("did basics", {
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
  
  expect_equal(res$overall_att$overall.att, 1, tolerance=.5)
  dyn_idx <- res$event_study$egt == 0
  expect_equal(res$event_study$att.egt[dyn_idx], 1, tolerance=0.5)
  
  # compare to results from did package
  cs_res <- did::att_gt(yname="Y",
                        gname="G",
                        tname="period",
                        idname="id",
                        data=data,
                        xformla=~X)
  
  cs_overall <- did::aggte(cs_res, type="group")$overall.att
  expect_equal(res$overall_att$overall.att, cs_overall)
  cs_dyn <- did::aggte(cs_res, type="dynamic")$att.egt[dyn_idx]
  expect_equal(res$event_study$att.egt[dyn_idx], cs_dyn)
c})