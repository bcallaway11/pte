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

test_that("empirical bootstrap", {
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
             xformla=~X,
             boot_type="empirical",
             biters=10) # just checking that this runs
  
  expect_equal(res$overall_att$overall.att, 1)
  message("this is failing because the names are not correct on the returns 
          for the empirical bootstrap case")
})

test_that("periods that look like years works ok and unbalanced groups", {
  data(mpdta)
  res <- pte(
    yname = "lemp",
    gname = "first.treat",
    tname = "year",
    idname = "countyreal",
    data = mpdta,
    setup_pte_fun = setup_pte,
    subset_fun = two_by_two_subset,
    attgt_fun = did_attgt,
    xformla =  ~ lpop
  )
  # this is to test if summary is working // had issues with ife version of this
  expect_equal(summary(res)$overall_att$overall_att,-0.0323)
  dyn_idx <- summary(res)$event_study[, "Event Time"] == 0
  expect_equal(summary(res)$event_study$Estimat[dyn_idx],-0.0201)
  
  #------------------------------------------------------------------------
  #  case where the group variable is named G
  #------------------------------------------------------------------------
  data(mpdta)
  mpdta$G <- mpdta$first.treat
  res <- pte(
    yname = "lemp",
    gname = "G",
    tname = "year",
    idname = "countyreal",
    data = mpdta,
    setup_pte_fun = setup_pte,
    subset_fun = two_by_two_subset,
    attgt_fun = did_attgt,
    xformla =  ~ lpop
  )
  # this is to test if summary is working // had issues with ife version of this
  expect_equal(summary(res)$overall_att$overall_att,-0.0323)
  dyn_idx <- summary(res)$event_study[, "Event Time"] == 0
  expect_equal(summary(res)$event_study$Estimat[dyn_idx],-0.0201)
})

test_that("no formula for covariates is ok", {
  sp <- did::reset.sim()
  data <- did::build_sim_dataset(sp)
  
  res <- pte(yname="Y",
             gname="G",
             tname="period",
             idname="id",
             data=data,
             setup_pte_fun=setup_pte,
             subset_fun=two_by_two_subset,
             attgt_fun=did_attgt)
  
  expect_equal(res$overall_att$overall.att, 1, tolerance=.5)
})