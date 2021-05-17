BMisc::source_all("~/Dropbox/pte/R/")

library(did)
data(mpdta)
pte(yname="lemp",
    gname="first.treat",
    tname="year",
    idname="countyreal",
    data=mpdta,
    subset_fun=two_by_two_subset,
    attgt_fun=covid_attgt,
    xformla=~1) 
