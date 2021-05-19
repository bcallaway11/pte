
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Panel Treatment Effects (pte) Package

The `pte` package compartmentalizes the steps needed to implement
estimators of group-time average treatment effects (and their
aggregations) in order to make it easier to apply the same sorts of
arguments outside of their “birthplace” in the literature on
difference-in-differences.

This code is lightweight, only works for balanced panels, and has
minimal error checking. That said, it should be useful projects that
build on top of group-time average treatment effects in order to deliver
estimates of causal effects in panel data settings.

Here are a couple of examples:

## Example 1: Difference in differences

The [`did` package](https://bcallaway11.github.io/did/), which is based
on [Callaway and Sant’Anna
(2021)](https://doi.org/10.1016/j.jeconom.2020.12.001), includes
estimates of group-time average treatment effects, \[ATT(g,t)\], based
on a difference in differences identification strategy. The following
example demonstrates that it is easy to compute group-time average
treatment effects using difference in differences using the `pte`
package. \[*Note:* This is definitely not the recommended way of doing
this (as there is very little error handling, etc. here), but it is
rather a proof of concept.\]

``` r
library(did)
data(mpdta)
did_res <- pte(yname="lemp",
           gname="first.treat",
           tname="year",
           idname="countyreal",
           data=mpdta,
           subset_fun=two_by_two_subset,
           attgt_fun=did_attgt,
           xformla=~lpop) 

summary(did_res)
#> 
#> Overall ATT:  
#>      ATT    Std. Error     [ 95%  Conf. Int.]  
#>  -0.0323        0.0116    -0.0551     -0.0094 *
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error   [95%  Conf. Band]  
#>          -3   0.0269     0.0124 -0.0043      0.0581  
#>          -2  -0.0050     0.0107 -0.0321      0.0221  
#>          -1  -0.0229     0.0148 -0.0603      0.0146  
#>           0  -0.0201     0.0130 -0.0528      0.0126  
#>           1  -0.0547     0.0158 -0.0947     -0.0148 *
#>           2  -0.1382     0.0436 -0.2484     -0.0280 *
#>           3  -0.1069     0.0294 -0.1812     -0.0326 *
#> ---
#> Signif. codes: `*' confidence band does not cover 0
ggpte(did_res)
```

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->

What’s most interesting here, is that the only “new” code that needs to
be writte is in the `did_attgt` function [available
here](https://github.com/bcallaway11/pte/blob/master/R/attgt_functions.R).
You will see that this is a very small amount of code.

## Example 2: Policy Evaluation during a Pandemic

As a next example, consider trying to estimate effects of Covid-19
related policies during a pandemic (the estimates below are for the
effects of state-leve shelter-in-place orders during the early part of
the pandemic).

[Callaway and Li (2021)](https://arxiv.org/abs/2105.06927) argue that a
particular unconfoundedness-type strategy is more appropriate in this
context than DID-type strategies due to the spread of Covid-19 cases
being highly nonlinear. However, they still deal with the challenge of
variation in treatment timing. Therefore, it is still useful to think
about group-time average treatment effects, but the DID strategy should
be replaced with their particular unconfoundedness type assumption.

The `pte` package is particularly useful here.

``` r
# formula for covariates
xformla <- ~ current + I(current^2) + region + totalTestResults
```

``` r
covid_res <- pte(yname="positive",
                 gname="group",
                 tname="time.period",
                 idname="state_id",
                 data=covid_data2,
                 subset_fun=two_by_two_subset,
                 attgt_fun=covid_attgt,
                 xformla=xformla,
                 max_e=21,
                 min_e=-10) 

summary(covid_res)
#> 
#> Overall ATT:  
#>      ATT    Std. Error     [ 95%  Conf. Int.] 
#>  14.8882       83.0773  -147.9403    177.7166 
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error     [95%  Conf. Band] 
#>         -10  -3.7266     2.6367  -10.0604      2.6072 
#>          -9   2.6607     1.5072   -0.9599      6.2812 
#>          -8   0.8290     2.3897   -4.9116      6.5695 
#>          -7   5.2843     2.5049   -0.7329     11.3016 
#>          -6   2.8555     1.7492   -1.3464      7.0574 
#>          -5   1.3589     3.7461   -7.6401     10.3579 
#>          -4   0.3294     4.5347  -10.5638     11.2226 
#>          -3  -4.2227     4.8133  -15.7852      7.3399 
#>          -2  -3.8447     2.7544  -10.4614      2.7720 
#>          -1  -0.2234     4.1382  -10.1641      9.7174 
#>           0 -10.8156     8.7618  -31.8632     10.2319 
#>           1 -13.7998    13.8698  -47.1178     19.5181 
#>           2  -7.8432    12.2763  -37.3333     21.6469 
#>           3  -4.5541    10.1365  -28.9040     19.7957 
#>           4  -3.5368     9.9319  -27.3952     20.3215 
#>           5   8.5221    10.7019  -17.1859     34.2301 
#>           6   1.1140    16.3065  -38.0574     40.2854 
#>           7   6.6384    18.3346  -37.4048     50.6817 
#>           8   7.1288    21.9389  -45.5727     59.8303 
#>           9  10.8758    32.3153  -66.7518     88.5034 
#>          10  17.5057    33.7126  -63.4784     98.4898 
#>          11  40.8318    32.9833  -38.4004    120.0639 
#>          12  48.6134    41.6148  -51.3534    148.5802 
#>          13  52.4228    68.2881 -111.6184    216.4640 
#>          14  50.2000    59.8736  -93.6280    194.0281 
#>          15  68.2960    64.3350  -86.2491    222.8411 
#>          16  44.7305    77.8088 -142.1814    231.6424 
#>          17  61.4670   111.9387 -207.4313    330.3652 
#>          18  50.4635   111.2992 -216.8987    317.8257 
#>          19  47.3392   124.5669 -251.8945    346.5728 
#>          20  28.6326   123.8868 -268.9673    326.2326 
#>          21   4.3445   130.7164 -309.6615    318.3506 
#> ---
#> Signif. codes: `*' confidence band does not cover 0
ggpte(covid_res) + ylim(c(-1000,1000))
```

![](man/figures/README-unnamed-chunk-6-1.png)<!-- -->

What’s most interesting is just how little code needs to be written
here. The only new code required is the `ppe::covid_attgt` function
which is [available
here](https://github.com/bcallaway11/ppe/blob/master/R/covid_attgt.R),
and, as you can see, this is very simple.
