
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
#>  -0.0323        0.0138    -0.0594     -0.0052 *
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error   [95%  Conf. Band]  
#>          -3   0.0269     0.0145 -0.0102      0.0640  
#>          -2  -0.0050     0.0131 -0.0385      0.0286  
#>          -1  -0.0229     0.0135 -0.0573      0.0116  
#>           0  -0.0201     0.0112 -0.0487      0.0084  
#>           1  -0.0547     0.0168 -0.0977     -0.0117 *
#>           2  -0.1382     0.0372 -0.2331     -0.0433 *
#>           3  -0.1069     0.0405 -0.2104     -0.0034 *
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
#> Warning in compute.aggte(MP = MP, type = type, balance_e = balance_e, min_e
#> = min_e, : Simultaneous conf. band is somehow smaller than pointwise one
#> using normal approximation. Since this is unusual, we are reporting pointwise
#> confidence intervals

summary(covid_res)
#> 
#> Overall ATT:  
#>      ATT    Std. Error     [ 95%  Conf. Int.] 
#>  14.8882       82.1318  -146.0872    175.8635 
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error     [95%  Conf. Band] 
#>         -10  -3.7266     3.0668  -11.6745      4.2212 
#>          -9   2.6607     1.4412   -1.0743      6.3957 
#>          -8   0.8290     2.2273   -4.9432      6.6011 
#>          -7   5.2843     2.2943   -0.6615     11.2302 
#>          -6   2.8555     2.1154   -2.6268      8.3378 
#>          -5   1.3589     4.6187  -10.6108     13.3286 
#>          -4   0.3294     3.7885   -9.4889     10.1476 
#>          -3  -4.2227     5.2106  -17.7262      9.2809 
#>          -2  -3.8447     2.6474  -10.7058      3.0163 
#>          -1  -0.2234     3.8307  -10.1509      9.7041 
#>           0 -10.8156     9.4804  -35.3848     13.7535 
#>           1 -13.7998    10.8085  -41.8108     14.2112 
#>           2  -7.8432    11.9213  -38.7380     23.0517 
#>           3  -4.5541    11.2907  -33.8148     24.7065 
#>           4  -3.5368    16.2044  -45.5319     38.4583 
#>           5   8.5221    13.5634  -26.6284     43.6726 
#>           6   1.1140    16.2143  -40.9066     43.1346 
#>           7   6.6384    18.8463  -42.2031     55.4800 
#>           8   7.1288    28.0897  -65.6678     79.9254 
#>           9  10.8758    32.5480  -73.4749     95.2266 
#>          10  17.5057    31.5339  -64.2169     99.2283 
#>          11  40.8318    40.1220  -63.1476    144.8111 
#>          12  48.6134    47.1346  -73.5395    170.7663 
#>          13  52.4228    46.6190  -68.3939    173.2395 
#>          14  50.2000    57.7241  -99.3964    199.7964 
#>          15  68.2960    70.4012 -114.1541    250.7462 
#>          16  44.7305    84.2624 -173.6421    263.1031 
#>          17  61.4670    76.5716 -136.9742    259.9081 
#>          18  50.4635    92.6913 -189.7533    290.6803 
#>          19  47.3392   121.7477 -268.1794    362.8578 
#>          20  28.6326   134.8792 -320.9172    378.1825 
#>          21   4.3445   165.0635 -423.4302    432.1193 
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

## Example 3: Empirical Bootstrap

The code above used the multiplier bootstrap. The great thing about the
multiplier bootstrap is that it’s fast. But in order to use it, you have
to work out the influence function for the estimator of *ATT(g,t)*.
Although I pretty much always end up doing this, it can be tedious, and
it can be nice to get a working version of the code for a project going
before working out the details on the influence function.

The `pte` package can be used with the empirical bootstrap. There are a
few limitations. First, it’s going to be substantially slower. Second,
this code just reports pointwise standard errors. This basically is set
up to fit into my workflow, and I see this as a way to get preliminary
results.

Let’s demonstrate it. To do this, consider the same setup as in Example
1, but where no influence function is returned. Let’s write the code for
this:

``` r
# did with no influence function
did_attgt_noif <- function(gt_data, xformla, ...) {

  # call original function
  did_gt <- did_attgt(gt_data, xformla, ...)

  # remove influence function
  did_gt$inf_func <- NULL

  did_gt
}
```

Now, we can show the same sorts of results as above

``` r
did_res_noif <- pte(yname="lemp",
                    gname="first.treat",
                    tname="year",
                    idname="countyreal",
                    data=mpdta,
                    subset_fun=two_by_two_subset,
                    attgt_fun=did_attgt_noif, #this is only diff.
                    xformla=~lpop) 

summary(did_res_noif)
#> 
#> Overall ATT:  
#>      ATT    Std. Error     [ 95%  Conf. Int.]  
#>  -0.0323        0.0123    -0.0563     -0.0082 *
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error [95% Pointwise  Conf. Band]  
#>          -3   0.0269     0.0124          0.0026      0.0513 *
#>          -2  -0.0050     0.0122         -0.0288      0.0189  
#>          -1  -0.0229     0.0124         -0.0472      0.0015  
#>           0  -0.0201     0.0117         -0.0430      0.0027  
#>           1  -0.0547     0.0171         -0.0883     -0.0211 *
#>           2  -0.1382     0.0357         -0.2081     -0.0683 *
#>           3  -0.1069     0.0365         -0.1785     -0.0353 *
#> ---
#> Signif. codes: `*' confidence band does not cover 0
ggpte(did_res_noif)
```

![](man/figures/README-unnamed-chunk-8-1.png)<!-- -->
