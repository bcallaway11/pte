
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
(2021)](https://doi.org/10.1016/j.jeconom.2020.12.001) includes
estimates of group-time average treatment effects, \(ATT(g,t)\), based
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
#> Warning in compute.aggte(MP = MP, type = type, balance_e = balance_e, min_e
#> = min_e, : Simultaneous conf. band is somehow smaller than pointwise one
#> using normal approximation. Since this is unusual, we are reporting pointwise
#> confidence intervals

summary(did_res)
#> 
#> Overall ATT:  
#>      ATT    Std. Error     [ 95%  Conf. Int.]  
#>  -0.0323        0.0126     -0.057     -0.0076 *
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error   [95%  Conf. Band]  
#>          -3   0.0269     0.0124 -0.0048      0.0587  
#>          -2  -0.0050     0.0138 -0.0404      0.0305  
#>          -1  -0.0229     0.0117 -0.0530      0.0072  
#>           0  -0.0201     0.0125 -0.0521      0.0118  
#>           1  -0.0547     0.0174 -0.0994     -0.0101 *
#>           2  -0.1382     0.0382 -0.2362     -0.0402 *
#>           3  -0.1069     0.0366 -0.2008     -0.0130 *
#> ---
#> Signif. codes: `*' confidence band does not cover 0
ggpte(did_res)
```

![](man/figures/README-unnamed-chunk-3-1.png)<!-- --> What’s most
interesting here, is that the only “new” code that needs to be writte is
in the `did_attgt` function [available
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
#>  14.8882       78.5352  -139.0381    168.8144 
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error     [95%  Conf. Band] 
#>         -10  -3.7266     3.9879  -13.9130      6.4598 
#>          -9   2.6607     1.2830   -0.6166      5.9379 
#>          -8   0.8290     1.9977   -4.2738      5.9317 
#>          -7   5.2843     2.6228   -1.4150     11.9837 
#>          -6   2.8555     1.6957   -1.4758      7.1868 
#>          -5   1.3589     3.8776   -8.5456     11.2635 
#>          -4   0.3294     4.0663  -10.0572     10.7160 
#>          -3  -4.2227     4.9141  -16.7747      8.3294 
#>          -2  -3.8447     2.6881  -10.7109      3.0214 
#>          -1  -0.2234     3.4578   -9.0556      8.6089 
#>           0 -10.8156     9.6646  -35.5018     13.8705 
#>           1 -13.7998    14.5361  -50.9292     23.3295 
#>           2  -7.8432    10.1117  -33.6713     17.9850 
#>           3  -4.5541    10.2906  -30.8394     21.7311 
#>           4  -3.5368    12.5854  -35.6835     28.6099 
#>           5   8.5221    12.7553  -24.0586     41.1028 
#>           6   1.1140    18.4808  -46.0912     48.3192 
#>           7   6.6384    18.1989  -39.8469     53.1238 
#>           8   7.1288    21.6655  -48.2112     62.4687 
#>           9  10.8758    35.0682  -78.6985    100.4501 
#>          10  17.5057    30.8734  -61.3541     96.3655 
#>          11  40.8318    43.6360  -70.6272    152.2908 
#>          12  48.6134    46.3820  -69.8597    167.0864 
#>          13  52.4228    63.8458 -110.6579    215.5035 
#>          14  50.2000    64.4851 -114.5137    214.9138 
#>          15  68.2960    72.3489 -116.5042    253.0962 
#>          16  44.7305    91.5393 -189.0875    278.5485 
#>          17  61.4670    88.7371 -165.1934    288.1273 
#>          18  50.4635    99.9847 -204.9265    305.8535 
#>          19  47.3392   114.5371 -245.2219    339.9003 
#>          20  28.6326   142.3240 -334.9042    392.1694 
#>          21   4.3445   130.7575 -329.6480    338.3371 
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
