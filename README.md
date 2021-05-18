
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

*Note:* This is not the recommended way of doing this (as there is very
little error handling, etc. here), but it is rather a proof of concept.

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
#>  -0.0323        0.0131    -0.0579     -0.0066 *
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error   [95%  Conf. Band]  
#>          -3   0.0269     0.0108 -0.0030      0.0569  
#>          -2  -0.0050     0.0120 -0.0384      0.0285  
#>          -1  -0.0229     0.0165 -0.0687      0.0230  
#>           0  -0.0201     0.0115 -0.0522      0.0119  
#>           1  -0.0547     0.0134 -0.0920     -0.0175 *
#>           2  -0.1382     0.0341 -0.2331     -0.0433 *
#>           3  -0.1069     0.0315 -0.1945     -0.0193 *
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

This comes from [Callaway and Li
(2021)](https://arxiv.org/abs/2105.06927).

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
#>  14.8882        77.553  -137.1129    166.8892 
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error     [95%  Conf. Band]  
#>         -10  -3.7266     3.6125  -12.8736      5.4203  
#>          -9   2.6607     1.7348   -1.7319      7.0533  
#>          -8   0.8290     2.2388   -4.8396      6.4976  
#>          -7   5.2843     1.8953    0.4855     10.0831 *
#>          -6   2.8555     2.0560   -2.3502      8.0613  
#>          -5   1.3589     3.7756   -8.2008     10.9187  
#>          -4   0.3294     3.6367   -8.8787      9.5375  
#>          -3  -4.2227     4.8141  -16.4120      7.9667  
#>          -2  -3.8447     3.2166  -11.9893      4.2998  
#>          -1  -0.2234     3.6594   -9.4889      9.0421  
#>           0 -10.8156     9.9308  -35.9604     14.3291  
#>           1 -13.7998    15.4408  -52.8962     25.2965  
#>           2  -7.8432     8.5558  -29.5064     13.8201  
#>           3  -4.5541    12.7291  -36.7843     27.6760  
#>           4  -3.5368    10.5843  -30.3363     23.2627  
#>           5   8.5221    13.0787  -24.5933     41.6375  
#>           6   1.1140    22.1987  -55.0931     57.3211  
#>           7   6.6384    22.9861  -51.5625     64.8394  
#>           8   7.1288    24.8603  -55.8177     70.0753  
#>           9  10.8758    32.6271  -71.7363     93.4880  
#>          10  17.5057    35.6081  -72.6543    107.6657  
#>          11  40.8318    45.3079  -73.8880    155.5516  
#>          12  48.6134    48.9781  -75.3996    172.6264  
#>          13  52.4228    69.6198 -123.8551    228.7007  
#>          14  50.2000    58.7475  -98.5490    198.9490  
#>          15  68.2960    73.9277 -118.8895    255.4816  
#>          16  44.7305    63.8885 -117.0356    206.4967  
#>          17  61.4670    81.1021 -143.8841    266.8181  
#>          18  50.4635   120.0314 -253.4569    354.3839  
#>          19  47.3392   105.1978 -219.0224    313.7008  
#>          20  28.6326   120.8400 -277.3351    334.6003  
#>          21   4.3445   151.1525 -378.3746    387.0637  
#> ---
#> Signif. codes: `*' confidence band does not cover 0
ggpte(covid_res) + ylim(c(-1000,1000))
```

![](man/figures/README-unnamed-chunk-6-1.png)<!-- -->

What’s most interesting here is just how little code needs to be written
here. The only new code required is the `ppe::covid_attgt` function
which is [available
here](https://github.com/bcallaway11/ppe/blob/master/R/covid_attgt.R),
and, as you can see, this is very simple.
