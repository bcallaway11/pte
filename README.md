
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
#>  -0.0323        0.0107    -0.0532     -0.0113 *
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error   [95%  Conf. Band]  
#>          -3   0.0269     0.0122 -0.0081      0.0619  
#>          -2  -0.0050     0.0118 -0.0388      0.0289  
#>          -1  -0.0229     0.0140 -0.0630      0.0173  
#>           0  -0.0201     0.0111 -0.0519      0.0116  
#>           1  -0.0547     0.0174 -0.1045     -0.0049 *
#>           2  -0.1382     0.0305 -0.2255     -0.0509 *
#>           3  -0.1069     0.0337 -0.2032     -0.0106 *
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
#> Warning in compute.aggte(MP = MP, type = type, balance_e = balance_e, min_e
#> = min_e, : Simultaneous conf. band is somehow smaller than pointwise one
#> using normal approximation. Since this is unusual, we are reporting pointwise
#> confidence intervals

summary(covid_res)
#> 
#> Overall ATT:  
#>      ATT    Std. Error     [ 95%  Conf. Int.] 
#>  14.8882       78.8621  -139.6787     169.455 
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error     [95%  Conf. Band] 
#>         -10  -3.7266     3.8567  -14.1757      6.7224 
#>          -9   2.6607     1.6111   -1.7044      7.0257 
#>          -8   0.8290     2.5378   -6.0467      7.7047 
#>          -7   5.2843     1.9881   -0.1020     10.6707 
#>          -6   2.8555     1.9138   -2.3297      8.0408 
#>          -5   1.3589     3.6701   -8.5845     11.3024 
#>          -4   0.3294     3.6304   -9.5066     10.1653 
#>          -3  -4.2227     4.6770  -16.8943      8.4490 
#>          -2  -3.8447     3.5066  -13.3453      5.6558 
#>          -1  -0.2234     4.7110  -12.9870     12.5402 
#>           0 -10.8156     8.6978  -34.3808     12.7495 
#>           1 -13.7998    13.9226  -51.5209     23.9213 
#>           2  -7.8432    10.1647  -35.3827     19.6964 
#>           3  -4.5541    13.3483  -40.7191     31.6108 
#>           4  -3.5368    10.6750  -32.4591     25.3854 
#>           5   8.5221    14.6706  -31.2256     48.2698 
#>           6   1.1140    18.1890  -48.1660     50.3940 
#>           7   6.6384    24.6226  -60.0725     73.3494 
#>           8   7.1288    26.3708  -64.3186     78.5762 
#>           9  10.8758    27.1115  -62.5782     84.3299 
#>          10  17.5057    32.5995  -70.8174    105.8288 
#>          11  40.8318    39.6704  -66.6487    148.3122 
#>          12  48.6134    54.2235  -98.2962    195.5230 
#>          13  52.4228    52.1405  -88.8434    193.6890 
#>          14  50.2000    58.8198 -109.1626    209.5626 
#>          15  68.2960    82.2076 -154.4319    291.0239 
#>          16  44.7305    67.6093 -138.4457    227.9067 
#>          17  61.4670    82.9975 -163.4011    286.3351 
#>          18  50.4635    97.0905 -212.5873    313.5143 
#>          19  47.3392   119.9502 -277.6461    372.3245 
#>          20  28.6326   120.7810 -298.6037    355.8689 
#>          21   4.3445   118.7309 -317.3374    326.0265 
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
