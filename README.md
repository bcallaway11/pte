
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

The main function is called `pte`. The most important paramters that it
takes in are `subset_fun` and `attgt_fun`. These are functions that the
user should pass to `pte`.

`subset_fun` takes in the overall data, a group, a time period, and
possibly other arguments and returns a `data.frame` containing the
relevant subset of the data, an outcome, and whether or not a unit
should be considered to be in the treated or comparison group for that
group/time. There is one example of a relevant subset function provided
in the package: [the `two_by_two_subset`
function](https://github.com/bcallaway11/pte/blob/master/R/subset_functions.R).
This function takes an original dataset, subsets it into pre- and
post-treatment periods and denotes treated and untreated units. This
particular subset is perhaps the most common/important one for thinking
about treatment effects with panel data.

The other main function is `attgt_fun`. This function should be able to
take in the correct subset of data, possibly along with other arguments
to the function, and report an *ATT* for that subset. With minor
modification, this function should be availble for most any sort of
treatment effects application — for example, if you can solve the
baseline 2x2 case in difference in differences, you should use that
function here, and the `pte` package will take care of dealing with the
variation in treatment timing.

If `attgt_fun` returns an influence function, then the `pte` package
will also conduct inference using the multiplier bootstrap (which is
fast) and produce uniform confidence bands (which adjust for multiple
testing).

The default output of `pte` is an overall treatment effect on the
treated (i.e., across all groups that participate in the treatment in
any time period) and dynamic effects (i.e., event studies). More
aggregations are possible, but these seem to be the leading cases;
aggregations of group-time average treatment effects are discussed at
length in [Callaway and Sant’Anna
(2021)](https://doi.org/10.1016/j.jeconom.2020.12.001).

Here are a few examples:

## Example 1: Difference in differences

The [`did` package](https://bcallaway11.github.io/did/), which is based
on [Callaway and Sant’Anna
(2021)](https://doi.org/10.1016/j.jeconom.2020.12.001), includes
estimates of group-time average treatment effects, *ATT(g,t)*, based on
a difference in differences identification strategy. The following
example demonstrates that it is easy to compute group-time average
treatment effects using difference in differences using the `pte`
package. \[*Note:* This is definitely not the recommended way of doing
this as there is very little error handling, etc. here, but it is rather
a proof of concept. You should use the `did` package for this case.\]

This example reproduces DID estimates of the effect of the minimum wage
on employment using data from the `did` package.

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
#>  -0.0323        0.0136    -0.0589     -0.0056 *
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error   [95%  Conf. Band]  
#>          -3   0.0269     0.0115 -0.0015      0.0554  
#>          -2  -0.0050     0.0109 -0.0317      0.0218  
#>          -1  -0.0229     0.0155 -0.0611      0.0154  
#>           0  -0.0201     0.0119 -0.0496      0.0093  
#>           1  -0.0547     0.0162 -0.0946     -0.0148 *
#>           2  -0.1382     0.0343 -0.2228     -0.0536 *
#>           3  -0.1069     0.0323 -0.1865     -0.0273 *
#> ---
#> Signif. codes: `*' confidence band does not cover 0
ggpte(did_res)
```

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->

What’s most interesting here, is that the only “new” code that needs to
be writte is in [the `did_attgt`
function](https://github.com/bcallaway11/pte/blob/master/R/attgt_functions.R).
You will see that this is a very small amount of code.

## Example 2: Policy Evaluation during a Pandemic

As a next example, consider trying to estimate effects of Covid-19
related policies during a pandemic. The estimates below are for the
effects of state-leve shelter-in-place orders during the early part of
the pandemic.

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
#>  14.8882        77.625   -137.254    167.0303 
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error     [95%  Conf. Band] 
#>         -10  -3.7266     3.1514  -11.7673      4.3141 
#>          -9   2.6607     1.3939   -0.8958      6.2171 
#>          -8   0.8290     2.3131   -5.0726      6.7306 
#>          -7   5.2843     2.7465   -1.7232     12.2919 
#>          -6   2.8555     1.9551   -2.1328      7.8438 
#>          -5   1.3589     4.2282   -9.4291     12.1469 
#>          -4   0.3294     3.7990   -9.3634     10.0222 
#>          -3  -4.2227     6.1901  -20.0164     11.5711 
#>          -2  -3.8447     2.6841  -10.6930      3.0035 
#>          -1  -0.2234     3.4084   -8.9198      8.4730 
#>           0 -10.8156     9.5554  -35.1956     13.5644 
#>           1 -13.7998    16.5274  -55.9684     28.3688 
#>           2  -7.8432    11.0211  -35.9626     20.2763 
#>           3  -4.5541    12.1361  -35.5186     26.4103 
#>           4  -3.5368    13.2898  -37.4447     30.3710 
#>           5   8.5221    13.2492  -25.2823     42.3266 
#>           6   1.1140    16.4968  -40.9763     43.2043 
#>           7   6.6384    21.6967  -48.7192     61.9961 
#>           8   7.1288    27.8903  -64.0314     78.2890 
#>           9  10.8758    32.4714  -71.9727     93.7244 
#>          10  17.5057    36.4513  -75.4971    110.5085 
#>          11  40.8318    41.5868  -65.2740    146.9375 
#>          12  48.6134    59.0086 -101.9429    199.1696 
#>          13  52.4228    67.8219 -120.6198    225.4654 
#>          14  50.2000    51.4692  -81.1198    181.5199 
#>          15  68.2960    79.7460 -135.1703    271.7623 
#>          16  44.7305    84.5580 -171.0131    260.4742 
#>          17  61.4670    86.2579 -158.6138    281.5477 
#>          18  50.4635    93.7979 -188.8552    289.7822 
#>          19  47.3392   114.3480 -244.4116    339.0900 
#>          20  28.6326   128.1748 -298.3962    355.6615 
#>          21   4.3445   110.3306 -277.1561    285.8451 
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
this code just reports pointwise confidence intervals. However, this
basically is set up to fit into my typical workflow, and I see this as a
way to get preliminary results.

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
#>  -0.0323        0.0118    -0.0554     -0.0092 *
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error [95% Pointwise  Conf. Band]  
#>          -3   0.0269     0.0147         -0.0019      0.0557  
#>          -2  -0.0050     0.0125         -0.0294      0.0195  
#>          -1  -0.0229     0.0166         -0.0555      0.0098  
#>           0  -0.0201     0.0118         -0.0433      0.0030  
#>           1  -0.0547     0.0145         -0.0831     -0.0264 *
#>           2  -0.1382     0.0365         -0.2098     -0.0666 *
#>           3  -0.1069     0.0376         -0.1806     -0.0332 *
#> ---
#> Signif. codes: `*' confidence band does not cover 0
ggpte(did_res_noif)
```

![](man/figures/README-unnamed-chunk-8-1.png)<!-- --> What’s exciting
about this is just how little new code needs to be written.
