
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
               setup_pte_fun=setup_pte,
               subset_fun=two_by_two_subset,
               attgt_fun=did_attgt,
               xformla=~lpop) 

summary(did_res)
#> 
#> Overall ATT:  
#>      ATT    Std. Error     [ 95%  Conf. Int.]  
#>  -0.0323        0.0101     -0.052     -0.0126 *
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error   [95%  Conf. Band]  
#>          -3   0.0269     0.0147 -0.0105      0.0644  
#>          -2  -0.0050     0.0131 -0.0383      0.0284  
#>          -1  -0.0229     0.0143 -0.0594      0.0137  
#>           0  -0.0201     0.0088 -0.0427      0.0024  
#>           1  -0.0547     0.0182 -0.1012     -0.0082 *
#>           2  -0.1382     0.0373 -0.2335     -0.0429 *
#>           3  -0.1069     0.0412 -0.2121     -0.0017 *
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
                 setup_pte_fun=setup_pte,
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
#>  14.8882      100.3851   -181.863    211.6393 
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error     [95%  Conf. Band] 
#>         -10  -3.7266     3.5748  -13.1626      5.7093 
#>          -9   2.6607     1.2414   -0.6162      5.9375 
#>          -8   0.8290     1.8916   -4.1639      5.8219 
#>          -7   5.2843     2.3999   -1.0503     11.6190 
#>          -6   2.8555     1.3051   -0.5895      6.3005 
#>          -5   1.3589     3.5285   -7.9548     10.6726 
#>          -4   0.3294     4.1550  -10.6379     11.2966 
#>          -3  -4.2227     4.8362  -16.9882      8.5429 
#>          -2  -3.8447     2.6832  -10.9271      3.2376 
#>          -1  -0.2234     3.3093   -8.9586      8.5118 
#>           0 -10.8156     7.8844  -31.6269      9.9956 
#>           1 -13.7998    11.9935  -45.4573     17.8577 
#>           2  -7.8432     9.8990  -33.9722     18.2859 
#>           3  -4.5541    10.8512  -33.1964     24.0881 
#>           4  -3.5368    10.8932  -32.2901     25.2164 
#>           5   8.5221    10.6931  -19.7031     36.7473 
#>           6   1.1140    20.8776  -53.9937     56.2216 
#>           7   6.6384    24.0598  -56.8687     70.1456 
#>           8   7.1288    27.7870  -66.2168     80.4743 
#>           9  10.8758    28.6149  -64.6550     86.4066 
#>          10  17.5057    34.2001  -72.7676    107.7790 
#>          11  40.8318    40.4794  -66.0161    147.6797 
#>          12  48.6134    49.3890  -81.7519    178.9787 
#>          13  52.4228    56.0141  -95.4297    200.2753 
#>          14  50.2000    53.3274  -90.5608    190.9609 
#>          15  68.2960    56.3082  -80.3329    216.9249 
#>          16  44.7305    53.2602  -95.8530    185.3140 
#>          17  61.4670    89.2068 -173.9997    296.9336 
#>          18  50.4635   111.7107 -244.4035    345.3305 
#>          19  47.3392   109.7565 -242.3697    337.0480 
#>          20  28.6326   115.2639 -275.6134    332.8786 
#>          21   4.3445   172.8947 -452.0213    460.7104 
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
                    setup_pte_fun=setup_pte,
                    subset_fun=two_by_two_subset,
                    attgt_fun=did_attgt_noif, #this is only diff.
                    xformla=~lpop) 

summary(did_res_noif)
#> 
#> Overall ATT:  
#>      ATT    Std. Error     [ 95%  Conf. Int.]  
#>  -0.0323        0.0135    -0.0587     -0.0058 *
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error [95% Pointwise  Conf. Band]  
#>          -3   0.0269     0.0143         -0.0011      0.0550  
#>          -2  -0.0050     0.0134         -0.0312      0.0213  
#>          -1  -0.0229     0.0140         -0.0502      0.0045  
#>           0  -0.0201     0.0130         -0.0456      0.0053  
#>           1  -0.0547     0.0188         -0.0916     -0.0178 *
#>           2  -0.1382     0.0367         -0.2101     -0.0663 *
#>           3  -0.1069     0.0366         -0.1785     -0.0353 *
#> ---
#> Signif. codes: `*' confidence band does not cover 0
ggpte(did_res_noif)
```

![](man/figures/README-unnamed-chunk-8-1.png)<!-- -->

What’s exciting about this is just how little new code needs to be
written.
