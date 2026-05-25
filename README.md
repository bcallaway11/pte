
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Panel Treatment Effects (pte) Package <img src="man/figures/logo.png" align="right" alt="pte" width="155" />

> **Note:** This package is superseded by
> [`ptetools`](https://bcallaway11.github.io/ptetools/)
> ([GitHub](https://github.com/bcallaway11/ptetools)), which provides
> all of the same functionality and is actively maintained. This
> repository is kept for reference and reproducibility of earlier work.

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
#> Warning in pte(yname = "lemp", gname = "first.treat", tname = "year", idname = "countyreal", : 'pte' is deprecated.
#> Use 'pte2' instead.
#> See help("Deprecated")
#> Warning in BMisc::getListElement(selective.se.inner, "se"): 'BMisc::getListElement' is deprecated.
#> Use 'get_list_element' instead.
#> See help("Deprecated")
#> Warning in BMisc::getListElement(selective.se.inner, "inf.func"): 'BMisc::getListElement' is deprecated.
#> Use 'get_list_element' instead.
#> See help("Deprecated")
#> Warning in BMisc::getListElement(dynamic.se.inner, "se"): 'BMisc::getListElement' is deprecated.
#> Use 'get_list_element' instead.
#> See help("Deprecated")
#> Warning in BMisc::getListElement(dynamic.se.inner, "inf.func"): 'BMisc::getListElement' is deprecated.
#> Use 'get_list_element' instead.
#> See help("Deprecated")

summary(did_res)
#> 
#> Overall ATT:  
#>      ATT    Std. Error     [ 95%  Conf. Int.]  
#>  -0.0323        0.0118    -0.0554     -0.0091 *
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error   [95%  Conf. Band]  
#>          -3   0.0269     0.0117 -0.0021      0.0560  
#>          -2  -0.0050     0.0119 -0.0344      0.0245  
#>          -1  -0.0229     0.0135 -0.0563      0.0106  
#>           0  -0.0201     0.0139 -0.0546      0.0143  
#>           1  -0.0547     0.0152 -0.0923     -0.0171 *
#>           2  -0.1382     0.0366 -0.2288     -0.0476 *
#>           3  -0.1069     0.0344 -0.1919     -0.0219 *
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
                 setup_pte_fun=setup_pte_basic,
                 subset_fun=two_by_two_subset,
                 attgt_fun=covid_attgt,
                 xformla=xformla,
                 max_e=21,
                 min_e=-10) 
#> Warning in pte(yname = "positive", gname = "group", tname = "time.period", : 'pte' is deprecated.
#> Use 'pte2' instead.
#> See help("Deprecated")
#> Warning in BMisc::getListElement(selective.se.inner, "se"): 'BMisc::getListElement' is deprecated.
#> Use 'get_list_element' instead.
#> See help("Deprecated")
#> Warning in BMisc::getListElement(selective.se.inner, "inf.func"): 'BMisc::getListElement' is deprecated.
#> Use 'get_list_element' instead.
#> See help("Deprecated")
#> Warning in BMisc::getListElement(dynamic.se.inner, "se"): 'BMisc::getListElement' is deprecated.
#> Use 'get_list_element' instead.
#> See help("Deprecated")
#> Warning in BMisc::getListElement(dynamic.se.inner, "inf.func"): 'BMisc::getListElement' is deprecated.
#> Use 'get_list_element' instead.
#> See help("Deprecated")

summary(covid_res)
#> 
#> Overall ATT:  
#>      ATT    Std. Error     [ 95%  Conf. Int.] 
#>  14.8882       91.3936    -164.24    194.0163 
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error     [95%  Conf. Band] 
#>         -10  -3.7266     3.2071  -11.5308      4.0775 
#>          -9   2.6607     1.1809   -0.2129      5.5343 
#>          -8   0.8290     2.2298   -4.5970      6.2550 
#>          -7   5.2843     2.4317   -0.6328     11.2015 
#>          -6   2.8555     2.2453   -2.6082      8.3193 
#>          -5   1.3589     3.7392   -7.7399     10.4578 
#>          -4   0.3294     3.5391   -8.2826      8.9414 
#>          -3  -4.2227     4.9704  -16.3174      7.8721 
#>          -2  -3.8447     2.7244  -10.4743      2.7848 
#>          -1  -0.2234     3.5703   -8.9113      8.4645 
#>           0 -10.8156     8.9321  -32.5508     10.9195 
#>           1 -13.7998    15.5493  -51.6372     24.0376 
#>           2  -7.8432    11.5933  -36.0541     20.3678 
#>           3  -4.5541    11.2359  -31.8955     22.7872 
#>           4  -3.5368    10.4700  -29.0144     21.9407 
#>           5   8.5221    10.2973  -16.5351     33.5793 
#>           6   1.1140    20.0732  -47.7317     49.9597 
#>           7   6.6384    23.4109  -50.3292     63.6061 
#>           8   7.1288    27.6501  -60.1545     74.4121 
#>           9  10.8758    32.7738  -68.8754     90.6271 
#>          10  17.5057    30.9114  -57.7134     92.7248 
#>          11  40.8318    36.7505  -48.5961    130.2597 
#>          12  48.6134    42.6849  -55.2554    152.4821 
#>          13  52.4228    51.7367  -73.4722    178.3178 
#>          14  50.2000    60.0231  -95.8590    196.2591 
#>          15  68.2960    72.0307 -106.9822    243.5742 
#>          16  44.7305    79.9407 -149.7958    239.2568 
#>          17  61.4670    77.4408 -126.9760    249.9099 
#>          18  50.4635   110.5784 -218.6159    319.5429 
#>          19  47.3392   113.7345 -229.4203    324.0987 
#>          20  28.6326   126.6005 -279.4347    336.6999 
#>          21   4.3445   127.9147 -306.9207    315.6098 
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
#> Warning in pte(yname = "lemp", gname = "first.treat", tname = "year", idname = "countyreal", : 'pte' is deprecated.
#> Use 'pte2' instead.
#> See help("Deprecated")

summary(did_res_noif)
#> 
#> Overall ATT:  
#>      ATT    Std. Error     [ 95%  Conf. Int.]  
#>  -0.0323        0.0121    -0.0559     -0.0086 *
#> 
#> 
#> Dynamic Effects:
#>  Event Time Estimate Std. Error [95% Pointwise  Conf. Band]  
#>          -3   0.0269     0.0133          0.0009      0.0530 *
#>          -2  -0.0050     0.0134         -0.0313      0.0213  
#>          -1  -0.0229     0.0153         -0.0528      0.0071  
#>           0  -0.0201     0.0118         -0.0433      0.0030  
#>           1  -0.0547     0.0164         -0.0869     -0.0226 *
#>           2  -0.1382     0.0334         -0.2036     -0.0728 *
#>           3  -0.1069     0.0353         -0.1762     -0.0377 *
#> ---
#> Signif. codes: `*' confidence band does not cover 0
ggpte(did_res_noif)
```

![](man/figures/README-unnamed-chunk-8-1.png)<!-- -->

What’s exciting about this is just how little new code needs to be
written.
