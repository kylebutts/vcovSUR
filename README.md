
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vcovSUR

<!-- badges: start -->
<!-- badges: end -->

The goal of vcovSUR is to allow estimation of the variance-covariance
matrix and perform hypotheses tests of [multiple regression
equations](https://en.wikipedia.org/wiki/Seemingly_unrelated_regressions).

This package was inspired by Peter Hull’s tweet on ‘stacking’
regressions to compare coefficients across estimates:
<https://twitter.com/instrumenthull/status/1492915860763250691>. Instead
of needing to stack the regressions (which is tedious and easy to
mess-up), you can use estimate each regression separately and then use
`vcovSUR` to estimate the variance-covariance matrix of the coefficients
or `sur_hypotheses` to conduct hypothesis tests *across regression
specifications*.

## Installation

You can install the development version of vcovSUR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("kylebutts/vcovSUR")
```

## Example

Consider running two regressions with different outcome variables. For
our example, we will use the `CASchools` dataset from the `{AER}`
package. The dataset contains data on test performance, school
characteristics and student demographic backgrounds for school districts
in California. Each row is a school.

We want to see if the correlation between percent of students qualifying
for reduced-price lunch (`lunch`) and test scores differ between the
reading score (`read`) and the math score (`math`).

``` r
library(vcovSUR)
#> Loading required package: marginaleffects
#> Loading required package: Matrix
library(fixest)
data("CASchools", package = "AER")
CASchools$obs_id <- 1:nrow(CASchools)

(est_read <- feols(read ~ lunch, data = CASchools))
#> OLS estimation, Dep. Var.: read
#> Observations: 420 
#> Standard-errors: IID 
#>               Estimate Std. Error  t value  Pr(>|t|)    
#> (Intercept) 684.096210   0.904470 756.3506 < 2.2e-16 ***
#> lunch        -0.651506   0.017303 -37.6533 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> RMSE: 9.58362   Adj. R2: 0.771758
(est_math <- feols(math ~ lunch, data = CASchools))
#> OLS estimation, Dep. Var.: math
#> Observations: 420 
#> Standard-errors: IID 
#>               Estimate Std. Error  t value  Pr(>|t|)    
#> (Intercept) 678.782827   1.004175 675.9608 < 2.2e-16 ***
#> lunch        -0.569066   0.019210 -29.6232 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> RMSE: 10.6   Adj. R2: 0.676581
```

Since we are using the same sample for both equations, there is likely a
correlation between the error terms in each equation (shocks that impact
reading scores for a school also are likely impacting math scores). We
can estimate the variance-covariance matrix of estimates *across
regression equations* using `vcovSUR`. Note that we need to specify the
`cluster` argument to indicate which observations we think could have
correlated error terms across regressions. This could be a variable for
each observation or some higher level grouping variable (e.g. state,
county, etc.).

Note that this works even if the dataset used in each regression
differs. For example, you might have two subsamples (e.g. male and
females) but you cluster at the county level to allow county-level
shocks to be common across the subsamples.

``` r
# No correlation between the coefficients since we are not clustering
vcovSUR(list(est_read, est_math))
#>             (Intercept)         lunch (Intercept)        lunch
#> (Intercept)  0.90856128 -0.0149816563  0.00000000  0.000000000
#> lunch       -0.01498166  0.0003250524  0.00000000  0.000000000
#> (Intercept)  0.00000000  0.0000000000  1.24436505 -0.019706980
#> lunch        0.00000000  0.0000000000 -0.01970698  0.000393882

# Correlation between the coefficients since we are clustering by `obs_id`
vcovSUR(list(est_read, est_math), cluster = "obs_id")
#>             (Intercept)         lunch (Intercept)         lunch
#> (Intercept)  0.90856128 -0.0149816563  0.85619311 -0.0139560157
#> lunch       -0.01498166  0.0003250524 -0.01395602  0.0002854246
#> (Intercept)  0.85619311 -0.0139560157  1.24436505 -0.0197069803
#> lunch       -0.01395602  0.0002854246 -0.01970698  0.0003938820
```

With the joint variance-covariance matrix, we can perform tests across
regression specifications. However, this is a bit of a pain to do
manually, so I have included the `sur_hypotheses` function which is a
light wrapper around `hypotheses` from the [`marginaleffects`
package](https://vincentarelbundock.github.io/marginaleffects/articles/hypothesis.html).
The first two arguments are the same as `vcovSUR`, namely the list of
estimates and the cluster variable name.

The third argument is a string of the hypothesis to test. Since there
are multiple regressions and you need to be able specify which
regression you want a coefficient from (is it test from read or math?),
I suffix each coefficient name with `_#` where the `#` corresponds to
the position of the estimate in the `ests` list. Here’s an example
testing if the coefficient on `lunch` is the same across specifications:

``` r
sur_hypotheses(
  list(est_read, est_math), cluster = "obs_id",
  hypothesis = "lunch_1 = lunch_2"
)
#> 
#>               Term Estimate Std. Error     z Pr(>|z|)    S  2.5 %  97.5 %
#>  lunch_1 = lunch_2  -0.0824     0.0122 -6.77   <0.001 36.2 -0.106 -0.0586
#> 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high
```

### `fixest_multi`

You can also include `fixest_multi` objects in the `ests` parameter, and
they will be numbered based on the order in `c()`.

``` r
est_multi <- feols(c(read, math) ~ lunch, CASchools)

sur_hypotheses(
  est_multi, cluster = "obs_id",
  hypothesis = "lunch_1 = lunch_2"
)
#> 
#>               Term Estimate Std. Error     z Pr(>|z|)    S  2.5 %  97.5 %
#>  lunch_1 = lunch_2  -0.0824     0.0122 -6.77   <0.001 36.2 -0.106 -0.0586
#> 
#> Columns: term, estimate, std.error, statistic, p.value, s.value, conf.low, conf.high
```
