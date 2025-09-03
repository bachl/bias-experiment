Analyses
================

# Required Packages & Reproducibility

``` r
rm(list=ls())
source(here::here("src/lib/functions.R"))
```

# Analyses

``` r
load(here::here("data/intermediate/cleaned_data_joined.RData"))
```

## Balance Checks

The figure below shows that the data is not unbalanced for any of the
variables: As described in the Pre-Analysis Plan (p.10), we will not add
any covariates to the analyses as controls.

``` r
source(here("src/analysis/balance-test.R"))
df #Figure OA3
```

<img src="../../report/figures/balance-checks-1.png" style="display: block; margin: auto;" />

\#Descriptives: Profiles

``` r
source(here("src/analysis/profiles.R"))
prof1 #Figure 2
```

<img src="../../report/figures/explore-profiles-1.png" style="display: block; margin: auto;" />

``` r
prof2 #Figure 3
```

<img src="../../report/figures/explore-profiles-2.png" style="display: block; margin: auto;" />

## Ideological Bias hypothesis (H1)

``` r
source(here("src/analysis/h1.R"))
p1a #Figure 4a
```

<img src="../../report/figures/h1-1.png" style="display: block; margin: auto;" />

``` r
p1b #Figure 4b
```

<img src="../../report/figures/h1-2.png" style="display: block; margin: auto;" />

``` r
(p1b_1+p1b_2)/(p1b_3 + p1b_4) + 
  plot_layout(guides = "collect") & theme(legend.position = 'bottom') #Figure 4b
```

<img src="../../report/figures/h1-3.png" style="display: block; margin: auto;" />

``` r
rm(df, df_nl, df_us, h1a, h1b, p1b_1, 
   pred1b_1, pred1b_2, pred1b_3, pred1b_4, tmp)
```

## Overinterpretation Hypotheses (H2)

``` r
source(here("src/analysis/h2.R"))
p2a #Figure 5
```

<img src="../../report/figures/h2-1.png" style="display: block; margin: auto;" />

``` r
p2b
```

<img src="../../report/figures/h2-2.png" style="display: block; margin: auto;" />

``` r
p2c1
```

<img src="../../report/figures/h2-3.png" style="display: block; margin: auto;" />

``` r
p2c2
```

<img src="../../report/figures/h2-4.png" style="display: block; margin: auto;" />

``` r
(p2a_1+p2a_2)/(p2a_3 + p2a_4) + 
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
```

<img src="../../report/figures/h2-5.png" style="display: block; margin: auto;" />

``` r
(p2b_1+p2b_2)/(p2b_3 + p2b_4) + 
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
```

<img src="../../report/figures/h2-6.png" style="display: block; margin: auto;" />

``` r
rm(df, df_nl, df_us, h2a, h2a_2, h2b, h2b_2, tmp, tmpp,
   pred2a_1, pred2a_2, pred2a_3, pred2a_4, 
   pred2b_1, pred2b_2, pred2b_3, pred2b_4)
```

## Masking Hypothesis (H3)

``` r
source(here("src/analysis/h3.R"))
p3a #Figure 6
```

<img src="../../report/figures/h3-1.png" style="display: block; margin: auto;" />

``` r
p3b
```

<img src="../../report/figures/h3-2.png" style="display: block; margin: auto;" />

``` r
p3c1
```

<img src="../../report/figures/h3-3.png" style="display: block; margin: auto;" />

``` r
p3c2
```

<img src="../../report/figures/h3-4.png" style="display: block; margin: auto;" />

``` r
(p3a_1+p3a_2)/(p3a_3 + p3a_4) + 
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
```

<img src="../../report/figures/h3-5.png" style="display: block; margin: auto;" />

``` r
(p3b_1+p3b_2)/(p3b_3 + p3b_4) + 
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')
```

<img src="../../report/figures/h3-6.png" style="display: block; margin: auto;" />

``` r
rm(df, df_nl, df_us, h3a, h3a_2, h3b, 
   h3b_2,h3b_bs, h3b_2bs, tmp, tmpp,
   pred3a_1, pred3a_2, pred3a_3, pred3a_4, 
   pred3b_1, pred3b_2, pred3b_3, pred3b_4)
```

## Exploration

### Issues effect

``` r
source(here("src/analysis/issue_specific.R"))
b_i #Figure OA.4
```

<img src="../../report/figures/issues-1.png" style="display: block; margin: auto;" />

``` r
rm(b1, b2, df, df_nl, df_us, tmp, issues_nl, issues_us, i)
```

### Baseline Model

``` r
source(here("src/analysis/baseline.R"))
b #Figure OA.5
```

<img src="../../report/figures/baseline-1.png" style="display: block; margin: auto;" />

``` r
rm(b_b, b1, b2, df, df_nl, df_us, tmp)
```

### Different ways of measuring ideological distance

``` r
source(here("src/analysis/exploration.R")) #Figures OA.6
p_b1
```

<img src="../../report/figures/exploration-1.png" style="display: block; margin: auto;" />

``` r
p_b2
```

<img src="../../report/figures/exploration-2.png" style="display: block; margin: auto;" />

``` r
p_e1a
```

<img src="../../report/figures/exploration-3.png" style="display: block; margin: auto;" />

``` r
p_e1b
```

<img src="../../report/figures/exploration-4.png" style="display: block; margin: auto;" />

``` r
p_e1c
```

<img src="../../report/figures/exploration-5.png" style="display: block; margin: auto;" />
