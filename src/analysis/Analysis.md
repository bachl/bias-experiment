Analyses
================

# Required Packages & Reproducibility

``` r
rm(list=ls())
source(here::here("src/lib/functions.R"))
```

# Analyses

``` r
load(here("data/intermediate/cleaned_data_nl.RData"))
load(here("data/intermediate/cleaned_data_us.RData"))
```

## Balance Checks

The figure below shows that the data is not unbalanced for any of the
variables: As described in the Pre-Analysis Plan (p.10), we will not add
any covariates to the analyses as controls.

``` r
source(here("src/analysis/balance-test.R"))
df
```

<img src="../../report/figures/balance-checks-1.png" style="display: block; margin: auto;" />

## Baseline Model

``` r
source(here("src/analysis/baseline.R"))
b
```

<img src="../../report/figures/baseline-1.png" style="display: block; margin: auto;" />

``` r
rm(b_b, b1, b2, d, df, df_nl, df_us, tmp)
```

``` r
source(here("src/analysis/issue_specific.R"))
b_i
```

<img src="../../report/figures/issues-1.png" style="display: block; margin: auto;" />

``` r
rm(b1, b2, d, df, df_nl, df_us, tmp)
```

## Ideological Bias hypothesis

``` r
source(here("src/analysis/h1.R"))
p1a
```

<img src="../../report/figures/h1-1.png" style="display: block; margin: auto;" />

``` r
p1b
```

<img src="../../report/figures/h1-2.png" style="display: block; margin: auto;" />

``` r
rm(df, df_nl, df_us, h1a, h1b, tmp)
```

## Overinterpretation Hypotheses

``` r
source(here("src/analysis/h2.R"))
p2a
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
rm(df, df_nl, df_us, h2a, h2a_2, h2b, h2b_2, tmp, tmpp)
```

## Masking Hypothesis

``` r
source(here("src/analysis/h3.R"))
p3a
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
rm(df, df_nl, df_us, h3a, h3a_2, h3b, h3b_2,h3b_bs, h3b_2bs, tmp, tmpp)
```

## Exploration

``` r
source(here("src/analysis/exploration.R"))
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
source(here("src/analysis/profile_resp.R"))
kbl(e1_nlp, booktabs =T, caption = "\\label{tab:conditions}Profile Dutch Stance Annotators") %>%
  kable_styling(latex_options = c("striped", "hold_position"),
                full_width = F, fixed_thead = T, position = "center") %>%
  column_spec(1, width = "7cm") %>%
  column_spec(2, width = "7cm")
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Profile Dutch Stance Annotators
</caption>
<thead>
<tr>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
Incorrectly Identified Stance (Strict Interpretation)
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
Correctly Identified Stance (Strict Interpretation)
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;width: 7cm; ">
Male
</td>
<td style="text-align:left;width: 7cm; ">
Male
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
High-levels of education
</td>
<td style="text-align:left;width: 7cm; ">
High-levels of education
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
West of Netherlands
</td>
<td style="text-align:left;width: 7cm; ">
West of Netherlands
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Fulltime Employed
</td>
<td style="text-align:left;width: 7cm; ">
Fulltime Employed
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
D66
</td>
<td style="text-align:left;width: 7cm; ">
D66
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Age: 48
</td>
<td style="text-align:left;width: 7cm; ">
Age: 46
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Income: 3250
</td>
<td style="text-align:left;width: 7cm; ">
Income: 3250
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Position on Immigration: 3
</td>
<td style="text-align:left;width: 7cm; ">
Position on Immigration: 3
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Position on Environment: 1
</td>
<td style="text-align:left;width: 7cm; ">
Position on Environment: 1
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Position on Tax: 3
</td>
<td style="text-align:left;width: 7cm; ">
Position on Tax: 3
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Position on EU: 1
</td>
<td style="text-align:left;width: 7cm; ">
Position on EU: 1
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Ideological Position: 5
</td>
<td style="text-align:left;width: 7cm; ">
Ideological Position: 4
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Ideological Distance: 2
</td>
<td style="text-align:left;width: 7cm; ">
Ideological Distance: 2
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Issue Congruence: 0
</td>
<td style="text-align:left;width: 7cm; ">
Issue Congruence: 0
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Political Knowledge: 2
</td>
<td style="text-align:left;width: 7cm; ">
Political Knowledge: 2
</td>
</tr>
</tbody>
</table>

``` r
kbl(e1_ss, booktabs =T, caption = "\\label{tab:conditions2}Profile Dutch Stance Annotators") %>%
  kable_styling(latex_options = c("striped", "hold_position"),
                full_width = F, fixed_thead = T, position = "center") %>%
  column_spec(1, width = "7cm") %>%
  column_spec(2, width = "7cm")
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Profile Dutch Stance Annotators
</caption>
<thead>
<tr>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
Incorrectly Identified Stance (Lenient Interpretation)
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
Correctly Identified Stance (Lenient Interpretation)
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;width: 7cm; ">
Male
</td>
<td style="text-align:left;width: 7cm; ">
Male
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
High-levels of education
</td>
<td style="text-align:left;width: 7cm; ">
High-levels of education
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
West of Netherlands
</td>
<td style="text-align:left;width: 7cm; ">
West of Netherlands
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Fulltime Employed
</td>
<td style="text-align:left;width: 7cm; ">
Fulltime Employed
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
D66
</td>
<td style="text-align:left;width: 7cm; ">
D66
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Age: 49
</td>
<td style="text-align:left;width: 7cm; ">
Age: 46
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Income: 3250
</td>
<td style="text-align:left;width: 7cm; ">
Income: 3250
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Position on Immigration: 3
</td>
<td style="text-align:left;width: 7cm; ">
Position on Immigration: 3
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Position on Environment: 1
</td>
<td style="text-align:left;width: 7cm; ">
Position on Environment: 1
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Position on Tax: 3
</td>
<td style="text-align:left;width: 7cm; ">
Position on Tax: 3
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Position on EU: 1
</td>
<td style="text-align:left;width: 7cm; ">
Position on EU: 1
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Ideological Position: 5
</td>
<td style="text-align:left;width: 7cm; ">
Ideological Position: 4
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Ideological Distance: 3
</td>
<td style="text-align:left;width: 7cm; ">
Ideological Distance: 2
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Issue Congruence: 0
</td>
<td style="text-align:left;width: 7cm; ">
Issue Congruence: 0
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Political Knowledge: 2
</td>
<td style="text-align:left;width: 7cm; ">
Political Knowledge: 2
</td>
</tr>
</tbody>
</table>

``` r
kbl(e1_us_nlp, booktabs =T, caption = "\\label{tab:conditions_us}Profile American Stance Annotators") %>%
  kable_styling(latex_options = c("striped", "hold_position"),
                full_width = F, fixed_thead = T, position = "center") %>%
  column_spec(1, width = "7cm") %>%
  column_spec(2, width = "7cm")
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Profile American Stance Annotators
</caption>
<thead>
<tr>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
Incorrectly Identified Stance (Strict Interpretation)
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
Correctly Identified Stance (Strict Interpretation)
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;width: 7cm; ">
Male
</td>
<td style="text-align:left;width: 7cm; ">
Male
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
High-level of education
</td>
<td style="text-align:left;width: 7cm; ">
High-level of education
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Southeast of the United States
</td>
<td style="text-align:left;width: 7cm; ">
Southeast of the United States
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Working now
</td>
<td style="text-align:left;width: 7cm; ">
Working now
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Democrat
</td>
<td style="text-align:left;width: 7cm; ">
Democrat
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Age: 24
</td>
<td style="text-align:left;width: 7cm; ">
Age: 24
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Income: 3250
</td>
<td style="text-align:left;width: 7cm; ">
Income: 3250
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Position on Immigration: 3
</td>
<td style="text-align:left;width: 7cm; ">
Position on Immigration: 3
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Position on Environment: 2
</td>
<td style="text-align:left;width: 7cm; ">
Position on Environment: 2
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Position on Tax: 4
</td>
<td style="text-align:left;width: 7cm; ">
Position on Tax: 4
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Position on Foreign Policy: 3
</td>
<td style="text-align:left;width: 7cm; ">
Position on Foreign Policy: 3
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Ideological Position: 2
</td>
<td style="text-align:left;width: 7cm; ">
Ideological Position: 2
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Ideological Distance: 3
</td>
<td style="text-align:left;width: 7cm; ">
Ideological Distance: 2
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Issue Congruence: 0
</td>
<td style="text-align:left;width: 7cm; ">
Issue Congruence: 1
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Political Knowledge: 1
</td>
<td style="text-align:left;width: 7cm; ">
Political Knowledge: 1
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Bullshit Receptivity: 3
</td>
<td style="text-align:left;width: 7cm; ">
Bullshit Receptivity: 2
</td>
</tr>
</tbody>
</table>

``` r
kbl(e1_us_ss, booktabs =T, caption = "\\label{tab:conditions_us2}Profile American Stance Annotators") %>%
  kable_styling(latex_options = c("striped", "hold_position"),
                full_width = F, fixed_thead = T, position = "center") %>%
  column_spec(1, width = "7cm") %>%
  column_spec(2, width = "7cm")
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Profile American Stance Annotators
</caption>
<thead>
<tr>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
Incorrectly Identified Stance (Lenient Interpretation)
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
Correctly Identified Stance (Lenient Interpretation)
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;width: 7cm; ">
Male
</td>
<td style="text-align:left;width: 7cm; ">
Male
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
High-level of education
</td>
<td style="text-align:left;width: 7cm; ">
High-level of education
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Southeast of the United States
</td>
<td style="text-align:left;width: 7cm; ">
Southeast of the United States
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Working now
</td>
<td style="text-align:left;width: 7cm; ">
Working now
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Democrat
</td>
<td style="text-align:left;width: 7cm; ">
Democrat
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Age: 25
</td>
<td style="text-align:left;width: 7cm; ">
Age: 24
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Income: 3250
</td>
<td style="text-align:left;width: 7cm; ">
Income: 3250
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Position on Immigration: 3
</td>
<td style="text-align:left;width: 7cm; ">
Position on Immigration: 3
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Position on Environment: 2
</td>
<td style="text-align:left;width: 7cm; ">
Position on Environment: 2
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Position on Tax: 4
</td>
<td style="text-align:left;width: 7cm; ">
Position on Tax: 4
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Position on Foreign Policy: 3
</td>
<td style="text-align:left;width: 7cm; ">
Position on Foreign Policy: 3
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Ideological Position: 2
</td>
<td style="text-align:left;width: 7cm; ">
Ideological Position: 2
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Ideological Distance: 3
</td>
<td style="text-align:left;width: 7cm; ">
Ideological Distance: 2
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Issue Congruence: 0
</td>
<td style="text-align:left;width: 7cm; ">
Issue Congruence: 1
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Political Knowledge: 1
</td>
<td style="text-align:left;width: 7cm; ">
Political Knowledge: 1
</td>
</tr>
<tr>
<td style="text-align:left;width: 7cm; ">
Bullshit Receptivity: 3
</td>
<td style="text-align:left;width: 7cm; ">
Bullshit Receptivity: 2
</td>
</tr>
</tbody>
</table>
