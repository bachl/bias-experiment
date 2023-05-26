Prepare Data
================

- <a href="#required-packages--reproducibility"
  id="toc-required-packages--reproducibility">Required Packages &amp;
  Reproducibility</a>
- <a href="#tidy-data" id="toc-tidy-data">Tidy Data</a>
- <a href="#save-data-for-analysis" id="toc-save-data-for-analysis">Save
  Data for Analysis</a>
- <a href="#visualization-of-data"
  id="toc-visualization-of-data">Visualization of Data</a>
  - <a href="#dependent-variable-by-issue"
    id="toc-dependent-variable-by-issue">Dependent Variable by Issue</a>
  - <a href="#covariates" id="toc-covariates">Covariates</a>
  - <a href="#correlations-matrix" id="toc-correlations-matrix">Correlations
    Matrix</a>

## Required Packages & Reproducibility

``` r
rm(list=ls())
source(here::here("src/lib/functions.R"))
#renv::snapshot()
```

## Tidy Data

This code chuck downloads the data from Qualtrics using the API and
cleans the raw data.

``` r
#Clean Dutch Data
d_nl <- read_sav(here("data/raw-private/NLOZ2206_Mariken_Bias-experiment.sav"))
source(here("src/data-processing/clean_data_nl.R"))

#Clean US Data
d_us <- fetch_survey(surveyID = "SV_86XBcsFUJka9eke", 
                    verbose = TRUE, force_request = T,
                    label = FALSE, convert = FALSE)
source(here("src/data-processing/clean_data_us.R"))
```

## Save Data for Analysis

``` r
save(d_nl, file = here("data/intermediate/cleaned_data_nl.RData"))
save(d_us, file = here("data/intermediate/cleaned_data_us.RData"))
```

## Visualization of Data

### Dependent Variable by Issue

<img src="../../report/figures/Dependent Variable-1.png" style="display: block; margin: auto;" />

### Covariates

<img src="../../report/figures/Independent Variables-1.png" style="display: block; margin: auto;" />

### Correlations Matrix

<img src="../../report/figures/Correlations Matrix-1.png" style="display: block; margin: auto;" /><img src="../../report/figures/Correlations Matrix-2.png" style="display: block; margin: auto;" />
