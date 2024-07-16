Prepare Data
================

- [Required Packages &
  Reproducibility](#required-packages--reproducibility)
- [Tidy Data](#tidy-data)
- [Save Data for Analysis](#save-data-for-analysis)
- [Visualization of Data](#visualization-of-data)
  - [Dependent Variable by Issue](#dependent-variable-by-issue)
  - [Covariates](#covariates)
  - [Correlations Matrix](#correlations-matrix)

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
#Load Dutch Data
d_nl <- read_sav(here("data/raw-private/NLOZ2206_Mariken_Bias-experiment.sav"))

#Load US Data
d_us <- fetch_survey(surveyID = "SV_86XBcsFUJka9eke", 
                    verbose = TRUE, force_request = T,
                    label = FALSE, convert = FALSE)
#Clean data
source(here("src/data-processing/clean_data.R"))
```

## Save Data for Analysis

``` r
save(d, file = here("data/intermediate/cleaned_data_joined.RData"))
```

## Visualization of Data

### Dependent Variable by Issue

<img src="../../report/figures/Dependent Variable-1.png" style="display: block; margin: auto;" />

### Covariates

<img src="../../report/figures/Independent Variables-1.png" style="display: block; margin: auto;" />

### Correlations Matrix

<img src="../../report/figures/Correlations Matrix-1.png" style="display: block; margin: auto;" /><img src="../../report/figures/Correlations Matrix-2.png" style="display: block; margin: auto;" />
