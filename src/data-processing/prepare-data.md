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

### Covariates

### Correlations Matrix
