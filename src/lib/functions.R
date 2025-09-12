library(tidyverse)
library(haven)
library(here)
library(hrbrthemes)
library(ggstatsplot)
library(lme4)
library(broom.mixed)
library(cobalt)
library(patchwork)
library(kableExtra)
library(qualtRics)

fig_cols <- yarrr::piratepal(palette = "basel", 
                             trans = .2)
fig_cols <- as.character(fig_cols[1:9])

# api_key_fn <- here("data/raw-private/qualtrics_api_key.txt")
# API <- read_file(api_key_fn) %>% trimws()