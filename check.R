# Data and functions
source(here::here("src/lib/functions.R")) # edited to remove qualtrics stuff
load(here::here("data/intermediate/cleaned_data_joined.RData"))

# From h1.R
# For US
df_us <- d |>  
  filter(country == "US") |> 
  select(stance_nlp2, stance_ss2, masking, specified, distance, PT7, issue)
df_us |> 
  names() # does not include "sentence" 

# Get complete data for US
d |>  
  filter(country == "US") |> 
  distinct(issue) # Duplicated issues

# Fix duplicated issues 
df_us2 <- d |>  
  filter(country == "US") |> 
  mutate(issue2 = case_when(
    str_detect(issue, "carbon") ~ "carbon",
    str_detect(issue, "tax") ~ "tax",
    str_detect(issue, "military") ~ "military",
    str_detect(issue, "immigration") ~ "immigration"
  ))
df_us2 |> 
  distinct(issue2, issue) |>
  arrange(issue2) # check: 4 unique issues

# Fit model from H1.R
lmer(stance_nlp2 ~ masking + specified + 
       distance + PT7 + issue2 +
       (1 | sentence), data=df_us2) |> 
  summary(correlation = FALSE) # seems fine

# For NL
df_nl <- d |> 
  filter(country == "NL") |> 
  select(stance_nlp2, stance_ss2, masking, specified, distance, PT7, issue)
df_nl |> 
  names() # does not include "sentence" 

# Get complete data for NL
d |>  
  filter(country == "NL") |> 
  distinct(issue) # Duplicated issues

# Fix duplicated issues 
df_nl2 <- d |>  
  filter(country == "NL") |> 
  mutate(issue2 = case_when(
    str_detect(issue, "immigra") ~ "immigratie",
    str_detect(issue, "stikstof") ~ "stikstof",
    str_detect(issue, "belasting") ~ "belasting",
    str_detect(issue, "Europese Unie") ~ "EU"
  ))
df_nl2 |> 
  distinct(issue2, issue) |>
  arrange(issue2) # check: 4 unique issues

# Fit model from H1.R
lmer(stance_nlp2 ~ masking + specified + 
       distance + PT7 + issue2 +
       (1 | sentence), data=df_nl2) |> 
  summary(correlation = FALSE) # seems fine
