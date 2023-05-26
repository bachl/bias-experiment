## NL Data
covs <- d_nl %>%
  mutate(treat = if_else(masking == "Masked" & specification == "Specified", 1,
                 if_else(masking == "Masked" & specification == "Underspecified", 2,
                 if_else(masking == "Party" & specification == "Specified", 3, 4)))) %>%
  select(treat, D1:D6, PT1:PT7, attention) %>%
  filter(PT1 != "NA", D6 != "NA")

balanced <-bal.tab(treat ~ D1 + factor(D2) + factor(D3) + 
                     factor(D4) +D5 + factor(D6) + factor(PT1) + PT2 + 
                     PT3 + PT4 + PT5 + PT6 + PT7 + attention,
                   data = covs,
                   thresholds = c(m = 0.05))[[1]] 

df_nl <- balanced %>%
  mutate(variable = c( "Age", "Male",
                       "High Levels of Education",
                       "Low Levels of Education", "Medium Levels of Education",
                       "Region: Big Cities",
                       "Region: East", "Region: North",
                       "Region: South", "Region: West",  
                       "Income", "Job: Fulltime employed",
                      "Job: Retired", "Job: Stay at home", "Job: Student",
                      "Job: Unemployed", "Job: Unemployed and searching",
                      "50+","Bij1", "BBB","CDA",
                      "ChristenUnie", "D66","Denk", 
                      "Forum for Democracy","GroenLinks", "JA21", 
                      "Animal Rights Party",
                      "PvdA",  "PVV","SGP", "SP", "VOLT","VVD",
                      "Migration", "Climate", "Tax", "EU", "Ideology",
                      "Political Knowledge","Attention Check")) %>%
  mutate(variable = factor(variable,
                           levels = c("Age", "Male",
                                      "High Levels of Education",
                                      "Low Levels of Education", "Medium Levels of Education",
                                      "Region: Big Cities",
                                      "Region: East", "Region: North",
                                      "Region: South", "Region: West",  
                                      "Income", "Job: Fulltime employed",
                                      "Job: Retired", "Job: Stay at home", "Job: Student",
                                      "Job: Unemployed", "Job: Unemployed and searching",
                                      "50+","Bij1", "BBB","CDA",
                                      "ChristenUnie", "D66","Denk", 
                                      "Forum for Democracy","GroenLinks", "JA21", 
                                      "Animal Rights Party",
                                      "PvdA",  "PVV","SGP", "SP", "VOLT","VVD",
                                      "Migration", "Climate", "Tax", "EU", "Ideology",
                                      "Political Knowledge","Attention Check")),
         difference = Corr.Un,2) %>%
  filter(variable != "factor(D6)_NA",
         variable != "factor(PT1)_NA") %>% 
  select(variable, difference) %>%
  drop_na() %>%
  mutate(type = if_else(difference <= -.05, "below",
                        if_else(difference >= .05, "below", "above")),
         country = "The Netherlands")

rm(covs, balanced)

## US Data
covs <- d_us %>%
  mutate(treat = if_else(masking == "Masked" & specification == "Specified", 1,
                         if_else(masking == "Masked" & specification == "Underspecified", 2,
                                 if_else(masking == "Party" & specification == "Specified", 3, 4)))) %>%
  select(treat, D1:D6, PT1:PT7b, attention) %>%
  filter(PT1 != "NA", D6 != "NA", D2!= "NA",
         D3 != "NA")

balanced <-bal.tab(treat ~ D1 + factor(D2) + factor(D3) + 
                     factor(D4) +D5 + factor(D6) + factor(PT1) +
                     factor(PT1b) + PT2 + PT3 + PT4 + PT5 + 
                     PT6 + PT7 + PT7b + attention,
                   data = covs,
                   thresholds = c(m = 0.05))[[1]] 

df_us <- balanced %>%
  mutate(variable = c("Age", "Male",
                       "High Levels of Education",
                       "Low Levels of Education", "Medium Levels of Education",
                       "Region: Midwest",
                       "Region: Northeast", "Region: Southeast",
                       "Region: Southwest", "Region: West",  
                       "Income", "Job: Homemaker",
                       "Job: Prmanently disabled", "Job: Retired", "Job: Student",
                       "Job: Temporarily laid off", "Job: Unemployed", "Job: Working now",
                       "PID: Democrat","PID: Independent", "PID: Republican",
                       "PID: Something else","PID Strength: Strong",
                       "Migration", "Climate", "Tax", "Foreign Policy", "Ideology",
                       "Political Knowledge", "Bullshit Receptivity",
                       "Attention Check")) %>%
  mutate(variable = factor(variable,
                           levels = c("Age", "Male",
                                      "High Levels of Education",
                                      "Low Levels of Education", "Medium Levels of Education",
                                      "Region: Midwest",
                                      "Region: Northeast", "Region: Southeast",
                                      "Region: Southwest", "Region: West",  
                                      "Income", "Job: Homemaker",
                                      "Job: Prmanently disabled", "Job: Retired", "Job: Student",
                                      "Job: Temporarily laid off", "Job: Unemployed", "Job: Working now",
                                      "PID: Democrat","PID: Independent", "PID: Republican",
                                      "PID: Something else","PID Strength: Strong",
                                      "Migration", "Climate", "Tax", "Foreign Policy", "Ideology",
                                      "Political Knowledge", "Bullshit Receptivity",
                                      "Attention Check")),
         difference = Corr.Un,2) %>%
  select(variable, difference) %>%
  drop_na() %>%
  mutate(type = if_else(difference <= -.05, "below",
                        if_else(difference >= .05, "below", "above")),
         country = "United States")

df <- df_nl %>% 
  add_case(df_us) %>% 
  ggplot(aes(x = variable, y = difference, color = type)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("above"=fig_cols[3],
                                "below"=fig_cols[2])) +
  
  labs(x="", y= "Standardized Mean Differences") +
  coord_flip() +
  theme_ipsum() +
  facet_grid(.~country, scales = "free") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none",) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  geom_hline(yintercept = -0.05, linetype = "dashed")
  

  
  rm(covs, balanced)
