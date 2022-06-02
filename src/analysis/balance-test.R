covs <- d %>%
  mutate(treat = if_else(actor == "masked" & sentence == "specified", 1,
                 if_else(actor == "masked" & sentence == "underspecified", 2,
                 if_else(actor == "party" & sentence == "specified", 3, 4)))) %>%
  select(treat, D1:D7, PT1, PT7) %>%
  drop_na()

balanced <-bal.tab(treat ~ factor(D1) +  D2 + factor(D3) + 
                     D5 + D7 + factor(PT1) + PT2 + 
                     PT3 + PT4 + PT5 + PT6 + PT7,
                   data = covs,
                   thresholds = c(m = 0.05))[[1]] 

df <- balanced %>%
  mutate(variable = c("Male", "Age", "Region: Big Cities",
                      "Region: West", "Region: North", "Region: East", 
                      "Region: South", "Income",
                      "High Levels of Education",
                      "Low Levels of Education", "Medium Levels of Education",
                      "50+", "BBB", "Bij1",  "Blanco", "CDA",
                      "ChristenUnie", "D66", "Didn't vote", "Don't know",
                      "Forum for Democracy","GroenLinks", "JA21", 
                      "Not eligible", "Other Party","PvdA", "Animal Rights Party", "PVV", 
                      "SGP", "SP", "VOLT","VVD",
                      "Migration", "Climate", "Tax", "EU")) %>%
  mutate(variable = factor(variable,
                           levels = c("Male", "Age", "Region: Big Cities",
                                      "Region: West", "Region: North", "Region: East", 
                                      "Region: South", "Income",
                                      "High Levels of Education",
                                      "Low Levels of Education", "Medium Levels of Education",
                                      "50+", "BBB", "Bij1",  "Blanco", "CDA",
                                      "ChristenUnie", "D66", "Didn't vote", "Don't know",
                                      "Forum for Democracy","GroenLinks", "JA21", 
                                      "Not eligible", "Other Party","PvdA", "Animal Rights Party", "PVV", 
                                      "SGP", "SP", "VOLT","VVD",
                                      "Migration", "Climate", "Tax", "EU")),
         difference = Corr.Un,2) %>%
  select(variable, difference) %>%
  drop_na() %>%
  mutate(type = if_else(difference <= -.05, "below",
                        if_else(difference >= .05, "below", "above"))) %>%
  ggplot(aes(x = variable, y = difference, color = type)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("above"=fig_cols[3],
                                "below"=fig_cols[2])) +
  
  labs(x="", y= "Standardized Mean Differences") +
  coord_flip() +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none",) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  geom_hline(yintercept = -0.05, linetype = "dashed") +

rm(covs, balanced)
