df_us <- d %>% 
  filter(country == "US") %>% 
  select(stance_nlp2, stance_ss2, masking, specified, interpret_nlp2, interpret_ss2, country, issue) |> 
  mutate(issue2 = recode(issue,
                         `tax on carbon emissions` = "Climate",
                         `a wealth tax for the richest Americans` = "Tax",
                         `the tax system` = "Tax",
                         `military presence in the Pacific Ocean` = "EU/Foreign Policy",
                         `military build-up in the Pacific Ocean` = "EU/Foreign Policy"))
df_nl <- d %>% 
  filter(country == "NL") %>% 
  select(stance_nlp2, stance_ss2, masking, specified, interpret_nlp2, interpret_ss2, country, issue) |> 
  mutate(issue2 = recode(issue,
                         `immigranten` = "Immigration",
                         `immigratie` = "Immigration",
                         `het tegengaan van stikstofuitstoot` = "Climate",
                         `het stikstofbeleid` = "Climate",
                         `het verhogen van het belastingstarief voor de hoogste inkomens` = "Tax",
                         `het belastingstelsel` = "Tax",
                         `de  rol van Nederland in de Europese Unie` = "EU/Foreign Policy",
                         `het lidmaatschap van de Europese Unie voor Nederland` = "EU/Foreign Policy"))

issues_us <- unique(df_us$issue2)
issues_nl <- unique(df_nl$issue2)

for(i in 1:length(issues_us)){
  df <- df_us %>% filter(issue==issues_us[i])
  if(i==1){
    b1 <- broom::tidy(lm(stance_nlp2 ~ masking + specified , data= df_us)) %>%
      mutate(y = "Y: Correctly Interpreting Stance (Strict Interpretation)",
             issue = issues_us[i],
             type = "United States")}
  else{
    tmp <- broom::tidy(lm(stance_nlp2 ~ masking + specified , data= df_us)) %>%
      mutate(y = "Y: Correctly Interpreting Stance (Strict Interpretation)",
             issue = issues_us[i],
             type = "United States")
    b1 <- b1 %>% 
      add_case(tmp)}
}

for(i in 1:length(issues_us)){
  df <- df_us %>% filter(issue==issues_us[i])
  tmp <- broom::tidy(lm(stance_ss2 ~ masking + specified , data= df_us)) %>%
    mutate(y = "Y: Correctly Interpreting Stance (Lenient Interpretation)",
           issue = issues_us[i],
           type = "United States")
  b1 <- b1 %>% 
    add_case(tmp)
}

for(i in 1:length(issues_nl)){
  df <- df_nl %>% filter(issue==issues_nl[i])
  tmp <- broom::tidy(lm(stance_nlp2 ~ masking + specified , data= df_nl)) %>%
    mutate(y = "Y: Correctly Interpreting Stance (Strict Interpretation)",
           issue = issues_nl[i],
           type = "The Netherlands")
  b1 <- b1 %>% 
    add_case(tmp)
}

for(i in 1:length(issues_nl)){
  df <- df_nl %>% filter(issue==issues_nl[i])
  tmp <- broom::tidy(lm(stance_ss2 ~ masking + specified , data= df_nl)) %>%
    mutate(y = "Y: Correctly Interpreting Stance (Lenient Interpretation)",
           issue = issues_nl[i],
           type = "The Netherlands")
  b1 <- b1 %>% 
    add_case(tmp)
}

for(i in 1:length(issues_us)){
  df <- df_us %>% filter(issue==issues_us[i])
  if(i==1){
    b2 <- broom::tidy(lm(interpret_nlp2 ~ masking + specified, data= df_us)) %>%
      mutate(y = "Y: Overinterpreting Stance (Strict Interpretation)",
             issue = issues_us[i],
             type = "United States")
  }
  else{
    tmp <- broom::tidy(lm(interpret_nlp2 ~ masking + 
                            specified, data= df_us)) %>%
      mutate(y = "Y: Overinterpreting Stance (Strict Interpretation)",
             issue = issues_us[i],
             type = "United States")
    b2 <- b2 %>% 
      add_case(tmp)
  }
}

for(i in 1:length(issues_us)){
  df <- df_us %>% filter(issue==issues_us[i])
  tmp <- broom::tidy(lm(interpret_ss2 ~ masking + specified , data= df_us)) %>%
    mutate(y = "Y: Overinterpreting Stance (Lenient Interpretation)",
           issue = issues_us[i],
           type = "United States")
  b2 <- b2 %>% 
    add_case(tmp)
}


for(i in 1:length(issues_nl)){
  df <- df_nl %>% filter(issue==issues_nl[i])
  tmp <- broom::tidy(lm(interpret_nlp2 ~ masking + specified , data= df_nl)) %>%
    mutate(y = "Y: Overinterpreting Stance (Strict Interpretation)",
           issue = issues_nl[i],
           type = "The Netherlands")
  b2 <- b2 %>% 
    add_case(tmp)
}

for(i in 1:length(issues_nl)){
  df <- df_nl %>% filter(issue==issues_nl[i])
  tmp <- broom::tidy(lm(interpret_ss2 ~ masking + specified , data= df_nl)) %>%
    mutate(y = "Y: Overinterpreting Stance (Lenient Interpretation)",
           issue = issues_nl[i],
           type = "The Netherlands")
  b2 <- b2 %>% 
    add_case(tmp)
}

b_i <- b1 %>% 
  add_case(b2) %>% 
  filter(term %in% c("(Intercept)",
                     "maskingParty",
                     "specifiedUnderspecified")) %>% 
  mutate(term = recode(term,
                       `(Intercept)` = "Intercept",
                       `maskingParty` = "Condition:Masked Political Actor",
                       `specifiedUnderspecified` = "Condition: Underspecified Sentence"),
         issue = ifelse(issue=="Foreign Policy" & type == "The Netherlands",
                        "EU", issue)) %>% 
  ggplot(aes(y = term, x = estimate,
             xmin = estimate -1.56*std.error,
             xmax = estimate +1.56*std.error,
             color = y)) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(position = position_dodge(.5), width = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray25") +
  facet_grid(issue~type) +
  labs(y = "", x = "Predicted Effect for Y") +
  theme_ipsum() +
  scale_colour_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))
