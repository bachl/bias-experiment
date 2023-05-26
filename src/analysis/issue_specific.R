df_us <- d_us %>% 
  select(stance, masking, specification, interpretation, country, issue)
df_nl <- d_nl %>% 
  select(stance, masking, specification, interpretation, country, issue) %>% 
  mutate(issue =ifelse(issue=="EU", "Foreign Policy", issue))

d <- df_nl %>% 
  add_case(df_us)

issues <- unique(d$issue)

for(i in 1:length(issues)){
  df <- d %>% filter(issue==issues[i])
  if(i==1){
    b1 <- broom.mixed::tidy(lm(stance ~ masking + specification + 
                                   factor(country), data= df)) %>%
      mutate(y = "Y: Correctly Interpreting Stance",
             issue = issues[i],
             type = "Pooled Analysis")
  }
  else{
    tmp <- broom.mixed::tidy(lm(stance ~ masking + specification + 
                                    factor(country), data= df)) %>%
      mutate(y = "Y: Correctly Interpreting Stance",
             issue = issues[i],
             type = "Pooled Analysis")
    b1 <- b1 %>% 
      add_case(tmp)
  }
}

for(i in 1:length(issues)){
  df <- df_us %>% filter(issue==issues[i])
  tmp <- broom::tidy(lm(stance ~ masking + specification , data= df)) %>%
      mutate(y = "Y: Correctly Interpreting Stance",
             issue = issues[i],
             type = "United States")
    b1 <- b1 %>% 
      add_case(tmp)
}

for(i in 1:length(issues)){
  df <- df_nl %>% filter(issue==issues[i])
  tmp <- broom::tidy(lm(stance ~ masking + specification , data= df)) %>%
    mutate(y = "Y: Correctly Interpreting Stance",
           issue = issues[i],
           type = "The Netherlands")
  b1 <- b1 %>% 
    add_case(tmp)
}


for(i in 1:length(issues)){
  df <- d %>% filter(issue==issues[i])
  if(i==1){
    b2 <- broom::tidy(lm(interpretation ~ masking + specification, data= df)) %>%
      mutate(y = "Y: Overinterpreting Stance",
             issue = issues[i],
             type = "Pooled Analysis")
  }
  else{
    tmp <- broom::tidy(lm(interpretation ~ masking + 
                            specification + factor(country), data= df)) %>%
      mutate(y = "Y: Overinterpreting Stance",
             issue = issues[i],
             type = "Pooled Analysis")
    b2 <- b2 %>% 
      add_case(tmp)
  }
}

for(i in 1:length(issues)){
  df <- df_us %>% filter(issue==issues[i])
  tmp <- broom::tidy(lm(interpretation ~ masking + specification , data= df)) %>%
    mutate(y = "Y: Overinterpreting Stance",
           issue = issues[i],
           type = "United States")
  b2 <- b2 %>% 
    add_case(tmp)
}

for(i in 1:length(issues)){
  df <- df_nl %>% filter(issue==issues[i])
  tmp <- broom::tidy(lm(interpretation ~ masking + specification , data= df)) %>%
    mutate(y = "Y: Overinterpreting Stance",
           issue = issues[i],
           type = "The Netherlands")
  b2 <- b2 %>% 
    add_case(tmp)
}

b_i <- b1 %>% 
  add_case(b2) %>% 
  filter(term %in% c("(Intercept)",
                     "maskingParty",
                     "specificationUnderspecified"),
         type != "Pooled Analysis") %>% 
  mutate(term = recode(term,
                       `(Intercept)` = "Intercept",
                       `maskingParty` = "Condition:Masked Political Actor",
                       `specificationUnderspecified` = "Condition: Underspecified Sentence"),
         issue = ifelse(issue=="Foreign Policy" & type == "The Netherlands",
                        "EU", issue)) %>% 
  ggplot(aes(y = term, x = estimate,
             xmin = estimate -1.56*std.error,
             xmax = estimate +1.56*std.error,
             color = type)) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(position = position_dodge(.5), width = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray25") +
  facet_grid(issue~y) +
  labs(y = "", x = "Predicted Effect for Y") +
  theme_ipsum() +
  scale_colour_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  theme(legend.position = "bottom",
        legend.title = element_blank())

b_ib <- b1 %>% 
  add_case(b2) %>% 
  filter(term %in% c("(Intercept)",
                     "maskingParty",
                     "specificationUnderspecified")) %>% 
  mutate(term = recode(term,
                       `(Intercept)` = "Intercept",
                       `maskingParty` = "Condition:Masked Political Actor",
                       `specificationUnderspecified` = "Condition: Underspecified Sentence")) %>% 
  ggplot(aes(y = term, x = estimate,
             xmin = estimate -1.56*std.error,
             xmax = estimate +1.56*std.error,
             color = type)) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(position = position_dodge(.5), width = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray25") +
  facet_grid(issue~y) +
  labs(y = "", x = "Predicted Effect for Y") +
  theme_ipsum() +
  scale_colour_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  theme(legend.position = "bottom",
        legend.title = element_blank())
