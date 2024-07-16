#df_us <- d_us %>% 
#  mutate(country = "United States") %>% 
#  select(stance_nlp, stance_ss, masking, specification, interpretation_nlp, interpretation_ss, country, issue)
#df_nl <- d_nl %>% 
#  mutate(country = "The Netherlands") %>% 
#  select(stance_nlp, stance_ss, masking, specification, interpretation_nlp, interpretation_ss, country, issue)

#d <- df_nl %>% 
#  add_case(df_us)

b1 <- broom.mixed::tidy(lmer(stance_nlp2 ~ masking + specified + 
                         (country | issue), data= d)) %>%
  mutate(y = "Y: Correctly Interpreting Stance (Strict Interpretation)",
         type = "Pooled Analysis")

tmp <- broom.mixed::tidy(lmer(stance_ss2 ~ masking + specified + 
                               (country | issue), data= d)) %>%
  mutate(y = "Y: Correctly Interpreting Stance (Lenient Interpretation)",
         type = "Pooled Analysis")

b1 <- b1 %>% 
  add_case(tmp)

df_us <- d |> 
  filter(country == "US")
tmp <-  broom.mixed::tidy(lmer(stance_nlp2 ~ masking + specified + 
                                (1 | issue), data = df_us)) %>%
  mutate(y = "Y: Correctly Interpreting Stance (Strict Interpretation)",
         type = "United States")

b1 <- b1 %>% 
  add_case(tmp)

tmp <- broom.mixed::tidy(lmer(stance_ss2 ~ masking + specified + 
                                (1 | issue), data= df_us)) %>%
  mutate(y = "Y: Correctly Interpreting Stance (Lenient Interpretation)",
         type = "United States")

b1 <- b1 %>% 
  add_case(tmp)

df_nl <- d |> 
  filter(country == "NL")
tmp <- broom.mixed::tidy(lmer(stance_nlp2 ~ masking + specified + 
                                (1 | issue), data= df_nl)) %>%
  mutate(y = "Y: Correctly Interpreting Stance (Strict Interpretation)",
         type = "The Netherlands")

b1 <- b1 %>% 
  add_case(tmp)

tmp <- broom.mixed::tidy(lmer(stance_ss2 ~ masking + specified + 
                                (1 | issue), data= df_nl)) %>%
  mutate(y = "Y: Correctly Interpreting Stance (Lenient Interpretation)",
         type = "The Netherlands")

b1 <- b1 %>% 
  add_case(tmp)

b2 <- broom.mixed::tidy(lmer(interpret_nlp2 ~ masking + specified + 
                               (country | issue), data= d)) %>%
  mutate(y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "Pooled Analysis")

tmp <- broom.mixed::tidy(lmer(interpret_ss2 ~ masking + specified + 
                                   (country | issue), data= d)) %>%
  mutate(y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "Pooled Analysis")

b2 <- b2 %>% 
  add_case(tmp)

tmp <- broom.mixed::tidy(lmer(interpret_nlp2 ~ masking + specified + 
                                (1 | issue), data= df_us)) %>%
  mutate(y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "United States")

b2 <- b2 %>% 
  add_case(tmp)

tmp <- broom.mixed::tidy(lmer(interpret_ss2 ~ masking + specified + 
                                (1 | issue), data= df_us)) %>%
  mutate(y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "United States")

b2 <- b2 %>% 
  add_case(tmp)

tmp <- broom.mixed::tidy(lmer(interpret_nlp2 ~ masking + specified + 
                                (1 | issue), data= df_nl)) %>%
  mutate(y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "The Netherlands")

b2 <- b2 %>% 
  add_case(tmp)

tmp <- broom.mixed::tidy(lmer(interpret_ss2 ~ masking + specified + 
                                (1 | issue), data= df_nl)) %>%
  mutate(y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "The Netherlands")

b2 <- b2 %>% 
  add_case(tmp)
  
b <- b1 %>% 
  add_case(b2) %>% 
  filter(term %in% c("(Intercept)",
                     "masking1",
                     "specifiedUnderspecified"),
         type != "Pooled Analysis") %>% 
  mutate(term = recode(term,
                       `(Intercept)` = "Intercept",
                       `masking1` = "Condition: Masked Political Actor",
                       `specifiedUnderspecified` = "Condition: Underspecified Sentence")) %>% 
  ggplot(aes(y = term, x = estimate,
             xmin = estimate -1.56*std.error,
             xmax = estimate +1.56*std.error,
             color = y)) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(position = position_dodge(.5), width = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray25") +
  facet_grid(.~type) +
  labs(y = "", x = "Predicted Effect for Y") +
  theme_ipsum() +
  scale_colour_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))

b_b <- b1 %>% 
  #add_case(b2) %>% 
  filter(term %in% c("(Intercept)",
                     "masking1",
                     "specifiedUnderspecified")) %>% 
  mutate(term = recode(term,
                       `(Intercept)` = "Intercept",
                       `masking1` = "Condition:Masked Political Actor",
                       `specifiedUnderspecified` = "Condition: Underspecified Sentence")) %>% 
  ggplot(aes(y = term, x = estimate,
             xmin = estimate -1.56*std.error,
             xmax = estimate +1.56*std.error,
             color = y)) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(position = position_dodge(.5), width = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray25") +
  facet_grid(.~type) +
  labs(y = "", x = "Predicted Effect for Y") +
  theme_ipsum() +
  scale_colour_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))
