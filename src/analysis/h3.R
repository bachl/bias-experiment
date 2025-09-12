df_us <- d |>  
  filter(country == "US") |>
  select(stance_nlp2, stance_ss2, masking, specified, distance, PT7, PT7b, issue, sentence2)
df_nl <- d |>  
  filter(country == "NL") |> 
  select(stance_nlp2, stance_ss2, masking, specified, distance, PT7, issue, sentence2)

df <- df_us %>% 
  mutate(b= factor(masking),
         a = distance)
h3a <- lmer(stance_nlp2 ~ specified + b * a +
              PT7 + issue + (1 | sentence2), data= df)
h3a_2 <- tidy(h3a) %>% 
  mutate(hyp = "H3a: Ideological Distance",
         type = "United States",
         y = "Correctly Interpreting Stance (Strict Interpretation)")
h3a <- broom.mixed::tidy(margins::margins(h3a, variables = "b",
                             at = list("a" = 0:1))) %>% 
  mutate(hyp = "H3a: Ideological Distance",
         type = "United States",
         y = "Correctly Interpreting Stance (Strict Interpretation)")

tmp <- lmer(stance_ss2 ~ specified + b * a +
              PT7 + issue + (1 | sentence2), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "H3a: Ideological Distance",
         type = "United States",
         y = "Correctly Interpreting Stance (Lenient Interpretation)")
h3a_2 <- h3a_2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:1))) %>% 
  mutate(hyp = "H3a: Ideological Distance",
         type = "United States",
         y = "Correctly Interpreting Stance (Lenient Interpretation)")

h3a <- h3a %>% 
  add_case(tmp)

df <- df_nl %>% 
  mutate(b= factor(masking),
         a = distance)

tmp <- lmer(stance_nlp2 ~ specified + b * a +
              PT7 + issue + (1 | sentence2), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "H3a: Ideological Distance",
         type = "The Netherlands",
         y = "Correctly Interpreting Stance (Strict Interpretation)")
h3a_2 <- h3a_2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:1))) %>% 
  mutate(hyp = "H3a: Ideological Distance",
         type = "The Netherlands",
         y = "Correctly Interpreting Stance (Strict Interpretation)")

h3a <- h3a %>% 
  add_case(tmp)


tmp <- lmer(stance_ss2 ~ specified + b * a +
              PT7 + issue + (1 | sentence2), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "H3a: Ideological Distance",
         type = "The Netherlands",
         y = "Correctly Interpreting Stance (Lenient Interpretation)")
h3a_2 <- h3a_2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:1))) %>% 
  mutate(hyp = "H3a: Ideological Distance",
         type = "The Netherlands",
         y = "Correctly Interpreting Stance (Lenient Interpretation)")

h3a <- h3a %>% 
  add_case(tmp)


df <- df_us %>% 
  mutate(b= factor(masking),
         a = PT7)
h3b <- lmer(stance_nlp2 ~ specified + b * a +
              distance + issue + (1 | sentence2), data= df)
h3b_2 <- tidy(h3b) %>% 
  mutate(hyp = "H3b: Political Knowledge",
         type = "United States",
         y = "Correctly Interpreting Stance (Strict Interpretation)")
h3b <- tidy(margins::margins(h3b, variables = "b",
                             at = list("a" = 0:1))) %>% 
  mutate(hyp = "H3b: Political Knowledge",
         type = "United States",
         y = "Correctly Interpreting Stance (Strict Interpretation)")

tmp <- lmer(stance_ss2 ~ specified + b * a +
              distance + issue + (1 | sentence2), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "H3b: Political Knowledge",
         type = "United States",
         y = "Correctly Interpreting Stance (Lenient Interpretation)")
h3b_2 <- h3b_2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:1))) %>% 
  mutate(hyp = "H3b: Political Knowledge",
         type = "United States",
         y = "Correctly Interpreting Stance (Lenient Interpretation)")

h3b <- h3b %>% 
  add_case(tmp)

df <- df_nl %>% 
  mutate(b= factor(masking),
         a = PT7)

tmp <- lmer(stance_nlp2 ~ specified + b * a +
              distance + issue + (1 | sentence2), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "H3b: Political Knowledge",
         type = "The Netherlands",
         y = "Correctly Interpreting Stance (Strict Interpretation)")
h3b_2 <- h3b_2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:1))) %>% 
  mutate(hyp = "H3b: Political Knowledge",
         type = "The Netherlands",
         y = "Correctly Interpreting Stance (Strict Interpretation)")

h3b <- h3b %>% 
  add_case(tmp)

tmp <- lmer(stance_ss2 ~ specified + b * a +
              distance + issue + (1 | sentence2), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "H3b: Political Knowledge",
         type = "The Netherlands",
         y = "Correctly Interpreting Stance (Lenient Interpretation)")
h3b_2 <- h3b_2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:1))) %>% 
  mutate(hyp = "H3b: Political Knowledge",
         type = "The Netherlands",
         y = "Correctly Interpreting Stance (Lenient Interpretation)")

h3b <- h3b %>% 
  add_case(tmp)


p3a <- h3a_2  %>% 
  add_case(h3b_2) %>% 
  filter(term %in% c("a","b1", "b1:a")) %>% 
  mutate(term = ifelse(term == "a" & hyp == "H3a: Ideological Distance",
                       "Ideological Distance from Party", term),
         term = ifelse(term == "a" & hyp != "H3a: Ideological Distance",
                       "Poltical Knowledge", term),
         term = ifelse(term == "b1", "Condition: Masked Actor", term),
         term = ifelse(term == "b1:a" & hyp == "H3a: Ideological Distance",
                       "Interaction: Masking * Distance", term),
         term = ifelse(term == "b1:a" & hyp != "H3a: Ideological Distance",
                       "Interaction: Masking * Knowledge", term), 
         lower = estimate - 1.645 * std.error,
         upper = estimate + 1.645 * std.error) %>% 
  ggplot(aes(x = estimate, y = term,
             xmin = lower, xmax = upper,
             color = y)) +
  geom_point(position = position_dodge(0.5)) +
  geom_errorbar(width = 0,
                position = position_dodge(0.5)) +
  facet_grid(hyp~type, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray25") +
  labs(x = "Predicted Effect for Correctly Interpreting Stance", y = "") +
  theme_ipsum() +
  scale_colour_manual(values = fig_cols) +
  theme(legend.position = "bottom",
        legend.title = element_blank())


p3b <- h3a  %>% 
  add_case(h3b) %>% 
  ggplot(aes(x = `at.value`, y = estimate,
             ymin = (estimate - (1.645 * `std.error`)),
             ymax = (estimate + (1.645 * `std.error`)),
             color = y, fill = y)) +
  geom_ribbon(alpha = 0.2, position = position_dodge(.2)) +
  geom_line(position = position_dodge(.2)) +
  facet_grid(hyp~type, scales = "free") +
  labs(x = "",
       y = "Marginal Effect of Masking \n for Correctly Interpreting Stance") +
  theme_ipsum() +
  geom_hline(yintercept = 0, color = "lightgray", size = .8,
             linetype = "dashed") +
  scale_colour_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  theme(legend.position = "bottom",
        legend.title = element_blank())


df <- df_us %>% 
  mutate(b= factor(masking),
         a = PT7b)

h3b_bs <- lmer(stance_nlp2 ~ specified + b * a +
              distance + issue + (1 | sentence2), data= df)
h3b_2bs <- tidy(h3b_bs) %>% 
  mutate(hyp = "Exploration: Bullshit Receptivity",
         type = "United States",
         y = "Correctly Interpreting Stance (Strict Interpretation)")
h3b_bs <- tidy(margins::margins(h3b_bs, variables = "b",
                             at = list("a" = 1:5))) %>% 
  mutate(hyp = "Exploration: Bullshit Receptivity",
         type = "United States",
         y = "Correctly Interpreting Stance (Strict Interpretation)")

tmp <- lmer(stance_ss2 ~ specified + b * a +
              distance + issue + (1 | sentence2), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Exploration: Bullshit Receptivity",
         type = "United States",
         y = "Correctly Interpreting Stance (Lenient Interpretation)")
h3b_2bs <- h3b_2bs %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 1:5))) %>% 
  mutate(hyp = "Exploration: Bullshit Receptivity",
         type = "United States",
         y = "Correctly Interpreting Stance (Lenient Interpretation)")

h3b_bs <- h3b_bs %>% 
  add_case(tmp)

p3c1 <- h3b_2bs %>% 
  filter(term %in% c("a","b1", "b1:a")) %>% 
  mutate(term = ifelse(term == "a" & hyp == "H3a: Ideological Distance",
                       "Ideological Distance from Party", term),
         term = ifelse(term == "a",
                       "Bullshit Receptivity", term),
         term = ifelse(term == "b1", "Condition: Masked Actor", term),
         term = ifelse(term == "b1:a",
                       "Interaction: Masking * Bullshit Receptivity", term),
         lower = estimate - 1.645 * std.error,
         upper = estimate + 1.645 * std.error,
         hyp = factor(hyp,
                      levels = c("H3b: Political Knowledge",
                                 "Exploration: Bullshit Receptivity"))) %>% 
  ggplot(aes(x = estimate, y = term,
             xmin = lower, xmax = upper,
             color = y)) +
  geom_point(position = position_dodge(0.5)) +
  geom_errorbar(width = 0,
                position = position_dodge(0.5)) +
  facet_grid(hyp~type, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray25") +
  labs(x = "Predicted Effect for Correctly Interpreting Stance", y = "") +
  theme_ipsum() +
  scale_colour_manual(values = fig_cols) +
  theme(legend.position = "bottom",
        legend.title = element_blank())

p3c2 <- h3b_bs  %>% 
  add_case(h3b) %>% 
  filter(type== "United States") %>%
  mutate(hyp = factor(hyp,
                      levels = c("H3b: Political Knowledge",
                                 "Exploration: Bullshit Receptivity"))) %>% 
  ggplot(aes(x = `at.value`, y = estimate,
             ymin = (estimate - (1.65 * `std.error`)),
             ymax = (estimate + (1.65 * `std.error`)),
             color = y, fill = y)) +
  geom_ribbon(alpha = 0.2, position = position_dodge(.2)) +
  geom_line(position = position_dodge(.2)) +
  facet_grid(type~hyp, scales = "free") +
  labs(x = "",
       y = "Marginal Effect of Masking \n for Correctly Interpreting Stance") +
  theme_ipsum() +
  scale_colour_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  theme(legend.position = "none",
        legend.title = element_blank())

## Effects plot
df <- d |> 
  filter(country == "US") |> 
  mutate(masking = factor(masking,
                          labels = c("Party", "Masked")))

pred3a_1 <- lmer(stance_nlp2 ~ specified + masking * distance +
                   PT7 + issue + (1 | sentence2), data= df)
p3a_1 <- sjPlot::plot_model(pred3a_1, type = "pred", 
                            terms = c("distance", "masking"), 
                            colors = fig_cols) +
  labs(x = "Ideological Distance",
       y = "Predicted Effect for Correctly Interpreting Stance",
       title = "Strict Interpretation - US") +
  theme_ipsum() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

pred3a_2 <- lmer(stance_ss2 ~ specified + masking * distance +
                   PT7 + issue + (1 | sentence2), data= df)
p3a_2 <- sjPlot::plot_model(pred3a_2, type = "pred", terms = c("distance", "masking"), colors = fig_cols) +
  labs(x = "Ideological Distance",
       y = "Predicted Effect for Correctly Interpreting Stance",
       title = "Lenient Interpretation - US") +
  theme_ipsum() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

pred3b_1 <- lmer(stance_nlp2 ~  specified + masking * PT7 +
                   PT7 + issue + (1 | sentence2), data= df)
p3b_1 <- sjPlot::plot_model(pred3b_1, type = "pred", terms = c("PT7", "masking"), colors = fig_cols) +
  labs(x = "Political Knowledge",
       y = "Predicted Effect for Correctly Interpreting Stance",
       title = "Strict Interpretation - US") +
  theme_ipsum() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

pred3b_2 <- lmer(stance_ss2 ~ specified + masking * PT7 +
                   PT7 + issue + (1 | sentence2), data= df)
p3b_2 <- sjPlot::plot_model(pred3b_2, type = "pred", terms = c("PT7", "masking"), colors = fig_cols) +
  labs(x = "Political Knowledge",
       y = "Predicted Effect for Correctly Interpreting Stance",
       title = "Lenient Interpretation - US") +
  theme_ipsum() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

df <- d |> 
  filter(country == "NL") |> 
  mutate(masking = factor(masking,
                          labels = c("Party", "Masked")))

pred3a_3 <- lmer(stance_nlp2 ~ specified +  masking * distance +
                   PT7 + issue + (1 | sentence2), data= df)
p3a_3 <- sjPlot::plot_model(pred3a_3, type = "pred", terms = c("distance", "masking"),
                            colors = fig_cols) +
  labs(x = "Ideological Distance",
       y = "Predicted Effect for Correctly Interpreting Stance",
       title = "Strict Interpretation - NL") +
  theme_ipsum() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

pred3a_4 <- lmer(stance_ss2 ~ specified + masking * distance +
                   PT7 + issue + (1 | sentence2), data= df)
p3a_4 <- sjPlot::plot_model(pred3a_4, type = "pred", terms = c("distance", "masking"), colors = fig_cols) +
  labs(x = "Ideological Distance",
       y = "Predicted Effect for Correctly Interpreting Stance",
       title = "Lenient Interpretation - NL") +
  theme_ipsum() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

pred3b_3 <- lmer(stance_nlp2 ~ specified + masking * PT7 +
                   PT7 + issue + (1 | sentence2), data= df)
p3b_3 <- sjPlot::plot_model(pred3b_3, type = "pred", terms = c("PT7", "masking"), colors = fig_cols) +
  labs(x = "Political Knowledge",
       y = "Predicted Effect for Correctly Interpreting Stance",
       title = "Strict Interpretation - NL") +
  theme_ipsum() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

pred3b_4 <- lmer(stance_ss2 ~ specified + masking * PT7 +
                   PT7 + issue + (1 | sentence2), data= df)
p3b_4 <- sjPlot::plot_model(pred3b_4, type = "pred", terms = c("PT7", "masking"), colors = fig_cols) +
  labs(x = "Political Knowledge",
       y = "Predicted Effect for Correctly Interpreting Stance",
       title = "Lenient Interpretation - NL") +
  theme_ipsum() +
  theme(legend.position = "bottom",
        legend.title = element_blank())



