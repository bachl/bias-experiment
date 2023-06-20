df_us <- d_us %>% 
  select(stance_nlp, stance_ss, interpretation_nlp, interpretation_ss,
         masking, specification, distance, PT7, PT7b, issue)
df_nl <- d_nl %>% 
  select(stance_nlp, stance_ss, interpretation_nlp, interpretation_ss,
         masking, specification, distance, PT7, issue)

df <- df_us %>% 
  mutate(b = specification,
         a = distance)
h2a <- lmer(interpretation_nlp ~ masking + b * a +
              PT7 + (1 | issue), data= df)
h2a_2 <- tidy(h2a) %>% 
  mutate(hyp = "H2a: Ideological Distance",
         type = "United States",
         y = "Overinterpreting Stance (Strict Interpretation)")
h2a <- tidy(margins::margins(h2a, variables = "b",
                             at = list("a" = 0:7))) %>% 
  mutate(hyp = "H2a: Ideological Distance",
         type = "United States",
         y = "Overinterpreting Stance (Strict Interpretation)")

tmp <- lmer(interpretation_ss ~ masking + b * a +
              PT7 + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "H2a: Ideological Distance",
         type = "United States",
         y = "Overinterpreting Stance (Lenient Interpretation)")
h2a_2 <- h2a_2 %>% 
  add_case(tmpp) 
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:7))) %>%
  mutate(hyp = "H2a: Ideological Distance",
         type = "United States",
         y = "Overinterpreting Stance (Lenient Interpretation)")

h2a <- h2a %>% 
  add_case(tmp)

df <- df_nl %>% 
  mutate(b = specification,
         a = distance)

tmp <- lmer(interpretation_nlp ~ masking + b * a +
              PT7 + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "H2a: Ideological Distance",
         type = "The Netherlands",
         y = "Overinterpreting Stance (Strict Interpretation)")
h2a_2 <- h2a_2 %>% 
  add_case(tmpp) 
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:8.5))) %>%
  mutate(hyp = "H2a: Ideological Distance",
         type = "The Netherlands",
         y = "Overinterpreting Stance (Strict Interpretation)")

h2a <- h2a %>% 
  add_case(tmp)

tmp <- lmer(interpretation_ss ~ masking + b * a +
              PT7 + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "H2a: Ideological Distance",
         type = "The Netherlands",
         y = "Overinterpreting Stance (Lenient Interpretation)")
h2a_2 <- h2a_2 %>% 
  add_case(tmpp) 
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:8.5))) %>%
  mutate(hyp = "H2a: Ideological Distance",
         type = "The Netherlands",
         y = "Overinterpreting Stance (Lenient Interpretation)")

h2a <- h2a %>% 
  add_case(tmp)

df <- df_us %>% 
  mutate(b = specification,
         a = PT7)
h2b <- lmer(interpretation_nlp ~ masking + b * a +
              distance + (1 | issue), data= df)
h2b_2 <- tidy(h2b) %>% 
  mutate(hyp = "H2b: Political Knowledge",
         type = "United States",
         y = "Overinterpreting Stance (Strict Interpretation)")
h2b <- tidy(margins::margins(h2b, variables = "b",
                             at = list("a" = 0:3))) %>% 
  mutate(hyp = "H2b: Political Knowledge",
         type = "United States",
         y = "Overinterpreting Stance (Strict Interpretation)")

tmp <- lmer(interpretation_ss ~ masking + b * a +
              distance + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "H2b: Political Knowledge",
         type = "United States",
         y = "Overinterpreting Stance (Lenient Interpretation)")
h2b_2 <- h2b_2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:3))) %>% 
  mutate(hyp = "H2b: Political Knowledge",
         type = "United States",
         y = "Overinterpreting Stance (Lenient Interpretation)")

h2b <- h2b %>% 
  add_case(tmp)

df <- df_nl %>% 
  mutate(b = specification,
         a = PT7)

tmp <- lmer(interpretation_nlp ~ masking + b * a +
              distance + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "H2b: Political Knowledge",
         type = "The Netherlands",
         y = "Overinterpreting Stance (Strict Interpretation)")
h2b_2 <- h2b_2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:4))) %>% 
  mutate(hyp = "H2b: Political Knowledge",
         type = "The Netherlands",
         y = "Overinterpreting Stance (Strict Interpretation)")

h2b <- h2b %>% 
  add_case(tmp)

tmp <- lmer(interpretation_ss ~ masking + b * a +
              distance + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "H2b: Political Knowledge",
         type = "The Netherlands",
         y = "Overinterpreting Stance (Lenient Interpretation)")
h2b_2 <- h2b_2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:4))) %>% 
  mutate(hyp = "H2b: Political Knowledge",
         type = "The Netherlands",
         y = "Overinterpreting Stance (Lenient Interpretation)")

h2b <- h2b %>% 
  add_case(tmp)


p2a <- h2a_2  %>% 
  add_case(h2b_2) %>% 
  filter(term %in% c("a","b", "bUnderspecified", "bUnderspecified:a")) %>% 
  mutate(term = ifelse(term == "a" & hyp == "H2a: Ideological Distance",
                         "Ideological Distance from Party", term),
         term = ifelse(term == "a" & hyp != "H2a: Ideological Distance",
                       "Poltical Knowledge", term),
         term = ifelse(term == "bUnderspecified", "Condition: Underspecified Sentence", term),
         term = ifelse(term == "bUnderspecified:a" & hyp == "H2a: Ideological Distance",
                       "Interaction: Underspecified * Distance", term),
         term = ifelse(term == "bUnderspecified:a" & hyp != "H2a: Ideological Distance",
                       "Interaction: Underspecified * Knowledge", term), 
         lower = estimate - 1.56 * std.error,
         upper = estimate + 1.56 * std.error) %>% 
  ggplot(aes(x = estimate, y = term,
             xmin = lower, xmax = upper,
             color = y)) +
  geom_point(position = position_dodge(0.5)) +
  geom_errorbar(width = 0,
                position = position_dodge(0.5)) +
  facet_grid(hyp~type, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray25") +
  labs(x = "Predicted Effect for Overinterpreting Stance", y = "") +
  theme_ipsum() +
  scale_colour_manual(values = fig_cols) +
  theme(legend.position = "bottom",
        legend.title = element_blank())
  
p2b <- h2a  %>% 
  add_case(h2b) %>%
  mutate(term = ifelse(term == "a" & hyp == "H2a: Ideological Distance \n(0=Low, 10 = High)",
                       "Ideological Distance from Party", term),
         term = ifelse(term == "a" & hyp != "H2a: Ideological Distance \n(0=Low, 10 = High)",
                       "Poltical Knowledge", term),
         term = ifelse(term == "bUnderspecified", "Condition: Underspecified Sentence", term),
         term = ifelse(term == "bUnderspecified:a" & hyp == "H2a: Ideological Distance \n(0=Low, 10 = High)",
                       "Interaction: Underspecified * Distance", term),
         term = ifelse(term == "bUnderspecified:a" & hyp != "H2a: Ideological Distance \n(0=Low, 10 = High)",
                       "Interaction: Underspecified * Knowledge", term), 
         lower = estimate - 1.56 * std.error,
         upper = estimate + 1.56 * std.error) %>% 
  ggplot(aes(x = `at.value`, y = estimate,
             ymin = (estimate - (1.65 * `std.error`)),
             ymax = (estimate + (1.65 * `std.error`)),
             color = y, fill = y)) +
  geom_ribbon(alpha = 0.2, position = position_dodge(.2)) +
  geom_line(position = position_dodge(.2)) +
  facet_grid(hyp~type, scales = "free") +
  labs(x = "",
       y = "Marginal Effect of Sentence Underspecification \n for Overinterpreting Stance") +
  theme_ipsum() +
  scale_colour_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  theme(legend.position = "bottom",
        legend.title = element_blank())

##
df <- d_us %>% 
  mutate(b = specification,
         a = PT7b)
tmp <- lmer(interpretation_nlp ~ masking + b * a +
              distance + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Exploration: Bullshit Receptivity \n (0 = Low, 5 = High)",
         type = "United States",
         y = "Overinterpreting Stance (Strict Interpretation)")

h2b_2 <- h2b_2 %>% 
  add_case(tmpp)

tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 1:5))) %>% 
  mutate(hyp = "Exploration: Bullshit Receptivity \n (0 = Low, 5 = High)",
         type = "United States",
         y = "Overinterpreting Stance (Strict Interpretation)")

h2b <- h2b  %>% 
  add_case(tmp)

tmp <- lmer(interpretation_ss ~ masking + b * a +
              distance + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Exploration: Bullshit Receptivity \n (0 = Low, 5 = High)",
         type = "United States",
         y = "Overinterpreting Stance (Lenient Interpretation)")
h2b_2 <- h2b_2 %>% 
  add_case(tmpp)

tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 1:5))) %>% 
  mutate(hyp = "Exploration: Bullshit Receptivity \n (0 = Low, 5 = High)",
         type = "United States",
         y = "Overinterpreting Stance (Lenient Interpretation)")

h2b <- h2b  %>% 
  add_case(tmp)

p2c1 <- h2b_2 %>% 
  add_case(tmpp) %>% 
  filter(term %in% c("a","b", "bUnderspecified", "bUnderspecified:a")) %>% 
  filter(type == "United States") %>% 
  mutate(term = ifelse(term == "a" & hyp == "H2a: Ideological Distance",
                       "Ideological Distance from Party", term),
         term = ifelse(term == "a" & hyp != "H2a: Ideological Distance",
                       "Poltical Knowledge", term),
         term = ifelse(term == "bUnderspecified", "Condition: Underspecified Sentence", term),
         term = ifelse(term == "bUnderspecified:a" & hyp == "H2a: Ideological Distance",
                       "Interaction: Underspecified * Distance", term),
         term = ifelse(term == "bUnderspecified:a" & hyp != "H2a: Ideological Distance",
                       "Interaction: Underspecified * Knowledge", term), 
         lower = estimate - 1.56 * std.error,
         upper = estimate + 1.56 * std.error) %>% 
  ggplot(aes(x = estimate, y = term,
             xmin = lower, xmax = upper,
             color = y)) +
  geom_point(position = position_dodge(0.5)) +
  geom_errorbar(width = 0,
                position = position_dodge(0.5)) +
  facet_grid(hyp~type, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray25") +
  labs(x = "Predicted Effect for Overinterpreting Stance", y = "") +
  theme_ipsum() +
  scale_colour_manual(values = fig_cols) +
  theme(legend.position = "bottom",
        legend.title = element_blank())

p2c2 <- h2b  %>% 
  filter(type== "United States") %>%
  mutate(hyp = factor(hyp,
                      levels = c("H2b: Political Knowledge",
                                 "Exploration: Bullshit Receptivity \n (0 = Low, 5 = High)"))) %>% 
  ggplot(aes(x = `at.value`, y = estimate,
             ymin = (estimate - (1.65 * `std.error`)),
             ymax = (estimate + (1.65 * `std.error`)),
             color = y, fill = y)) +
  geom_ribbon(alpha = 0.2, position = position_dodge(.2)) +
  geom_line(position = position_dodge(.2)) +
  facet_grid(.~hyp, scales = "free") +
  labs(x = "",
       y = "Marginal Effect of Sentence Underspecification \n for Overinterpreting Stance") +
  theme_ipsum() +
  scale_colour_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  theme(legend.position = "none",
        legend.title = element_blank())
