df_us <- d_us %>% 
  select(stance, interpretation, masking, specification, distance, PT7, country, issue)
df_nl <- d_nl %>% 
  select(stance, interpretation, masking, specification, distance, PT7, country, issue)

d <- df_nl %>% 
  add_case(df_us)

df <- d %>% 
  mutate(b = specification,
         a = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
b3_a <- lmer(stance ~ b * a +
                 (country | issue), data= df)
b3_a <- tidy(margins::margins(b3_a, variables = "b",
                               at = list("a" = 0:1))) %>% 
  mutate(condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance",
         type = "Pooled Analysis")

df <- df_us %>% 
  mutate(b = specification,
         a = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
tmp <- lmer(stance ~ b * a + (1 | issue), data= df)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:1))) %>% 
  mutate(condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance",
         type = "United States")

b3_a <- b3_a %>% 
  add_case(tmp)

df <- df_nl %>% 
  mutate(b = specification,
         a = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
tmp <- lmer(stance ~ b * a + (1 | issue), data= df)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:1))) %>% 
  mutate(condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance",
         type = "The Netherlands")

b3_a <- b3_a %>% 
  add_case(tmp)

df <- d %>% 
  mutate(b = specification,
         a = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
b3_b <- lmer(interpretation ~ b * a +
               (country | issue), data= df)
b3_b <- tidy(margins::margins(b3_b, variables = "b",
                              at = list("a" = 0:1))) %>% 
  mutate(condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance",
         type = "Pooled Analysis")

df <- df_us %>% 
  mutate(b = specification,
         a = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
tmp <- lmer(interpretation ~ b * a + (1 | issue), data= df)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:1))) %>% 
  mutate(condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance",
         type = "United States")

b3_b <- b3_b %>% 
  add_case(tmp)

df <- df_nl %>% 
  mutate(b = specification,
         a = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
tmp <- lmer(interpretation ~ b * a + (1 | issue), data= df)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:1))) %>% 
  mutate(condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance",
         type = "The Netherlands")

b3_b <- b3_b %>% 
  add_case(tmp)

p_b <- b3_a %>% 
  add_case(b3_b) %>%
  filter(type != "Pooled Analysis") %>% 
  mutate(`at.value` = recode(`at.value`,
                             `0` = "Political Actor Revealed",
                             `1` = "Political Actor Masked")) %>% 
  ggplot(aes(y = `at.value`, x = estimate,
             xmin = estimate -1.56*std.error,
             xmax = estimate +1.56*std.error,
             color = type)) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(position = position_dodge(.5), width = 0) +
  #geom_vline(xintercept = 0, linetype = "dashed", color = "gray25") +
  facet_grid(.~y, scales = "free") +
  labs(y = "", x = "Average Marginal Effect of Underspecified Sentence for Y") +
  theme_ipsum() +
  scale_colour_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  theme(legend.position = "bottom",
        legend.title = element_blank())

p_b2 <- b3_a %>% 
  add_case(b3_b) %>%
  mutate(`at.value` = recode(`at.value`,
                             `0` = "Political Actor Revealed",
                             `1` = "Political Actor Masked")) %>% 
  ggplot(aes(y = `at.value`, x = estimate,
             xmin = estimate -1.56*std.error,
             xmax = estimate +1.56*std.error,
             color = type)) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(position = position_dodge(.5), width = 0) +
  #geom_vline(xintercept = 0, linetype = "dashed", color = "gray25") +
  facet_grid(.~y, scales = "free") +
  labs(y = "", x = "Average Marginal Effect of Underspecified Sentence for Y") +
  theme_ipsum() +
  scale_colour_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  theme(legend.position = "bottom",
        legend.title = element_blank())

d <- d_nl %>% 
  mutate(congruence = 0,
         congruence = ifelse(PT2<3 & issue=="Immigration", 1,
                             congruence),
         congruence = ifelse(PT3<3 & issue=="Environment", 1,
                             congruence),
         congruence = ifelse(PT4>3 & issue=="Tax", 1,
                             congruence),
         congruence = ifelse(PT5>3 & issue=="EU", 1,
                             congruence),
         pid = "Other party",
         pid = ifelse(PT1=="Forum voor Democratie", 
                      "Forum voor Democratie", pid),
         pid = ifelse(PT1=="PVV", 
                      "PVV", pid),
         pid = ifelse(PT1=="GroenLinks", 
                      "GroenLinks", pid),
         pid = ifelse(PT1=="PvdA", 
                      "PvdA", pid),
         pid = factor(pid))

d <- within(d, pid <- relevel(pid, ref = "Other party"))

df <- d %>% 
  mutate(b = specification,
         a = congruence)
h2a_e <- lmer(stance ~ masking + b * a +
              PT7 + (1 | issue), data= df)
h2a_e <- tidy(margins::margins(h2a_e, variables = "b",
                             at = list("a" = 0:1))) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance",
         type = "The Netherlands")

df <- d %>% 
  mutate(b = specification,
         a = pid)
h2b_e <- lmer(stance ~ masking + b * a +
                PT7 + (1 | issue), data= df)
h2b_e <- tidy(margins::margins(h2b_e, variables = "b",
                               at = list("a" = c("Forum voor Democratie","GroenLinks", "PvdA","PVV")))) %>% 
  mutate(hyp = "Party Voted For/ Party ID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance",
         type = "The Netherlands")

df <- d %>% 
  mutate(b = specification,
         a = PT6)
h2c_e <- lmer(stance ~ masking + b * a +
                PT7 + (1 | issue), data= df)
h2c_e <- tidy(margins::margins(h2c_e, variables = "b",
                               at = list("a" = 0:10))) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance",
         type = "The Netherlands")

df <- d %>% 
  mutate(b = masking,
         a = congruence,
         b = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
h3a_e <- lmer(stance ~ specification + b * a +
                PT7 + (1 | issue), data= df)
h3a_e <- tidy(margins::margins(h3a_e, variables = "b",
                               at = list("a" = 0:1))) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance",
         type = "The Netherlands")

df <- d %>% 
  mutate(b = masking,
         a = pid,
         b = recode(masking,
                            `Masked` = 1,
                            `Party` = 0))
h3b_e <- lmer(stance ~ specification + b * a +
                PT7 + (1 | issue), data= df)
h3b_e <- tidy(margins::margins(h3b_e, variables = "b",
                               at = list("a" = c("Forum voor Democratie","GroenLinks", "PvdA","PVV")))) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance",
         type = "The Netherlands")

df <- d %>% 
  mutate(b = masking,
         a = PT6,
         b = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
h3c_e <- lmer(stance ~ specification + b * a +
                PT7 + (1 | issue), data= df)
h3c_e <- tidy(margins::margins(h3c_e, variables = "b",
                               at = list("a" = 0:10))) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance",
         type = "The Netherlands")

df <- d %>% 
  mutate(b = specification,
         a = congruence)
h2a_e2 <- lmer(interpretation ~ masking + b * a +
                PT7 + (1 | issue), data= df)
h2a_e2 <- tidy(margins::margins(h2a_e2, variables = "b",
                               at = list("a" = 0:1))) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance",
         type = "The Netherlands")

df <- d %>% 
  mutate(b = specification,
         a = pid)
h2b_e2 <- lmer(interpretation ~ masking + b * a +
                PT7 + (1 | issue), data= df)
h2b_e2 <- tidy(margins::margins(h2b_e2, variables = "b",
                               at = list("a" = c("Forum voor Democratie","GroenLinks", "PvdA","PVV")))) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance",
         type = "The Netherlands")

df <- d %>% 
  mutate(b = specification,
         a = PT6)
h2c_e2 <- lmer(interpretation ~ masking + b * a +
                PT7 + (1 | issue), data= df)
h2c_e2 <- tidy(margins::margins(h2c_e2, variables = "b",
                               at = list("a" = 0:10))) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance",
         type = "The Netherlands")

df <- d %>% 
  mutate(b = masking,
         a = congruence,
         b = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
h3a_e2 <- lmer(interpretation ~ specification + b * a +
                PT7 + (1 | issue), data= df)
h3a_e2 <- tidy(margins::margins(h3a_e2, variables = "b",
                               at = list("a" = 0:1))) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance",
         type = "The Netherlands")

df <- d %>% 
  mutate(b = masking,
         a = pid,
         b = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
h3b_e2 <- lmer(interpretation ~ specification + b * a +
                PT7 + (1 | issue), data= df)
h3b_e2 <- tidy(margins::margins(h3b_e2, variables = "b",
                               at = list("a" = c("Forum voor Democratie","GroenLinks", "PvdA","PVV")))) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance",
         type = "The Netherlands")

df <- d %>% 
  mutate(b = masking,
         a = PT6,
         b = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
h3c_e2 <- lmer(interpretation ~ specification + b * a +
                PT7 + (1 | issue), data= df)
h3c_e2 <- tidy(margins::margins(h3c_e2, variables = "b",
                               at = list("a" = 0:10))) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance",
         type = "The Netherlands")

d <- d_us %>% 
  mutate(congruence = 0,
         congruence = ifelse(PT2>3 & issue=="Immigration", 1,
                             congruence),
         congruence = ifelse(PT3<3 & issue=="Environment", 1,
                             congruence),
         congruence = ifelse(PT4>3 & issue=="Tax", 1,
                             congruence),
         congruence = ifelse(PT5>3 & issue=="Foreign Policy", 1,
                             congruence),
         pid = "Other",
         pid = ifelse(PT1=="Republican" & PT1b=="Strong", 
                      "Strong Republican", pid),
         pid = ifelse(PT1=="Republican" & PT1b!="Strong", 
                      "Not very strong Republican", pid),
         pid = ifelse(PT1=="Democrat" & PT1b=="Strong", 
                      "Strong Democrat", pid),
         pid = ifelse(PT1=="Democrat" & PT1b!="Strong", 
                      "Not very strong Democrat", pid),
         pid = factor(pid,
                      levels = c("Other","Strong Democrat",
                                 "Not very strong Democrat",
                                 "Strong Republican",
                                 "Not very strong Republican")))

d <- within(d, pid <- relevel(pid, ref = "Other"))

df <- d %>% 
  mutate(b = specification,
         a = congruence)
h2a_e_us <- lmer(stance ~ masking + b * a +
                PT7 + (1 | issue), data= df)
h2a_e_us <- tidy(margins::margins(h2a_e_us, variables = "b",
                               at = list("a" = 0:1))) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance",
         type = "United States")

df <- d %>% 
  mutate(b = specification,
         a = pid)
h2b_e_us <- lmer(stance ~ masking + b * a +
                PT7 + (1 | issue), data= df)
h2b_e_us <- tidy(margins::margins(h2b_e_us, variables = "b",
                               at = list("a" = c("Strong Democrat","Not very strong Democrat", "Strong Republican","Not very strong Republican")))) %>% 
  mutate(hyp = "Party Voted For/ Party ID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance",
         type = "United States")

df <- d %>% 
  mutate(b = specification,
         a = PT6)
h2c_e_us <- lmer(stance ~ masking + b * a +
                PT7 + (1 | issue), data= df)
h2c_e_us <- tidy(margins::margins(h2c_e_us, variables = "b",
                               at = list("a" = 0:6))) %>% 
  mutate(hyp = "Ideological Position (0 = Extremely Liberal, 6 = Extremely Conservative)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance",
         type = "United States")

df <- d %>% 
  mutate(b = masking,
         a = congruence,
         b = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
h3a_e_us <- lmer(stance ~ specification + b * a +
                PT7 + (1 | issue), data= df)
h3a_e_us <- tidy(margins::margins(h3a_e_us, variables = "b",
                               at = list("a" = 0:1))) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance",
         type = "United States")

df <- d %>% 
  mutate(b = masking,
         a = pid,
         b = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
h3b_e_us <- lmer(stance ~ specification + b * a +
                PT7 + (1 | issue), data= df)
h3b_e_us <- tidy(margins::margins(h3b_e_us, variables = "b",
                                  at = list("a" = c("Strong Democrat","Not very strong Democrat", "Strong Republican","Not very strong Republican")))) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance",
         type = "United States")

df <- d %>% 
  mutate(b = masking,
         a = PT6,
         b = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
h3c_e_us <- lmer(stance ~ specification + b * a +
                PT7 + (1 | issue), data= df)
h3c_e_us <- tidy(margins::margins(h3c_e_us, variables = "b",
                               at = list("a" = 0:6))) %>% 
  mutate(hyp = "Ideological Position (0 = Extremely Liberal, 6 = Extremely Conservative)",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance",
         type = "United States")

df <- d %>% 
  mutate(b = specification,
         a = congruence)
h2a_e2_us <- lmer(interpretation ~ masking + b * a +
                 PT7 + (1 | issue), data= df)
h2a_e2_us <- tidy(margins::margins(h2a_e2_us, variables = "b",
                                at = list("a" = 0:1))) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance",
         type = "United States")

df <- d %>% 
  mutate(b = specification,
         a = pid)
h2b_e2_us <- lmer(interpretation ~ masking + b * a +
                 PT7 + (1 | issue), data= df)
h2b_e2_us <- tidy(margins::margins(h2b_e2_us, variables = "b",
                                at = list("a" = c("Strong Democrat","Not very strong Democrat", "Strong Republican","Not very strong Republican")))) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance",
         type = "United States")

df <- d %>% 
  mutate(b = specification,
         a = PT6)
h2c_e2_us <- lmer(interpretation ~ masking + b * a +
                 PT7 + (1 | issue), data= df)
h2c_e2_us <- tidy(margins::margins(h2c_e2_us, variables = "b",
                                at = list("a" = 0:6))) %>% 
  mutate(hyp = "Ideological Position (0 = Extremely Liberal, 6 = Extremely Conservative)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance",
         type = "United States")

df <- d %>% 
  mutate(b = masking,
         a = congruence,
         b = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
h3a_e2_us <- lmer(interpretation ~ specification + b * a +
                 PT7 + (1 | issue), data= df)
h3a_e2_us <- tidy(margins::margins(h3a_e2_us, variables = "b",
                                at = list("a" = 0:1))) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance",
         type = "United States")

df <- d %>% 
  mutate(b = masking,
         a = pid,
         b = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
h3b_e2_us <- lmer(interpretation ~ specification + b * a +
                 PT7 + (1 | issue), data= df)
h3b_e2_us <- tidy(margins::margins(h3b_e2_us, variables = "b",
                                at = list("a" = c("Strong Democrat","Not very strong Democrat", "Strong Republican","Not very strong Republican")))) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance",
         type = "United States")

df <- d %>% 
  mutate(b = masking,
         a = PT6,
         b = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
h3c_e2_us <- lmer(interpretation ~ specification + b * a +
                 PT7 + (1 | issue), data= df)
h3c_e2_us <- tidy(margins::margins(h3c_e2_us, variables = "b",
                                at = list("a" = 0:6))) %>% 
  mutate(hyp = "Ideological Position (0 = Extremely Liberal, 6 = Extremely Conservative)",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance",
         type = "United States")

p_e1 <- h2a_e %>% 
  add_case(h2a_e_us) %>% 
  add_case(h3a_e) %>% 
  add_case(h3a_e_us) %>% 
  add_case(h2a_e2) %>% 
  add_case(h2a_e2_us) %>% 
  add_case(h3a_e2) %>% 
  add_case(h3a_e2_us) %>% 
  mutate(`at.value` = ifelse(`at.value` == 0, "Incongruent", "Congruent")) %>% 
  ggplot(aes(y = `at.value`, x = estimate,
             xmin = (estimate - (1.65 * `std.error`)),
             xmax = (estimate + (1.65 * `std.error`)),
             color = type)) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(width = 0,
                position = position_dodge(.5)) +
  facet_grid(y ~ condition, scales = "free") +
  theme_ipsum() +
  labs(x = "Marginal Effect of Condition for Y", y = "") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray75")+
  scale_colour_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  theme(legend.position = "bottom",
        legend.title = element_blank())

p_e2 <- h2b_e %>% 
  add_case(h2b_e_us) %>% 
  add_case(h3b_e) %>% 
  add_case(h3b_e_us) %>% 
  add_case(h2b_e2) %>% 
  add_case(h2b_e2_us) %>% 
  add_case(h3b_e2) %>%
  add_case(h3b_e2_us) %>%
  ggplot(aes(y = `at.value`, x = estimate,
             xmin = (estimate - (1.65 * `std.error`)),
             xmax = (estimate + (1.65 * `std.error`)),
             color = type)) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(width = 0,
                position = position_dodge(.5)) +
  facet_grid(y ~ condition, scales = "free") +
  theme_ipsum() +
  labs(x = "Marginal Effect of Condition for Y", y = "") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray75") +
  scale_colour_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  theme(legend.position = "bottom",
        legend.title = element_blank())

p_e3_1 <- h2c_e %>% 
  add_case(h2c_e_us) %>% 
  add_case(h3c_e) %>% 
  add_case(h3c_e_us) %>% 
  add_case(h2c_e2) %>% 
  add_case(h2c_e2_us) %>% 
  add_case(h3c_e2) %>% 
  add_case(h3c_e2_us) %>% 
  filter(type == "The Netherlands") %>% 
  ggplot(aes(x = `at.value`, y = estimate,
             ymin = (estimate - (1.65 * `std.error`)),
             ymax = (estimate + (1.65 * `std.error`)),
             color = type, fill = type)) +
  facet_grid(y ~ condition, scales = "free") +
  geom_ribbon(alpha = 0.2) +
  geom_line() +
  theme_ipsum() +
  labs(y = "Marginal Effect of Condition for Y", 
       x = "Ideological Position") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray75")  +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  scale_colour_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols)

p_e3_2 <- h2c_e %>% 
  add_case(h2c_e_us) %>% 
  add_case(h3c_e) %>% 
  add_case(h3c_e_us) %>% 
  add_case(h2c_e2) %>% 
  add_case(h2c_e2_us) %>% 
  add_case(h3c_e2) %>% 
  add_case(h3c_e2_us) %>% 
  filter(type != "The Netherlands") %>% 
  ggplot(aes(x = `at.value`, y = estimate,
             ymin = (estimate - (1.65 * `std.error`)),
             ymax = (estimate + (1.65 * `std.error`)),
             color = type, fill = type)) +
  facet_grid(y ~ condition, scales = "free") +
  geom_ribbon(alpha = 0.2) +
  geom_line() +
  theme_ipsum() +
  labs(y = "Marginal Effect of Condition for Y", 
       x = "Ideological Position") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray75")  +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  scale_colour_manual(values = fig_cols[2]) +
  scale_fill_manual(values = fig_cols[2])
