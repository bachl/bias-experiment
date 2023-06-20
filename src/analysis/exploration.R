df_us <- d_us %>% 
  select(stance_nlp, stance_ss, interpretation_nlp, interpretation_ss, masking, specification, distance, PT7, issue)
df_nl <- d_nl %>% 
  select(stance_nlp, stance_ss, interpretation_nlp, interpretation_ss, masking, specification, distance, PT7, issue)

df <- d_us %>% 
  mutate(b = specification,
         a = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
b3_a <- lmer(stance_nlp ~ b * a +
                 (1 | issue), data= df)
b3_a2 <- tidy(b3_a) %>% 
  mutate(condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "United States")
b3_a <- tidy(margins::margins(b3_a, variables = "b",
                               at = list("a" = 0:1))) %>% 
  mutate(condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "United States")

tmp <- lmer(stance_ss ~ b * a + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "United States")
b3_a2 <- b3_a2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:1))) %>% 
  mutate(condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "United States")

b3_a <- b3_a %>% 
  add_case(tmp)

df <- df_nl %>% 
  mutate(b = specification,
         a = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
tmp <- lmer(stance_nlp ~ b * a + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "The Netherlands")
b3_a2 <- b3_a2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:1))) %>% 
  mutate(condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "The Netherlands")

b3_a <- b3_a %>% 
  add_case(tmp)

tmp <- lmer(stance_ss ~ b * a + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "The Netherlands")
b3_a2 <- b3_a2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:1))) %>% 
  mutate(condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "The Netherlands")

b3_a <- b3_a %>% 
  add_case(tmp)

####
df <- df_us %>% 
  mutate(b = specification,
         a = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
b3_b <- lmer(interpretation_nlp ~ b * a +
               (1 | issue), data= df)
b3_b2 <- tidy(b3_b) %>% 
  mutate(condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "United States")
b3_b <- tidy(margins::margins(b3_b, variables = "b",
                              at = list("a" = 0:1))) %>% 
  mutate(condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "United States")

tmp <- lmer(interpretation_ss ~ b * a + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "United States")
b3_b2 <- b3_b2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:1))) %>% 
  mutate(condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "United States")

b3_b <- b3_b %>% 
  add_case(tmp)

df <- df_nl %>% 
  mutate(b = specification,
         a = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
tmp <- lmer(interpretation_nlp ~ b * a +
               (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "The Netherlands")
b3_b2 <- b3_b2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                              at = list("a" = 0:1))) %>% 
  mutate(condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "The Netherlands")
b3_b <- b3_b %>% 
  add_case(tmp)

tmp <- lmer(interpretation_ss ~ b * a + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "The Netherlands")
b3_b2 <- b3_b2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:1))) %>% 
  mutate(condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "The Netherlands")

b3_b <- b3_b %>% 
  add_case(tmp)

p_b1 <- b3_a2 %>% 
  add_case(b3_b2) %>%
  filter(term %in% c("a", "bUnderspecified", "bUnderspecified:a")) %>% 
  mutate(`term` = recode(`term`,
                             `a` = "Condition: Actor Revealed",
                             `bUnderspecified` = "Condition: Underspecified Sentence",
                         `bUnderspecified:a` = "Interaction: Masking * Specification")) %>% 
  ggplot(aes(y = `term`, x = estimate,
             xmin = estimate -1.56*std.error,
             xmax = estimate +1.56*std.error,
             color = y)) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(position = position_dodge(.5), width = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray25") +
  facet_grid(.~type, scales = "free") +
  labs(y = "", x = "Predicted Effect for Y") +
  theme_ipsum() +
  scale_colour_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))

p_b2 <- b3_a %>% 
  add_case(b3_b) %>%
  mutate(`at.value` = recode(`at.value`,
                             `0` = "Political Actor Revealed",
                             `1` = "Political Actor Masked")) %>% 
  ggplot(aes(y = `at.value`, x = estimate,
             xmin = estimate -1.56*std.error,
             xmax = estimate +1.56*std.error,
             color = y)) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(position = position_dodge(.5), width = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray25") +
  facet_grid(.~type, scales = "free") +
  labs(y = "", x = "Average Marginal Effect of Underspecified Sentence for Y") +
  theme_ipsum() +
  scale_colour_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))

rm(b3_a, b3_a2, b3_b, b3_b2, df, df_nl, df_us, tmp, tmpp)

##
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
h2a_e <- lmer(stance_nlp ~ masking + b * a +
              PT7 + (1 | issue), data= df)
h2a_e2 <- tidy(h2a_e) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "The Netherlands")
h2a_e <- tidy(margins::margins(h2a_e, variables = "b",
                             at = list("a" = 0:1))) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "The Netherlands")

tmp <- lmer(stance_ss ~ masking + b * a +
                PT7 + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "The Netherlands")
h2a_e2 <- h2a_e2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                               at = list("a" = 0:1))) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "The Netherlands")
h2a_e <- h2a_e %>% 
  add_case(tmp)

df <- d %>% 
  mutate(b = specification,
         a = pid)
h2b_e <- lmer(stance_nlp ~ masking + b * a +
                PT7 + (1 | issue), data= df)
h2b_e2 <- tidy(h2b_e) %>% 
  mutate(hyp = "Party Voted For/ Party ID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "The Netherlands")
h2b_e <- tidy(margins::margins(h2b_e, variables = "b",
                               at = list("a" = c("Forum voor Democratie","GroenLinks", "PvdA","PVV")))) %>% 
  mutate(hyp = "Party Voted For/ Party ID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "The Netherlands")

tmp <- lmer(stance_ss ~ masking + b * a +
                PT7 + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Party Voted For/ Party ID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "The Netherlands")
h2b_e2 <- h2b_e2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                               at = list("a" = c("Forum voor Democratie","GroenLinks", "PvdA","PVV")))) %>% 
  mutate(hyp = "Party Voted For/ Party ID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "The Netherlands")

df <- d %>% 
  mutate(b = specification,
         a = PT6)
h2c_e <- lmer(stance_nlp ~ masking + b * a +
                PT7 + (1 | issue), data= df)
h2c_e2 <- tidy(h2c_e) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "The Netherlands")
h2c_e <- tidy(margins::margins(h2c_e, variables = "b",
                               at = list("a" = 0:10))) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "The Netherlands")

tmp <- lmer(stance_ss ~ masking + b * a +
                PT7 + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "The Netherlands")
h2c_e2 <- h2c_e2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                               at = list("a" = 0:10))) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "The Netherlands")
h2c_e <- h2c_e %>% 
  add_case(tmp)

df <- d %>% 
  mutate(b = masking,
         a = congruence,
         b = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
h3a_e <- lmer(stance_nlp ~ specification + b * a +
                PT7 + (1 | issue), data= df)
h3a_e2 <- tidy(h3a_e) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "The Netherlands")
h3a_e <- tidy(margins::margins(h3a_e, variables = "b",
                               at = list("a" = 0:1))) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "The Netherlands")

tmp <- lmer(stance_ss ~ specification + b * a +
                PT7 + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "The Netherlands")
h3a_e2 <- h3a_e2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                               at = list("a" = 0:1))) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "The Netherlands")
h3a_e <- h3a_e %>% 
  add_case(tmp)

df <- d %>% 
  mutate(b = masking,
         a = pid,
         b = recode(masking,
                            `Masked` = 1,
                            `Party` = 0))
h3b_e <- lmer(stance_nlp ~ specification + b * a +
                PT7 + (1 | issue), data= df)
h3b_e2 <- tidy(h3b_e) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "The Netherlands")
h3b_e <- tidy(margins::margins(h3b_e, variables = "b",
                               at = list("a" = c("Forum voor Democratie","GroenLinks", "PvdA","PVV")))) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "The Netherlands")

tmp <- lmer(stance_ss ~ specification + b * a +
                PT7 + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "The Netherlands")
h3b_e2 <- h3b_e2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                               at = list("a" = c("Forum voor Democratie","GroenLinks", "PvdA","PVV")))) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "The Netherlands")

h3b_e <- h3b_e %>% 
  add_case(tmp)

df <- d %>% 
  mutate(b = masking,
         a = PT6,
         b = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
h3c_e <- lmer(stance_nlp ~ specification + b * a +
                PT7 + (1 | issue), data= df)
h3c_e2 <- tidy(h3c_e) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "The Netherlands")
h3c_e <- tidy(margins::margins(h3c_e, variables = "b",
                               at = list("a" = 0:10))) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "The Netherlands")

tmp <- lmer(stance_ss ~ specification + b * a +
                PT7 + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "The Netherlands")
h3c_e2 <- h3c_e2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                               at = list("a" = 0:10))) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "The Netherlands")
h3c_e <- h3c_e %>% 
  add_case(tmp)

df <- d %>% 
  mutate(b = specification,
         a = congruence)
h2a_e_ic <- lmer(interpretation_nlp ~ masking + b * a +
                PT7 + (1 | issue), data= df)
h2a_e_ic2 <- tidy(h2a_e_ic) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "The Netherlands")
h2a_e_ic <- tidy(margins::margins(h2a_e_ic, variables = "b",
                               at = list("a" = 0:1))) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "The Netherlands")

tmp <- lmer(interpretation_ss ~ masking + b * a +
                   PT7 + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "The Netherlands")
h2a_e_ic2 <- h2a_e_ic2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                                  at = list("a" = 0:1))) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "The Netherlands")
h2a_e_ic <- h2a_e_ic %>% 
  add_case(tmp)

df <- d %>% 
  mutate(b = specification,
         a = pid)
h2b_e_pid <- lmer(interpretation_nlp ~ masking + b * a +
                PT7 + (1 | issue), data= df)
h2b_e_pid2 <- tidy(h2b_e_pid) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "The Netherlands")
h2b_e_pid <- tidy(margins::margins(h2b_e_pid, variables = "b",
                               at = list("a" = c("Forum voor Democratie","GroenLinks", "PvdA","PVV")))) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Strict Interpretation",
         type = "The Netherlands")

tmp <- lmer(interpretation_ss ~ masking + b * a +
                    PT7 + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "The Netherlands")
h2b_e_pid2 <- h2b_e_pid2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                                at = list("a" = c("Forum voor Democratie","GroenLinks", "PvdA","PVV")))) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Strict Interpretation",
         type = "The Netherlands")
h2b_e_pid <- h2b_e_pid %>% 
  add_case(tmp)

df <- d %>% 
  mutate(b = specification,
         a = PT6)
h2c_e_ip <- lmer(interpretation_nlp ~ masking + b * a +
                PT7 + (1 | issue), data= df)
h2c_e_ip2 <- tidy(h2c_e_ip) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "The Netherlands")
h2c_e_ip <- tidy(margins::margins(h2c_e_ip, variables = "b",
                               at = list("a" = 0:10))) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "The Netherlands")

tmp <- lmer(interpretation_ss ~ masking + b * a +
                   PT7 + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "The Netherlands")
h2c_e_ip2 <- h2c_e_ip2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                                  at = list("a" = 0:10))) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "The Netherlands")
h2c_e_ip <- h2c_e_ip %>% 
  add_case(tmp)

df <- d %>% 
  mutate(b = masking,
         a = congruence,
         b = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
h3a_e_ic <- lmer(interpretation_nlp ~ specification + b * a +
                PT7 + (1 | issue), data= df)
h3a_e_ic2 <- tidy(h3a_e_ic)  %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "The Netherlands")
h3a_e_ic <- tidy(margins::margins(h3a_e_ic, variables = "b",
                               at = list("a" = 0:1))) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "The Netherlands")

tmp <- lmer(interpretation_ss ~ specification + b * a +
                   PT7 + (1 | issue), data= df)
tmpp <- tidy(tmp)  %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "The Netherlands")
h3a_e_ic2 <- h3a_e_ic2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                                  at = list("a" = 0:1))) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "The Netherlands")
h3a_e_ic <- h3a_e_ic %>% 
  add_case(tmp)

df <- d %>% 
  mutate(b = masking,
         a = pid,
         b = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
h3b_e_pid <- lmer(interpretation_nlp ~ specification + b * a +
                PT7 + (1 | issue), data= df)
h3b_e_pid2 <- tidy(h3b_e_pid) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "The Netherlands")
h3b_e_pid <- tidy(margins::margins(h3b_e_pid, variables = "b",
                               at = list("a" = c("Forum voor Democratie","GroenLinks", "PvdA","PVV")))) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "The Netherlands")

tmp <- lmer(interpretation_ss ~ specification + b * a +
                    PT7 + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "The Netherlands")
h3b_e_pid2 <- h3b_e_pid2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                                   at = list("a" = c("Forum voor Democratie","GroenLinks", "PvdA","PVV")))) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "The Netherlands")
h3b_e_pid <- h3b_e_pid %>% 
  add_case(tmp)

df <- d %>% 
  mutate(b = masking,
         a = PT6,
         b = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
h3c_e_ip <- lmer(interpretation_nlp ~ specification + b * a +
                PT7 + (1 | issue), data= df)
h3c_e_ip2 <- tidy(h3c_e_ip) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "The Netherlands")
h3c_e_ip <- tidy(margins::margins(h3c_e_ip, variables = "b",
                               at = list("a" = 0:10))) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "The Netherlands")

tmp <- lmer(interpretation_ss ~ specification + b * a +
                   PT7 + (1 | issue), data= df)
h3c_e_ip2 <- tidy(tmp) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "The Netherlands")
tmp <- tidy(margins::margins(tmp, variables = "b",
                                  at = list("a" = 0:10))) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "The Netherlands")
h3c_e_ip <- h3c_e_ip %>% 
  add_case(tmp)

exp_nl_int <- h2a_e %>% 
  add_case(h2a_e_ic) %>% 
  add_case(h2c_e) %>% 
  add_case(h2c_e_ip) %>% 
  add_case(h3a_e) %>% 
  add_case(h3a_e_ic) %>% 
  add_case(h3c_e) %>% 
  add_case(h3c_e_ip) 

exp_nl_char <- h2b_e %>% 
  add_case(h2b_e_pid) %>% 
  add_case(h3b_e) %>% 
  add_case(h3b_e_pid)
  
exp_nl_int2 <- h2a_e2 %>% 
  add_case(h2a_e_ic2) %>% 
  add_case(h2c_e2) %>% 
  add_case(h2c_e_ip2) %>% 
  add_case(h3a_e2) %>% 
  add_case(h3a_e_ic2) %>% 
  add_case(h3c_e2) %>% 
  add_case(h3c_e_ip2) 

exp_nl_char2 <- h2b_e2 %>% 
  add_case(h2b_e_pid2) %>% 
  add_case(h3b_e2) %>% 
  add_case(h3b_e_pid2)

rm(h2a_e,h2a_e_ic, h2b_e, h2b_e_pid, 
   h2c_e, h2c_e_ip, h3a_e, h3a_e_ic,
   h3b_e, h3b_e_pid, h3c_e, h3c_e_pid,
   h2a_e2,h2a_e_ic2, h2b_e2, h2b_e_pid2, 
   h2c_e2, h2c_e_ip2, h3a_e2, h3a_e_ic2,
   h3c_e_ip, h3c_e_ip2,
   h3b_e2, h3b_e_pid2, h3c_e2, h3c_e_pid2,
   tmp, tmpp)

##
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

h2a_e <- lmer(stance_nlp ~ masking + b * a +
                PT7 + (1 | issue), data= df)
h2a_e2 <- tidy(h2a_e) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "United States")
h2a_e <- tidy(margins::margins(h2a_e, variables = "b",
                               at = list("a" = 0:1))) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "United States")

tmp <- lmer(stance_ss ~ masking + b * a +
              PT7 + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "United States")
h2a_e2 <- h2a_e2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:1))) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "United States")
h2a_e <- h2a_e %>% 
  add_case(tmp)

df <- d %>% 
  mutate(b = specification,
         a = pid)
h2b_e <- lmer(stance_nlp ~ masking + b * a +
                PT7 + (1 | issue), data= df)
h2b_e2 <- tidy(h2b_e) %>% 
  mutate(hyp = "Party Voted For/ Party ID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "United States")
h2b_e <- tidy(margins::margins(h2b_e, variables = "b",
                               at = list("a" = c("Strong Democrat","Not very strong Democrat", "Strong Republican","Not very strong Republican")))) %>% 
  mutate(hyp = "Party Voted For/ Party ID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "United States")

tmp <- lmer(stance_ss ~ masking + b * a +
              PT7 + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Party Voted For/ Party ID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "United States")
h2b_e2 <- h2b_e2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = c("Strong Democrat","Not very strong Democrat", "Strong Republican","Not very strong Republican")))) %>% 
  mutate(hyp = "Party Voted For/ Party ID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "United States")

df <- d %>% 
  mutate(b = specification,
         a = PT6)
h2c_e <- lmer(stance_nlp ~ masking + b * a +
                PT7 + (1 | issue), data= df)
h2c_e2 <- tidy(h2c_e) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "United States")
h2c_e <- tidy(margins::margins(h2c_e, variables = "b",
                               at = list("a" = 0:10))) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "United States")

tmp <- lmer(stance_ss ~ masking + b * a +
              PT7 + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "United States")
h2c_e2 <- h2c_e2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:10))) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "United States")
h2c_e <- h2c_e %>% 
  add_case(tmp)

df <- d %>% 
  mutate(b = masking,
         a = congruence,
         b = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
h3a_e <- lmer(stance_nlp ~ specification + b * a +
                PT7 + (1 | issue), data= df)
h3a_e2 <- tidy(h3a_e) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "United States")
h3a_e <- tidy(margins::margins(h3a_e, variables = "b",
                               at = list("a" = 0:1))) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "United States")

tmp <- lmer(stance_ss ~ specification + b * a +
              PT7 + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "United States")
h3a_e2 <- h3a_e2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:1))) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "United States")
h3a_e <- h3a_e %>% 
  add_case(tmp)

df <- d %>% 
  mutate(b = masking,
         a = pid,
         b = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
h3b_e <- lmer(stance_nlp ~ specification + b * a +
                PT7 + (1 | issue), data= df)
h3b_e2 <- tidy(h3b_e) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "United States")
h3b_e <- tidy(margins::margins(h3b_e, variables = "b",
                               at = list("a" = c("Strong Democrat","Not very strong Democrat", "Strong Republican","Not very strong Republican")))) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "United States")

tmp <- lmer(stance_ss ~ specification + b * a +
              PT7 + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "United States")
h3b_e2 <- h3b_e2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = c("Strong Democrat","Not very strong Democrat", "Strong Republican","Not very strong Republican")))) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "United States")

h3b_e <- h3b_e %>% 
  add_case(tmp)

df <- d %>% 
  mutate(b = masking,
         a = PT6,
         b = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
h3c_e <- lmer(stance_nlp ~ specification + b * a +
                PT7 + (1 | issue), data= df)
h3c_e2 <- tidy(h3c_e) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "United States")
h3c_e <- tidy(margins::margins(h3c_e, variables = "b",
                               at = list("a" = 0:10))) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "United States")

tmp <- lmer(stance_ss ~ specification + b * a +
              PT7 + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "United States")
h3c_e2 <- h3c_e2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:10))) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "United States")
h3c_e <- h3c_e %>% 
  add_case(tmp)

df <- d %>% 
  mutate(b = specification,
         a = congruence)
h2a_e_ic <- lmer(interpretation_nlp ~ masking + b * a +
                   PT7 + (1 | issue), data= df)
h2a_e_ic2 <- tidy(h2a_e_ic) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "United States")
h2a_e_ic <- tidy(margins::margins(h2a_e_ic, variables = "b",
                                  at = list("a" = 0:1))) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "United States")

tmp <- lmer(interpretation_ss ~ masking + b * a +
              PT7 + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "United States")
h2a_e_ic2 <- h2a_e_ic2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:1))) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "United States")
h2a_e_ic <- h2a_e_ic %>% 
  add_case(tmp)

df <- d %>% 
  mutate(b = specification,
         a = pid)
h2b_e_pid <- lmer(interpretation_nlp ~ masking + b * a +
                    PT7 + (1 | issue), data= df)
h2b_e_pid2 <- tidy(h2b_e_pid) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "United States")
h2b_e_pid <- tidy(margins::margins(h2b_e_pid, variables = "b",
                                   at = list("a" = c("Strong Democrat","Not very strong Democrat", "Strong Republican","Not very strong Republican")))) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Strict Interpretation",
         type = "United States")

tmp <- lmer(interpretation_ss ~ masking + b * a +
              PT7 + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "United States")
h2b_e_pid2 <- h2b_e_pid2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = c("Strong Democrat","Not very strong Democrat", "Strong Republican","Not very strong Republican")))) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Strict Interpretation",
         type = "United States")
h2b_e_pid <- h2b_e_pid %>% 
  add_case(tmp)

df <- d %>% 
  mutate(b = specification,
         a = PT6)
h2c_e_ip <- lmer(interpretation_nlp ~ masking + b * a +
                   PT7 + (1 | issue), data= df)
h2c_e_ip2 <- tidy(h2c_e_ip) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "United States")
h2c_e_ip <- tidy(margins::margins(h2c_e_ip, variables = "b",
                                  at = list("a" = 0:10))) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "United States")

tmp <- lmer(interpretation_ss ~ masking + b * a +
              PT7 + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "United States")
h2c_e_ip2 <- h2c_e_ip2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:10))) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "United States")
h2c_e_ip <- h2c_e_ip %>% 
  add_case(tmp)

df <- d %>% 
  mutate(b = masking,
         a = congruence,
         b = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
h3a_e_ic <- lmer(interpretation_nlp ~ specification + b * a +
                   PT7 + (1 | issue), data= df)
h3a_e_ic2 <- tidy(h3a_e_ic)  %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "United States")
h3a_e_ic <- tidy(margins::margins(h3a_e_ic, variables = "b",
                                  at = list("a" = 0:1))) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "United States")

tmp <- lmer(interpretation_ss ~ specification + b * a +
              PT7 + (1 | issue), data= df)
tmpp <- tidy(tmp)  %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "United States")
h3a_e_ic2 <- h3a_e_ic2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:1))) %>% 
  mutate(hyp = "Issue Congruence",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "United States")
h3a_e_ic <- h3a_e_ic %>% 
  add_case(tmp)

df <- d %>% 
  mutate(b = masking,
         a = pid,
         b = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
h3b_e_pid <- lmer(interpretation_nlp ~ specification + b * a +
                    PT7 + (1 | issue), data= df)
h3b_e_pid2 <- tidy(h3b_e_pid) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "United States")
h3b_e_pid <- tidy(margins::margins(h3b_e_pid, variables = "b",
                                   at = list("a" = c("Strong Democrat","Not very strong Democrat", "Strong Republican","Not very strong Republican")))) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "United States")

tmp <- lmer(interpretation_ss ~ specification + b * a +
              PT7 + (1 | issue), data= df)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "United States")
h3b_e_pid2 <- h3b_e_pid2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = c("Strong Democrat","Not very strong Democrat", "Strong Republican","Not very strong Republican")))) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "United States")
h3b_e_pid <- h3b_e_pid %>% 
  add_case(tmp)

df <- d %>% 
  mutate(b = masking,
         a = PT6,
         b = recode(masking,
                    `Masked` = 1,
                    `Party` = 0))
h3c_e_ip <- lmer(interpretation_nlp ~ specification + b * a +
                   PT7 + (1 | issue), data= df)
h3c_e_ip2 <- tidy(h3c_e_ip) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "United States")
h3c_e_ip <- tidy(margins::margins(h3c_e_ip, variables = "b",
                                  at = list("a" = 0:10))) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "United States")

tmp <- lmer(interpretation_ss ~ specification + b * a +
              PT7 + (1 | issue), data= df)
h3c_e_ip2 <- tidy(tmp) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "United States")
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:10))) %>% 
  mutate(hyp = "Ideological Position (0 = Left, 10 = Right)",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "United States")
h3c_e_ip <- h3c_e_ip %>% 
  add_case(tmp)

exp_us_int <- h2a_e %>% 
  add_case(h2a_e_ic) %>% 
  add_case(h2c_e) %>% 
  add_case(h2c_e_ip) %>% 
  add_case(h3a_e) %>% 
  add_case(h3a_e_ic) %>% 
  add_case(h3c_e) %>% 
  add_case(h3c_e_ip) 

exp_us_int <- h2b_e %>% 
  add_case(h2b_e_pid) %>% 
  add_case(h3b_e) %>% 
  add_case(h3b_e_pid)

exp_us_int2 <- h2a_e2 %>% 
  add_case(h2a_e_ic2) %>% 
  add_case(h2c_e2) %>% 
  add_case(h2c_e_ip2) %>% 
  add_case(h3a_e2) %>% 
  add_case(h3a_e_ic2) %>% 
  add_case(h3c_e2) %>% 
  add_case(h3c_e_ip2) 

exp_us_char2 <- h2b_e2 %>% 
  add_case(h2b_e_pid2) %>% 
  add_case(h3b_e2) %>% 
  add_case(h3b_e_pid2)

rm(h2a_e,h2a_e_ic, h2b_e, h2b_e_pid, 
   h2c_e, h2c_e_ip, h3a_e, h3a_e_ic,
   h3b_e, h3b_e_pid, h3c_e, h3c_e_pid,
   h2a_e2,h2a_e_ic2, h2b_e2, h2b_e_pid2, 
   h2c_e2, h2c_e_ip2, h3a_e2, h3a_e_ic2,
   h3c_e_ip, h3c_e_ip2,
   h3b_e2, h3b_e_pid2, h3c_e2, h3c_e_pid2,
   tmp, tmpp)

p_e1a <- exp_nl_int2 %>% 
  add_case(exp_us_int2) %>% 
  filter(term %in% c("a", "b", "b:a")) %>% 
  mutate(hyp = ifelse(hyp=="Ideological Position (0 = Left, 10 = Right)", "Ideological Position",
                      hyp),
         term = ifelse(term == "a", hyp, term),
         term = ifelse(term == "b", condition, term),
         term = ifelse(term == "b:a", "Interaction", term),
         lower = estimate - (1.96 * std.error),
         upper = estimate + (1.96 * std.error)) %>% 
  ggplot(aes(x = estimate, y = term,
             xmin = lower, xmax = upper, color = y)) +
  geom_point(position = position_dodge(0.5)) +
  geom_errorbar(width = 0,
                position = position_dodge(0.5)) +
  facet_grid(hyp~type, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray25") +
  labs(x = "Predicted Effect for Overinterpreting Stance", y = "") +
  theme_ipsum() +
  scale_colour_manual(values = fig_cols) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))

p_e1b <- exp_nl_char2 %>% 
  add_case(exp_us_char2) %>% 
  filter(term %in% c("aForum voor Democratie", "aGroenLinks", "aPvdA", "aPVV",
                     "aNot very strong Democrat", "aNot very strong Republican",
                     "aStrong Democrat", "aStrong Republican",
                     "b", "bUnderspecified",
                     "b:aNot very strong Democrat", "b:aNot very strong Republican",
                     "b:aStrong Democrat", "b:aStrong Republican",
                     "b:aForum voor Democratie", "b:aGroenLinks", "b:aPvdA", "b:aPVV",
                     "bUnderspecified:aForum voor Democratie", "bUnderspecified:aGroenLinks", 
                     "bUnderspecified:aPvdA", "bUnderspecified:aPVV",
                     "bUnderspecified:aNot very strong Democrat",
                     "bUnderspecified:aStrong Democrat",
                     "bUnderspecified:aNot very strong Republican",
                     "bUnderspecified:aStrong Republican")) %>% 
  mutate(hyp = ifelse(hyp=="Ideological Position (0 = Left, 10 = Right)", "Ideological Position",
                      hyp),
         term = ifelse(term == "aForum voor Democratie", "Forum voor Democratie", term),
         term = ifelse(term == "aGroenLinks", "GroenLinks", term),
         term = ifelse(term == "aPvdA", "PvdA", term),
         term = ifelse(term == "aPVV", "PVV", term),
         term = ifelse(term == "aNot very strong Democrate", "Not very strong Democrat", term),
         term = ifelse(term == "aNot very strong Republican", "Not very strong Republican", term),
         term = ifelse(term == "aStrong Democrat", "Strong Democrat", term),
         term = ifelse(term == "aStrong Republican", "Strong Republican", term),
         term = ifelse(term == "b", condition, term),
         term = ifelse(term == "bUnderspecified", condition, term),
         term = ifelse(term == "b:aForum voor Democratie", "Interaction", term),
         term = ifelse(term == "b:aPvdA voor Democratie", "Interaction", term),
         term = ifelse(term == "b:aGroenLinks voor Democratie", "Interaction", term),
         term = ifelse(term == "b:aPVV voor Democratie", "Interaction", term),
         term = ifelse(term == "b:aNot very strong Democrat", "Interaction", term),
         term = ifelse(term == "b:aNot very strong Republican", "Interaction", term),
         term = ifelse(term == "b:aStrong Democrat", "Interaction", term),
         term = ifelse(term == "b:aStrong Republican", "Interaction", term),
         term = ifelse(term == "bUnderspecified:aForum voor Democratie", "Interaction", term),
         term = ifelse(term == "bUnderspecified:aPvdA", "Interaction", term),
         term = ifelse(term == "bUnderspecified:aGroenLinks", "Interaction", term),
         term = ifelse(term == "bUnderspecified:aPVV", "Interaction", term),
         term = ifelse(term == "bUnderspecified:aNot very strong Democrat", "Interaction", term),
         term = ifelse(term == "bUnderspecified:aNot very strong Republican", "Interaction", term),
         term = ifelse(term == "bUnderspecified:aStrong Democrat", "Interaction", term),
         term = ifelse(term == "bUnderspecified:aStrong Republican", "Interaction", term),
         lower = estimate - (1.96 * std.error),
         upper = estimate + (1.96 * std.error)) %>% 
  ggplot(aes(x = estimate, y = term,
             xmin = lower, xmax = upper, color = y)) +
  geom_point(position = position_dodge(0.5)) +
  geom_errorbar(width = 0,
                position = position_dodge(0.5)) +
  facet_grid(hyp~type, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray25") +
  labs(x = "Predicted Effect for Overinterpreting Stance", y = "") +
  theme_ipsum() +
  scale_colour_manual(values = fig_cols) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))
