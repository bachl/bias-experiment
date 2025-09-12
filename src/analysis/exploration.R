df_us <- d |> 
  filter(country == "US") |> 
  select(stance_nlp2, stance_ss2, interpret_nlp2, interpret_ss2, masking, specified, distance, PT7, issue, sentence2)
df_nl <- d |> 
  filter(country == "NL") |> 
  select(stance_nlp2, stance_ss2, interpret_nlp2, interpret_ss2, masking, specified, distance, PT7, issue, sentence2)

df <- df_us %>% 
  mutate(b = specified,
         a = as.numeric(masking)) 

b3_a <- lmer(stance_nlp2 ~ b * a +
                 issue + (1 | sentence2), data= df)
b3_a2 <- tidy(b3_a) %>% 
  mutate(condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "United States")
b3_a <- tidy(margins::margins(b3_a, variables = "b",
                               at = list("a" = 0:1))) %>% 
  mutate(condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "United States")

tmp <- lmer(stance_ss2 ~ b * a + issue + (1 | sentence2), data= df)
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

df <- df_us %>% 
  mutate(b = specified,
         a = as.numeric(masking))
tmp <- lmer(stance_nlp2 ~ b * a + issue + (1 | sentence2), data= df)
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

tmp <- lmer(stance_ss2 ~ b * a + issue + (1 | sentence2), data= df)
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
  mutate(b = specified,
         a = as.numeric(masking))
b3_b <- lmer(interpret_nlp2 ~ b * a +
               issue + (1 | sentence2), data= df)
b3_b2 <- tidy(b3_b) %>% 
  mutate(condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "United States")
b3_b <- tidy(margins::margins(b3_b, variables = "b",
                              at = list("a" = 0:1))) %>% 
  mutate(condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "United States")

tmp <- lmer(interpret_ss2 ~ b * a + issue + (1 | sentence2), data= df)
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
  mutate(b = specified,
         a = as.numeric(masking))
tmp <- lmer(interpret_nlp2 ~ b * a +
               issue + (1 | sentence2), data= df)
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

tmp <- lmer(interpret_ss2 ~ b * a + issue + (1 | sentence2), data= df)
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
                         `bUnderspecified:a` = "Interaction: Masking * specified")) %>% 
  ggplot(aes(y = `term`, x = estimate,
             xmin = estimate -1.645*std.error,
             xmax = estimate +1.645*std.error,
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
             xmin = estimate -1.645*std.error,
             xmax = estimate +1.645*std.error,
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
d_nl <- d |> 
  filter(country == "NL") |> 
  mutate(issue2 = recode(issue,
                         `immigranten` = "Immigration",
                         `immigratie` = "Immigration",
                         `het tegengaan van stikstofuitstoot` = "Climate",
                         `het stikstofbeleid` = "Climate",
                         `het verhogen van het belastingstarief voor de hoogste inkomens` = "Tax",
                         `het belastingstelsel` = "Tax",
                         `de  rol van Nederland in de Europese Unie` = "EU/Foreign Policy",
                         `het lidmaatschap van de Europese Unie voor Nederland` = "EU/Foreign Policy"),
         congruence = 0,
         congruence = ifelse(PT2<3 & issue2=="Immigration", 1,
                             congruence),
         congruence = ifelse(PT3<3 & issue2=="Climate", 1,
                             congruence),
         congruence = ifelse(PT4>3 & issue2=="Tax", 1,
                             congruence),
         congruence = ifelse(PT5>3 & issue2=="EU/Foreign Policy", 1,
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

df <- within(d_nl, pid <- relevel(pid, ref = "Other party"))

d_nl <- df %>% 
  mutate(b = specified,
         a = congruence)
h2a_e <- lmer(stance_nlp2 ~ masking + b * a +
              PT7 + issue + (1 | sentence2), data= d_nl)
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

tmp <- lmer(stance_ss2 ~ masking + b * a +
                PT7 + issue + (1 | sentence2), data= d_nl)
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

d_nl <- d_nl %>% 
  mutate(b = specified,
         a = pid)
h2b_e <- lmer(stance_nlp2 ~ masking + b * a +
                PT7 + issue + (1 | sentence2), data= d_nl)
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

tmp <- lmer(stance_ss2 ~ masking + b * a +
                PT7 + issue + (1 | sentence2), data= d_nl)
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

d_nl <- d_nl %>% 
  mutate(b = specified,
         a = PT6)
h2c_e <- lmer(stance_nlp2 ~ masking + b * a +
                PT7 + issue + (1 | sentence2), data= d_nl)
h2c_e2 <- tidy(h2c_e) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "The Netherlands")
h2c_e <- tidy(margins::margins(h2c_e, variables = "b",
                               at = list("a" = 0:1))) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "The Netherlands")

tmp <- lmer(stance_ss2 ~ masking + b * a +
                PT7 + issue + (1 | sentence2), data= d_nl)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "The Netherlands")
h2c_e2 <- h2c_e2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                               at = list("a" = 0:1))) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "The Netherlands")
h2c_e <- h2c_e %>% 
  add_case(tmp)

d_nl <- d_nl %>% 
  mutate(b = masking,
         a = congruence)
h3a_e <- lmer(stance_nlp2 ~ specified + b * a +
                PT7 + issue + (1 | sentence2), data= d_nl)
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

tmp <- lmer(stance_ss2 ~ specified + b * a +
                PT7 + issue + (1 | sentence2), data= d_nl)
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

d_nl <- d_nl %>% 
  mutate(b = masking,
         a = pid)
h3b_e <- lmer(stance_nlp2 ~ specified + b * a +
                PT7 + issue + (1 | sentence2), data= d_nl)
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

tmp <- lmer(stance_ss2 ~ specified + b * a +
                PT7 + issue + (1 | sentence2), data= d_nl)
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

d_nl <- d_nl %>% 
  mutate(b = masking,
         a = PT6)
h3c_e <- lmer(stance_nlp2 ~ specified + b * a +
                PT7 + issue + (1 | sentence2), data= d_nl)
h3c_e2 <- tidy(h3c_e) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "The Netherlands")
h3c_e <- tidy(margins::margins(h3c_e, variables = "b",
                               at = list("a" = 0:1))) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "The Netherlands")

tmp <- lmer(stance_ss2 ~ specified + b * a +
                PT7 + issue + (1 | sentence2), data= d_nl)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "The Netherlands")
h3c_e2 <- h3c_e2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                               at = list("a" = 0:1))) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "The Netherlands")
h3c_e <- h3c_e %>% 
  add_case(tmp)

d_nl <- d_nl %>% 
  mutate(b = specified,
         a = congruence)
h2a_e_ic <- lmer(interpret_nlp2 ~ masking + b * a +
                PT7 + issue + (1 | sentence2), data= d_nl)
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

tmp <- lmer(interpret_ss2 ~ masking + b * a +
                   PT7 + issue + (1 | sentence2), data= d_nl)
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

d_nl <- d_nl %>% 
  mutate(b = specified,
         a = pid)
h2b_e_pid <- lmer(interpret_nlp2 ~ masking + b * a +
                PT7 + issue + (1 | sentence2), data= d_nl)
h2b_e_pid2 <- tidy(h2b_e_pid) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "The Netherlands")
h2b_e_pid <- tidy(margins::margins(h2b_e_pid, variables = "b",
                               at = list("a" = c("Forum voor Democratie","GroenLinks", "PvdA","PVV")))) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "The Netherlands")

tmp <- lmer(interpret_ss2 ~ masking + b * a +
                    PT7 + issue + (1 | sentence2), data= d_nl)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "The Netherlands")
h2b_e_pid2 <- h2b_e_pid2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                                at = list("a" = c("Forum voor Democratie","GroenLinks", "PvdA","PVV")))) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "The Netherlands")
h2b_e_pid <- h2b_e_pid %>% 
  add_case(tmp)

d_nl <- d_nl %>% 
  mutate(b = specified,
         a = PT6)
h2c_e_ip <- lmer(interpret_nlp2 ~ masking + b * a +
                PT7 + issue + (1 | sentence2), data= d_nl)
h2c_e_ip2 <- tidy(h2c_e_ip) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "The Netherlands")
h2c_e_ip <- tidy(margins::margins(h2c_e_ip, variables = "b",
                               at = list("a" = 0:1))) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "The Netherlands")

tmp <- lmer(interpret_ss2 ~ masking + b * a +
                   PT7 + issue + (1 | sentence2), data= d_nl)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "The Netherlands")
h2c_e_ip2 <- h2c_e_ip2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                                  at = list("a" = 0:1))) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "The Netherlands")
h2c_e_ip <- h2c_e_ip %>% 
  add_case(tmp)

d_nl <- d_nl %>% 
  mutate(b = masking,
         a = congruence)
h3a_e_ic <- lmer(interpret_nlp2 ~ specified + b * a +
                PT7 + issue + (1 | sentence2), data= d_nl)
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

tmp <- lmer(interpret_ss2 ~ specified + b * a +
                   PT7 + issue + (1 | sentence2), data= d_nl)
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

d_nl <- d_nl %>% 
  mutate(b = masking,
         a = pid)
h3b_e_pid <- lmer(interpret_nlp2 ~ specified + b * a +
                PT7 + issue + (1 | sentence2), data= d_nl)
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

tmp <- lmer(interpret_ss2 ~ specified + b * a +
                    PT7 + issue + (1 | sentence2), data= d_nl)
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

d_nl <- d_nl %>% 
  mutate(b = masking,
         a = PT6)
h3c_e_ip <- lmer(interpret_nlp2 ~ specified + b * a +
                PT7 + issue + (1 | sentence2), data= d_nl)
h3c_e_ip2 <- tidy(h3c_e_ip) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "The Netherlands")
h3c_e_ip <- tidy(margins::margins(h3c_e_ip, variables = "b",
                               at = list("a" = 0:1))) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative))",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "The Netherlands")

tmp <- lmer(interpret_ss2 ~ specified + b * a +
                   PT7 + issue + (1 | sentence2), data= d_nl)
h3c_e_ip2 <- tidy(tmp) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "The Netherlands")
tmp <- tidy(margins::margins(tmp, variables = "b",
                                  at = list("a" = 0:1))) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)",
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
d_us <- d |> 
  filter(country == "US") |> 
  mutate(issue2 = recode(issue,
                         `tax on carbon emissions` = "Climate",
                         `a wealth tax for the richest Americans` = "Tax",
                         `the tax system` = "Tax",
                         `military presence in the Pacific Ocean` = "EU/Foreign Policy",
                         `military build-up in the Pacific Ocean` = "EU/Foreign Policy",
                         `immigration` = "Immigration"),
         congruence = 0,
         congruence = ifelse(PT2>3 & issue2=="Immigration", 1,
                             congruence),
         congruence = ifelse(PT3<3 & issue2=="Climate", 1,
                             congruence),
         congruence = ifelse(PT4>3 & issue2=="Tax", 1,
                             congruence),
         congruence = ifelse(PT5>3 & issue2=="Foreign Policy", 1,
                             congruence),
         pid = "Other",
         pid = ifelse(PT1=="Republican",
                      "Republican", pid),
         pid = ifelse(PT1=="Democrat", 
                      "Democrat", pid),
         pid = factor(pid,
                      levels = c("Other","Democrat",
                                 "Republican")))

df <- within(d_us, pid <- relevel(pid, ref = "Other"))

d_us <- df %>% 
  mutate(b = specified,
         a = congruence)

h2a_e <- lmer(stance_nlp2 ~ masking + b * a +
                PT7 + issue + (1 | sentence2), data= d_us)
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

tmp <- lmer(stance_ss2 ~ masking + b * a +
              PT7 + issue + (1 | sentence2), data= d_us)
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

d_us <- d_us %>% 
  mutate(b = specified,
         a = pid)
h2b_e <- lmer(stance_nlp2 ~ masking + b * a +
                PT7 + issue + (1 | sentence2), data= d_us)
h2b_e2 <- tidy(h2b_e) %>% 
  mutate(hyp = "Party Voted For/ Party ID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "United States")
h2b_e <- tidy(margins::margins(h2b_e, variables = "b",
                               at = list("a" = c("Democrat","Republican")))) %>% 
  mutate(hyp = "Party Voted For/ Party ID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "United States")

tmp <- lmer(stance_ss2 ~ masking + b * a +
              PT7 + issue + (1 | sentence2), data= d_us)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Party Voted For/ Party ID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "United States")
h2b_e2 <- h2b_e2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = c("Democrat","Republican")))) %>% 
  mutate(hyp = "Party Voted For/ Party ID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "United States")

d_us <- d_us %>% 
  mutate(b = specified,
         a = PT6)
h2c_e <- lmer(stance_nlp2 ~ masking + b * a +
                PT7 + issue + (1 | sentence2), data= d_us)
h2c_e2 <- tidy(h2c_e) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "United States")
h2c_e <- tidy(margins::margins(h2c_e, variables = "b",
                               at = list("a" = 0:1))) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "United States")

tmp <- lmer(stance_ss2 ~ masking + b * a +
              PT7 + issue + (1 | sentence2), data= d_us)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "United States")
h2c_e2 <- h2c_e2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:1))) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "United States")
h2c_e <- h2c_e %>% 
  add_case(tmp)

d_us <- d_us %>% 
  mutate(b = masking,
         a = congruence)
h3a_e <- lmer(stance_nlp2 ~ specified + b * a +
                PT7 + issue + (1 | sentence2), data= d_us)
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

tmp <- lmer(stance_ss2 ~ specified + b * a +
              PT7 + issue + (1 | sentence2), data= d_us)
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

d_us <- d_us %>% 
  mutate(b = masking,
         a = pid)
h3b_e <- lmer(stance_nlp2 ~ specified + b * a +
                PT7 + issue + (1 | sentence2), data= d_us)
h3b_e2 <- tidy(h3b_e) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "United States")
h3b_e <- tidy(margins::margins(h3b_e, variables = "b",
                               at = list("a" = c("Democrat","Republican")))) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "United States")

tmp <- lmer(stance_ss2 ~ specified + b * a +
              PT7 + issue + (1 | sentence2), data= d_us)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "United States")
h3b_e2 <- h3b_e2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = c("Democrat","Republican")))) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "United States")

h3b_e <- h3b_e %>% 
  add_case(tmp)

d_us <- d_us %>% 
  mutate(b = masking,
         a = PT6)
h3c_e <- lmer(stance_nlp2 ~ specified + b * a +
                PT7 + issue + (1 | sentence2), data= d_us)
h3c_e2 <- tidy(h3c_e) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "United States")
h3c_e <- tidy(margins::margins(h3c_e, variables = "b",
                               at = list("a" = 0:1))) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Strict Interpretation)",
         type = "United States")

tmp <- lmer(stance_ss2 ~ specified + b * a +
              PT7 + issue + (1 | sentence2), data= d_us)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "United States")
h3c_e2 <- h3c_e2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:1))) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)",
         condition = "Condition: Party Masked",
         y = "Y: Correctly Identifying Stance (Lenient Interpretation)",
         type = "United States")
h3c_e <- h3c_e %>% 
  add_case(tmp)

d_us <- d_us %>% 
  mutate(b = specified,
         a = congruence)
h2a_e_ic <- lmer(interpret_nlp2 ~ masking + b * a +
                   PT7 + issue + (1 | sentence2), data= d_us)
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

tmp <- lmer(interpret_ss2 ~ masking + b * a +
              PT7 + issue + (1 | sentence2), data= d_us)
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

d_us <- d_us %>% 
  mutate(b = specified,
         a = pid)
h2b_e_pid <- lmer(interpret_nlp2 ~ masking + b * a +
                    PT7 + issue + (1 | sentence2), data= d_us)
h2b_e_pid2 <- tidy(h2b_e_pid) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "United States")
h2b_e_pid <- tidy(margins::margins(h2b_e_pid, variables = "b",
                                   at = list("a" = c("Democrat","Republican")))) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "United States")

tmp <- lmer(interpret_ss2 ~ masking + b * a +
              PT7 + issue + (1 | sentence2), data= d_us)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "United States")
h2b_e_pid2 <- h2b_e_pid2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = c("Democrat","Republican")))) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "United States")
h2b_e_pid <- h2b_e_pid %>% 
  add_case(tmp)

d_us <- d_us %>% 
  mutate(b = specified,
         a = PT6)
h2c_e_ip <- lmer(interpret_nlp2 ~ masking + b * a +
                   PT7 + issue + (1 | sentence2), data= d_us)
h2c_e_ip2 <- tidy(h2c_e_ip) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "United States")
h2c_e_ip <- tidy(margins::margins(h2c_e_ip, variables = "b",
                                  at = list("a" = 0:1))) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "United States")

tmp <- lmer(interpret_ss2 ~ masking + b * a +
              PT7 + issue + (1 | sentence2), data= d_us)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "United States")
h2c_e_ip2 <- h2c_e_ip2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:1))) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)",
         condition = "Condition: Underspecified Sentence",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "United States")
h2c_e_ip <- h2c_e_ip %>% 
  add_case(tmp)

d_us <- d_us %>% 
  mutate(b = masking,
         a = congruence)
h3a_e_ic <- lmer(interpret_nlp2 ~ specified + b * a +
                   PT7 + issue + (1 | sentence2), data= d_us)
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

tmp <- lmer(interpret_ss2 ~ specified + b * a +
              PT7 + issue + (1 | sentence2), data= d_us)
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

d_us <- d_us %>% 
  mutate(b = masking,
         a = pid)
h3b_e_pid <- lmer(interpret_nlp2 ~ specified + b * a +
                    PT7 + issue + (1 | sentence2), data= d_us)
h3b_e_pid2 <- tidy(h3b_e_pid) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "United States")
h3b_e_pid <- tidy(margins::margins(h3b_e_pid, variables = "b",
                                   at = list("a" = c("Democrat","Republican")))) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "United States")

tmp <- lmer(interpret_ss2 ~ specified + b * a +
              PT7 + issue + (1 | sentence2), data= d_us)
tmpp <- tidy(tmp) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "United States")
h3b_e_pid2 <- h3b_e_pid2 %>% 
  add_case(tmpp)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = c("Democrat","Republican")))) %>% 
  mutate(hyp = "Party Voted For/ PID",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "United States")
h3b_e_pid <- h3b_e_pid %>% 
  add_case(tmp)

d_us <- d_us %>% 
  mutate(b = masking,
         a = PT6)
h3c_e_ip <- lmer(interpret_nlp2 ~ specified + b * a +
                   PT7 + issue + (1 | sentence2), data= d_us)
h3c_e_ip2 <- tidy(h3c_e_ip) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "United States")
h3c_e_ip <- tidy(margins::margins(h3c_e_ip, variables = "b",
                                  at = list("a" = 0:1))) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Strict Interpretation)",
         type = "United States")

tmp <- lmer(interpret_ss2 ~ specified + b * a +
              PT7 + issue + (1 | sentence2), data= d_us)
h3c_e_ip2 <- tidy(tmp) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)",
         condition = "Condition: Party Masked",
         y = "Y: Overinterpreting Stance (Lenient Interpretation)",
         type = "United States")
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:1))) %>% 
  mutate(hyp = "Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)",
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
  filter(term %in% c("a", "bUnderspecified", "bUnderspecified:a",
                     "b1", "b1:a")) %>% 
  mutate(hyp = ifelse(hyp=="Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)", "Ideological Position",
                      hyp),
         term = ifelse(term == "a", hyp, term),
         term = ifelse(term == "b1", condition, term),
         term = ifelse(term == "b1:a", "Interaction", term),
         term = ifelse(term == "bUnderspecified", condition, term),
         term = ifelse(term == "bUnderspecified:a", "Interaction", term),
         lower = estimate - (1.96 * std.error),
         upper = estimate + (1.96 * std.error)) %>% 
  filter(hyp == "Issue Congruence") |> 
  ggplot(aes(x = estimate, y = term,
             xmin = lower, xmax = upper, color = y)) +
  geom_point(position = position_dodge(0.5)) +
  geom_errorbar(width = 0,
                position = position_dodge(0.5)) +
  facet_grid(condition~type, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray25") +
  labs(x = "Predicted Effect for Overinterpreting Stance", y = "") +
  theme_ipsum() +
  scale_colour_manual(values = fig_cols) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))

p_e1b <- exp_nl_int2 %>% 
  add_case(exp_us_int2) %>% 
  filter(term %in% c("a", "bUnderspecified", "bUnderspecified:a",
                     "b1", "b1:a")) %>% 
  mutate(hyp = ifelse(hyp=="Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)", "Ideological Position",
                      hyp),
         term = ifelse(term == "a", hyp, term),
         term = ifelse(term == "b1", condition, term),
         term = ifelse(term == "b1:a", "Interaction", term),
         term = ifelse(term == "bUnderspecified", condition, term),
         term = ifelse(term == "bUnderspecified:a", "Interaction", term),
         lower = estimate - (1.96 * std.error),
         upper = estimate + (1.96 * std.error)) %>% 
  filter(hyp != "Issue Congruence") |> 
  ggplot(aes(x = estimate, y = term,
             xmin = lower, xmax = upper, color = y)) +
  geom_point(position = position_dodge(0.5)) +
  geom_errorbar(width = 0,
                position = position_dodge(0.5)) +
  facet_grid(condition~type, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray25") +
  labs(x = "Predicted Effect for Overinterpreting Stance", y = "") +
  theme_ipsum() +
  scale_colour_manual(values = fig_cols) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))

p_e1c <- exp_nl_char2 %>% 
  add_case(exp_us_char2) %>% 
  filter(term %in% c("aForum voor Democratie", "aGroenLinks", "aPvdA", "aPVV",
                     "aDemocrat", "aRepublican",
                     "b1", "bUnderspecified",
                     "b1:aDemocrat", "b1:aRepublican",
                     "b1:aForum voor Democratie", "b1:aGroenLinks", "b1:aPvdA", "b1:aPVV",
                     "bUnderspecified:aForum voor Democratie", "bUnderspecified:aGroenLinks", 
                     "bUnderspecified:aPvdA", "bUnderspecified:aPVV",
                     "bUnderspecified:aDemocrat",
                     "bUnderspecified:aRepublican")) %>% 
  mutate(hyp = ifelse(hyp=="Ideological Position (0 = Left/Liberal, 1 = Right/Conservative)", "Ideological Position",
                      hyp),
         term = ifelse(term == "aForum voor Democratie", "Forum voor Democratie", term),
         term = ifelse(term == "aGroenLinks", "GroenLinks", term),
         term = ifelse(term == "aPvdA", "PvdA", term),
         term = ifelse(term == "aPVV", "PVV", term),
         term = ifelse(term == "aDemocrat", "Democrat", term),
         term = ifelse(term == "aRepublican", "Republican", term),
         term = ifelse(term == "b1", condition, term),
         term = ifelse(term == "bUnderspecified", condition, term),
         term = ifelse(term == "b1:aForum voor Democratie", "Interaction Forum voor Democratie", term),
         term = ifelse(term == "b1:aPvdA", "Interaction PvdA", term),
         term = ifelse(term == "b1:aGroenLinks", "Interaction GroenLinks", term),
         term = ifelse(term == "b1:aPVV", "Interaction PVV", term),
         term = ifelse(term == "b1:aDemocrat", "Interaction Democrat", term),
         term = ifelse(term == "b1:aRepublican", "Interaction Republican", term),
         term = ifelse(term == "bUnderspecified:aForum voor Democratie", "Interaction Forum voor Democratie", term),
         term = ifelse(term == "bUnderspecified:aPvdA", "Interaction PvdA", term),
         term = ifelse(term == "bUnderspecified:aGroenLinks", "Interaction GroenLinks", term),
         term = ifelse(term == "bUnderspecified:aPVV", "Interaction PVV", term),
         term = ifelse(term == "bUnderspecified:aDemocrat", "Interaction Democrat", term),
         term = ifelse(term == "bUnderspecified:aRepublican", "Interaction Republican", term),
         term = factor(term, 
                       levels = c("Interaction Republican", "Interaction Democrat",
                                  "Interaction PVV", "Interaction PvdA",
                                  "Interaction GroenLinks", "Interaction Forum voor Democratie",
                                  "Republican", "Democrat",
                                  "PVV", "PvdA", "GroenLinks", "Forum voor Democratie",
                                  "Condition: Underspecified Sentence", "Condition: Party Masked")),
         lower = estimate - (1.96 * std.error),
         upper = estimate + (1.96 * std.error)) %>% 
  ggplot(aes(x = estimate, y = term,
             xmin = lower, xmax = upper, color = y)) +
  geom_point(position = position_dodge(0.5)) +
  geom_errorbar(width = 0,
                position = position_dodge(0.5)) +
  facet_grid(condition~type, scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray25") +
  labs(x = "Predicted Effect for Overinterpreting Stance", y = "") +
  theme_ipsum() +
  scale_colour_manual(values = fig_cols) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))
