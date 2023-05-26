df_us <- d_us %>% 
  select(stance, masking, specification, distance, PT7, country, issue)
df_nl <- d_nl %>% 
  select(stance, masking, specification, distance, PT7, country, issue)

d <- df_nl %>% 
  add_case(df_us)

df <- d %>% 
  mutate(b = recode(masking,
                    `Masked` = 1,
                    `Party` = 0),
         a = distance)
h3a <- lmer(stance ~ specification + b * a +
              PT7 + (country | issue), data= df)
h3a <- broom.mixed::tidy(margins::margins(h3a, variables = "b",
                             at = list("a" = 0:8.5))) %>% 
  mutate(hyp = "H3a: Ideological Distance \n(0=Low, 10 = High)",
         type = "Pooled Analysis")

df <- df_us %>% 
  mutate(b = recode(masking,
                    `Masked` = 1,
                    `Party` = 0),
         a = distance)
tmp <- lmer(stance ~ specification + b * a +
              PT7 + (1 | issue), data= df)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:4))) %>% 
  mutate(hyp = "H3a: Ideological Distance \n(0=Low, 10 = High)",
         type = "United States")

h3a <- h3a %>% 
  add_case(tmp)

df <- df_nl %>% 
  mutate(b = recode(masking,
                    `Masked` = 1,
                    `Party` = 0),
         a = distance)
tmp <- lmer(stance ~ specification + b * a +
              PT7 + (1 | issue), data= df)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:8.5))) %>% 
  mutate(hyp = "H3a: Ideological Distance \n(0=Low, 10 = High)",
         type = "The Netherlands")

h3a <- h3a %>% 
  add_case(tmp)


df <- d %>% 
  mutate(b = recode(masking,
                    `Masked` = 1,
                    `Party` = 0),
         a = PT7)
h3b <- lmer(stance ~ specification + b * a +
              distance + (country | issue), data= df)
h3b <- tidy(margins::margins(h3b, variables = "b",
                             at = list("a" = 0:4))) %>% 
  mutate(hyp = "H3b: Political Knowledge \n (0=Low, 6 = High)",
         type = "Pooled Analysis")

df <- df_us %>% 
  mutate(b = recode(masking,
                    `Masked` = 1,
                    `Party` = 0),
         a = PT7)
tmp <- lmer(stance ~ specification + b * a +
              distance + (1 | issue), data= df)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:3))) %>% 
  mutate(hyp = "H3b: Political Knowledge \n (0=Low, 6 = High)",
         type = "United States")

h3b <- h3b %>% 
  add_case(tmp)

df <- df_nl %>% 
  mutate(b = recode(masking,
                    `Masked` = 1,
                    `Party` = 0),
         a = PT7)
tmp <- lmer(stance ~ specification + b * a +
              distance + (1 | issue), data= df)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:4))) %>% 
  mutate(hyp = "H3b: Political Knowledge \n (0=Low, 6 = High)",
         type = "The Netherlands")

h3b <- h3b %>% 
  add_case(tmp)

df <- d_us %>% 
  mutate(b = specification,
         a = PT7b)
tmp <- lmer(stance ~ masking + b * a +
              distance + (1 | issue), data= df)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:5))) %>% 
  mutate(hyp = "Exploration: Bullshit Receptivity \n (0 = Low, 5 = High)",
         type = "United States")


p3 <- h3a  %>% 
  add_case(h3b) %>% 
  filter(type != "Pooled Analysis") %>% 
  ggplot(aes(x = `at.value`, y = estimate,
             ymin = (estimate - (1.65 * `std.error`)),
             ymax = (estimate + (1.65 * `std.error`)),
             color = type, fill = type)) +
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
  theme(legend.position = "none",
        legend.title = element_blank())

p3b <- h3a  %>% 
  add_case(h3b) %>%
  ggplot(aes(x = `at.value`, y = estimate,
             ymin = (estimate - (1.65 * `std.error`)),
             ymax = (estimate + (1.65 * `std.error`)),
             color = type, fill = type)) +
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
  theme(legend.position = "none",
        legend.title = element_blank())

p3c <- h3b  %>% 
  add_case(tmp) %>%
  filter(type== "United States") %>%
  mutate(hyp = factor(hyp,
                      levels = c("H3b: Political Knowledge \n (0=Low, 6 = High)",
                                 "Exploration: Bullshit Receptivity \n (0 = Low, 5 = High)"))) %>% 
  ggplot(aes(x = `at.value`, y = estimate,
             ymin = (estimate - (1.65 * `std.error`)),
             ymax = (estimate + (1.65 * `std.error`)),
             color = type, fill = type)) +
  geom_ribbon(alpha = 0.2, position = position_dodge(.2)) +
  geom_line(position = position_dodge(.2)) +
  geom_hline(yintercept = 0, color = "lightgray", size = .8,
             linetype = "dashed") +
  facet_grid(hyp~type, scales = "free") +
  labs(x = "",
       y = "Marginal Effect of Masking \n for Correctly Interpreting Stance") +
  theme_ipsum() +
  scale_colour_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  theme(legend.position = "none",
        legend.title = element_blank())

