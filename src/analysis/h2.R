df_us <- d_us %>% 
  select(interpretation, masking, specification, distance, PT7, country, issue)
df_nl <- d_nl %>% 
  select(interpretation, masking, specification, distance, PT7, country, issue)

d <- df_nl %>% 
  add_case(df_us)

df <- d %>% 
  mutate(b = specification,
         a = distance)
h2a <- lmer(interpretation ~ masking + b * a +
              PT7 + (country | issue), data= df)
h2a <- broom.mixed::tidy(margins::margins(h2a, variables = "b",
                             at = list("a" = 0:8.5))) %>% 
  mutate(hyp = "H2a: Ideological Distance \n(0=Low, 10 = High)",
         type = "Pooled Analysis")

df <- df_us %>% 
  mutate(b = specification,
         a = distance)
tmp <- lmer(interpretation ~ masking + b * a +
              PT7 + (1 | issue), data= df)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:5))) %>% 
  mutate(hyp = "H2a: Ideological Distance \n(0=Low, 10 = High)",
         type = "United States")

h2a <- h2a %>% 
  add_case(tmp)

df <- df_nl %>% 
  mutate(b = specification,
         a = distance)
tmp <- lmer(interpretation ~ masking + b * a +
              PT7 + (1 | issue), data= df)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:8.5))) %>% 
  mutate(hyp = "H2a: Ideological Distance \n(0=Low, 10 = High)",
         type = "The Netherlands")

h2a <- h2a %>% 
  add_case(tmp)

df <- d %>% 
  mutate(b = specification,
         a = PT7)
h2b <- lmer(interpretation ~ masking + 
              b * a + 
              distance + (country | issue), data= df)
h2b <- tidy(margins::margins(h2b, variables = "b",
                             at = list("a" = 0:4))) %>% 
  mutate(hyp = "H2b: Political Knowledge \n (0=Low, 6 = High)",
         type = "Pooled Analysis")

df <- df_us %>% 
  mutate(b = specification,
         a = PT7)
tmp <- lmer(interpretation ~ masking + b * a +
              distance + (1 | issue), data= df)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:3))) %>% 
  mutate(hyp = "H2b: Political Knowledge \n (0=Low, 6 = High)",
         type = "United States")

h2b <- h2b %>% 
  add_case(tmp)

df <- df_nl %>% 
  mutate(b = specification,
         a = PT7)
tmp <- lmer(interpretation ~ masking + b * a +
              distance + (1 | issue), data= df)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:4))) %>% 
  mutate(hyp = "H2b: Political Knowledge \n (0=Low, 6 = High)",
         type = "The Netherlands")

h2b <- h2b %>% 
  add_case(tmp)

df <- d_us %>% 
  mutate(b = specification,
         a = PT7b)
tmp <- lmer(interpretation ~ masking + b * a +
              distance + (1 | issue), data= df)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:5))) %>% 
  mutate(hyp = "Exploration: Bullshit Receptivity \n (0 = Low, 5 = High)",
         type = "United States")

p2 <- h2a  %>% 
  add_case(h2b) %>%
  filter(type != "Pooled Analysis") %>% 
  ggplot(aes(x = `at.value`, y = estimate,
             ymin = (estimate - (1.65 * `std.error`)),
             ymax = (estimate + (1.65 * `std.error`)),
         color = type, fill = type)) +
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

p2b <- h2a  %>% 
  add_case(h2b) %>%
  ggplot(aes(x = `at.value`, y = estimate,
             ymin = (estimate - (1.65 * `std.error`)),
             ymax = (estimate + (1.65 * `std.error`)),
             color = type, fill = type)) +
  geom_ribbon(alpha = 0.2, position = position_dodge(.2)) +
  geom_line(position = position_dodge(.2)) +
  facet_grid(hyp~type, scales = "free") +
  labs(x = "",
       y = "Marginal Effect of Sentence Underspecification \n for Overinterpreting Stance") +
  theme_ipsum() +
  scale_colour_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  theme(legend.position = "none",
        legend.title = element_blank())

p2c <- h2b  %>% 
  add_case(tmp) %>%
  filter(type== "United States") %>%
  mutate(hyp = factor(hyp,
                      levels = c("H2b: Political Knowledge \n (0=Low, 6 = High)",
                                 "Exploration: Bullshit Receptivity \n (0 = Low, 5 = High)"))) %>% 
  ggplot(aes(x = `at.value`, y = estimate,
             ymin = (estimate - (1.65 * `std.error`)),
             ymax = (estimate + (1.65 * `std.error`)),
             color = type, fill = type)) +
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
