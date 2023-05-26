df_us <- d_us %>% 
  select(stance, masking, specification, distance, PT7, country, issue)
df_nl <- d_nl %>% 
  select(stance, masking, specification, distance, PT7, country, issue)

d <- df_nl %>% 
  add_case(df_us)

h1a <- broom.mixed::tidy(lmer(stance ~ masking + specification + 
              distance + PT7 +
              (country | issue), data= d)) %>% 
  mutate(hyp = "H1a",
         type = "Pooled Analysis") %>% 
  filter(term %in% c("distance"))

tmp <- broom.mixed::tidy(lmer(stance ~ masking + specification + 
                                distance + PT7 +
                                (1 | issue), data= df_us)) %>% 
  mutate(hyp = "H1a",
         type = "United States") %>% 
  filter(term %in% c("distance"))

h1a <- h1a %>% 
  add_case(tmp)

tmp <- broom.mixed::tidy(lmer(stance ~ masking + specification + 
                                distance + PT7 +
                                (1 | issue), data= df_nl)) %>% 
  mutate(hyp = "H1a",
         type = "the Netherlands") %>% 
  filter(term %in% c("distance"))

h1a <- h1a %>% 
  add_case(tmp)
rm(tmp)

h1b <- broom.mixed::tidy(lmer(stance ~ masking + 
                                specification * distance +
                                PT7 + (country | issue), data= d)) %>% 
  mutate(hyp = "H1b",
         type = "Pooled Analysis") %>% 
  filter(term %in% c("specificationUnderspecified",
                     "distance",
                     "specificationUnderspecified:distance"))

tmp <- broom.mixed::tidy(lmer(stance ~ masking + 
                                specification * distance +
                                PT7 + (1 | issue), data= df_us)) %>% 
  mutate(hyp = "H1b",
         type = "United States") %>% 
  filter(term %in% c("specificationUnderspecified",
                     "distance",
                     "specificationUnderspecified:distance"))

h1b <- h1b %>% 
  add_case(tmp)

tmp <- broom.mixed::tidy(lmer(stance ~ masking + 
                                specification * distance +
                                PT7 + (1 | issue), data= df_nl)) %>% 
  mutate(hyp = "H1b",
         type = "the Netherlands") %>% 
  filter(term %in% c("specificationUnderspecified",
                     "distance",
                     "specificationUnderspecified:distance"))

h1b <- h1b %>% 
  add_case(tmp)
rm(tmp)

p1 <- h1a %>% 
  add_case(h1b) %>% 
  filter(type != "Pooled Analysis") %>% 
  mutate(term = recode(term,
                       `maskingParty` = "Condition: Political Actor Revealed",
                       `specificationUnderspecified` = "Condition: Underspecified Sentence",
                       `distance` = "Ideological Distance from Party",
                       `specificationUnderspecified:distance` = "Interaction: Underspecfied * Distance",
                       `factor(country)United States` = "Country: United States"),
         term = factor(term,
                       levels = c("Country: United States",
                                  "Interaction: Underspecfied * Distance",
                                  "Ideological Distance from Party",
                                  "Condition: Underspecified Sentence",
                                  "Condition: Political Actor Revealed")), 
         lower = estimate - 1.56 * std.error,
         upper = estimate + 1.56 * std.error) %>% 
    ggplot(aes(x = estimate, y = term,
             xmin = lower, xmax = upper,
             color = type)) +
  geom_point(position = position_dodge(0.5)) +
  geom_errorbar(width = 0,
                position = position_dodge(0.5)) +
  facet_grid(hyp~., scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray25") +
  labs(x = "Predicted Effect for Interpreting Stance Correctly", y = "") +
  theme_ipsum() +
  scale_colour_manual(values = fig_cols) +
  theme(legend.position = "bottom",
        legend.title = element_blank())

p1_2 <- h1a %>% 
  add_case(h1b) %>% 
  mutate(term = recode(term,
                       `maskingParty` = "Condition: Political Actor Revealed",
                       `specificationUnderspecified` = "Condition: Underspecified Sentence",
                       `distance` = "Ideological Distance from Party",
                       `specificationUnderspecified:distance` = "Interaction: Underspecfied * Distance",
                       `factor(country)United States` = "Country: United States"),
         term = factor(term,
                       levels = c("Country: United States",
                                  "Interaction: Underspecfied * Distance",
                                  "Ideological Distance from Party",
                                  "Condition: Underspecified Sentence",
                                  "Condition: Political Actor Revealed")), 
         lower = estimate - 1.56 * std.error,
         upper = estimate + 1.56 * std.error) %>% 
  ggplot(aes(x = estimate, y = term,
             xmin = lower, xmax = upper,
             color = type)) +
  geom_point(position = position_dodge(0.5)) +
  geom_errorbar(width = 0,
                position = position_dodge(0.5)) +
  facet_grid(hyp~., scales = "free") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray25", size = .8) +
  labs(x = "Predicted Effect for Interpreting Stance Correctly", y = "") +
  theme_ipsum() +
  scale_colour_manual(values = fig_cols) +
  theme(legend.position = "bottom",
        legend.title = element_blank())

## Marginal Effects
df <- d %>% 
  mutate(b = specification,
         a = distance)
h1b <- lmer(stance ~ masking +
              b * a + 
              PT7 +
              (country | issue), data= df)
h1b <- tidy(margins::margins(h1b, variables = "b",
                             at = list("a" = 0:8.5))) %>% 
  mutate(type = "Pooled Analysis")

df <- df_nl %>% 
  mutate(b = specification,
         a = distance)
tmp <- lmer(stance ~ masking +
              b * a + 
              PT7 + (1 | issue), data= df)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:8.5))) %>% 
  mutate(type = "The Netherlands")

h1b <- h1b %>% 
  add_case(tmp)

df <- df_us %>% 
  mutate(b = specification,
         a = distance)
tmp <- lmer(stance ~ masking +
              b * a + 
              PT7 + (1 | issue), data= df)
tmp <- tidy(margins::margins(tmp, variables = "b",
                             at = list("a" = 0:3))) %>% 
  mutate(type = "United States")

h1b <- h1b %>% 
  add_case(tmp)

p1b <- h1b %>% 
  filter(type != "Pooled Analysis") %>% 
  ggplot(aes(x = `at.value`, y = estimate,
             ymin = (estimate - (1.65 * `std.error`)),
             ymax = (estimate + (1.65 * `std.error`)),
             color = type, fill = type)) +
  geom_ribbon(alpha = 0.2,
              position = position_dodge(.5)) +
  geom_line(position = position_dodge(.5)) +
  #ylim(c(-0.4, -0.3)) +
  #xlim(c(1,8)) +
  labs(x = "Ideological Distance",
       y = "Marginal Effect of Sentence Underspecification \n for Correctly Interpreting Stance") +
  theme_ipsum() +
  facet_grid(.~type, scales = "free") +
  scale_colour_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  theme(legend.position = "bottom",
        legend.title = element_blank())

p1b2 <- h1b %>% 
  ggplot(aes(x = `at.value`, y = estimate,
             ymin = (estimate - (1.65 * `std.error`)),
             ymax = (estimate + (1.65 * `std.error`)),
             color = type, fill = type)) +
  geom_ribbon(alpha = 0.2,
              position = position_dodge(.5)) +
  geom_line(position = position_dodge(.5)) +
  facet_grid(.~type, scales = "free") +
  labs(x = "Ideological Distance",
       y = "Marginal Effect of Sentence Underspecification \n for Correctly Interpreting Stance") +
  theme_ipsum() +
  scale_colour_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  theme(legend.position = "none",
        legend.title = element_blank())


