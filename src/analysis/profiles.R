# |> group_by(country, round, specified, sentence, stance) |>
#  summarize(PT6=mean(PT6, na.rm=T)) |>
#  ggplot(aes(x=stance, y=sentence, label=round(PT6, 1), fill=PT6)) + geom_tile() + geom_text()


d2 <- d |> 
  mutate(mask=if_else(actor == "X", "Masked", "Party")) |>
  filter(!is.na(stance), stance != "dontknow") |>
  group_by(country, round, specified, sentence, gold, mask, stance) |>
  summarize(PT6=mean(PT6, na.rm=T), n=n()) |>
  mutate(p=n/sum(n)) |>
  mutate(sentence=glue::glue("[{country}{round} {gold}] {sentence}")) 
#pivot_wider(names_from="stance", values_from=PT6) |>
prof1 <- ggplot(d2, aes(y=sentence, x=PT6)) + 
  geom_point(aes(size=p, color=stance), alpha=.6) +
  geom_text(data=filter(d2, p>.43), aes(label=round(p*100, 0)), size=3)+
  facet_grid(rows=vars(specified), cols=vars(mask), scales = "free_y") + 
  theme_ipsum() + 
  scale_colour_manual(values = fig_cols[c(2,1,3)]) +
  theme(legend.position="none",
        legend.title = element_blank()) + 
  labs(y = "", x ="Ideological Position")

d3 <- d |> 
  mutate(mask=if_else(actor == "X", "Masked", "Party")) |>
  filter(!is.na(stance), stance != "dontknow") |>
  group_by(country, round, specified, sentence, gold, mask, stance) |>
  summarize(PT7=mean(PT7, na.rm=T), n=n()) |>
  mutate(p=n/sum(n)) |>
  mutate(sentence=glue::glue("[{country}{round} {gold}] {sentence}")) 


prof2 <- ggplot(d3, aes(y=sentence, x=PT7)) + 
  geom_point(aes(size=p, color=stance), alpha=.6) +
  geom_text(data=filter(d3, p>.49), aes(label=round(p*100, 0)), size=3)+
  facet_grid(rows=vars(specified), cols=vars(mask), scales = "free_y") + 
  theme_ipsum() + 
  scale_colour_manual(values = fig_cols[c(2,1,3)]) +
  theme(legend.position="none",
        legend.title = element_blank()) + 
  labs(y = "", x ="Political Knowledge")


#ggridges::geom_density_ridges(aes(x=PT6, y=sentence, fill=stance), alpha=.5, bandwidth=.15) +
#  facet_grid(rows=vars(specified),scales = "free") 
#facet_grid(rows=vars(specified), cols=vars(stance), scales = "free") 


prof3 <- d |> 
  filter(!is.na(stance), stance != "dontknow") |>
  mutate(stance=if_else(stance == "neutral", "neutral", "stance")) |>
  group_by(country, round, specified, sentence, stance) |>
  summarize(PT6=mean(PT6, na.rm=T)) |>
  pivot_wider(names_from="stance", values_from=PT6) |>
  ggplot(aes(y=sentence, yend=sentence)) + 
  geom_segment(aes(x=neutral, xend=stance)) +
  geom_point(aes(x=neutral), color="blue") +
  geom_point(aes(x=stance), color="red") 

