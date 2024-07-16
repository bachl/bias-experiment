d |> group_by(country, round, specified, sentence, stance) |>
  summarize(PT6=mean(PT6, na.rm=T)) |>
  ggplot(aes(x=stance, y=sentence, label=round(PT6, 1), fill=PT6)) + geom_tile() + geom_text()

actors = d |> select(sentence, actor)  |> unique() |> filter(actor != "X")


d2 <- d |> 
  mutate(mask=if_else(actor == "X", "masked", "notmasked")) |>
  filter(!is.na(stance), stance != "dontknow") |>
  group_by(country, round, specified, sentence, gold, mask, stance) |>
  summarize(m=mean(PT6, na.rm=T), n=n(), sd=sd(PT6, na.rm=T)) |>
  inner_join(actors) |>
  mutate(p=n/sum(n)) |>
  mutate(sentence=if_else(!str_detect(sentence, "Euro"), sentence,
                          "zegt dat [de EU] slecht geweest voor Nederland is."
                          )) |>
  mutate(sentence=glue::glue("[{actor}/X] {sentence} ")) |>
  mutate(sentence = fct_reorder(sentence, round))
  

ggplot(d2, aes(y=sentence, x=m)) + 
  geom_point(aes(size=p, color=stance), alpha=.6) +
  geom_text(data=filter(d2, stance==gold), aes(label=round(p*100, 0)), size=3)+
  ggh4x::facet_nested(specified + country ~ mask , scales="free_y")+ 
  theme_minimal() + 
  scale_size(guide="none") + 
  theme(legend.position="bottom") + 
  xlab("Self-reported Left-Right") + 
  ylab("") +
  theme(legend.title = element_blank(), 
        strip.background = element_rect(color = "black", size = .5))

# With +/- 1 SD 'error' bars
ggplot(d2, aes(y=sentence, x=m)) + 
  geom_segment(aes(x=m-sd, xend=m+sd, color=stance), position = position_dodge(width=.5)) + 
  geom_point(aes(size=p, color=stance), alpha=.6, position = position_dodge(width=.5)) +
  geom_text(data=filter(d2, stance=="pro", gold=="pro"), aes(label=round(p*100, 0)), size=3, nudge_y = .15)+
  geom_text(data=filter(d2, stance=="anti", gold=="anti"), aes(label=round(p*100, 0)), size=3, nudge_y = -.15)+
  ggh4x::facet_nested(specified + country ~ mask , scales="free_y")+ 
  theme_minimal() + 
  scale_size(guide="none") + 
  theme(legend.position="bottom") + 
  xlab("Self-reported Left-Right") + 
  ylab("") +
  theme(legend.title = element_blank(), 
        strip.background = element_rect(color = "black", size = .5))
