d_nl <- d_nl %>% 
  zap_labels() %>% 
  filter(age_cont>17) %>% 
  select(id = ResponseId,
         D1 = age_cont, #age - continues
         D2 = sex_rec, #gender (2 categories)
         D3 = edu_3_rec, #education 3 categories
         D4 = geo_nielsen5_rec, #location (Nielen V)
         D5, #income: no income (1), <500 (2), 501-1000 (3), 1001-1500 (4); 1501-2000 (5); 2001-2500 (6); 2501-3000 (7); 3001-3500 (8); 3501-4000 (9); 4001-4500 (10); 4501-7500 (11); >7501 (12)
         D6, #job
         PT1, #vote recall
         PT2, #issue position: migration: 1 = neg, 5 = pos
         PT3, #issue position: climate: 1 = neg, 5 = pos
         PT4, #issue position: tax: 1 = neg, 5 = pos
         PT5, #issue position: EU: 1 = neg, 5 = pos
         PT6, #rile -self: 0 = left, 10 = right
         PT7_1_3, PT7_1_4, PT7_1_5, PT7_1_16, #coalition partners (polknow)
         PT7_2, #minister finance (polknow)
         PT7_3, #2nd largest party (polknow)
         PT7_4, #electing 1st chamber (polknow)
         PT7_5, #who makes laws (polknow)
         PT7_6, #coonstitutional court (polknow)
         actor1:actor4,
         issue1:issue4,
         specified1:specified4,
         S1:S4,
         Attention1_1:Attention1_9) %>% 
  mutate_at(vars(Attention1_1:Attention1_9), 
            funs(recode(., .missing = 0, .default = 1))) %>% 
  rowwise() %>%
  mutate(tmp = sum(c_across(starts_with("Attention1_")), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(D2 = recode(D2,
                         `1` = "Male",
                         `2` = "Female"),
         D3 = recode(D3,
                            `1` = "Low-level of education",
                            `2` = "Mid-levels of education",
                            `3` = "High-levels of education"),
         D4 = recode(D4,
                           `1` = "Big cities",
                           `2` = "West of Netherlands",
                           `3` = "North of Netherlands",
                           `4` = "East of Netherlands",
                           `5` = "South of Netherlands"),
         D5 = na_if(D5, 13),
         D5 = na_if(D5, 14),
         D6 = recode(D6,
                     `1` = "Fulltime employed",                            
                     `2` = "Parttime employed",                              
                     `3` = "Independent worker",           
                     `4` = "Unemployed and searching",                        
                     `5` = "Unemployed",
                     `6` = "Stay at home",                    
                     `7` = "Retired",                           
                     `8` = "Student",
                     .missing = "NA", .default = "NA"),
         PT1 = recode(PT1,
                      `1` = "VVD",
                      `2` = "D66",                        
                      `3` = "PVV",
                      `4` = "CDA",                        
                      `5` = "SP",
                      `6` = "PvdA",                       
                      `7` =  "GroenLinks",
                      `8` = "Forum voor Democratie",      
                      `9` =  "Partij voor de Dieren",
                      `10` = "ChristenUnie",               
                      `11` = "JA21" ,
                      `12` = "SGP",                        
                      `13` = "Volt",
                      `14` = "Denk" ,                      
                      `15` =  "50Plus Partij",
                      `16` = "BoerBurgerBeweging" ,        
                      `17` =  "Bij1",
                      `18` = "Other party",
                      `19` =  "Blanco" ,
                      `20` = "Not eligible",      
                      .missing = "NA", .default = "NA"),
         PT7_1 = ifelse(PT7_1_3==1 & PT7_1_4==1 & PT7_1_5==1 & PT7_1_16==1, 1,0),
         PT7_2 = ifelse(PT7_2 == 1, 1, 0),
         PT7_3 = ifelse(PT7_3 == 5, 1, 0),
         PT7_4 = ifelse(PT7_4 == 3, 1, 0),
         PT7_5 = ifelse(PT7_5 == 3, 1, 0),
         PT7_6 = ifelse(PT7_6 == 2, 1, 0),
         PT7 = (PT7_1 + PT7_2 + PT7_3 +PT7_4 + PT7_5 +PT7_6),
         attention = ifelse(Attention1_3==1 & Attention1_6==1 & tmp ==2, 1, 0),
         issue1 = "Immigration",
         issue2 = "Environment",
         issue3 = "Tax",
         issue4 = "EU",
         specified1 = recode(specified1,
                             `zegt dat immigratie moeilijker gemaakt moet worden.` = "Specified",
                             `zegt dat veel immigranten deze kant op komen.` = "Underspecified"),
         specified2 = recode(specified2,
                             `zegt dat het stikstofbeleid anders moet.` = "Underspecified",
                             `zegt dat stikstofuitstoot meer tegengegaan moet worden.` = "Specified"),
         specified3 = recode(specified3,
                             `zegt dat het belastingstelsel moet worden aangepast.` = "Underspecified",
                             `zegt dat het belastingtarief voor de hoogste inkomens omhoog moet.` = "Specified"),
         specified4 = recode(specified4,
                             `zegt dat het lidmaatschap van de Europese Unie tot nu toe vooral slecht geweest voor Nederland is.` = "Specified",
                             `zegt dat Nederland een andere rol in de Europese Unie moet hebben.` = "Underspecified"),
         stance1_nlp = ifelse(specified1 == "Specified" & S1 == 2, 
                          "correct", "not correct"),
         stance1_nlp = ifelse(specified1 == "Underspecified" & S1 == 3, 
                          "correct", stance1_nlp),
         stance1_ss = ifelse(specified1 == "Specified" & S1 == 2, 
                              "correct", "not correct"),
         stance1_ss = ifelse(specified1 == "Underspecified" & S1 == 3, 
                              "correct", stance1_ss),
         stance2_nlp = ifelse(specified2 == "Specified" & S2 == 1, 
                              "correct", "not correct"),
         stance2_nlp = ifelse(specified2 == "Underspecified" & S2 == 3, 
                              "correct", stance2_nlp),
         stance2_ss = ifelse(specified2 == "Specified" & S2 == 1, 
                          "correct", "not correct"),
         stance2_ss = ifelse(specified2 == "Underspecified" & S2 == 2, 
                             "correct", stance2_ss),
         stance2_ss= ifelse(specified2 == "Underspecified" & S2 == 3,
                          "correct", stance2_ss),
         stance3_nlp = ifelse(specified3 == "Specified" & S3 == 1,
                          "correct", "not correct"),
         stance3_nlp = ifelse(specified3 == "Underspecified" & S3 == 3, 
                          "correct", stance3_nlp),
         stance3_ss = ifelse(specified3 == "Specified" & S3 == 1,
                              "correct", "not correct"),
         stance3_ss = ifelse(specified3 == "Underspecified" & S3 == 2, 
                             "correct", stance3_ss),
         stance3_ss = ifelse(specified3 == "Underspecified" & S3 == 3, 
                              "correct", stance3_ss),
         stance4_nlp = ifelse(specified4 == "Specified" & S4 == 2, 
                          "correct", "not correct"),
         stance4_nlp = ifelse(specified4 == "Underspecified" & S4 == 3,
                          "correct", stance4_nlp),
         stance4_ss = ifelse(specified4 == "Specified" & S4 == 2, 
                              "correct", "not correct"),
         stance4_ss = ifelse(specified4 == "Underspecified" & S4 == 2,
                             "correct", stance4_ss),
         stance4_ss = ifelse(specified4 == "Underspecified" & S4 == 3,
                              "correct", stance4_ss),
         interpret1 = ifelse(specified1 == "Underspecified" & S1 == 1, 
                            "Stance", "No Stance"),
         interpret1 = ifelse(specified1 == "Underspecified" & S1 == 2, 
                                   "Stance", interpret1),
         interpret2 = ifelse(specified2 == "Underspecified" & S2 == 1, 
                             "Stance", "No Stance"),
         interpret2 = ifelse(specified2 == "Underspecified" & S2 == 2, 
                             "Stance", interpret2),
         interpret3 = ifelse(specified3 == "Underspecified" & S3 == 1, 
                             "Stance", "No Stance"),
         interpret3 = ifelse(specified3 == "Underspecified" & S3 == 2, 
                             "Stance", interpret3),
         interpret4 = ifelse(specified4 == "Underspecified" & S4 == 1, 
                             "Stance", "No Stance"),
         interpret4 = ifelse(specified4 == "Underspecified" & S4 == 2, 
                             "Stance", interpret4)) %>% 
  select(id, D1:D6, PT1:PT6, PT7, attention,
         actor1:actor4,
         issue1:issue4,
         specified1:specified4, 
         stance1_nlp:stance4_nlp,
         stance1_ss:stance4_ss,
         interpret1:interpret4) %>% 
  mutate(round1 = paste(actor1, issue1, specified1, stance1_nlp, stance1_ss, interpret1, sep = "-"),
         round2 = paste(actor2, issue2, specified2, stance2_nlp, stance2_ss, interpret2, sep = "-"),
         round3 = paste(actor3, issue3, specified3, stance3_nlp, stance3_ss, interpret3, sep = "-"),
         round4 = paste(actor4, issue4, specified4, stance4_nlp, stance4_ss, interpret4, sep = "-")) %>% 
  pivot_longer(cols = round1:round4,
               values_to = "condition",
               names_to = "round") %>% 
  separate(condition, c("actor", "issue", "specification", "stance_nlp", "stance_ss", "interpretation"), "-") %>% 
  select(id, D1:PT7, attention,
         round, actor, issue, specification, stance_nlp, stance_ss, interpretation) %>% 
  mutate(masking = ifelse(actor == "X", "Masked", "Party"),
         stance_nlp = ifelse(stance_nlp =="correct", 1 ,0),
         stance_ss = ifelse(stance_ss =="correct", 1 ,0),
         interpretation_nlp = ifelse(interpretation=="No Stance", 0 ,1),
         interpretation_ss = ifelse(interpretation=="No Stance" & issue =="Immigration" , 0 ,1),
         interpretation_ss = ifelse(interpretation=="Stance" & issue !="Immigration" , 0 ,interpretation_ss),
         distance = 0,
         distance = ifelse(round == "round1", abs(PT6-1.5), distance),
         distance = ifelse(round == "round2", abs(PT6-3), distance),
         distance = ifelse(round == "round3", abs(PT6-3.5), distance),
         distance = ifelse(round == "round4", abs(PT6-7), distance)) %>% 
  select(-round, -actor) %>% 
  filter(attention==1)


# Recode missing
d_nl <- d_nl %>% 
  mutate(D2 = replace_na(D2, "Male"),
         D3 = replace_na(D3, "High-levels of education"),
         D4 = replace_na(D4, "West of Netherlands"),
         missing_D5 = ifelse(is.na(D5),1,0),
         D5 = replace_na(D5, round(mean(D5, na.rm=T),0)),
         missing_D6 = ifelse(is.na(D6),1,0),
         D6 = replace_na(D6, "Fulltime employed"),
         PT1 = replace_na(PT1, "D66"),
         PT7 = replace_na(PT7, 0)) %>% 
  mutate(country = "The Netherlands")
          


