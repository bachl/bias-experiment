#library(haven)
#library(tidyverse)
#library(here)
#library(qualtRics)

# Load raw data

#read_file(here("data/raw-private/qualtrics_api_key.txt")) |>
#  trimws() |>
#  qualtrics_api_credentials("fra1.qualtrics.com")
#d_us_raw <- fetch_survey(surveyID = "SV_86XBcsFUJka9eke", 
#                         verbose = TRUE, force_request = T,
#                         label = FALSE, convert = FALSE)
#write_rds(d_us_raw, here("data/raw-private/d_us.rds"))

#d_nl_raw <- read_sav(here("data/raw-private/NLOZ2206_Mariken_Bias-experiment.sav"))
#d_us_raw <- read_rds(here("data/raw-private/d_us.rds"))


# Clean and select NL survey columns


d_nl <- d_nl |>
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
         Attention1_1:Attention1_9) |>
  mutate(D2 = recode(D2, `1` = "Male",
                     `2` = "Female"),
         D3 = recode(D3, `1` = "Low-level of education",
                     `2` = "Mid-levels of education",
                     `3` = "High-levels of education"),
         D4 = recode(D4, `1` = "Big cities",
                     `2` = "West of Netherlands",
                     `3` = "North of Netherlands",
                     `4` = "East of Netherlands",
                     `5` = "South of Netherlands"),
         D5 = na_if(D5, 13),
         D5 = na_if(D5, 14),
         D6 = recode(D6, `1` = "Fulltime employed",                            
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
         PT6 = PT6 / 10,
         PT7_1 = ifelse(PT7_1_3==1 & PT7_1_4==1 & PT7_1_5==1 & PT7_1_16==1, 1,0),
         PT7_1 = replace_na(PT7_1, 0),
         PT7_2 = ifelse(PT7_2 == 1, 1, 0),
         PT7_3 = ifelse(PT7_3 == 5, 1, 0),
         PT7_4 = ifelse(PT7_4 == 3, 1, 0),
         PT7_5 = ifelse(PT7_5 == 3, 1, 0),
         PT7_6 = ifelse(PT7_6 == 2, 1, 0),
         PT7 = (PT7_1 + PT7_2 + PT7_3 +PT7_4 + PT7_5 +PT7_6), 
         PT7 = PT7/4) |>
  select(-PT7_1_3:-PT7_6) |>
  add_column(country="NL", .after=1)

# Clean and select US survey columns
d_us <- d_us  |>  
  filter(consent == 1,
         screener == 2)  |>  
  unite("PT1b", matches("PT1b"),na.rm = TRUE)  |>  
  select(id = PID,
         gender = D1, #gender (3 categories)
         age = D2,  #age - continues
         state = D3,#state, 
         income = D5, #income: no income (1), <500 (2), 501-1000 (3), 1001-1500 (4); 1501-2000 (5); 2001-2500 (6); 2501-3000 (7); 3001-3500 (8); 3501-4000 (9); 4001-4500 (10); 4501-7500 (11); >7501 (12)
         job = D6, #job
         education = D7, #education
         PT1 = PT1a, #pid
         PT1b, #pid strength
         PT2, #issue position: migration: 1 = neg, 5 = pos
         PT3, #issue position: climate: 1 = neg, 5 = pos
         PT4, #issue position: tax: 1 = neg, 5 = pos
         PT5, #issue position: foreign policy: 1 = neg, 5 = pos
         PT6, #libcon -self: 0 = lib, 14 = cons
         PT7_1, #senator years (polknow)
         PT7_2, #majority in house (polknow)
         PT7_3, #majority in senate (polknow)
         PT7_4_1, PT7_4_2,  PT7_4_3, PT7_4_4, #bullshit receptivity
         actor1:actor4,
         issue1:issue4,
         specified1:specified4,
         S1:S4,
         Attention1_1:Attention1_9) %>% 
  select(id,
         D1 = age,
         D2 = gender,
         D3 = education,
         D4 = state,
         D5 = income,
         D6 = job,
         PT1:Attention1_9) %>% 
  mutate(D2 = recode(D2,
                     `1` = "Male",
                     `2` = "Female",
                     .missing = "NA", .default = "NA"),
         D3 = recode(D3,
                     `1` = "Low-level of education",
                     `9` = "Low-level of education",
                     `10` = "Low-level of education",
                     `11` = "Low-level of education",
                     `12` = "Low-level of education",
                     `13` = "Low-level of education",
                     `14` = "Low-level of education",
                     `15` = "Low-level of education",
                     `16` = "Mid-level of education",
                     `17` = "Mid-level of education",
                     `18` = "Mid-level of education",
                     `19` = "Mid-level of education",
                     `20` = "High-level of education",
                     `21` = "High-level of education",
                     `22` = "High-level of education",
                     `23` = "High-level of education",
                     .missing = "NA", .default = "NA"),
         D4 = recode(D4,
                     `1` = "Southeast",
                     `2` = "West",
                     `3` = "Southwest",
                     `4` = "Southeast",
                     `5` = "West",
                     `6` = "West",
                     `7` = "Northeast",
                     `8` = "Northeast",
                     `9` = "Northeast",
                     `10` = "Southeast",
                     `11` = "Southeast",
                     `12` = "West",
                     `13` = "West",
                     `14` = "Midwest",
                     `15` = "Midwest",
                     `16` = "Midwest",
                     `17` = "Midwest",
                     `18` = "Southeast",
                     `19` = "Southeast",
                     `20` = "Northeast",
                     `21` = "Northeast",
                     `22` = "Northeast",
                     `23` = "Midwest",
                     `24` = "Midwest",
                     `25` = "Southeast",
                     `26` = "Midwest",
                     `27` = "West",
                     `28` = "Midwest",
                     `29` = "West",
                     `30` = "Northeast",
                     `31` = "Northeast",
                     `32` = "Southwest",
                     `33` = "Northeast",
                     `34` = "Southeast",
                     `35` = "Midwest",
                     `36` = "Midwest",
                     `37` = "Southwest",
                     `38` = "West",
                     `39` = "Northeast",
                     `40` = "Southeast",
                     `41` = "Northeast",
                     `42` = "Southeast",
                     `43` = "Midwest",
                     `44` = "Southeast",
                     `45` = "Southwest",
                     `46` = "West",
                     `47` = "Northeast",
                     `48` = "Southwest",
                     `49` = "West",
                     `50` = "Southwest",
                     `51` = "Midwest",
                     `52` = "West",
                     .missing = "NA"),
         D5 = na_if(D5, 15),
         D5 = na_if(D5, 16),
         D6 = recode(D6,
                     `1` = "Working now",                            
                     `11` = "Temporarily laid off",                              
                     `12` = "Unemployed",           
                     `13` = "Retired",                        
                     `14` = "Permanently disable",
                     `15` = "Homemaker",                    
                     `17` = "Student",
                     .missing = "NA", .default = "NA"),
         PT1 = recode(PT1,
                      `1` = "Democrat",
                      `2` = "Republican",                        
                      `3` = "Independent",
                      `4` = "Something else",    
                      .missing = "NA", .default = "NA"),
         PT1b = recode(PT1b,
                       `1` = "Strong",
                       `2` = "Not very strong",     
                       .missing = "NA", .default = "NA"),
         PT6 = na_if(PT6, 15),
         PT6 = recode(PT6,
                      `0` = 0,
                      `1`= 1,
                      `2` = 2,
                      `11` = 3,
                      `12` = 4,
                      `13` = 5,
                      `14` = 6),
         PT6 = PT6 / 6,
         PT7_1 = ifelse(PT7_1==6, 1,0),
         PT7_2 = ifelse(PT7_2 == 1, 1, 0),
         PT7_3 = ifelse(PT7_3 == 2, 1, 0),
         PT7 = (PT7_1 + PT7_2 + PT7_3)/3,
         PT7b = ((PT7_4_1 + PT7_4_2 + PT7_4_3 + PT7_4_4)/4)) |>
  select(-PT7_1:-PT7_4_4) |>
  filter(!is.na(specified1)) |>
  add_column(country="US", .after = 1)

# Combine NL and US data and compute attention



# Code experimental conditions
conditions = tribble(
  ~specified, ~gold, ~party_rile,~sentence, 
  "Specified",      "anti", 1, "zegt dat immigratie moeilijker gemaakt moet worden.", 
  "Underspecified", "anti", 1, "zegt dat veel immigranten deze kant op komen.", 
  "Specified",      "pro",  0,  "zegt dat stikstofuitstoot meer tegengegaan moet worden.", 
  "Underspecified", "anti", 0,  "zegt dat het stikstofbeleid anders moet.",
  "Specified",      "pro",  0,  "zegt dat het belastingtarief voor de hoogste inkomens omhoog moet.",
  "Underspecified", "anti", 0,  "zegt dat het belastingstelsel moet worden aangepast.",
  "Specified",      "anti", 1, "zegt dat het lidmaatschap van de Europese Unie tot nu toe vooral slecht geweest voor Nederland is.", 
  "Underspecified", "anti", 1, "zegt dat Nederland een andere rol in de Europese Unie moet hebben.", 
  
  "Specified",      "anti", 1, "say immigration should be made more difficult.",
  "Underspecified", "anti", 1, "say many immigrants are crossing our borders.",
  "Specified",      "pro",  0,  "say we need to put a tax on carbon emissions",
  "Underspecified", "pro",  0,  "say carbon emissions policy should be implemented differently.",
  "Specified",      "pro",  0,  "say we should implement a wealth tax for the richest Americans.",
  "Underspecified", "anti", 0,  "say the tax system should be implemented differently.",
  "Specified",      "pro",  1, "say the U.S. needs to consider military build-up in the Pacific Ocean",
  "Underspecified", "anti", 1, "say there should be a different military presence in the Pacific Ocean.",
)  

# Interpret sentences
# Both US and NL have the same coding for the 4 rounds:
# actor1, specified1, issue1, S1 .... actor4 etc
# Make a 'pivot_spec' to pivot this in one operation
pivot_spec <- map(c("actor", "specified", "issue", "S"), 
                  ~tibble(.name=str_c(., 1:4), .value=., round=1:4)) |> list_rbind()

d <- bind_rows(d_nl, d_us) |>
  mutate(across(Attention1_1:Attention1_9, ~recode(., .missing = 0, .default = 1)),
         attention_sum = rowSums(across(Attention1_1:Attention1_9)),
         attention = ifelse(Attention1_3==1 & Attention1_6==1 & attention_sum == 2, 1, 0)) |>
  pivot_longer_spec(pivot_spec) |>
  rename(sentence=specified,
         stance=S) |>
  mutate(stance=case_match(stance,
                           1 ~ "pro",
                           2 ~ "anti",
                           3 ~ "neutral",
                           6 ~ "dontknow"
  )) |>
  left_join(conditions) |>
  mutate(
    stance_nlp=case_when(
      stance == "dontknow" ~ NA,   # incorrect?? --> runnenn met en zonder
      specified == "Underspecified" & stance == "neutral" ~ "correct",
      specified == "Specified" & stance == gold ~ "correct",
      T ~ "incorrect"),
    stance_ss=case_when(
      stance == "dontknow" ~ NA,
      specified == "Underspecified" & stance %in% c(gold, "neutral") ~ "correct",
      specified == "Specified" & stance == gold ~ "correct",
      T ~ "incorrect"),
    interpret_nlp = ifelse(stance=="neutral", "Correct" ,"Overinterpret"),
    interpret_ss = case_when(
      stance == "dontknow" ~ NA,
      specified == "Underspecified" & stance %in% c("pro", "anti") ~ "Overinterpret",
      T ~ "Correct"
    ),
    distance=abs(PT6-party_rile),
    masking = ifelse(actor == "X", "1", "0"),
    stance_nlp2 = ifelse(stance_nlp == "correct", 1, 0),
    stance_ss2 = ifelse(stance_ss == "correct", 1, 0),
    interpret_nlp2  = ifelse(interpret_nlp == "Correct", 0, 1),
    interpret_ss2  = ifelse(interpret_ss == "Correct", 0, 1)
  ) |> 
  select(-PT7_1, -PT1b)

