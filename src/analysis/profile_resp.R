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

e1 <- d %>% 
  group_by(stance_nlp) %>% 
  summarise(Age = round(mean(D1),0),
            Gender= "Male",
            Education = "High-levels of education",
            Location = "West of Netherlands",
            Income = round(mean(D5),0),
            Job = "Fulltime Employed",
            `Vote Recall` = "D66",
            `Position on Immigration` = round(mean(PT2),0),
            `Position on Environment` = round(mean(PT3),0),
            `Position on Tax` = round(mean(PT4),0),
            `Position on EU` =round(mean(PT5),0),
            `Ideological Position` = round(mean(PT6),0),
            `Ideological Distance` = round(mean(distance),0),
            `Issue Congruence` = round(mean(congruence),0),
            `Political Knowledge` = round(mean(PT7),0)
            ) %>% 
  ungroup() 

e1b <- d %>% 
  group_by(stance_ss) %>% 
  summarise(Age = round(mean(D1),0),
            Gender= "Male",
            Education = "High-levels of education",
            Location = "West of Netherlands",
            Income = round(mean(D5),0),
            Job = "Fulltime Employed",
            `Vote Recall` = "D66",
            `Position on Immigration` = round(mean(PT2),0),
            `Position on Environment` = round(mean(PT3),0),
            `Position on Tax` = round(mean(PT4),0),
            `Position on EU` =round(mean(PT5),0),
            `Ideological Position` = round(mean(PT6),0),
            `Ideological Distance` = round(mean(distance),0),
            `Issue Congruence` = round(mean(congruence),0),
            `Political Knowledge` = round(mean(PT7),0)
  ) %>% 
  ungroup() 

e1_a <- e1 %>% 
  select(stance_nlp, Gender:Location, Job:`Vote Recall`) %>% 
  pivot_longer(cols = Gender:`Vote Recall`) %>% 
  filter(stance_nlp==0) %>% 
  select(`Incorrectly Identified Stance (Strict Interpretation)` = value)

e1_b <- e1 %>% 
  select(stance_nlp, Gender:Location, Job:`Vote Recall`) %>% 
  pivot_longer(cols = Gender:`Vote Recall`) %>% 
  filter(stance_nlp==1) %>% 
  select(`Correctly Identified Stance (Strict Interpretation)` = value)

e1_a2 <- e1b %>% 
  select(stance_ss, Gender:Location, Job:`Vote Recall`) %>% 
  pivot_longer(cols = Gender:`Vote Recall`) %>% 
  filter(stance_ss==0) %>% 
  select(`Incorrectly Identified Stance (Lenient Interpretation)` = value)

e1_b2 <- e1b %>% 
  select(stance_ss, Gender:Location, Job:`Vote Recall`) %>% 
  pivot_longer(cols = Gender:`Vote Recall`) %>% 
  filter(stance_ss==1) %>% 
  select(`Correctly Identified Stance (Lenient Interpretation)` = value)

e1_c <- e1 %>% 
  select(stance_nlp, Age, Income, `Position on Immigration`:`Political Knowledge`) %>% 
  mutate(Income = 3250) %>% 
  pivot_longer(cols = Age:`Political Knowledge`) %>% 
  mutate(vals = paste(name, value, sep = ": ")) %>% 
  filter(stance_nlp==0) %>% 
  select(`Incorrectly Identified Stance (Strict Interpretation)` = vals)

e1_d <- e1 %>% 
  select(stance_nlp, Age, Income, `Position on Immigration`:`Political Knowledge`) %>% 
  mutate(Income = 3250) %>% 
  pivot_longer(cols = Age:`Political Knowledge`) %>% 
  mutate(vals = paste(name, value, sep = ": ")) %>% 
  filter(stance_nlp==1) %>% 
  select(`Correctly Identified Stance (Strict Interpretation)` = vals)

e1_c2 <- e1b %>% 
  select(stance_ss, Age, Income, `Position on Immigration`:`Political Knowledge`) %>% 
  mutate(Income = 3250) %>% 
  pivot_longer(cols = Age:`Political Knowledge`) %>% 
  mutate(vals = paste(name, value, sep = ": ")) %>% 
  filter(stance_ss==0) %>% 
  select(`Incorrectly Identified Stance (Lenient Interpretation)` = vals)

e1_d2 <- e1b %>% 
  select(stance_ss, Age, Income, `Position on Immigration`:`Political Knowledge`) %>% 
  mutate(Income = 3250) %>% 
  pivot_longer(cols = Age:`Political Knowledge`) %>% 
  mutate(vals = paste(name, value, sep = ": ")) %>% 
  filter(stance_ss==1) %>% 
  select(`Correctly Identified Stance (Lenient Interpretation)` = vals)

e1_nlp <- cbind(rbind(e1_a, e1_c), rbind(e1_b, e1_d)) 
e1_ss <- cbind(rbind(e1_a2, e1_c2), rbind(e1_b2, e1_d2)) 

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

e1_us <- d %>% 
  drop_na() %>% 
  group_by(stance_nlp) %>% 
  summarise(Age = round(mean(D1),0),
            Gender= "Male",
            Education = "High-level of education",
            Location = "Southeast of the United States",
            Income = round(mean(D5),0),
            Job = "Working now",
            `PID` = "Democrat",
            `Position on Immigration` = round(mean(PT2),0),
            `Position on Environment` = round(mean(PT3),0),
            `Position on Tax` = round(mean(PT4),0),
            `Position on Foreign Policy` =round(mean(PT5),0),
            `Ideological Position` = round(mean(PT6),0),
            `Ideological Distance` = round(mean(distance),0),
            `Issue Congruence` = round(mean(congruence),0),
            `Political Knowledge` = round(mean(PT7),0),
            `Bullshit Receptivity` = round(mean(PT7b),0)
  ) %>% 
  ungroup() 

e1b_us <- d %>% 
  drop_na() %>% 
  group_by(stance_ss) %>% 
  summarise(Age = round(mean(D1),0),
            Gender= "Male",
            Education = "High-level of education",
            Location = "Southeast of the United States",
            Income = round(mean(D5),0),
            Job = "Working now",
            `PID` = "Democrat",
            `Position on Immigration` = round(mean(PT2),0),
            `Position on Environment` = round(mean(PT3),0),
            `Position on Tax` = round(mean(PT4),0),
            `Position on Foreign Policy` =round(mean(PT5),0),
            `Ideological Position` = round(mean(PT6),0),
            `Ideological Distance` = round(mean(distance),0),
            `Issue Congruence` = round(mean(congruence),0),
            `Political Knowledge` = round(mean(PT7),0),
            `Bullshit Receptivity` = round(mean(PT7b),0)
  ) %>% 
  ungroup() 

e1_a_us <- e1_us %>% 
  select(stance_nlp, Gender:Location, Job:`PID`) %>% 
  pivot_longer(cols = Gender:`PID`) %>% 
  filter(stance_nlp==0) %>% 
  select(`Incorrectly Identified Stance (Strict Interpretation)` = value)

e1_b_us <- e1_us %>% 
  select(stance_nlp, Gender:Location, Job:`PID`) %>% 
  pivot_longer(cols = Gender:`PID`) %>% 
  filter(stance_nlp==1) %>% 
  select(`Correctly Identified Stance (Strict Interpretation)` = value)

e1_a2_us <- e1b_us %>% 
  select(stance_ss, Gender:Location, Job:`PID`) %>% 
  pivot_longer(cols = Gender:`PID`) %>% 
  filter(stance_ss==0) %>% 
  select(`Incorrectly Identified Stance (Lenient Interpretation)` = value)

e1_b2_us <- e1b_us %>% 
  select(stance_ss, Gender:Location, Job:`PID`) %>% 
  pivot_longer(cols = Gender:`PID`) %>% 
  filter(stance_ss==1) %>% 
  select(`Correctly Identified Stance (Lenient Interpretation)` = value)

e1_c_us <- e1_us %>% 
  select(stance_nlp, Age, Income, `Position on Immigration`:`Bullshit Receptivity`) %>% 
  mutate(Income = 3250) %>% 
  pivot_longer(cols = Age:`Bullshit Receptivity`) %>% 
  mutate(vals = paste(name, value, sep = ": ")) %>% 
  filter(stance_nlp==0) %>% 
  select(`Incorrectly Identified Stance (Strict Interpretation)` = vals)

e1_d_us <- e1_us %>% 
  select(stance_nlp, Age, Income, `Position on Immigration`:`Bullshit Receptivity`) %>% 
  mutate(Income = 3250) %>% 
  pivot_longer(cols = Age:`Bullshit Receptivity`) %>% 
  mutate(vals = paste(name, value, sep = ": ")) %>% 
  filter(stance_nlp==1) %>% 
  select(`Correctly Identified Stance (Strict Interpretation)` = vals)

e1_c2_us <- e1b_us %>% 
  select(stance_ss, Age, Income, `Position on Immigration`:`Bullshit Receptivity`) %>% 
  mutate(Income = 3250) %>% 
  pivot_longer(cols = Age:`Bullshit Receptivity`) %>% 
  mutate(vals = paste(name, value, sep = ": ")) %>% 
  filter(stance_ss==0) %>% 
  select(`Incorrectly Identified Stance (Lenient Interpretation)` = vals)

e1_d2_us <- e1b_us %>% 
  select(stance_ss, Age, Income, `Position on Immigration`:`Bullshit Receptivity`) %>% 
  mutate(Income = 3250) %>% 
  pivot_longer(cols = Age:`Bullshit Receptivity`) %>% 
  mutate(vals = paste(name, value, sep = ": ")) %>% 
  filter(stance_ss==1) %>% 
  select(`Correctly Identified Stance (Lenient Interpretation)` = vals)

e1_us_nlp <- cbind(rbind(e1_a_us, e1_c_us), rbind(e1_b_us, e1_d_us))
e1_us_ss <- cbind(rbind(e1_a2_us, e1_c2_us), rbind(e1_b2_us, e1_d2_us))

