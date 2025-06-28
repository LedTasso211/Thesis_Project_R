source("scripts/02_clean_data.R")

#Remove observations for single surveyed households 
agg_data<-full_data %>% 
  group_by(year, DiaryID) %>% 
  mutate(count = n()) %>% 
  filter(count != 1) %>% 
  select(-count) %>% 
  ungroup()

#Storing data for rural/urban and regions for each household 
reg_data<-agg_data %>% 
  select(year, DiaryID, RegNo, UrbanOrRural) %>% 
  unique()



agg_data<-agg_data %>% 
  select(-MonthYear, -QuartNo, -UID, -RegNo, -UrbanOrRural, -FamilySize, -Eq_Adult, -Eq_Skale_0_6, -Ed_Skale_0_8)

#Custom formula for OECD modified equivalence scale 
agg_data <- agg_data %>%
  mutate(Childern = Childern + Adult) %>% 
  select(-Adult) %>% 
  mutate(Adult = Working_age_man + Working_age_Woman + Pensioner_age_man + Pensioner_age_Woman) %>% 
  mutate(
    eq_adults_OECD = 1                                        
    + pmax(Adult - 1, 0) * 0.5              
    + Childern * 0.3                        
  )

agg_data<-agg_data %>% 
  mutate(across(
    .cols   = -c(year, DiaryID, eq_adults_OECD),   # all except these ones 
    .fns    = ~ .x / eq_adults_OECD             
  )) %>% 
  select(-Adult, Childern)


agg_data <- agg_data %>%
  select(-eq_adults_OECD) %>% 
  group_by(year, DiaryID) %>%
  summarise(
    across(
      .cols = where(is.numeric) & !any_of(c("Weights")),
      .fns  = ~ weighted.mean(.x, w = Weights)
    ),
    Weights = mean(Weights),
    .groups = "drop"
  )

agg_data<-agg_data %>% 
  mutate(adj_income = ifelse(total_consumption>total, total_consumption-other_cash_inflows,total_income))

agg_data<-agg_data %>% 
  inner_join(reg_data) %>% 
  relocate(Weights, .before = total_consumption) %>% 
  relocate(RegNo, .before = Weights) %>% 
  relocate(UrbanOrRural, .after = RegNo) 
