source("scripts/02.1_data_preparation.R")

filtered_data <- keep(cleaned_data, names(all_data) %in% c("2004-2008", "2009", "2010"))

first_year <- 2020
last_year <- 2024

t1 <- imap_dfr(filtered_data, ~ {
  merged_year <- .x$survey_design %>%
    left_join(.x$household_structure) %>%
    left_join(.x$consumption_data) %>%
    left_join(.x$income_components) %>% 
    left_join(.x$demographics)
  merged_year %>% mutate(year = .y)
}) %>%
  mutate(Year = ifelse(is.na(Year), year, Year)) %>%
  select(-year) %>%
  rename(year = Year) %>%
  mutate(year = as.numeric(year))

attr(t1$Education, "labels")

t1<-t1 %>% 
  mutate(Education = case_when(
    Education %in% c(1) ~ "Primary",
    Education %in% c(2,3,4,5) ~ "Secondary", 
    Education %in% c(6) ~ "Tertiary",
    TRUE ~ "None"
  ))

filtered_data <- keep(cleaned_data, names(all_data) %in% c("2011", "2012", "2013", "2014", "2015", "2016"))

t2 <- imap_dfr(filtered_data, ~ {
  merged_year <- .x$survey_design %>%
    left_join(.x$household_structure) %>%
    left_join(.x$consumption_data) %>%
    left_join(.x$income_components) %>% 
    left_join(.x$demographics)
  merged_year %>% mutate(year = .y)
}) %>% mutate(year = as.numeric(year))

attr(t2$Education, "labels")

t2<-t2 %>% 
  mutate(Education = case_when(
    Education %in% c(3) ~ "Primary",
    Education %in% c(4,5,6,7,8) ~ "Secondary", 
    Education %in% c(9,10,11) ~ "Tertiary",
    TRUE ~ "None"
  ))

filtered_data <- keep(cleaned_data, names(all_data) %in% c("2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024"))

t3 <- imap_dfr(filtered_data, ~ {
  merged_year <- .x$survey_design %>%
    left_join(.x$household_structure) %>%
    left_join(.x$consumption_data) %>%
    left_join(.x$income_components) %>% 
    left_join(.x$demographics)
  merged_year %>% mutate(year = .y)
}) %>% mutate(year = as.numeric(year))

attr(t3$Education, "labels")

t3<-t3 %>% 
  mutate(Education = case_when(
    Education %in% c(4) ~ "Primary",
    Education %in% c(5,6,7,8,9,10) ~ "Secondary", 
    Education %in% c(11,12,13) ~ "Tertiary",
    TRUE ~ "None"
  ))

rif_data<-bind_rows(t1,t2,t3) 

rif_data<-rif_data %>% 
  select(year, DiaryID, Education, Age) %>% 
  mutate(Education = case_when(
    Education == "Primary" ~ 1, 
    Education == "Secondary" ~ 2, 
    Education == "Tertiary" ~ 3,
    TRUE ~ 0
  ))

rif_data <- rif_data %>% 
  group_by(year, DiaryID) %>%
  summarise(
    avg_age = mean(Age, na.rm = TRUE),
    max_age = if (all(is.na(Age))) NA_real_ else max(Age, na.rm = TRUE),
    sh_prim = mean(Education == 1, na.rm = TRUE),
    sh_sec = mean(Education == 2, na.rm = TRUE),
    sh_ter = mean(Education == 3, na.rm = TRUE),
    .groups = "drop"
  )

rif_data<-rif_data%>% 
  inner_join(reg_data)

rif_data <- agg_data %>%
  select(year, DiaryID, adj_income, Weights) %>%
  left_join(rif_data, by = c("year", "DiaryID")) %>%
  mutate(urban = if_else(UrbanOrRural == 1, 1, 0)) %>%
  mutate(stratum = interaction(RegNo, UrbanOrRural, drop = TRUE)) %>%
  select(-UrbanOrRural, -RegNo) %>%
  left_join(cpi_data %>% 
              select(year, FP.CPI.TOTL) %>% 
              rename(cpi = FP.CPI.TOTL),
            by = "year") %>% 
  mutate(adj_income = adj_income/(cpi/100)) %>% 
  select(-cpi) 

rif_data <- rif_data %>% 
  filter(year %in% c(first_year, last_year)) %>% 
  mutate(post = if_else(year == last_year, 1, 0),
         lny  = log(adj_income)) 

des04_07 <- svydesign(
  ids     = ~ DiaryID,           
  strata  = ~ stratum,       
  weights = ~ Weights,       
  data    = rif_data
)

summary(des04_07)

taus <- c(0.1, 0.2, 0.4, 0.6, 0.8, 0.9)

rif_formula <- as.formula(
  "lny ~ post + avg_age + I(avg_age^2) + sh_sec + sh_ter + urban +
   post:avg_age + post:I(avg_age^2) + post:sh_sec + post:sh_ter + post:urban"
)

rif_fit <- rifreg(
  formula                = rif_formula,
  data                   = rif_data,              
  statistic              = "quantiles",
  weights                = rif_data$Weights,      
  probs                  = taus,                  
  bootstrap              = TRUE,                  
  bootstrap_iterations   = 1000,
  cores                  = max(1, detectCores() - 1)   
)

rif_fit$estimates       
rif_fit$bootstrap_se 

estimates<-rif_fit$estimates       
bootstrap_se<-rif_fit$bootstrap_se 

estimates_df <- data.frame(Variable = rownames(estimates), estimates, row.names = NULL)
bootstrap_se_df <- data.frame(Variable = rownames(bootstrap_se), bootstrap_se, row.names = NULL)

save_results_to_excel(estimates_df, sheet_name = "RIF", output_file = "output/Tables/Stat_P4.xlsx")
save_results_to_excel(bootstrap_se_df, sheet_name = "RIF_bootstrap", output_file = "output/Tables/Stat_P4.xlsx")
