source("scripts/01_import_data.R") 


all_labels <- list()

for (year in names(all_data)) {
  for (df_name in names(all_data[[year]])) {
    labels <- var_label(all_data[[year]][[df_name]], unlist = TRUE)
    if (length(labels) > 0) {
      all_labels <- c(all_labels, labels)
    }
  }
}

all_labels <- all_labels[!duplicated(names(all_labels), fromLast = TRUE)]

# Create new list to store cleaned data
cleaned_data <- list()

for (year in names(all_data)) {
  raw <- all_data[[year]]
  
  if (year == "2004-2008") {
    #Special handling for the 2004–2008 combined file
    cleaned_data[[year]] <- list(
      survey_design = raw[["_sysschedule"]],
      
      household_structure = raw[["_familysize"]] %>%
        select(UID, FamilySize, Eq_Adult, Working_age_man, Working_age_Woman,
               Pensioner_age_man, Pensioner_age_Woman, Childern, Adult, Eq_Skale_0_6, Ed_Skale_0_8),
      
      consumption_data = raw[["_tblexpenditures"]] %>%
        select(UID, MTlianimoxmareba_),
      
      income_components = raw[["_tblincomes"]],
      
      demographics = raw[["_tblshinda02"]] %>%
        select(UID, MemberNo, Gender, Education, Age)
    )
    
  } else {
    # Regular yearly files (2009, 2010, …, 2023)
    cleaned_data[[year]] <- list(
      survey_design = raw[["sysschedule"]],
      
      household_structure = raw[["familysize"]] %>%
        select(UID, FamilySize, Eq_Adult, Working_age_man, Working_age_Woman,
               Pensioner_age_man, Pensioner_age_Woman, Childern, Adult, Eq_Skale_0_6, Ed_Skale_0_8),
      
      consumption_data = raw[["tblexpenditures"]] %>%
        select(UID, MTlianimoxmareba_),
      
      income_components = raw[["tblincomes"]], 
      
      demographics = raw[["tblshinda02"]] %>%
        select(UID, MemberNo, Gender, Education, Age)
    )
  }
}

rm(year, raw)

#Merging all years data 
full_data <- imap_dfr(cleaned_data, ~ {
  merged_year <- .x$survey_design %>%
    left_join(.x$household_structure) %>%
    left_join(.x$consumption_data) %>%
    left_join(.x$income_components)
  merged_year %>% mutate(year = .y)
}) %>% 
  mutate(Year = ifelse(is.na(Year),year, Year)) %>% 
  select(-year) %>% 
  rename(year = Year) %>% 
  mutate(year = as.numeric(year))


labels_to_apply <- all_labels[names(all_labels) %in% names(full_data)]
full_data <- set_variable_labels(full_data, .labels = labels_to_apply)

#Renaming some of the variables 
full_data <- full_data %>% 
  rename(total_consumption = MTlianimoxmareba_, 
         income_emp = ShemDaq, 
         income_self_emp = ShemTviTdasaqm, 
         income_agriculture = Shem_Sof, 
         income_property = Qonebidan, 
         income_Pens_Schol_assist = PensStipDaxm, 
         income_remittances = Ucxoetidan, 
         income_gift = Axloblebisagan, 
         non_cash_icnome = ArafuladiMoxmareba, 
         total_income = Shemosavalisul, 
         cash_income_and_transfers = ShemosavaliDaTransf, 
         selling_prop = QonebisGayidvit, 
         loans_and_savings = SesxAnDanazog, 
         total_cash_inflows = Fuladisaxsrebi, 
         other_cash_inflows = SxvaFuladiSaxsrebi,
         total = Saxsrebi_Sul) 


rm(df_name, labels, labels_to_apply)
