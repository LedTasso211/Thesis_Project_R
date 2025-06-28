library(broom)
library(dplyr)


attrition_data <- full_data %>%
  group_by(DiaryID, year) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  mutate(
    observation_group = factor(if_else(count == 1, "Single Observation", "Panel (2+ Obs)"))
  )

summary_continuous <- attrition_data %>%
  group_by(observation_group) %>%
  summarise(
    mean_income = mean(total_income, na.rm = TRUE),
    median_income = median(total_income, na.rm = TRUE),
    mean_consumption = mean(total_consumption, na.rm = TRUE),
    median_consumption = median(total_consumption, na.rm = TRUE),
    mean_members = mean(FamilySize, na.rm = TRUE),
    median_members = median(FamilySize, na.rm = TRUE)
  )

tidy_tests <- bind_rows(
  tidy(t.test(total_income ~ observation_group, data = attrition_data)) %>% mutate(variable = "Income (Mean)"),
  tidy(t.test(total_consumption ~ observation_group, data = attrition_data)) %>% mutate(variable = "Consumption (Mean)"),
  tidy(t.test(FamilySize ~ observation_group, data = attrition_data)) %>% mutate(variable = "Members (Mean)"),
  tidy(wilcox.test(total_income ~ observation_group, data = attrition_data)) %>% mutate(variable = "Income (Median)"),
  tidy(wilcox.test(total_consumption ~ observation_group, data = attrition_data)) %>% mutate(variable = "Consumption (Median)"),
  tidy(wilcox.test(FamilySize ~ observation_group, data = attrition_data)) %>% mutate(variable = "Members (Median)")
)


tidy_tests$estimate1[tidy_tests$variable == "Income (Median)"] <- summary_continuous$median_income[summary_continuous$observation_group == "Panel (2+ Obs)"]
tidy_tests$estimate2[tidy_tests$variable == "Income (Median)"] <- summary_continuous$median_income[summary_continuous$observation_group == "Single Observation"]


tidy_tests$estimate1[tidy_tests$variable == "Consumption (Median)"] <- summary_continuous$median_consumption[summary_continuous$observation_group == "Panel (2+ Obs)"]
tidy_tests$estimate2[tidy_tests$variable == "Consumption (Median)"] <- summary_continuous$median_consumption[summary_continuous$observation_group == "Single Observation"]


tidy_tests$estimate1[tidy_tests$variable == "Members (Median)"] <- summary_continuous$median_members[summary_continuous$observation_group == "Panel (2+ Obs)"]
tidy_tests$estimate2[tidy_tests$variable == "Members (Median)"] <- summary_continuous$median_members[summary_continuous$observation_group == "Single Observation"]



continuous_analysis_final_table <- tidy_tests %>%
  rename(
    group1_Panel_stat = estimate1,
    group2_SingleObs_stat = estimate2
  ) %>%
  select(
    variable,
    group1_Panel_stat,
    group2_SingleObs_stat,
    statistic,
    p.value
  )

print(continuous_analysis_final_table)


summary_region <- attrition_data %>%
  count(observation_group, RegNo) %>%
  group_by(observation_group) %>%
  mutate(percentage = n / sum(n) * 100)

summary_urban_rural <- attrition_data %>%
  count(observation_group, UrbanOrRural) %>%
  group_by(observation_group) %>%
  mutate(percentage = n / sum(n) * 100)


print(summary_region)

print(summary_urban_rural)


region_table <- table(attrition_data$observation_group, attrition_data$RegNo)
region_chisq <- chisq.test(region_table)

urban_rural_table <- table(attrition_data$observation_group, attrition_data$UrbanOrRural)
urban_rural_chisq <- chisq.test(urban_rural_table)


categorical_tests_summary <- bind_rows(
  tidy(region_chisq) %>% mutate(variable = "Region"),
  tidy(urban_rural_chisq) %>% mutate(variable = "Urban/Rural")
) %>%
  select(variable, statistic, p.value, method)

print(categorical_tests_summary)

save_results_to_excel(continuous_analysis_final_table, sheet_name = "Single-Survey Analysis", output_file = "output/Tables/Single-Survey Analysis.xlsx")
