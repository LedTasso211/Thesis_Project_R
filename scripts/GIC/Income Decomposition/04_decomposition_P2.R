source("scripts/02.1_data_preparation.R")

first_year <- 2008
last_year <- 2012
num_years <- last_year - first_year


inc_vars <- c("adj_income",                   
              "income_emp", "income_self_emp", "income_agriculture",
              "income_property", "income_Pens_Schol_assist",
              "income_remittances", "income_gift",
              "non_cash_icnome", "unreported_income")


decomposition_data <- agg_data %>%
  mutate(stratum = interaction(RegNo, UrbanOrRural, drop = TRUE)) %>%
  mutate(unreported_income = ifelse(total_consumption > total, total_consumption - total, 0)) %>%
  filter(year %in% c(first_year, last_year)) %>%
  select(
    year, DiaryID, stratum, Weights, adj_income,
    income_emp, income_self_emp, income_agriculture, income_property,
    income_Pens_Schol_assist, income_remittances, income_gift,
    non_cash_icnome, unreported_income
  ) %>% 
  left_join(cpi_data %>% 
              select(year, FP.CPI.TOTL) %>% 
              rename(cpi = FP.CPI.TOTL),
            by = "year") %>% 
  mutate(across(all_of(inc_vars),
                ~ .x / (cpi/100))) %>%          
  select(year, DiaryID, stratum, Weights, all_of(inc_vars)) 


decomposition_data <- decomposition_data %>%           
  group_by(year) %>%                                  
  mutate(
    quintile = cut(
      adj_income,
      breaks = c(                                                   
        -Inf,
        wtd.quantile(adj_income, weights = Weights,
                     probs = c(0.2, 0.4, 0.6, 0.8), na.rm = TRUE),
        Inf
      ),
      labels = 1:5,
      include.lowest = TRUE,
      right  = TRUE
    )
  ) %>%
  ungroup()


des_period_with_quintiles <- svydesign(
  ids = ~DiaryID,
  strata = ~stratum,
  weights = ~Weights,
  data = decomposition_data
)


component_vars <- c(
  "income_emp", "income_self_emp", "income_agriculture", "income_property",
  "income_Pens_Schol_assist", "income_remittances", "income_gift",
  "non_cash_icnome", "unreported_income"
)


formula_str <- paste("~adj_income +", paste(component_vars, collapse = " + "))
survey_formula <- as.formula(formula_str)


mean_components <- svyby(
  formula = survey_formula,
  by = ~ quintile + year,
  design = des_period_with_quintiles,
  FUN = svymean,
  na.rm = TRUE
)



changes_wide <- mean_components %>%
  as_tibble() %>%
  select(quintile, year, adj_income, all_of(component_vars)) %>%
  pivot_wider(
    names_from = year,
    values_from = c(adj_income, all_of(component_vars))
  )


decomposition_results <- changes_wide %>%
  mutate(
    annual_change_total_income = ((adj_income_2012 / adj_income_2008)^(1/num_years) - 1)*100,
    annual_change_emp = ((income_emp_2012 / income_emp_2008)^(1/num_years) - 1)*100,
    annual_change_self_emp = ((income_self_emp_2012 / income_self_emp_2008)^(1/num_years) - 1)*100,
    annual_change_agri = ((income_agriculture_2012 / income_agriculture_2008)^(1/num_years) - 1)*100,
    annual_change_prop = ((income_property_2012 / income_property_2008)^(1/num_years) - 1)*100,
    annual_change_social = ((income_Pens_Schol_assist_2012 / income_Pens_Schol_assist_2008)^(1/num_years) - 1)*100,
    annual_change_remit = ((income_remittances_2012 / income_remittances_2008)^(1/num_years) - 1)*100,
    annual_change_gift = ((income_gift_2012 / income_gift_2008)^(1/num_years) - 1)*100,
    annual_change_non_cash = ((non_cash_icnome_2012 / non_cash_icnome_2008)^(1/num_years) - 1)*100,
    annual_change_unreported =  ((unreported_income_2012 / unreported_income_2008)^(1/num_years) - 1)*100
  ) %>%
  select(quintile, starts_with("annual_change_"))


decomposition_levels <- changes_wide %>%
  mutate(
    delta_total_income = adj_income_2012 - adj_income_2008,
    delta_emp          = income_emp_2012 - income_emp_2008,
    delta_self_emp     = income_self_emp_2012 - income_self_emp_2008,
    delta_agri         = income_agriculture_2012 - income_agriculture_2008,
    delta_prop         = income_property_2012 - income_property_2008,
    delta_social       = income_Pens_Schol_assist_2012 - income_Pens_Schol_assist_2008,
    delta_remit        = income_remittances_2012 - income_remittances_2008,
    delta_gift         = income_gift_2012 - income_gift_2008,
    delta_non_cash     = non_cash_icnome_2012 - non_cash_icnome_2008,
    delta_unreported   = unreported_income_2012 - unreported_income_2008
  ) %>%
  select(quintile, starts_with("delta_"))


contribution_table_signed <- decomposition_levels %>%
  mutate(
    across(
      starts_with("delta_") & !matches("delta_total_income"),
      ~ (.x / delta_total_income) * 100,
      .names = "cont_{.col}"
    )
  ) %>%
  select(quintile, delta_total_income, starts_with("cont_")) %>%
  mutate(across(where(is.numeric), round, 2))


save_results_to_excel(decomposition_results, sheet_name = "decomposition", output_file = "output/Tables/Stat_P2.xlsx")
save_results_to_excel(contribution_table_signed, sheet_name = "contribution", output_file = "output/Tables/Stat_P2.xlsx")

#Significance levels 
mean_components_se <- svyby(
  formula = survey_formula,
  by       = ~ quintile + year,
  design   = des_period_with_quintiles,
  FUN      = svymean,
  vartype  = "se",         
  na.rm    = TRUE
)


mean_se_long <- mean_components_se %>%
  pivot_longer(
    cols      = -c(quintile, year, starts_with("se.")),
    names_to  = "component",
    values_to = "mean"
  ) %>%
  left_join(
    mean_components_se %>%
      pivot_longer(
        cols         = starts_with("se."),
        names_to     = "component",
        names_prefix = "se.",
        values_to    = "std_error"
      ),
    by = c("quintile", "year", "component")
  ) %>%
  mutate(
    z       = mean / std_error,
    p_value = 2 * (1 - pnorm(abs(z))),
    signif  = p_value < 0.05,
    lower   = mean - 1.96 * std_error,
    upper   = mean + 1.96 * std_error
  )


test_delta_glm <- function(design, q, comp) {
  subd <- subset(design, quintile == q)
  f    <- as.formula(paste0(comp, " ~ factor(year)"))
  fit  <- svyglm(f, design = subd)
  broom::tidy(fit) %>%
    filter(term == "factor(year)2012") %>%
    transmute(
      quintile  = q,
      component = comp,
      estimate  = estimate,      
      std_error = std.error,
      p_value   = p.value,
      signif    = p.value < 0.05,
      lower     = estimate - 1.96 * std.error,
      upper     = estimate + 1.96 * std.error
    )
}

all_glm_tests <- map_dfr(
  .x = 1:5,
  .f = function(q) {
    map_dfr(component_vars, ~ test_delta_glm(des_period_with_quintiles, q, .x))
  }
)

print(all_glm_tests)

save_results_to_excel(all_glm_tests, sheet_name = "glm", output_file = "output/Tables/Stat_P2.xlsx")