source("scripts/02.2_data_preparation_OECD.R")

first  <- 2004
last   <- 2007
n_years <- last - first

percentiles <- seq(0.01, 0.99, 0.01)

q_first <- agg_data %>%
  filter(year == first) %>%
  left_join(cpi_data %>%
              rename(cpi = FP.CPI.TOTL),
            by = "year") %>%
  mutate(real_income = adj_income / (cpi/100)) %>%
  pull(real_income) %>%
  wtd.quantile(weights = agg_data$Weights[agg_data$year == first],
               probs   = percentiles,
               na.rm   = TRUE)

q_last  <- agg_data %>%
  filter(year == last) %>%
  left_join(cpi_data %>%
              rename(cpi = FP.CPI.TOTL),
            by = "year") %>%
  mutate(real_income = adj_income / (cpi/100)) %>%
  pull(real_income) %>%
  wtd.quantile(weights = agg_data$Weights[agg_data$year == last],
               probs   = percentiles,
               na.rm   = TRUE)

cimg_data <- tibble(
  percentile    = percentiles,
  income_first    = q_first,
  income_last     = q_last
) %>%
  # annualized growth rate
  mutate(
    growth_rate_annualized = ((income_last / income_first)^(1 / n_years)) - 1,
    income_base_year       = income_first
  ) %>%
  
  arrange(percentile) %>%
  mutate(total_income          = sum(income_base_year),
         income_share_s_j     = income_base_year / total_income,
         numerator_component  = growth_rate_annualized * income_share_s_j,
         cumulative_numerator = cumsum(numerator_component),
         cumulative_denominator = cumsum(income_share_s_j),
         cimg_value           = cumulative_numerator / cumulative_denominator
  )

mean_growth_rate <- last(cimg_data$cimg_value) 

cimg_data <- cimg_data %>%
  mutate(mean_growth_rate = mean_growth_rate)

cimg<-ggplot(cimg_data, aes(x = percentile*100)) +
  geom_line(aes(y = growth_rate_annualized, color = "GIC"), linetype = "dashed", alpha = 0.7) +
  geom_line(aes(y = cimg_value, color = "CIMG"), size = 1.2) +
  geom_line(aes(y = mean_growth_rate, color = "Mean Growth"), linetype = "dotted", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "CIMG and GIC (2004-2007)",
    x = "Percentile of the Population",
    y = "Annualised Growth Rate (%)",
    color = "Curve"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) + 
  scale_color_manual(values = c("GIC" = "grey50", "CIMG" = "#131D4F", "Mean Growth" = "#901E3E")) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line()
  )

print(cimg)

ggsave(
  filename = "output/OECD Graphs/CIMG_P1.png",  
  plot     = cimg,                       
  width    = 8,                            
  height   = 5,                         
  dpi      = 300                           
)
