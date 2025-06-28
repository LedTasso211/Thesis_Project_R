source("scripts/02.2_data_preparation_OECD.R")

first  <- 2013
last   <- 2019
n_years <- last - first

first_year<-agg_data %>% 
  filter(year == first) %>% 
  left_join(cpi_data %>% 
              select(year, FP.CPI.TOTL) %>% 
              rename(cpi = FP.CPI.TOTL )) %>% 
  mutate(real_income = adj_income / (cpi / 100))

last_year<-agg_data %>% 
  filter(year == last) %>% 
  left_join(cpi_data %>% 
              select(year, FP.CPI.TOTL) %>% 
              rename(cpi = FP.CPI.TOTL )) %>% 
  mutate(real_income = adj_income / (cpi / 100))

percentiles <- seq(0.01, 0.99, 0.01)


q_1 <- wtd.quantile(first_year$real_income, weights = first_year$Weights, probs = percentiles)
q_2 <- wtd.quantile(last_year$real_income, weights = last_year$Weights, probs = percentiles)

gic_df <- data.frame(
  percentile = percentiles,
  income_first = q_1,
  income_last = q_2
) %>%
  mutate(
    growth_rate = ((income_last / income_first)^(1 / n_years)) - 1,
  ) 

# Weighted means
mean_start <- weighted.mean(first_year$real_income, first_year$Weights) 
mean_end <- weighted.mean(last_year$real_income, last_year$Weights)

mean_growth <- ((mean_end / mean_start)^(1 / n_years)) - 1

# Weighted medians
median_start <- wtd.quantile(first_year$real_income, first_year$Weights, probs = 0.5)
median_end <- wtd.quantile(last_year$real_income, last_year$Weights, probs = 0.5)

median_growth <- ((median_end / median_start)^(1 / n_years)) - 1


gic<-ggplot(gic_df, aes(x = percentile * 100, y = growth_rate)) +
  geom_line(color = "#222831", size = 1) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = mean_growth, linetype = "dashed", color = "gray") +
  geom_hline(yintercept = median_growth, linetype = "dashed", color = "gray") +
  
  labs(
    x = "Percentile of the Population",
    y = "Annualised Growth Rate (%)", 
    title = "Growth Incidence Curve for Georgia (2013-2019)"
  ) +
  
  scale_x_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line()
  ) + 
  annotate("text", x = 95, y = mean_growth + 0.0004, label = "Mean Growth", color = "black", size = 3, hjust = 1) +
  annotate("text", x = 95, y = median_growth + 0.0004, label = "Median Growth", color = "black", size = 3, hjust = 1)

print(gic)

ggsave(
  filename = "output/OECD Graphs/GIC_P3.png", 
  plot     = gic,                        
  width    = 8,                            
  height   = 5,                           
  dpi      = 300                           
)
