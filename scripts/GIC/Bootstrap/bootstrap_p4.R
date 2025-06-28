source("scripts/02.1_data_preparation.R")

set.seed(42)       

first      <- 2020
last       <- 2024
n_years    <- last - first
percentiles <- seq(0.01, 0.99, 0.01)
B          <- 1000           
conf_lvl   <- 0.95          


w_boot <- function(df, weight_col) {
  w   <- df[[weight_col]] / sum(df[[weight_col]])       # normalise
  idx <- sample(seq_len(nrow(df)), replace = TRUE, prob = w)
  df[idx, , drop = FALSE]
}

base_first<-agg_data %>% 
  filter(year == first) %>% 
  left_join(cpi_data %>% 
              select(year, FP.CPI.TOTL) %>% 
              rename(cpi = FP.CPI.TOTL )) %>% 
  mutate(real_income = adj_income / (cpi / 100))

base_last<-agg_data %>% 
  filter(year == last) %>% 
  left_join(cpi_data %>% 
              select(year, FP.CPI.TOTL) %>% 
              rename(cpi = FP.CPI.TOTL )) %>% 
  mutate(real_income = adj_income / (cpi / 100))

get_quant <- function(df) 
  wtd.quantile(df$real_income, weights = df$Weights, probs = percentiles)

gic_base <- tibble(
  percentile  = percentiles,
  income_first = get_quant(base_first),
  income_last  = get_quant(base_last)
) %>% 
  mutate(growth_rate = (income_last / income_first)^(1 / n_years) - 1)


mean_growth   <- {
  m1 <- weighted.mean(base_first$real_income, base_first$Weights)
  m2 <- weighted.mean(base_last$real_income,  base_last$Weights)
  (m2/m1)^(1 / n_years) - 1
}
median_growth <- {
  q1 <- wtd.quantile(base_first$real_income, base_first$Weights, 0.5)
  q2 <- wtd.quantile(base_last$real_income,  base_last$Weights, 0.5)
  (q2/q1)^(1 / n_years) - 1
}


boot_mat <- replicate(B, {
  d1 <- w_boot(base_first, "Weights")
  d2 <- w_boot(base_last,  "Weights")
  q1 <- get_quant(d1)
  q2 <- get_quant(d2)
  (q2 / q1)^(1 / n_years) - 1          
})


alpha <- (1 - conf_lvl) / 2
ci_lo <- apply(boot_mat, 1, quantile, probs = alpha)
ci_hi <- apply(boot_mat, 1, quantile, probs = 1 - alpha)

gic_df <- gic_base %>% 
  mutate(ci_lo = ci_lo,
         ci_hi = ci_hi)


gic <- ggplot(gic_df, aes(x = percentile * 100)) +
  
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi),
              fill = "grey70", alpha = 0.3) +
  
  geom_line(aes(y = growth_rate),
            colour = "#222831", size = 1) +
  
  geom_hline(yintercept = 0,           linetype = "dashed", colour = "black") +
  geom_hline(yintercept = mean_growth, linetype = "dashed", colour = "grey40") +
  geom_hline(yintercept = median_growth, linetype = "dashed", colour = "grey40") +
  
  annotate("text", x = 95, y = mean_growth   + 0.002,
           label = "Growth in Mean", colour = "black", size = 3, hjust = 1) +
  annotate("text", x = 95, y = median_growth + 0.002,
           label = "Median", colour = "black", size = 3, hjust = 1) +
  
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  
  labs(x = "Percentile of the Population",
       y = "Annualised Growth Rate (%)",
       title = "Growth Incidence Curve for Georgia (2020–2024)",
       subtitle = paste0(conf_lvl*100, "% bootstrap CI, B = ", B)) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line        = element_line(colour = "black"),
    axis.ticks       = element_line()
  )

print(gic)

ggsave(
  filename = "output/Graphs/GIC_bootstrap_P4.png",  
  plot     = gic,                        
  width    = 8,                            
  height   = 5,                           
  dpi      = 300                         
)