source("scripts/02.1_data_preparation.R")

#Distribution 
dist<-agg_data %>%
  left_join(cpi_data %>% 
              select(year, FP.CPI.TOTL) %>% 
              rename(cpi = FP.CPI.TOTL )) %>% 
  mutate(income_real = adj_income / (cpi / 100)) %>% 
  group_by(year) %>%
  group_modify(~ {
    df <- .x
    cuts <- wtd.quantile(df$income_real,
                         weights = df$Weights,
                         probs   = seq(0, 1, 0.2),
                         na.rm   = TRUE)
    df %>% mutate(
      quintile = factor(
        findInterval(df$income_real, vec = cuts,
                     rightmost.closed = TRUE,
                     all.inside       = TRUE),
        levels = 1:5,
        labels = c("0–20%", "20–40%", "40–60%", "60–80%", "80–100%")
      )
    )
  }) %>%
  ungroup() %>% 
  filter(!is.na(income_real))

share_by_quintile <- dist %>%
  group_by(year, quintile) %>%
  summarise(
    tot_inc = sum(income_real * Weights, na.rm = TRUE),
    .groups   = "drop_last"
  ) %>%
  group_by(year) %>%
  mutate(
    share = tot_inc / sum(tot_inc) * 100
  ) %>%
  ungroup()

share_by_quintile <- share_by_quintile %>%
  mutate(
    quintile = fct_rev(quintile)  
  )

palette <- c(
  "0–20%"   = "#213448",
  "20–40%"  = "#547792",
  "40–60%"  = "#94B4C1",
  "60–80%"  = "#B8CFCE",
  "80–100%" = "#EBF0F0"
)

income_distribution<-ggplot(share_by_quintile, aes(x = year, y = share, fill = quintile)) +
  geom_area(color = NA) +  
  
  scale_fill_manual(values = palette, guide = guide_legend(nrow = 1, byrow = TRUE, reverse = TRUE)) +
  
  scale_x_continuous(
    breaks = seq(min(share_by_quintile$year), max(share_by_quintile$year), by = 2),
    expand = c(0, 0.5)
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 100, 10),
    labels = percent_format(scale = 1),
    expand = c(0, 0)
  ) +
  
  labs(
    x    = NULL,
    y    = "Share of total income (%)",
    fill = NULL
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border     = element_blank(),
    axis.text.x      = element_text(margin = margin(t = 5)),
    axis.text.y      = element_text(margin = margin(r = 5)),
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.margin    = margin(t = 10, b = 0),
    legend.text      = element_text(size = 8),        
    legend.key.size  = unit(0.4, "cm"),                
    legend.spacing.x = unit(0.2, "cm")                 
  )

print(income_distribution)


ggsave(
  filename = "output/Graphs/income_distribution.png",  # path + file name
  plot     = income_distribution,                      # the plot object
  width    = 8,                            # in inches
  height   = 5,                            # in inches
  dpi      = 300                           # resolution
)
