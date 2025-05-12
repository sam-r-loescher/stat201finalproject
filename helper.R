# Note: percent map is designed to work with the counties data set
# It may not work correctly with other data sets if their row order does
# not exactly match the order in which the maps package plots counties
library(ggplot2)
library(dplyr)

percentile_income <- read.csv("pip_global_percentiles.csv")


percent_map <- percentiles %>% 
  ggplot(aes(x = factor(percentile), y = income)) +
  geom_col(width = 0.8) +              # or geom_bar(stat="identity")
  labs(
    x = "Income Percentile",
    y = "Income Threshold",
    title = "Income by Global Percentile"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )


