library(tidyverse)
library(lubridate)

fish.data <- read_csv("ADU Specimen Analysis.csv")

fish.data %>%
  group_by(`Sampling Year`) %>%
  summarise(total.count = sum(`Specimen Count`)) %>%
  ggplot(aes(x = `Sampling Year`, y = total.count)) +
  geom_bar(stat = "identity")

x <- unique(fish.data$`Common Name`)
x
