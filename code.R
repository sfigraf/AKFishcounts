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
#h1 - h6 coresonds to the text size: h1 is largest, then it sgets smaller as you go

# tags$div(
#   tags$h1("Heading"), 
#   tags$h2("Subheading"), 
#   tags$h3("Subsubheading"), 
#   tags$h4("Subsubsubheading"), 
#   tags$h5("Subsubsubsubheading"), 
#   tags$h6("Subsubsubsubsubheading") 
# ),

# tags$div(
#   HTML(paste("Select a year to view Alaska Fish Counts ", tags$span(style="color:red", "red"), sep = ""))
# ),