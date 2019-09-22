library(tidyverse)
library(lubridate)

fish.data <- read_csv("ADU Specimen Analysis.csv")

fish.data %>%
  filter(`Common Name` == "lingcod",
         `Sampling Year` == 2018) %>%
  group_by(`Management Area(s)`, `Age Bin`) %>%
  summarise(total.count = sum(`Specimen Count`)) %>%
  ggplot(aes(x = `Age Bin`, y = total.count)) +
  geom_bar(stat = "identity",
           aes(fill = `Management Area(s)`))

x <- unique(fish.data$`Common Name`)
x

includeHTML(paste0(getwd(),"/www/","adfgaboutus1.html"))

#h1 - h6 coresonds to the text size: h1 is largest, then it sgets smaller as you go

# tags$div(
#   tags$h1("Heading"), 
#   tags$h2("Subheading"), 
#   tags$h3("Subsubheading"), 
#   tags$h4("Subsubsubheading"), 
#   tags$h5("Subsubsubsubheading"), 
#   tags$h6("Subsubsubsubsubheading") 
# ),
###HTML notes for images, banner, etc
# tags$div(
#   HTML(paste("Select a year to view Alaska Fish Counts ", tags$span(style="color:red", "red"), sep = ""))
# ),

#tags$h1(id = "title1", "Alaska Fish Counts from 1976 - 2019"),
#    #titlePanel("Alaska Fish Counts from 1976 - 2019"),
#    #tags$b("This text is bold."),
#    #tags$img(src = "fish_and_gamelogo.png", width = "50px", height = "50px"), ##NEED to put images in a another folder called www. Don't know why
#    div(
#      h1(style="background-color: black; color: white; height: 125px; padding: 10px; margin: 0px",
#         
#         HTML('<img src="fish_and_gamelogo.png" style="float:right; padding-right:25px" width="125px" height="100px"/>',
#              'Alaska Fish Counts <br> 1976-2019'
#         ))
#    ),
#    tags$div(
#      tags$blockquote(
#        tags$em("Science in the Last Frontier", cite = "Alaska Fish and Game"))
#      
#    ),
#    #div creates a division of an html document
#    
#    HTML('<hr >'), #this makes a horizontonal line break; also works by tags$hr()