library(tidyverse)
library(lubridate)
library(plotly)
library(shiny)
library(ggthemes)

###To do
#Use HTML to pretty up the ui
#make graph looking better

data <- read_csv("ADU Specimen Analysis.csv")

fish.data <- data %>%
  mutate(mgmtarea = ifelse(is.na(`Management Area(s)`),
                              "Not Specified",
                              `Management Area(s)`))
# fish.data <- fish.data %>%
#   mutate(`Sampling Year` = year(`Sampling Year`))


# Define UI for application that draws a histogram
ui <- fluidPage(
  
   
   # Application title
   titlePanel("Alaska Fish Counts from 1976 - 2019"),
   
   sidebarLayout(
      sidebarPanel(
         sliderInput("slider1",
                     "Select a Year",
                     min = 1976,
                     max = 2019,
                     value = 2015,
                     sep = "")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("plot1")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  fishyear <- reactive(fish.data %>%
                         filter(`Sampling Year` == input$slider1))
   output$plot1 <- renderPlotly({
      fishyear() %>%
       ggplot(aes(x = `Common Name`, y = `Specimen Count`)) +
       geom_bar(stat = "identity",
                aes(fill = mgmtarea)) +
       labs(title = paste(as.character(input$slider1), "Sample Counts of Alaskan Fish"),
            subtitle = "Data from Alaska Fish and Game",
            y = "Count",
            x = "Fish Common Name") +
       theme_calc()
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

