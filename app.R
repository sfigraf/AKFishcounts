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
   #tags$h1(id = "title1", "Alaska Fish Counts from 1976 - 2019"),
   #titlePanel("Alaska Fish Counts from 1976 - 2019"),
   #tags$b("This text is bold."),
   #tags$img(src = "fish_and_gamelogo.png", width = "50px", height = "50px"), ##NEED to put images in a another folder called www. Don't know why
   div(
     h1(style="background-color: black; color: white; height: 125px; padding: 10px; margin: 0px",
        
        HTML('<img src="fish_and_gamelogo.png" style="float:right; padding-right:25px" width="125px" height="100px"/>',
             'Alaska Fish Counts <br> 1976-2019'
        ))
   ),
   tags$div(
     tags$blockquote(
       tags$em("Science in the Last Frontier", cite = "Alaska Fish and Game"))
     
   ),
   #div creates a division of an html document
   
   HTML('<hr >'), #this makes a horizontonal line break; also works by tags$hr()
   
   #tags$hr(),
   sidebarLayout(
      sidebarPanel(
         sliderInput("slider1",
                     label = tags$span(style = "color:blue", "Select a Year"),
                     min = 1976,
                     max = 2019,
                     value = 2015,
                     sep = "")
                     #width="200px") #controls width of the slider
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

