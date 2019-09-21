library(tidyverse)
library(lubridate)
library(plotly)
library(shiny)
library(shinydashboard)
library(ggthemes)

##can run the app from any computer if you use the code runGitHub( "AKFishcounts", "sfigraf") after loading the shiny library

###To do
#Use HTML to pretty up the ui
#change it all to shinyDashboard but keep the adfg logo
#make graph looking better
#have an optino to sort by species
#tab that desrcibes technique or gathering data

data <- read_csv("ADU Specimen Analysis.csv")

fish.data <- data %>%
  mutate(mgmtarea = ifelse(is.na(`Management Area(s)`),
                              "Not Specified",
                              `Management Area(s)`))
# fish.data <- fish.data %>%
#   mutate(`Sampling Year` = year(`Sampling Year`))


ui <- dashboardPage(
  
  dashboardHeader(title = "Alaska Fish Counts",
                  #have to add your image/html in a li wrapper with class dropdown in order to get it to display
                  tags$li(class = "dropdown",
                          tags$a(href="fish_and_gamelogo3.png", target="_blank", 
                                 tags$img(height = "20px", alt="SNAP Logo", src="fish_and_gamelogo3.png")
                          )) 
                  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Charts", tabName = "charts", icon = icon("dashboard"),
               menuSubItem("Sub-item 1", tabName = "subitem1"),
               menuSubItem("Sub-item 2", tabName = "subitem2")
      ),
      menuItem("Data Methods", tabName = "methods", icon = icon("th"))
    )),
    
    
  dashboardBody(
    tabItems(
            tabItem("subitem1",
              box(sliderInput("slider1",
                          label = tags$span(style = "color:blue", "Select a Year"),
                          min = 1976,
                          max = 2019,
                          value = 2015,
                          sep = "")),
              box(plotlyOutput("plot1"))),
      tabItem("subitem2",
              box(
                selectInput("select1",
                            label = tags$h2("select a fish"),
                            choices = unique(fish.data$`Common Name`))
              ),
              box(
                plotlyOutput("plot2")))
      )
    )
  )
 
# ui <- fluidPage(
#   
#    
#    # Application title
#    
#    
#    #tags$hr(),
#    sidebarLayout(
#       sidebarPanel(
#          sliderInput("slider1",
#                      label = tags$span(style = "color:blue", "Select a Year"),
#                      min = 1976,
#                      max = 2019,
#                      value = 2015,
#                      sep = ""),
#                      #width="200px") #controls width of the slider
#          selectInput("select1",
#                      label = tags$h2("select a fish"),
#                      choices = unique(fish.data$`Common Name`))
#       ),
#       
#       # Show a plot of the generated distribution
#       mainPanel(
#          plotlyOutput("plot1")
#       )
#    )
# )

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  fishyear <- reactive(fish.data %>%
                         filter(`Sampling Year` == input$slider1) %>%
                        group_by(`mgmtarea`,`Common Name`) %>% #having two groubys and sumamrizing with sum gets the graph to show aggregated columns
                        summarize(`Specimen Count` = sum(`Specimen Count`)
                                  ) 
  )
  
   output$plot1 <- renderPlotly({
      fishyear() %>%
       ggplot(aes(x = `Common Name`, y = `Specimen Count`)) +
       geom_bar(stat = "identity",
                aes(fill = mgmtarea)) +
       labs(title = paste(as.character(input$slider1), "Sample Counts of Alaskan Fish"),
            caption = "(based on data from Alaska Fish and Game)",
            y = "Count",
            x = "Fish Common Name") +
       theme_economist() +
       theme(plot.title = element_text(color = "black", size = 16, face = "bold"),
             axis.text.x = element_text(angle = 90, hjust = 1))
      
   })
   
   specificfish <- reactive(fish.data %>%
                              filter(`Common Name` == input$select1) %>%
                              group_by(`mgmtarea`,`Sampling Year`) %>%
                              summarize(`Specimen Count` = sum(`Specimen Count`))
                )
   output$plot2 <- renderPlotly({
     specificfish() %>%
       ggplot(aes(x = `Sampling Year`, y = `Specimen Count`)) +
       geom_bar(stat = "identity",
                aes(fill = mgmtarea)) +
       labs(title = paste(as.character(input$select1), "Sample Counts"),
            caption = "(based on data from Alaska Fish and Game)",
            y = "Count",
            x = "Year") +
       theme_economist() +
       theme(plot.title = element_text(color = "black", size = 16, face = "bold"),
             axis.text.x = element_text(angle = 90, hjust = 1))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

