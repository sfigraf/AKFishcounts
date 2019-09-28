library(tidyverse)
library(lubridate)
library(plotly)
library(shiny)
library(shinydashboard)
library(ggthemes)

##can run the app from any computer if you use the code runGitHub( "AKFishcounts", "sfigraf") after loading the shiny library

###To do
#Use HTML to pretty up the ui
#stop age axis of bar graph shoing decimals
#look at chart icon

data <- read_csv("ADU Specimen Analysis.csv")
data$`Age Bin`[data$`Age Bin` %in% "20-Nov"] <- "11-20" #changes anything read as 20-nov in the age bin column as 11-20 instead
data$`Sampling Year` <- as.character(data$`Sampling Year`)

fish.data <- data %>%
  mutate(`Management Area(s)` = ifelse(is.na(`Management Area(s)`),
                              "Not Specified",
                              `Management Area(s)`))
# fish.data <- fish.data %>%
#   mutate(`Sampling Year` = year(`Sampling Year`))


ui <- dashboardPage(
  
  dashboardHeader(title = "Age Determination Unit",
                  titleWidth = 300,
                  #have to add your image/html in a li wrapper with class dropdown in order to get it to display
                  tags$li(class = "dropdown",
                          tags$a(href="fish_and_gamelogo3.png", target="_blank", 
                                 tags$img(height = "20px", alt="SNAP Logo", src="fish_and_gamelogo3.png")
                          )) 
                  ),
  dashboardSidebar(
    sidebarUserPanel("Alaska Fish and Game",
                     # Image file should be in www/ subdir
                     image = "fish_and_gamelogo3.png"
    ),
    sidebarMenu(
      menuItem("Charts", tabName = "charts", icon = icon("dashboard"),
               menuSubItem("Individual Species", tabName = "subitem1"),
               menuSubItem("All Species Sample Counts", tabName = "subitem2")
      ),
      menuItem("About ADU", tabName = "methods", icon = icon("th"))
    )),
    
    
  dashboardBody(
    tabItems(
            tabItem("subitem2",
              box(sliderInput("slider1",
                          label = tags$span(style = "color:blue", "Select a Year"),
                          min = 1976,
                          max = 2019,
                          value = 2015,
                          sep = ""),
                  width = 3),
              box(plotlyOutput("plot1"),
                  width = 9)),
      tabItem("subitem1",
              box(
                selectInput("select1",
                            label = tags$h4(tags$b("Select a Fish")),
                            choices = sort(unique(fish.data$`Common Name`)))),
              box(sliderInput("slider2",
                              label = tags$h4(tags$b("Select a Year")),
                              min = 1976,
                              max = 2019,
                              value = 2015,
                              sep = "")),
              box(
                plotlyOutput("plot2")
                ),
              box(
                plotOutput("plot3")
              )),
      tabItem("methods",
              includeHTML(paste0(getwd(),"/www/","adfgaboutus1.html"))
      )
      )
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  fishyear <- reactive(fish.data %>%
                         filter(`Sampling Year` == input$slider1) %>%
                        group_by(`Management Area(s)`,`Common Name`) %>% #having two groubys and sumamrizing with sum gets the graph to show aggregated columns
                        summarize(`Specimen Count` = sum(`Specimen Count`)
                                  ) 
  )
  
   output$plot1 <- renderPlotly({
      fishyear() %>%
       ggplot(aes(x = `Common Name`, y = `Specimen Count`)) +
       geom_bar(stat = "identity",
                aes(fill = `Management Area(s)`)) +
       labs(title = paste(as.character(input$slider1), "Counts of Alaskan Fish Aged by ADU"),
            caption = "(based on data from Alaska Fish and Game)",
            y = "Count",
            x = "Fish Common Name") +
       theme_economist() +
       theme(plot.title = element_text(color = "black", size = 16, face = "bold"),
             axis.text.x = element_text(angle = 90, hjust = 1))
    })
   
   specificfish <- reactive(fish.data %>%
                              filter(`Common Name` == input$select1) %>%
                              group_by(`Management Area(s)`,`Sampling Year`) %>%
                              summarize(`Specimen Count` = sum(`Specimen Count`))
                )
   output$plot2 <- renderPlotly({
     specificfish() %>%
       ggplot(aes(x = `Sampling Year`, y = `Specimen Count`)) +
       geom_bar(stat = "identity",
                aes(fill = `Management Area(s)`)) +
       labs(title = paste0("Amount of \"", as.character(input$select1), "\" Aged by ADU"),
            caption = "(based on data from Alaska Fish and Game)",
            y = "Count",
            x = "Year") +
       theme_economist() +
       theme(plot.title = element_text(color = "black", size = 16, face = "bold"),
             axis.text.x = element_text(angle = 90, hjust = 1))
             
   })
   
   specificfishyear <- reactive(fish.data %>%
                              filter(`Common Name` == input$select1,
                                     `Sampling Year` == input$slider2) %>%
                              group_by(`Management Area(s)`,`Age Bin`) %>%
                              summarize(`Specimen Count` = sum(`Specimen Count`)))
   
   output$plot3 <- renderPlot({
     specificfishyear() %>%
       ggplot(aes(x = `Age Bin`,
                  y = `Specimen Count`)) +
       geom_bar(stat = "identity",
                aes(fill = `Management Area(s)`)) +
       labs(title = paste0("Age Distribution of Sampled \"",as.character(input$select1), "\" from ",as.character(input$slider2)),
            caption = "(based on data from Alaska Fish and Game)",
            y = "Count",
            x = "Age Bin") +
       theme_economist() +
       theme(plot.title = element_text(color = "black", size = 16, face = "bold"))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

