#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)
library(leaflet)
library(shinydashboard)
library(leaflet.extras)
df<- read_csv("SPD_Crime_Data__2008-Present.csv") %>%
  mutate(year = substr(`Report Number`, start = 1, stop = 4)) %>%
  filter(year %in% c("2022","2021","2020","2019")) %>%
  filter(Longitude < -120 & Longitude > -125, Latitude > 46 & Latitude < 48)
  



# Define UI for application that draws a histogram

ui <- dashboardPage(
  dashboardHeader(title = "Seattle Crimes"),
  dashboardSidebar("Use this sidebar to customize the map to the right. Specify years of interest",checkboxGroupInput("offense", "Select which offenses to include (2019-2022)", 
                                      choices = unique(df$`Offense Parent Group`), selected = "ROBBERY"),
                   checkboxGroupInput("year","Select which years to include", choices = c("2019", "2020", "2021",
                                                                                          "2022"), selected = "2022")

    
  ),
  dashboardBody(
    fluidRow(box("This interactive Shiny App allows you to explore crime occurences in the Seattle Area from 2019-2022. Use the sidebar menu to the left to toggle options for what crimes you want to be mapped below. Be warned that choosing many cases to display will cause the visualization to lag.", width = "100%")),
    
    
    fluidRow(
      box(leafletOutput("mymap2"),width = "50%")
      

    ),
    fluidRow(br(),box("Use this visualization to explore changes for a specific crime across different neighborhoods")),
    fluidRow(
     box(selectInput('xcol', 'Type of Offense', choices = unique(df$`Offense Parent Group`)))
    ),
    fluidRow(box(plotOutput("plot1",height= 450), width = 600)),
    
    fluidRow(br(),box("Use this visualization to explore changes in crime by a specific neighborhood")),
    
    fluidRow(box(selectInput('ycol', 'MCPP', choices = unique(df$MCPP)))),
    fluidRow(box(plotOutput("plot2", height = 450), width = 600))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    
 
    df %>%
      filter(`Offense Parent Group` == input$xcol) %>%
      group_by(MCPP,year) %>%
      summarise(n = n())
  })
  
  
  selectedData2 <- reactive({
    
    
    df %>%
      filter(MCPP == input$ycol) %>%
      group_by(`Offense Parent Group`,year) %>%
      summarise(n = n())
  })
 
    selectedDatamap <- reactive({ 
      if(input$xcol == "All"){
        df %>% filter(Longitude < 0)
      }else{
      df %>%  filter(`Offense Parent Group` == input$xcol)}})
  

    selectedDatamap2 <- reactive({ 
      df %>%
      filter(`Offense Parent Group` %in% input$offense,
             year %in% input$year)
      
   })
  
  output$plot1 <- renderPlot({
    
    
    ggplot(data = selectedData(), aes(x = MCPP, y = n, fill = year)) + geom_bar(stat = "identity", position = "dodge") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + xlab("Neighborhood") + ylab("Count")
  })
  
  
  
  pal <- colorFactor(
    palette = 'Dark2',
    domain = df$`Offense Parent Group`
  )
  
  output$mymap2 <- renderLeaflet({
    leaflet(data = selectedDatamap2()) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(lng = ~Longitude, lat = ~Latitude, radius = 7,  color = ~pal(`Offense Parent Group`),
                       opacity = .2)  %>%
      addLegend(pal = pal, values = ~`Offense Parent Group`, position = "bottomleft") 
    
  })
  
  
  output$plot2 <- renderPlot({
    
    
    ggplot(data = selectedData2(), aes(x = `Offense Parent Group`, y = n, fill = year)) + geom_bar(stat = "identity", position = "dodge") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ xlab("Type of Crime") + ylab("Count")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
