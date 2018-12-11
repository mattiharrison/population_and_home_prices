library(shiny)
library(fs)
library(tidyverse)
library(plotly)
library(shinythemes)
library(leaflet)
library(maps)
library(ggplot2)
library(cowplot)

data <- readRDS("la_ct.rds")
plot_1 <- read_rds("plot1.rds")
plot_2 <- read_rds("plot2.rds")
plot_3 <- read_rds("plot3.rds")
plot_4 <- read_rds("plot4.rds")
plot_5 <- read_rds("plot5.rds")
plot_6 <- read_rds("plot6.rds")
plot_7 <- read_rds("plot7.rds")
plot_8 <- read_rds("plot8.rds")
plot_9 <- read_rds("plot9.rds")
plot_10 <- read_rds("plot10.rds")

CA <- map_data("state", region = "california")
CT <- map_data("state", region = "connecticut")

# Define UI for application that draws a histogram
ui <- navbarPage("Population and its Affect on Single-Family Home Prices in LA and Connecticut", theme = shinytheme("flatly"),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                   
                   tabsetPanel(type = "tabs",
                               tabPanel("Intoduction", htmlOutput("introduction"), 
                                        htmlOutput("California"),
                                        plotOutput("CA.1", width = "50%"), leafletOutput("CA.2"), 
                                        htmlOutput("Connecticut"),
                                        plotOutput("CT.1", width = "50%"), leafletOutput("CT.2")),
                               tabPanel("Instructions", htmlOutput("instructions")),
                               tabPanel("Graphs", plotOutput("graph"),
                                        sidebarPanel(
                                          selectInput(
                                            inputId = "graph",
                                            label = "Choose a graph:",
                                            choices = c("LA population" = "Plot1", 
                                                        "LA home prices" = "Plot2",
                                                        "LA pop vs. price" = "Plot9",
                                                        "CT population" = "Plot3", 
                                                        "CT home prices" = "Plot4",
                                                        "CT pop vs. price" = "Plot10",
                                                        "Population Density in LA" = "Plot5", 
                                                        "Available Homes in CT" = "Plot6",
                                                        "Populations and Corresponding Home Prices in LA" = "Plot7", 
                                                        "Populations and Corresponding Home Prices in CT cities" = "Plot8"),
                                            selected = "LA population"
                                          ))),
                               tabPanel("Insights", htmlOutput("insights")))
                 ))

# Define server logic required to draw a histogram
server <- function(input, output) { 
  
  output$introduction <- renderUI({
    
    str1  <- paste("How does population affect home prices?")
    str2  <- paste("LA zipcodes and CT towns were tested to see if there is a relationship between population and home prices and how this relationship differs across coasts.")
    
    HTML(paste(h1(str1), p(str2)))
  })
  
  output$California <- renderUI({
    
    str1  <- paste("California")
    
    HTML(paste(h3(str1)))
  })
  
  output$CA.1 <- renderPlot({
    ggplot(CA, aes(x = long, y = lat)) + geom_polygon()
    
  })
  
  output$CA.2 <- renderPlot({
    leaflet() %>% setView(lng = -118.243683, lat = 34.052235, zoom = 9) %>% 
      addProviderTiles(providers$Esri.NatGeoWorldMap)
  })
  
  output$Connecticut <- renderUI({
    
    str1  <- paste("Connecticut")
    
    HTML(paste(h3(str1)))
  })
  
  output$CT.1 <- renderPlot({
    ggplot(CT, aes(x = long, y = lat)) + geom_polygon()
    
  })
  
  output$CT.2 <- renderPlot({
    leaflet() %>% setView(lng = -72.65065, lat = 41.56232, zoom = 8) %>% 
      addProviderTiles(providers$Esri.NatGeoWorldMap)
    
  })
  
  output$instructions <- renderUI({
    
    str1  <- paste("Summary")
    str2  <- paste("This app shows the population in different LA zipcodes and Connecticut towns in 2010 and their corresponding price for a single family home.")
    str3  <- paste("Instructions") 
    str4  <- paste("Click through the tabs to see the data in different ways and use the drop-down menus to go between different characteristics.")
    str5  <- paste("Notes")
    str6  <- paste("The data had to be cleaned to be merged by zipcode. Zipcodes with 0 as a population were removed from the dataset. Home prices were selected from December of 2012 to keep it consistent across economic policies such as interest rates and recovery from the Great Recession.")
    str7  <- paste("Sources")
    str8  <- paste("Census data was provided by Data.gov and home prices were from Zillow, specifically single-family homes. 
                   https://catalog.data.gov/dataset/2010-census-populations-by-zip-code, 
                   https://catalog.data.gov/dataset/2010-population-by-town,
                   https://www.zillow.com/research/data/")
    
    HTML(paste(h3(str1), p(str2), h3(str3), p(str4), h3(str5), p(str6), h3(str7), p(str8)))
  })
  
  output$graph <- renderPlot({   
    if (input$graph == "Plot1") return(plot_1)
    else if (input$graph == "Plot2") return(plot_2)
    else if (input$graph == "Plot3") return(plot_3)
    else if (input$graph == "Plot4") return(plot_4)
    else if (input$graph == "Plot5") return(plot_5)
    else if (input$graph == "Plot6") return(plot_6)
    else if (input$graph == "Plot7") return(plot_7)
    else if (input$graph == "Plot8") return(plot_8)
    else if (input$graph == "Plot9") return(plot_9)
    else if (input$graph == "Plot10") return(plot_10)
    
  })
  
  output$insights <- renderUI({
    
    str1 <- paste("Los Angeles")
    str2 <- paste("As population size for a specific zipcode increases, the household size increases, but as people get older their household size shrinks.  
                  This makes sense, children are leaving their parents homes and moving to homes with roommates or their families. As population increases, the price of homes decreases. 
                  This also makes sense because more homes have to fit in a smaller area to accomidate for the large population. Specifically the most expensive homes have the smallest number of occupants.")
    str3 <- paste("Connecticut") 
    str4 <- paste("As population increases in Connecticut, the more number of vacant homes there are in those specific cities. 
                  One reason for this is that companies are developing the city, thinking that even more people are going to move there. 
                  They are being preemptive in their business decisions. As the population increases, the prices of homes decrease.")
    str5 <- paste("Relating the Areas")
    str6 <- paste("In both LA and all of Connecticut, as the population increases, the prices of homes decrease. This relationship seems to be stronger in LA, but the relationship is still there for Connecticut. 
                  I think this has to do with the fact that Connecticut has more residential areas or suburbs, while LA is mostly city. 
                  The communities have to accomodate the growing proportion of people relative to the total space of the community.
                  Other reasons for this include the age of the areas, the average number of households, and specific jobs that are offered in those communities.")
    
    HTML(paste(h3(str1), p(str2), h3(str3), p(str4), h3(str5), p(str6)))})
}


# Run the application 
shinyApp(ui = ui, server = server)
