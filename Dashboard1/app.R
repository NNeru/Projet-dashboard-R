#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(gapminder)
library(dplyr)
library(ggplot2)


header<-dashboardHeader()
sidebar<-dashboardSidebar(
    sidebarMenu(
        menuItem(text = "Dashboard",tabName="dashboard"),
        menuItem(text="Inputs",tabName="inputs")
    ),
    
    sliderInput(
        inputId = "height",
        label = "Height",
        min=66,
        max=264,
        value=264)
)
body<-dashboardBody(tabItems(
    tabItem(
        tabName = "dashboard",
        tabBox(title="International Space Station Fun Facts",tabPanel("Fun Fact 1"),tabPanel("Fun Fact 2"))
    ),
    tabItem(tabName = "inputs")
)
)
    


# Define UI for application that draws a histogram
ui <- dashboardPage(header,sidebar,body

    
)

# Define server logic required to draw a histogram
server <- function(input, output) {}

# Run the application 
shinyApp(ui = ui, server = server)
