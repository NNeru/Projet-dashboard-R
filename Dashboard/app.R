
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
sidebar<-dashboardSidebar
body<-dashboardBody
# Define UI for application that draws a histogram
ui <- fluidPage(header,sidebar,body,

    # Application title
    titlePanel("Influence de la température sur les catastrophes naturelles"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("Year",
                        "Years:",
                        min = 2016,
                        max = 2020,
                        value = c(2016,2017,2018,2019,2020))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        ggplot(data=subset(catnat,format(as.Date(catnat$Date_arreté),"%Y")==2016)%>%count(Type_de_catnat),aes(x=n,y=Type_de_catnat))+geom_bar(stat="identity")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
