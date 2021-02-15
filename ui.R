library(lubridate)
library(gganimate)
library(shiny)
library(ggplot2)
library(raster)
library(shinydashboard)
library(gifski)
library(png)
library(tidyverse)
theme_set(theme_bw())

dashboardPage(
    title='Température et conséquences climatiques',
    skin="purple",
    dashboardHeader(title="Dashboard",titleWidth = "300px"),
    dashboardSidebar(width = "300px",
                     sidebarMenu(
                         menuItem("Rechauffement Climatique", tabName = "recclim", icon = icon("dashboard")),
                         menuItem("Température/différents facteurs", icon = icon("chart-bar"), tabName = "tempe"),
                         menuItem("Catastrophes naturelles", icon = icon("chart-bar"), tabName = "catnat"),
                         menuItem("Temperature/Catastrophes", icon = icon("chart-bar"), tabName = "tempecatnat")
                     ) 
    ),
    dashboardBody(
        tags$head(
            tags$style(HTML(".main-sidebar { font-size: 18px;}"))
        ),
        
        tabItems( 
            
            ##########################RECHAUFFEMENT CLIMATIQUE############################
            tabItem(tabName = "recclim",
                    h1("Réchauffement climatique ou non ?",style =      
                           "color:navy"),
                    fluidRow( 
                        box(title="Moyenne des températures par Région",background = "black",width = 7, solidHeader = TRUE,plotOutput("plot", height = 250)),
                        box(
                            title = "Année",background = "black",width = 5,
                            sliderInput("Date", "Annee:", value=2016,min=2016,max=2020,step=1)
                        )
                        
                    ),
                    fluidRow(
                        box(
                            title = "Ecart de température rapport à la normale de saison - Automne 13.1°c",width = 6,background = "red",
                            solidHeader = TRUE,plotOutput("automne", height = 250)),
                        box(
                            title = "Ecart de température rapport à la normale de saison - Hiver 5.4°c",width = 6,background = "teal",
                            solidHeader = TRUE,plotOutput("hiver", height = 250)
                        )
                    ),
                    
                    fluidRow(
                        box(
                            title = "Ecart de température rapport à la normale de saison - Printemps 11.6°c",width = 6,background = "green",
                            solidHeader = TRUE,plotOutput("printemps", height = 250)),
                        box(
                            title = "Ecart de température rapport à la normale de saison - Eté  19.9°c",width = 6,background = "orange",
                            solidHeader = TRUE,plotOutput("ete", height = 250)
                        )
                    ),
                    fluidRow(
                        tabBox(title = "Temperature par mois",width = 12,
                               tabPanel("2016",plotOutput("tempeMonthV2016", height = 250)),
                               tabPanel("2017",plotOutput("tempeMonthV2017", height = 250)),
                               tabPanel("2018",plotOutput("tempeMonthV2018", height = 250)),
                               tabPanel("2019",plotOutput("tempeMonthV2019", height = 250)),
                               tabPanel("2020",plotOutput("tempeMonthV2020", height = 250))
                        )
                    )
                    
                    
            ),
            ################################################################################
            
            ##############CLIMAT ET TEMPERATURE############################################
            tabItem(tabName = "tempe",
                    h1("La température en relation avec différents facteurs",style =      
                           "color:navy"),
                    fluidRow( 
                        box(title="Vitesse du vent maximale/Temperature",background = "lime",width = 12, solidHeader = TRUE,imageOutput("tempe_vent"),height = 560
                        )),
                    fluidRow( 
                        box(title="Hauteur de précipitation (en mm)/Temperature",background = "blue",width = 12, solidHeader = TRUE,imageOutput("tempe_pluie"),height = 560
                        )),
                    fluidRow( 
                        box(title="Ensoleillement (en heures)/Temperature",background = "orange",width = 12, solidHeader = TRUE,imageOutput("tempe_soleil"),height = 560
                        )
                    )
            ),
            ################################################################################
            
            #########################EVOLUTION DES CATNAT PAR ANNEE#########################
            tabItem(tabName="catnat",
                    h1("Comment les catastrophes naturelles évoluent-elles au cours des années ?",style =      
                           "color:navy"),
                    fluidRow(
                        box(title="Nombre de catastrophes naturelles par année",background = "blue", solidHeader = TRUE,plotOutput("catnatyear", height = 250)),
                        tabBox(title = "Catastrophe naturelle chaque année",
                               tabPanel("2016",plotOutput("catnat2016", height = 250)),
                               tabPanel("2017",plotOutput("catnat2017", height = 250)),
                               tabPanel("2018",plotOutput("catnat2018", height = 250)),
                               tabPanel("2019",plotOutput("catnat2019", height = 250)),
                               tabPanel("2020",plotOutput("catnat2020", height = 250))
                        )
                        # box(
                        # title = "Régions",background = "green",
                        # selectInput("Region","Regions",choices = regs, selected = regs[0])
                        # )
                    ),
                    fluidRow(
                        box(title="Nombre de catastrophes par type",background = "blue", width = 7, solidHeader = TRUE,plotOutput("typeyears", height = 250)),
                        box(
                            title = "Types",background = "blue",width = 5,
                            selectInput("Types","Types",choices = types, selected = types[0])
                        )
                        
                    )
            ),
            ################################################################################
            
            #########################EVOLUTION DES CATNAT PAR ANNEE#########################
            tabItem(tabName="tempecatnat",
                    h1("Les catastrophes naturelles sont-elles correlées à la température ?",style =      
                           "color:navy"),
                    
                    fluidRow(
                        tabBox(title = "Catastrophe naturelle par mois",
                               tabPanel("2016",plotOutput("catnatMonth2016", height = 250)),
                               tabPanel("2017",plotOutput("catnatMonth2017", height = 250)),
                               tabPanel("2018",plotOutput("catnatMonth2018", height = 250)),
                               tabPanel("2019",plotOutput("catnatMonth2019", height = 250)),
                               tabPanel("2020",plotOutput("catnatMonth2020", height = 250))
                        ),
                        tabBox(title = "Temperature par mois",
                               tabPanel("2016",plotOutput("tempeMonth2016", height = 250)),
                               tabPanel("2017",plotOutput("tempeMonth2017", height = 250)),
                               tabPanel("2018",plotOutput("tempeMonth2018", height = 250)),
                               tabPanel("2019",plotOutput("tempeMonth2019", height = 250)),
                               tabPanel("2020",plotOutput("tempeMonth2020", height = 250))
                        )
                    )
                    
            )
        )
    )
)