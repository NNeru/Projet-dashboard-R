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

function(input, output) {
    
    ##################RECHAUFFEMENT CLIMATIQUE############################
    #-  Temperature Moyenne par Region bar_plot  -#
    output$plot <- renderPlot({
        
        temperature_by_year_by_Region %>%
            filter(Annee==input$Date) %>%
            ggplot(aes(x=Moy, y=Region)) +
            geom_bar(stat='identity') +
            labs(x="Température moyenne (en°C)",y="Region")+
            xlim(0,20)
    })
    #-  Histogrammes des 4 saisons  -#
    output$automne <- renderPlot({
        
        tempAut %>%
            ggplot() +  geom_histogram(mapping = aes(x=Date,    y=EcartTemperature),stat='identity',alpha=0.85,fill=ifelse(tempAut$EcartTemperature<0,'lightblue','#FF6666')) +
            labs(x="Année",y="Ecart (en °C)")+
            ylim(-1,5.6)
    })
    output$hiver <- renderPlot({
        tempHiv %>%
            ggplot() +
            geom_histogram(mapping = aes(x=Date, y=EcartTemperature),stat='identity',alpha=0.85,fill=ifelse(tempHiv$EcartTemperature<0,'lightblue','#FF6666')) +
            labs(x="Année",y="Ecart (en °C)")+
            ylim(-1,5)
    })
    output$printemps <- renderPlot({
        tempPrint %>%
            ggplot() +
            geom_histogram(mapping = aes(x=Date, y=EcartTemperature),stat='identity',alpha=0.85,fill=ifelse(tempPrint$EcartTemperature<0,'lightblue','#FF6666')) +
            labs(x="Année",y="Ecart (en °C)")+
            ylim(-1,5)
    })
    output$ete <- renderPlot({
        tempEte %>%
            ggplot() +
            geom_histogram(mapping = aes(x=Date, y=EcartTemperature),stat='identity',alpha=0.85,fill=ifelse(tempEte$EcartTemperature<0,'lightblue','#FF6666')) +
            labs(x="Année",y="Ecart (en °C)")+
            ylim(-1,5)
    })
    
    #-Affichage des temperature par mois(VIOLON)#
    output$tempeMonthV2016 <- renderPlot({
        ggplot(temperature_by_day_2016,aes(x=Mois, y=Temperature)) +
            theme(axis.text.x=element_text(angle=90,hjust=1))+
            geom_violin(trim = TRUE)+geom_boxplot(width=0.3)+
            labs(x="Mois",y="Température (en °C)")+
            ylim(0,26)
    })
    output$tempeMonthV2017 <- renderPlot({
        ggplot(temperature_by_day_2017,aes(x=Mois, y=Temperature)) +
            theme(axis.text.x=element_text(angle=90,hjust=1))+
            geom_violin(trim = TRUE)+geom_boxplot(width=0.3)+
            labs(x="Mois",y="Température (en °C)")+
            ylim(0,26)
    })
    output$tempeMonthV2018 <- renderPlot({
        ggplot(temperature_by_day_2018,aes(x=Mois, y=Temperature)) +
            theme(axis.text.x=element_text(angle=90,hjust=1))+
            geom_violin(trim = TRUE)+geom_boxplot(width=0.3)+
            labs(x="Mois",y="Température (en °C)")+
            ylim(0,26)
    })
    output$tempeMonthV2019 <- renderPlot({
        ggplot(temperature_by_day_2019,aes(x=Mois, y=Temperature)) +
            theme(axis.text.x=element_text(angle=90,hjust=1))+
            geom_violin(trim = TRUE)+geom_boxplot(width=0.3)+
            labs(x="Mois",y="Température (en °C)")+
            ylim(0,26)
    })
    output$tempeMonthV2020 <- renderPlot({
        ggplot(temperature_by_day_2020,aes(x=Mois, y=Temperature)) +
            theme(axis.text.x=element_text(angle=90,hjust=1))+
            geom_violin(trim = TRUE)+geom_boxplot(width=0.3)+
            labs(x="Mois",y="Température (en °C)")+
            ylim(0,26)
    })
    ############################################################################
    
    
    ###########CLIMAT ET TEMPERATURE############################################
    output$tempe_vent <- renderImage({
        outfile<-tempfile(fileext = '.gif')
        
        p = ggplot(tempe_climat,aes(x=Temp_moy, y=Vitesse_max_vent, size=Nombre_catnat)) +
            geom_point(aes(colour=Region),alpha=0.7)+
            scale_size(name="Nombre de catastrophes naturelles",range = c(2, 15)) +
            labs(x = "Température moyenne", y = "Vitesse maximale du vent atteinte (en km/h)")+transition_time(Annee) +
            labs(title = "Annee: {frame_time}")+
            shadow_wake(wake_length = 0.05, alpha = 0.8)+
            view_follow(fixed_y = TRUE)
        
        anim_save("outfile.gif",gganimate::animate(p))
        
        list(src = "outfile.gif",
             contentType = 'image/gif')
    })
    
    output$tempe_pluie <- renderImage({
        outfile2<-tempfile(fileext = '.gif')
        
        p = ggplot(tempe_climat,aes(x=Temp_moy, y=Hauteur_precipitation_mm, size=Nombre_catnat)) +
            geom_point(aes(colour=Region),alpha=0.7)+
            scale_size(name="Nombre de catastrophes naturelles",range = c(2, 15)) +
            labs(x = "Température moyenne", y = "Hauteur des précipitations (en mm)")+transition_time(Annee) +
            labs(title = "Annee: {frame_time}")+
            shadow_wake(wake_length = 0.05, alpha = 0.8)+
            view_follow(fixed_y = TRUE)
        
        anim_save("outfile2.gif",gganimate::animate(p))
        
        list(src = "outfile2.gif",
             contentType = 'image/gif')
    })
    
    output$tempe_soleil <- renderImage({
        outfile3<-tempfile(fileext = '.gif')
        
        p = ggplot(tempe_climat,aes(x=Temp_moy, y=Heures_ensoleillement, size=Nombre_catnat)) +
            geom_point(aes(colour=Region),alpha=0.7)+
            scale_size(name="Nombre de catastrophes naturelles",range = c(2, 15)) +
            labs(x = "Température moyenne", y = "Ensoleillement (en heures)")+transition_time(Annee) +
            labs(title = "Annee: {frame_time}")+
            shadow_wake(wake_length = 0.05, alpha = 0.8)+
            view_follow(fixed_y = TRUE)
        
        anim_save("outfile3.gif",gganimate::animate(p))
        
        list(src = "outfile3.gif",
             contentType = 'image/gif')
    })
    
    
    ############################################################################
    
    #########################EVOLUTION DES CATNAT PAR ANNEE#####################
    
    #-  Nombre de catastrophe par année  -#
    output$catnatyear <- renderPlot({
        nb_catnatYears %>%
            ggplot() +
            geom_histogram(mapping = aes(x=Annee, y=n),stat='identity',alpha=0.85,fill='lightblue')+
            labs(x="Année",y="Nombre de catastrophe")
    })
    
    #-  Type de catastrophe par année  -#
    output$typeyears <- renderPlot({
        typeYears %>%
            filter(Type_de_catnat==input$Types) %>%
            ggplot() +
            geom_histogram(aes(x=Annee, y=n),stat='identity',color='blue',fill='black')+
            labs(x="Année",y="Nombre de catastrophe")
    })
    
    #-  Carte des catastrophe naturelle par région  -#
    output$catnat2016 <- renderPlot({
        spplot(formes, "n", col.regions=couleurs(30),  main=list(label="Nombre de catnat par régions 2016",cex=.8))
    })
    output$catnat2017 <- renderPlot({
        spplot(formes2017, "n", col.regions=couleurs(30),  main=list(label="Nombre de catnat par régions 2017",cex=.8))
    })
    output$catnat2018 <- renderPlot({
        spplot(formes2018, "n", col.regions=couleurs(30),  main=list(label="Nombre de catnat par régions 2018",cex=.8))
    })
    output$catnat2019 <- renderPlot({
        spplot(formes2019, "n", col.regions=couleurs(30),  main=list(label="Nombre de catnat par régions 2019",cex=.8))
    })
    output$catnat2020 <- renderPlot({
        spplot(formes2020, "n", col.regions=couleurs(30),  main=list(label="Nombre de catnat par régions 2020",cex=.8))
    })
    
    ############################################################################
    
    #######################TEMPERATURE ET CATASTROPHE#####################################
    
    #- Catastrophe naturelle par mois  -# 
    output$catnatMonth2016 <- renderPlot({
        catnat_months_2016 %>%
            ggplot() +
            theme(axis.text.x=element_text(angle=90,hjust=1))+
            geom_histogram(aes(x=Mois, y=n),stat='identity',alpha=0.85,fill='lightblue',color='black')+
            labs(x="Mois",y="Nombre de catastrophe")+
            ylim(0,2000)
    })
    output$catnatMonth2017 <- renderPlot({
        catnat_months_2017 %>%
            ggplot() +
            theme(axis.text.x=element_text(angle=90,hjust=1))+
            geom_histogram(aes(x=Mois, y=n),stat='identity',alpha=0.85,fill='lightblue',color='black')+
            labs(x="Mois",y="Nombre de catastrophe")+
            ylim(0,2000)
    })
    output$catnatMonth2018 <- renderPlot({
        catnat_months_2018 %>%
            ggplot() +
            theme(axis.text.x=element_text(angle=90,hjust=1))+
            geom_histogram(aes(x=Mois, y=n),stat='identity',alpha=0.85,fill='lightblue',color='black')+
            labs(x="Mois",y="Nombre de catastrophe")+
            ylim(0,2000)
    })
    output$catnatMonth2019 <- renderPlot({
        catnat_months_2019 %>%
            ggplot() +
            theme(axis.text.x=element_text(angle=90,hjust=1))+
            geom_histogram(aes(x=Mois, y=n),stat='identity',alpha=0.85,fill='lightblue',color='black')+
            labs(x="Mois",y="Nombre de catastrophe")+
            ylim(0,2000)
    })
    output$catnatMonth2020 <- renderPlot({
        catnat_months_2020 %>%
            ggplot() +
            theme(axis.text.x=element_text(angle=90,hjust=1))+
            geom_histogram(aes(x=Mois, y=n),stat='identity',alpha=0.85,fill='lightblue',color='black')+
            labs(x="Mois",y="Nombre de catastrophe")+
            ylim(0,2000)
    })
    
    
    #-  Affichage des temperature par mois  -#
    output$tempeMonth2016 <- renderPlot({
        temperature_by_month_2016 %>%
            ggplot() +
            theme(axis.text.x=element_text(angle=90,hjust=1))+
            geom_histogram(aes(x=Mois, y=Temp_moy),stat='identity',alpha=0.85,fill='red',color='black')+
            labs(x="Mois",y="Température moyenne (en°C)")+
            ylim(0,26)
    })
    output$tempeMonth2017 <- renderPlot({
        temperature_by_month_2017 %>%
            ggplot() +
            theme(axis.text.x=element_text(angle=90,hjust=1))+
            geom_histogram(aes(x=Mois, y=Temp_moy),stat='identity',alpha=0.85,fill='red',color='black')+
            labs(x="Mois",y="Température moyenne (en°C)")+
            ylim(0,26)
    })
    output$tempeMonth2018 <- renderPlot({
        temperature_by_month_2018 %>%
            ggplot() +
            theme(axis.text.x=element_text(angle=90,hjust=1))+
            geom_histogram(aes(x=Mois, y=Temp_moy),stat='identity',alpha=0.85,fill='red',color='black')+
            labs(x="Mois",y="Température moyenne (en°C)")+
            ylim(0,26)
    })
    output$tempeMonth2019 <- renderPlot({
        temperature_by_month_2019 %>%
            ggplot() +
            theme(axis.text.x=element_text(angle=90,hjust=1))+
            geom_histogram(aes(x=Mois, y=Temp_moy),stat='identity',alpha=0.85,fill='red',color='black')+
            labs(x="Mois",y="Température moyenne (en°C)")+
            ylim(0,26)
    })
    output$tempeMonth2020 <- renderPlot({
        temperature_by_month_2020 %>%
            ggplot() +
            theme(axis.text.x=element_text(angle=90,hjust=1))+
            geom_histogram(aes(x=Mois, y=Temp_moy),stat='identity',alpha=0.85,fill='red',color='black')+
            labs(x="Mois",y="Température moyenne (en°C)")+
            ylim(0,26)
    })
    ############################################################################
    
    deleteFile = TRUE
    
}