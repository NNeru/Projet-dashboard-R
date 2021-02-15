#Librairie

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

#Importation des données de température et de catastrophes naturelles

temperature<-read.csv('temperature-quotidienne-regionale.csv', header = TRUE,sep=";",fileEncoding = 'utf8')
catnat<-read.csv('catnat_gaspar.csv',header=TRUE,sep=";",fileEncoding = 'utf8')
climat<-read.csv('Climat_csv.csv',header=TRUE,sep=";",fileEncoding = 'utf8')

#rename columns data frame temperature
names(temperature)<-c('Date','Code_postale','Region','Température_Min_(°C)','Température_Max_(°C)','Température_Moy_(°C)')

#rename columns data frame catnat
names(catnat)<-c('Code_nationale_catnat','Code_postale','Commune','Num_risque','Type_de_catnat','Date_début','Date_fin','Date','Date_pub_jo','Date_maj')

#rename columns data frame climat
names(climat)<-c('Region','Vitesse_max_vent','Temperature_max','Heures_ensoleillement','Temperature_min','Hauteur_precipitation_mm','Annee')

catnat<-catnat[,-10]
catnat<-catnat[,-9]
catnat<-catnat[,-6]
catnat<-catnat[,-6]

#mettre les dates au bon format
temperature$Date<-as.Date(temperature$Date)
catnat$Date<-as.POSIXct(catnat$Date)
idxIDF <- which(temperature[,"Region"] == "Ile-de-France")
temperature[idxIDF,"Region"] = "Île-de-France"
idxGE <- which(temperature[,"Region"] == "Grand-Est")
temperature[idxGE,"Region"] = "Grand Est"

#dataframe avec les types de catastrophes naturelles
type_cat<-catnat[,4:5]
s<-type_cat %>% group_by(Num_risque)%>% slice(1)
type_cat<-s
rm(s)

#Dataframe des températures moeynnes par année par régions

temperature_by_year_by_Region<-temperature%>%group_by(Region,Annee=floor_date(temperature$Date,"12 months"))%>%summarize(Moy=mean(`Température_Moy_(°C)`))
idx2016<-which(temperature_by_year_by_Region[,"Annee"]=="2016-01-01")
idx2017<-which(temperature_by_year_by_Region[,"Annee"]=="2017-01-01")
idx2018<-which(temperature_by_year_by_Region[,"Annee"]=="2018-01-01")
idx2019<-which(temperature_by_year_by_Region[,"Annee"]=="2019-01-01")
idx2020<-which(temperature_by_year_by_Region[,"Annee"]=="2020-01-01")

#Transformation des dates en année
temperature_by_year_by_Region[,"Annee"] <- sapply(temperature_by_year_by_Region[,"Annee"], as.character)
temperature_by_year_by_Region[idx2016,"Annee"] = "2016"
temperature_by_year_by_Region[idx2017,"Annee"] = "2017"
temperature_by_year_by_Region[idx2018,"Annee"] = "2018"
temperature_by_year_by_Region[idx2019,"Annee"] = "2019"
temperature_by_year_by_Region[idx2020,"Annee"] = "2020"

#Dataframe des températures moyennes par mois par régions
temperature_by_month_by_Region<-temperature%>%group_by(Region,Date=floor_date(temperature$Date,"31 days"))%>%summarize(Moy=mean(`Température_Moy_(°C)`))

#Dataframe des températures moyennes de chaque mois de chaque année 
temperature_by_month<-temperature_by_month_by_Region%>%group_by(Date)%>%summarize(Moy=mean(Moy))
temperature_by_month_2016<-temperature_by_month%>%filter(temperature_by_month$Date<="2016-12-31")
temperature_by_month_2017<-temperature_by_month%>%filter(temperature_by_month$Date>"2016-12-31"&temperature_by_month$Date<="2017-12-31")
temperature_by_month_2018<-temperature_by_month%>%filter(temperature_by_month$Date>"2017-12-31"&temperature_by_month$Date<="2018-12-31")
temperature_by_month_2019<-temperature_by_month%>%filter(temperature_by_month$Date>"2018-12-31"&temperature_by_month$Date<="2019-12-31")
temperature_by_month_2020<-temperature_by_month%>%filter(temperature_by_month$Date>"2019-12-31"&temperature_by_month$Date<="2020-12-31")
names(temperature_by_month_2016)<-c('Mois','Temp_moy')
names(temperature_by_month_2017)<-c('Mois','Temp_moy')
names(temperature_by_month_2018)<-c('Mois','Temp_moy')
names(temperature_by_month_2019)<-c('Mois','Temp_moy')
names(temperature_by_month_2020)<-c('Mois','Temp_moy')
temperature_by_month_2016$Mois <- as.character(temperature_by_month_2016$Mois)
temperature_by_month_2017$Mois <- as.character(temperature_by_month_2017$Mois)
temperature_by_month_2018$Mois <- as.character(temperature_by_month_2018$Mois)
temperature_by_month_2019$Mois <- as.character(temperature_by_month_2019$Mois)
temperature_by_month_2020$Mois <- as.character(temperature_by_month_2020$Mois)

#tempereture moyenne par jour par région 
temperature_by_day_by_Region <- temperature%>%group_by(Region,Date)%>%summarize(Moy=mean(`Température_Moy_(°C)`))

#temperature moyenne par jour de chaque année
temperature_by_day<-temperature_by_day_by_Region%>%group_by(Date)%>%summarize(Moy=mean(Moy))
names(temperature_by_day)<-c('Mois','Temperature')

#Création de dataframe pour chaque année pour utilisation de tabBox
temperature_by_day_2016<-temperature_by_day%>%filter(temperature_by_day$Mois<="2016-12-31")
temperature_by_day_2016$Mois<- format(temperature_by_day_2016$Mois,"%Y-%m")
temperature_by_day_2017<-temperature_by_day%>%filter(temperature_by_day$Mois>"2016-12-31"&temperature_by_day$Mois<="2017-12-31")
temperature_by_day_2017$Mois<- format(temperature_by_day_2017$Mois,"%Y-%m")
temperature_by_day_2018<-temperature_by_day%>%filter(temperature_by_day$Mois>"2017-12-31"&temperature_by_day$Mois<="2018-12-31")
temperature_by_day_2018$Mois<- format(temperature_by_day_2018$Mois,"%Y-%m")
temperature_by_day_2019<-temperature_by_day%>%filter(temperature_by_day$Mois>"2018-12-31"&temperature_by_day$Mois<="2019-12-31")
temperature_by_day_2019$Mois<- format(temperature_by_day_2019$Mois,"%Y-%m")
temperature_by_day_2020<-temperature_by_day%>%filter(temperature_by_day$Mois>"2019-12-31"&temperature_by_day$Mois<="2020-12-31")
temperature_by_day_2020$Mois<- format(temperature_by_day_2020$Mois,"%Y-%m")

temperature_by_day_2016$Mois <- as.character(temperature_by_day_2016$Mois)
temperature_by_day_2017$Mois <- as.character(temperature_by_day_2017$Mois)
temperature_by_day_2018$Mois <- as.character(temperature_by_day_2018$Mois)
temperature_by_day_2019$Mois <- as.character(temperature_by_day_2019$Mois)
temperature_by_day_2020$Mois <- as.character(temperature_by_day_2020$Mois)

#temperature moyenne par saison par année


#2016
tempAut2016<-filter(temperature_by_day_by_Region,Date<="2016-11-30" & Date>="2016-09-01")

tempHiv2016<-filter(temperature_by_day_by_Region,Date<="2016-03-01" & Date>="2016-01-01")

tempPrint2016<-filter(temperature_by_day_by_Region,Date<="2016-05-31" & Date>="2016-03-02")

tempEte2016<-filter(temperature_by_day_by_Region,Date<="2016-08-31" & Date>="2016-06-01")

#2017
tempAut2017<-filter(temperature_by_day_by_Region,Date<="2017-11-30" & Date>="2017-09-01")

tempHiv2017<-filter(temperature_by_day_by_Region,Date<="2017-03-01" & Date>="2017-01-01")

tempPrint2017<-filter(temperature_by_day_by_Region,Date<="2017-05-31" & Date>="2017-03-02")

tempEte2017<-filter(temperature_by_day_by_Region,Date<="2017-08-31" & Date>="2017-06-01")

#2018
tempAut2018<-filter(temperature_by_day_by_Region,Date<="2018-11-30" & Date>="2018-09-01")

tempHiv2018<-filter(temperature_by_day_by_Region,Date<="2018-03-01" & Date>="2018-01-01")

tempPrint2018<-filter(temperature_by_day_by_Region,Date<="2018-05-31" & Date>="2018-03-02")

tempEte2018<-filter(temperature_by_day_by_Region,Date<="2018-08-31" & Date>="2018-06-01")

#2019
tempAut2019<-filter(temperature_by_day_by_Region,Date<="2019-11-30" & Date>="2019-09-01")

tempHiv2019<-filter(temperature_by_day_by_Region,Date<="2019-03-01" & Date>="2019-01-01")

tempPrint2019<-filter(temperature_by_day_by_Region,Date<="2019-05-31" & Date>="2019-03-02")

tempEte2019<-filter(temperature_by_day_by_Region,Date<="2019-08-31" & Date>="2019-06-01")

#2020
tempAut2020<-filter(temperature_by_day_by_Region,Date<="2020-11-30" & Date>="2020-09-01")

tempHiv2020<-filter(temperature_by_day_by_Region,Date<="2020-03-01" & Date>="2020-01-01")

tempPrint2020<-filter(temperature_by_day_by_Region,Date<="2020-05-31" & Date>="2020-03-02")

tempEte2020<-filter(temperature_by_day_by_Region,Date<="2020-08-31" & Date>="2020-06-01")

#Regroupement des températures d'Automne 
tempAut<-rbind(tempAut2016,tempAut2017,tempAut2018,tempAut2019,tempAut2020)
tempAut<-tempAut%>%group_by(Region,Date=floor_date(Date,"12 months"))
idx2016<-which(tempAut[,"Date"]=="2016-01-01")
idx2017<-which(tempAut[,"Date"]=="2017-01-01")
idx2018<-which(tempAut[,"Date"]=="2018-01-01")
idx2019<-which(tempAut[,"Date"]=="2019-01-01")
idx2020<-which(tempAut[,"Date"]=="2020-01-01")
#Transformation des dates en année pour utilisation des histogrammes
tempAut[,"Date"] <- sapply(tempAut[,"Date"], as.character)
tempAut[idx2016,"Date"] = "2016"
tempAut[idx2017,"Date"] = "2017"
tempAut[idx2018,"Date"] = "2018"
tempAut[idx2019,"Date"] = "2019"
tempAut[idx2020,"Date"] = "2020"
tempAut<-tempAut%>%group_by(Date)%>%summarize('EcartTemperature'=mean(`Moy`))
tempAut[,'EcartTemperature']=tempAut[,'EcartTemperature']-13.1

#Regroupement des températures d'Hiver
tempHiv<-rbind(tempHiv2016,tempHiv2017,tempHiv2018,tempHiv2019,tempHiv2020)
tempHiv<-tempHiv%>%group_by(Region,Date=floor_date(Date,"12 months"))
idx2016<-which(tempHiv[,"Date"]=="2016-01-01")
idx2017<-which(tempHiv[,"Date"]=="2017-01-01")
idx2018<-which(tempHiv[,"Date"]=="2018-01-01")
idx2019<-which(tempHiv[,"Date"]=="2019-01-01")
idx2020<-which(tempHiv[,"Date"]=="2020-01-01")
tempHiv[,"Date"] <- sapply(tempHiv[,"Date"], as.character)
tempHiv[idx2016,"Date"] = "2016"
tempHiv[idx2017,"Date"] = "2017"
tempHiv[idx2018,"Date"] = "2018"
tempHiv[idx2019,"Date"] = "2019"
tempHiv[idx2020,"Date"] = "2020"
tempHiv<-tempHiv%>%group_by(Date)%>%summarize('EcartTemperature'=mean(`Moy`))
tempHiv[,'EcartTemperature']=tempHiv[,'EcartTemperature']-5.4

#Regroupement des températures de Printemps 
tempPrint<-rbind(tempPrint2016,tempPrint2017,tempPrint2018,tempPrint2019,tempPrint2020)
tempPrint<-tempPrint%>%group_by(Region,Date=floor_date(Date,"12 months"))
idx2016<-which(tempPrint[,"Date"]=="2016-01-01")
idx2017<-which(tempPrint[,"Date"]=="2017-01-01")
idx2018<-which(tempPrint[,"Date"]=="2018-01-01")
idx2019<-which(tempPrint[,"Date"]=="2019-01-01")
idx2020<-which(tempPrint[,"Date"]=="2020-01-01")
tempPrint[,"Date"] <- sapply(tempPrint[,"Date"], as.character)
tempPrint[idx2016,"Date"] = "2016"
tempPrint[idx2017,"Date"] = "2017"
tempPrint[idx2018,"Date"] = "2018"
tempPrint[idx2019,"Date"] = "2019"
tempPrint[idx2020,"Date"] = "2020"
tempPrint<-tempPrint%>%group_by(Date)%>%summarize('EcartTemperature'=mean(`Moy`))
tempPrint[,'EcartTemperature']=tempPrint[,'EcartTemperature']-11.6

#Regroupement des températures d'Ete 
tempEte<-rbind(tempEte2016,tempEte2017,tempEte2018,tempEte2019,tempEte2020)
tempEte<-tempEte%>%group_by(Region,Date=floor_date(Date,"12 months"))
idx2016<-which(tempEte[,"Date"]=="2016-01-01")
idx2017<-which(tempEte[,"Date"]=="2017-01-01")
idx2018<-which(tempEte[,"Date"]=="2018-01-01")
idx2019<-which(tempEte[,"Date"]=="2019-01-01")
idx2020<-which(tempEte[,"Date"]=="2020-01-01")
tempEte[,"Date"] <- sapply(tempEte[,"Date"], as.character)
tempEte[idx2016,"Date"] = "2016"
tempEte[idx2017,"Date"] = "2017"
tempEte[idx2018,"Date"] = "2018"
tempEte[idx2019,"Date"] = "2019"
tempEte[idx2020,"Date"] = "2020"
tempEte<-tempEte%>%group_by(Date)%>%summarize('EcartTemperature'=mean(`Moy`))
tempEte[,'EcartTemperature']=tempEte[,'EcartTemperature']-19.9

#cSelection des catastrophe naturelles à partir de 2016
s<-catnat%>%dplyr::select(Code_nationale_catnat,Code_postale,Commune,Num_risque,Type_de_catnat,Date)%>%filter(catnat$Date>="2015-12-31")
catnat<-s
catnat$Region=substr(catnat$Code_postale,1,2)
catnat<-catnat[-2]
catnat<-catnat[-2]
rm(s)

#Changement dans les lignes des régions à l'aide d'index pour ne pas avoir de bug pour la carte de France
idxDom <- which(catnat[,"Region"] == "97")
catnat <- catnat[-c(idxDom),]

idxHDF<-which(catnat[,"Region"] == "62" | catnat[,"Region"] == "60" | catnat[,"Region"] == "59"| catnat[,"Region"] == "80" | catnat[,"Region"] == "02")
catnat[idxHDF,"Region"] = "Hauts-de-France"

idxN<-which(catnat[,"Region"] == "76" | catnat[,"Region"] == "27" | catnat[,"Region"] == "59"| catnat[,"Region"] == "61" | catnat[,"Region"] == "14" | catnat[,"Region"] == "50")
catnat[idxN,"Region"] = "Normandie"

idxB<-which(catnat[,"Region"] == "35" | catnat[,"Region"] == "56" | catnat[,"Region"] == "22"| catnat[,"Region"] == "29")
catnat[idxB,"Region"] = "Bretagne"

idxPDL<-which(catnat[,"Region"] == "53" | catnat[,"Region"] == "72" | catnat[,"Region"] == "49"| catnat[,"Region"] == "44" | catnat[,"Region"] == "85")
catnat[idxPDL,"Region"] = "Pays de la Loire"

idxCVDL<-which(catnat[,"Region"] == "28" | catnat[,"Region"] == "45" | catnat[,"Region"] == "41"| catnat[,"Region"] == "18" | catnat[,"Region"] == "36" | catnat[,"Region"] == "37")
catnat[idxCVDL,"Region"] = "Centre-Val de Loire"

idxBFC<-which(catnat[,"Region"] == "89" | catnat[,"Region"] == "21" | catnat[,"Region"] == "70"| catnat[,"Region"] == "90" | catnat[,"Region"] == "25" | catnat[,"Region"] == "39" | catnat[,"Region"] == "71" | catnat[,"Region"] == "58")
catnat[idxBFC,"Region"] = "Bourgogne-Franche-Comté"

idxGE<-which(catnat[,"Region"] == "08" | catnat[,"Region"] == "55" | catnat[,"Region"] == "54"| catnat[,"Region"] == "57" | catnat[,"Region"] == "67" | catnat[,"Region"] == "68" | catnat[,"Region"] == "88" | catnat[,"Region"] == "52" | catnat[,"Region"] == "10" | catnat[,"Region"] == "51")
catnat[idxGE,"Region"] = "Grand Est"

idxNA<-which(catnat[,"Region"] == "79" | catnat[,"Region"] == "86" | catnat[,"Region"] == "87"| catnat[,"Region"] == "23" | catnat[,"Region"] == "19" | catnat[,"Region"] == "16" | catnat[,"Region"] == "17" | catnat[,"Region"] == "33" | catnat[,"Region"] == "24" | catnat[,"Region"] == "47" | catnat[,"Region"] == "40" | catnat[,"Region"] == "64")
catnat[idxNA,"Region"] = "Nouvelle-Aquitaine"

idxARA<-which(catnat[,"Region"] == "03" | catnat[,"Region"] == "42" | catnat[,"Region"] == "69"| catnat[,"Region"] == "01" | catnat[,"Region"] == "74" | catnat[,"Region"] == "73" | catnat[,"Region"] == "38" | catnat[,"Region"] == "26" | catnat[,"Region"] == "07" | catnat[,"Region"] == "43" | catnat[,"Region"] == "63" | catnat[,"Region"] == "15")
catnat[idxARA,"Region"] = "Auvergne-Rhône-Alpes"

idxO<-which(catnat[,"Region"] == "46" | catnat[,"Region"] == "12" | catnat[,"Region"] == "48"| catnat[,"Region"] == "30" | catnat[,"Region"] == "34" | catnat[,"Region"] == "81" | catnat[,"Region"] == "82" | catnat[,"Region"] == "32" | catnat[,"Region"] == "31" | catnat[,"Region"] == "11" | catnat[,"Region"] == "66" | catnat[,"Region"] == "09" | catnat[,"Region"] == "65")
catnat[idxO,"Region"] = "Occitanie"

idxPACA<-which(catnat[,"Region"] == "05" | catnat[,"Region"] == "04" | catnat[,"Region"] == "06"| catnat[,"Region"] == "83" | catnat[,"Region"] == "13" | catnat[,"Region"] == "84")
catnat[idxPACA,"Region"] = "Provence-Alpes-Côte d'Azur"

idxC<-which(catnat[,"Region"] == "2B" | catnat[,"Region"] == "2A")
catnat[idxC,"Region"] = "Corse"

idxIDF<-which(catnat[,"Region"] == "93" | catnat[,"Region"] == "92" | catnat[,"Region"] == "94"| catnat[,"Region"] == "75" | catnat[,"Region"] == "77" | catnat[,"Region"] == "91" | catnat[,"Region"] == "78" | catnat[,"Region"] == "95")
catnat[idxIDF,"Region"] = "Île-de-France"

#Catastrophe naturelle en 2016
catnat2016<-catnat%>%filter(catnat$Date<="2016-12-31")
#Catastrophe naturelle par mois en 2016
catnat_months_2016<-catnat%>%filter(catnat$Date<="2016-12-31")
catnat_months_2016<-catnat_months_2016%>%group_by(Date=floor_date(Date,"31 days"))
catnat_months_2016<-catnat_months_2016%>%count(Date)
names(catnat_months_2016)<-c('Mois','n')
#Nombre total de catastrophes naturelles en 2016
nb_cat2016<-catnat2016%>%count(Region,Num_risque)
nb_cat2016<-inner_join(nb_cat2016,type_cat)
nb_cat2016$Num_risque <- NULL
catnat2016$Date<-"2016"
nb_cat2016<-catnat2016%>%count(Date)
names(nb_cat2016)<-c('Annee','n')
#Nombre total de catastrophes naturelles par région en 2016
nb_catRegion2016 <- catnat2016%>%count(Region)
nb_catRegion2016$Annee<- "2016"


#Catastrophe naturelle en 2017
catnat2017<-catnat%>%filter(catnat$Date<="2017-12-31" & catnat$Date>="2017-01-01")
#Catastrophe naturelle par mois en 2017
catnat_months_2017<-catnat%>%filter(catnat$Date>"2016-12-31"&catnat$Date<="2017-12-31")
catnat_months_2017<-catnat_months_2017%>%group_by(Date=floor_date(Date,"31 days"))
catnat_months_2017<-catnat_months_2017%>%count(Date)
names(catnat_months_2017)<-c('Mois','n')
#Nombre total de catastrophes naturelles en 2017
nb_cat2017<-catnat2017%>%count(Region,Num_risque)
nb_cat2017<-inner_join(nb_cat2017,type_cat)
nb_cat2017$Num_risque <- NULL
catnat2017$Date<-"2017"
nb_cat2017<-catnat2017%>%count(Date)
names(nb_cat2017)<-c('Annee','n')
#Nombre total de catastrophes naturelles par région en 2017
nb_catRegion2017 <- catnat2017%>%count(Region)
nb_catRegion2017$Annee<- "2017"

#Catastrophe naturelle en 2018
catnat2018<-catnat%>%filter(catnat$Date<="2018-12-31"& catnat$Date>="2018-01-01")
#Catastrophe naturelle par mois en 2017
catnat_months_2018<-catnat%>%filter(catnat$Date>"2017-12-31"&catnat$Date<="2018-12-31")
catnat_months_2018<-catnat_months_2018%>%group_by(Date=floor_date(Date,"31 days"))
catnat_months_2018<-catnat_months_2018%>%count(Date)
names(catnat_months_2018)<-c('Mois','n')
#Nombre total de catastrophes naturelles en 2018
nb_cat2018<-catnat2018%>%count(Region,Num_risque)
nb_cat2018<-inner_join(nb_cat2018,type_cat)
nb_cat2018$Num_risque <- NULL
catnat2018$Date<-"2018"
nb_cat2018<-catnat2018%>%count(Date)
names(nb_cat2018)<-c('Annee','n')
#Nombre total de catastrophes naturelles par région en 2018
nb_catRegion2018 <- catnat2018%>%count(Region)
nb_catRegion2018$Annee<- "2018"

#Catastrophe naturelle en 2019
catnat2019<-catnat%>%filter(catnat$Date<="2019-12-31"& catnat$Date>="2019-01-01")
#Catastrophe naturelle par mois en 2019
catnat_months_2019<-catnat%>%filter(catnat$Date>"2018-12-31"&catnat$Date<="2019-12-31")
catnat_months_2019<-catnat_months_2019%>%group_by(Date=floor_date(Date,"31 days"))
catnat_months_2019<-catnat_months_2019%>%count(Date)
names(catnat_months_2019)<-c('Mois','n')
#Nombre total de catastrophes naturelles en 2019
nb_cat2019<-catnat2019%>%count(Region,Num_risque)
nb_cat2019<-inner_join(nb_cat2019,type_cat)
nb_cat2019$Num_risque <- NULL
catnat2019$Date<-"2019"
nb_cat2019<-catnat2019%>%count(Date)
names(nb_cat2019)<-c('Annee','n')
#Nombre total de catastrophes naturelles par région en 2019
nb_catRegion2019 <- catnat2019%>%count(Region)
nb_catRegion2019$Annee<- "2019"

#Catastrophe naturelle en 2020
catnat2020<-catnat%>%filter(catnat$Date<="2020-12-31"& catnat$Date>="2020-01-01")
#Catastrophe naturelle par mois en 2020
catnat_months_2020<-catnat%>%filter(catnat$Date>"2019-12-31"&catnat$Date<="2020-12-31")
catnat_months_2020<-catnat_months_2020%>%group_by(Date=floor_date(Date,"31 days"))
catnat_months_2020<-catnat_months_2020%>%count(Date)
names(catnat_months_2020)<-c('Mois','n')
#Nombre total de catastrophes naturelles en 2020
nb_cat2020<-catnat2020%>%count(Region,Num_risque)
nb_cat2020<-inner_join(nb_cat2020,type_cat)
nb_cat2020$Num_risque <- NULL
catnat2020$Date<-"2020"
nb_cat2020<-catnat2020%>%count(Date)
#Nombre total de catastrophes naturelles par région en 2020
names(nb_cat2020)<-c('Annee','n')
nb_catRegion2020 <- catnat2020%>%count(Region)
nb_catRegion2020$Annee<- "2020"

#nombre total de catastrophe naturelle en France en fonction des années
catnatYear<-rbind(nb_catRegion2016,nb_catRegion2017,nb_catRegion2018,nb_catRegion2019,nb_catRegion2020)
nb_catnatYears<-rbind(nb_cat2016,nb_cat2017,nb_cat2018,nb_cat2019,nb_cat2020)

#nombre de catastrophe par type par année
type2016 <- catnat2016%>%count(Type_de_catnat,Date)
type2017 <- catnat2017%>%count(Type_de_catnat,Date)
type2018 <- catnat2018%>%count(Type_de_catnat,Date)
type2019 <- catnat2019%>%count(Type_de_catnat,Date)
type2020 <- catnat2020%>%count(Type_de_catnat,Date)

typeYears <- rbind(type2016,type2017,type2018,type2019,type2020)
names(typeYears)<-c('Type_de_catnat','Annee','n')

#On transforme les dates en type character pour permettre un affichage vertical dans l'histogramme
catnat_months_2016$Mois <- as.character(catnat_months_2016$Mois)
catnat_months_2017$Mois <- as.character(catnat_months_2017$Mois)
catnat_months_2018$Mois <- as.character(catnat_months_2018$Mois)
catnat_months_2019$Mois <- as.character(catnat_months_2019$Mois)
catnat_months_2020$Mois <- as.character(catnat_months_2020$Mois)

#temperature moyenne par region en 2016
temperature2016<-subset(temperature_by_year_by_Region, temperature_by_year_by_Region$Annee == "2016")

#temperature moyenne par region en 2017
temperature2017<-subset(temperature_by_year_by_Region, temperature_by_year_by_Region$Annee == "2017")

#temperature moyenne par region en 2018
temperature2018<-subset(temperature_by_year_by_Region, temperature_by_year_by_Region$Annee == "2018")

#temperature moyenne par region en 2019
temperature2019<-subset(temperature_by_year_by_Region, temperature_by_year_by_Region$Annee == "2019")

#temperature moyenne par region en 2020
temperature2020<-subset(temperature_by_year_by_Region, temperature_by_year_by_Region$Annee == "2020")

#Animation climat/Temperature page 2
tempe_climat <- merge(climat,temperature_by_year_by_Region)
tempe_climat <- merge(tempe_climat,catnatYear)
tempe_climat$Annee<-as.integer(tempe_climat$Annee)
tempe_climat<-tempe_climat%>%rename(Temp_moy=Moy, Nombre_catnat=n)

#Carte de catnat 2016
formes <- getData(name="GADM", country="FRA", level=2)
idx <- match(formes$NAME_1, nb_catRegion2016$Region)
concordance <- nb_catRegion2016[idx,"n"]
formes$n <- concordance
couleurs <- colorRampPalette(c('white','yellow','orange','red'))

#Carte de catnat 2017
formes2017 <- getData(name="GADM", country="FRA", level=2)
idx <- match(formes2017$NAME_1, nb_catRegion2017$Region)
concordance <- nb_catRegion2017[idx,"n"]
formes2017$n <- concordance
couleurs <- colorRampPalette(c('white','yellow','orange','red'))

#Carte de catnat 2018
formes2018 <- getData(name="GADM", country="FRA", level=2)
idx <- match(formes2018$NAME_1, nb_catRegion2018$Region)
concordance <- nb_catRegion2018[idx,"n"]
formes2018$n <- concordance
couleurs <- colorRampPalette(c('white','yellow','orange','red'))

#Carte de catnat 2019
formes2019 <- getData(name="GADM", country="FRA", level=2)
idx <- match(formes2019$NAME_1, nb_catRegion2019$Region)
concordance <- nb_catRegion2019[idx,"n"]
formes2019$n <- concordance
couleurs <- colorRampPalette(c('white','yellow','orange','red'))

#Carte de catnat 2020
formes2020 <- getData(name="GADM", country="FRA", level=2)
idx <- match(formes2020$NAME_1, nb_catRegion2020$Region)
concordance <- nb_catRegion2020[idx,"n"]
formes2020$n <- concordance
couleurs <- colorRampPalette(c('white','yellow','orange','red'))

#Liste utilisé pour le selector des type de catastrophe    
types <- sort(unique(typeYears$Type_de_catnat))
names(types) <- types
types <- c(All = 0, types)    

