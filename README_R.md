*RAPSODE Jomalhia*
*RAMANOELINA Neil-Matthieu*
*DSIA-4101A*

Projet Dashboard R

# 1.	INTRODUCTION

Le module "*R & Datavisualisation*", se conclut par la réalisation d'un projet dans lequel nos connaissances en R seront mise en application pour l'étude du sujet choisit.
Ce projet a été réalisé en binôme par **RAMANOELINA Neil-Matthieu** et **RAPSODE Jomalhia**

Ce projet consiste à développer une interface dite Single Page Application ou encore Multi Page Application à partir d’une base de données. Notre thème principal porte sur l’étude des catastrophes naturelles  et sur différents facteurs climatiques notamment la température. Notre problématique est la suivante :

# En quoi la température est-elle un facteur pouvant influencer les catastrophes naturelles ?

Pour répondre à cette question nous avons utilisé des bases de données open source et le langage de programmation R. L’objectif que nous nous sommes fixé est de déterminer si la température a une influence sur les catastrophes naturelles.

Les jeux de données sur lesquels nous avons choisi travailler sont les suivants:

*[Arrêtés de catastrophe naturelle en France métropolitaine](https://www.data.gouv.fr/fr/datasets/arretes-de-catastrophe-naturelle-en-france-metropolitaine-2/)*
  > *source: data.gouv.fr*
- * [Température quotidienne régionale (depuis janvier 2016) ](https://www.data.gouv.fr/fr/datasets/temperature-quotidienne-regionale-depuis-janvier-2016/)*
  > *source: data.gouv.fr*


# 2.	GUIDE UTILISATEUR

## 2.1.	Technologies 

-	R	version  4.0.2

### 2.1.1.  Installation

#### 2.1.1.1    R

    Afin d'exploiter notre projet, il est nécessaire premièrement d'installer le langage "*R version 4.0.2*". 
    Nous vous conseillons de plus d'installer un IDE Open Source tel que "*R Studio Desktop*"

#### 2.1.1.2	Packages

Il vous faudra ensuite installer les packages suivant:

lubridate       version 1.7.9 
 
libridate permet de travailler avec les dates-heures et les intervalles de temps en procédant à une analyse rapide et conviviale de ces données et par une extraction et une mise à jour des composantes d'une date-heure.
 
 
-   gganimate       version 1.0.7
 
gganimate est un paquet fournissant un ensemble de grammaire complètement nouveau, compatible avec "ggplot2" permettant de spécifier les transitions et les animations de manière flexible et extensible et permettant la création d’une visualisation statique.
 
 
-   shiny       version 1.5.0
 
Shiny permet de créer des applications web interactives avec R, avec seulement quelques lignes de code sans avoir besoin de JavaScript.
 
 
-   ggplot2         version 3.3.2
 
ggplot2 est un système de création déclarative de graphiques, basé sur La Grammaire du Graphique. Il suffit de fournir les données et de demander à ggplot2 comment associer les variables à l'esthétique ou quelles primitives graphiques utiliser, et il s'occupe des détails.
 
 
-   raster      version 3.3.13
 
Raster est un paquet qui met en œuvre des fonctions de base et de haut niveau pour les données matricielles et pour les opérations de données vectorielles telles que les intersections.
 
 
-   shinydashboard  version 0.7.1
 
Shinydashboard permet de créer facilement des tableaux de bord attrayants.
 
 
-   gifski      version 0.8.6
 
 gifski convertit les images en animations GIF à l'aide des palettes cross-frame.
 
 
-   png         version 0.1.7
 
png est un ensemble offrant un moyen simple et facile de lire, écrire et afficher des images bitmap stockées au format PNG.
 
 
-   tidyverse       version 1.3.0
 
Tidyverse est un ensemble de paquets qui fonctionnent en harmonie et  a pour but de faciliter l'installation et le chargement de plusieurs paquets "tidyverse" en une seule étape.
 


La commande ci-dessous permet d’installer les différents packages cités précédemment :

```bash
> install.packages('nom_du_package')
```

## 2.2.	Exécution

Pour lancer ce projet, il est nécessaire de :

Méthode 1:

•	Télécharger le dossier.
•	Ouvrir le fichier « global.R» avec RStudio.
•	Lancer l’application grâce au bouton «  RunApp » de RStudio ou d'utiliser la ligne de comande suivante:

```bash
> runApp()
```
•	Le dashboard s’ouvrira dans une fenêtre de RStudio. 


Méthode 2:


•	Se rendre sur git.

•	Cloner le fichier afin d’avoir une copie sur votre machine personnelle grâce au lien suivant :

> https://git.esiee.fr/ramanoen/projet-dashboard-r.git


•	Puis l‘ouvrir et le lancer sur RStudio grâce au bouton «  RunApp » ou utiliser la ligne de commande suivante:

```bash
> runApp()
```

•	Le Dashboard s’ouvrira dans une fenêtre de RStudio.


Méthode 3:

•	Pour visualiser le Dashboard on peut utiliser l’url suivant :
https://neilrama.shinyapps.io/projet-dashboard-r/


Le Dashboard est accessible à l'addresse suivante:

```bash
Listening on http://127.0.0.1:5734
```
## 2.3. DASHBOARD

On peut naviguer au travers de notre application grâce au menu :

![menu.png](images/menu.png)

Il y a quatre pages:
- *Réchauffement climatique*
- *Température/différents facteurs*
- *Catastrophes naturelles*
- *Temperature/Catstrophes*

### 2.3.1 Page 1 Réchauffement climatique ou non ?

![page1.png](images/page1.png)

La première page nous permet de montrer grâce à notre jeu de données qu'on observe bien une augmentation des températures, au fil du temps. 

### 2.3.2 Page 2 

![page2.png](images/page2.png)

 
Sur la deuxième page, on établit une corrélation entre la température et différents facteurs: l’ensoleillement, la hauteur des précipitations et la vitesse maximale du vent.


### 2.3.3 Page 3

![page3.png](images/page3.png)

 
La troisième page met en oeuvre l’évolution des catastrophes naturelles par région et par type selon les années.


### 2.3.3 Page 4

![page4.png](images/page4.png)

Enfin la quatrième page nous permet de comprendre si les catastrophes naturelles sont liées à l’évolution de la température, en réalisant une étude entre 2016 et 2020

# 3.	GUIDE DEVELOPPEUR

## 3.1.	Fichiers

Voici une présentation des fichiers utilisés dans ce projet :

### 3.1.1 « global.R »

•	Le fichier  « global.R » contient le code permettant de nettoyer les données. En effet, si l’utilisateur souhaite modifier les données permettant d’afficher les graphiques, il peut le faire dans « global.R ».

### 3.1.2 « ui.R »

•	Le fichier « ui.R » contient les données de l’interface du Dashboard. Ainsi, si on souhaite modifier l’interface on peut le faire dans le fichier « ui.R».

### 3.1.3 « server.R »

•	Le fichier « server.R » permet de connecter « global.R » et «u i.R ». Par exemple, pour modifier l’affichage des graphiques (plots) on pourra utiliser fichier.

### 3.1.4 Jeux de données

*[Arrêtés de catastrophe naturelle en France métropolitaine](https://www.data.gouv.fr/fr/datasets/arretes-de-catastrophe-naturelle-en-france-metropolitaine-2/)*
  > *source: data.gouv.fr*


- * [Température quotidienne régionale (depuis janvier 2016) ](https://www.data.gouv.fr/fr/datasets/temperature-quotidienne-regionale-depuis-janvier-2016/)*
  > *source: data.gouv.fr*

### 3.1.5  dossier image 

Ce dossier contient toutes les images auxquels il est sera fait référence de ce document

# 4.	RAPPORT D'ANALYSE 


## 4.1.	 Page 1

Dans un premier temps, la première page de notre Dashboard nous permet d’avérer ou d’infirmer, par la visualisation, une augmentation des températures au cours des cinq dernières années.

### 4.1.1  Moyenne des températures par Région

![MoyenneT.png](images/MoyenneT.png)

En outre la première représentation intitulée Moyenne des températures par Région, nous permet de voir une hausse de la température au fil des années malgré une légère diminution en 2018. On constate, que la température en 2017 augmente pour toutes les régions sauf pour la Corse où la température baisse légèrement .

A l’aide du diagramme Année, il est possible de modifier l’aspect du graphique “Moyennes de températures par Région”, en effet, il suffit de déplacer le curseur de gauche à droite pour passer d’une année à une autre.
 
En 2018, la température augmente puis diminue en 2019 pour toutes les régions. On connaît une forte augmentation des températures en 2020, cette observation s’explique du fait que les bases de données de températures ne sont pas encore complètes puisque l’année n‘est pas encore terminée.



### 4.1.1. Ecart de température rapport à la normale de saison 


 
Pour avoir une vue moins globale des températures au cours de l’année nous avons choisi de représenter graphiquement les écarts de températures en fonction des différentes saisons. Nous comparons ici la moyenne de température saisonnière à la normale de saison. 


#### 4.1.1.1. Ecart de température rapport à la normale de saison- Automne 13.1°c


![Autumn.png](images/Autumn.png)

En Automne les températures sont supérieures à la normale sauf en 2017 où il y a une légère baisse. De plus, l’écart de température en automne 2020 est très important ce qui s’explique par l’automne n’est pas terminé. 


#### 4.1.1.2. Ecart de température rapport à la normale de saison - Hiver 5.4°c

![Hiver.png](images/Hiver.png)

Pour l’Hiver, on remarque un écart très élevé en 2016 contrairement à l’année 2017 où l’écart est infime voire nul. De 2018 à 2020 on remarque que la différence de température augmente, elle est de 2,6°C pour l’année en cours.


#### 4.1.1.3. Ecart de température rapport à la normale de saison - Printemps 11.6°c

![Printemps.png](images/Printemps.png)

Au printemps, on remarque que les températures sont basses en 2016 et en 2019 par rapport à la normale de saison. 

#### 4.1.1.4. Ecart de température rapport à la normale de saison - Eté 19.9°c

![Ete.png](images/Ete.png)
Enfin, on remarque une baisse des écarts en les étés 2018 et 2020 malgré un écart de température positif en 2017. 


Ainsi globalement, on observe que l’écart entre les températures et les normales de saison demeure positif.

### Temperature par mois


![violins.png](images/violins.png)

Afin d’avoir une vison plus précise on réduit l’échelle de temps et on regarde l’évolution de la température en fonction des mois de l’année. De décembre à février la médiane de température à tendance à augmenter sauf en 2017 où elle diminue. De même, entre mars et mai la médiane augmente à l’exception de 2019 où elle diminue légèrement. On peut relier cette diminution à l’écart négatif à la normale de saison du printemps 2019. Entre juin et août la médiane suit l’évolution des écarts de températures par rapport à la normale de saison. Il en est de même entre septembre et novembre.


## 4.2.	 Page 2


Dans un second temps, la page deux du Dashboard permet de relier l’évolution de la température à divers facteurs.

### 4.2.1 Vitesse du vent maximale/Temperature

![vent.png](images/vent.png)

Pour les régions d'Occitanie, de Provence-Alpes-Côte d’Azur et de Nouvelle Aquitaine on remarque une augmentation de la vitesse du vent avec l’augmentation de la température. Cependant, les vents les plus forts semblent prendre place lorsque la température diminue contrairement aux catastrophes naturelles qui sont plus nombreuses lorsque la température augmente.

### 4.2.2 Hauteur de précipitation (en mm)/Temperature

![pluie.png](images/pluie.png)

La hauteur des précipitations augmente avec la température dans la majorité des régions durant l’année 2018, cette observation ne permet pas d’affirmer que les précipitations sont plus importantes lorsqu’il fait plus chaud. Au contraire il semblerait qu’il y ait plus de précipitations lorsque la température diminue. D’autre part lorsque la hauteur des précipitations augmente le nombre de catastrophes naturelles augmente aussi.



### 4.2.3 Ensoleillement (en heures)/Temperature

![soleil.png](images/soleil.png)

On ne remarque aucun lien direct entre l'ensoillement et la température. En effet, d'une année à l'autre lorsque la température augmente l'ensoillement diminue et vice versa. Ainsi, nous pouvons écarter l'hypothèse de l'influence de l'ensoleillement sur la température.

## 4.3.	 Page 3


Par ailleurs, la page 3 du Dashboard permet de visualiser l’évolution du nombre de catastrophes naturelles au cours des années. 
 
L’histogramme nous permet de dire que l’année 2018 marque le plus grand nombre de catastrophes naturelles. 



### 4.3.1 Nombre de catastrophes naturelles par année

![année.png](images/année.png)

Ensuite, la carte nous permet de voir les régions les plus affectées au cours du temps. Il suffit de sélectionner successivement les différentes années pour apercevoir les évolutions. 


## 4.3.2 Catastrophe naturelle chaque année

![carte.png](images/carte.png)

En 2016 la région la plus touchée est la région Centre-Val-de-Loire qui compte un peu moins de 900 catastrophes naturelles. Les régions du Nord-Est sont les secondes à être les plus atteintes avec un nombre de catastrophes naturelles oscillant entre 400 et 600. 
 
En 2O17, le nombre de catastrophes naturelles est inférieure à 800 et la région la plus frappée est l’Occitanie avec un effectif de 700. Par ailleurs, on remarque que les autres régions sont moins touchées et la quantité de catastrophes ne dépassent pas 200.
 
En 2018, le nombre maximum de catastrophes naturelles augmente fortement, celui-ci se situe à un peu plus de 1600, en effet l’Occitanie est toujours la région la plus atteinte mais le nombre de catastrophes naturelles augmentent dans les autres régions. Par exemple en Nouvelle-Aquitaine, on atteint 1200 cas.
 
En 2019,on remarque que la Bourgogne-Franche Compté compte le plus grand nombre de catastrophes naturelles. De surcroît, celles-ci sont plus répandu dans la majorité Est du pays. 
 
Finalement, en 2020, on remarque une diminution globale du nombre de catastrophes naturelles en France même si la région d’Occitanie demeure la plus affectée avec un peu plus de 700 cas.


### 4.3.3 Nombre de catastrophes par type

![type.png](images/type.png)

Le bar graph intitulé "Nombre de catastrophes par type” peut être mis en relation avec les animations de la page 2, particulièrement avec l’animation concernant la hauteur des précipitations. Nous avions pu voir précédemment que 2018 fut l’année où les précipitations étaient les plus fortes. On remarque grâce à cet histogramme qu’un grand nombre de catastrophes liées aux inondations se sont produites cette année là.

D’ailleurs, on peut mettre évidence que les régions les moins touché par les catastrophes naturelles les régions de Nord Ouest tandis que les régions les plus communément saisies sont les régions de sud et de l’Est.


La partie intitulée “Types” permet de spécifier les catastrophes et d’obtenir de nouveaux graphiques nous indiquant leur nombre en fonction des différentes années. Par exemple, on compte 90 catastrophes de type inondations par remontées de nappe phréatique en 2018, 10 en 2019 et 3 en 2020.

L'année la plus froide est essentiellement marqué par des inondations malgré tout moins nombreuses que les autres années. On peut donc peut être en déduire un lien entre la température et les catastrophes naturelles (on approfondira cela en 4e page du Dashboard). On Remarque globalement que les inondations et les coulées de boue ou mouvement de terrain sont les catastrophes naturelles présentent en plus grand nombre. 
 
D’ailleurs, on peut mettre évidence que les régions les moins touchées par les catastrophes naturelles sont les régions du Nord Ouest tandis que les régions les plus atteintes sont les régions du Sud et de l’Est.
 
En somme, sur cette page en peut voir que les catastrophes naturelles présentent en plus grand nombre sont en 2018, elles sont au cours du temps de plus en plus répandues sur le pays. De plus un lien entre catastrophes naturelles et températures commence à apparaître.


## 4.4.	 Page 4

### 4.4.1 Catastrophe naturelle par mois et température par mois

![temp.png](images/temp.png)

Enfin, la dernière page permet de répondre à notre problématique. 
En comparant le nombre de catastrophes naturelles en fonction des différentes années, on peut observer parallèlement les températures au cours du temps.
 
Cela nous permet de conclure que 2018 est toujours l’année la plus chaude. En juillet la température est plus élevée que les années précédentes. Entre autre, on peut établir que les catastrophes naturelles sont plus nombreuses aux mois les plus chauds de l’année. 
 
En dernier lieu, il nous est possible de conclure sur l’existence d’une relation entre le réchauffement climatique et le nombre de catastrophes naturelles en France.
