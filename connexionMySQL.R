#                            Projet Trafic Aérien                   #

#######################Importation des données#######################################

airports <- read.csv2(file = "airports.csv", sep = ",",dec = ".")

flights <- read.csv2(file = "flights.csv", sep = ",",dec = ".")

planes <- read.csv2(file = "planes.csv", sep = ",",dec = ".")

weather <- read.csv2(file = "weather.csv", sep = ",",dec = ".")

airlines <- read.csv2(file = "airlines.csv", sep = ",", dec = ".")


# clés primaires, clés étrangères et relations

library(RMySQL)

# connect to database (don't forget to disconnect)
DB <- dbConnect(MySQL(), user="root", host="localhost",
                password="Traore160410@", dbname="rmysql0")

#La base de données que nous venons de créer est vide :
dbListTables(DB) # list tables


#Nous allons y ajouter le tableau de données R suivant :
dbWriteTable(DB, "airlines", airlines, row.names = FALSE) # write dataframe to the database
dbWriteTable(DB, "planes", planes, row.names = FALSE)
dbWriteTable(DB, "airports", airports, row.names = FALSE)
dbWriteTable(DB, "flights", flights, row.names = FALSE)
dbWriteTable(DB, "weather", weather, row.names = FALSE)

nrow(flights)

nrow(planes)
# Nous avons nommé le tableau dat1 dans MySQL. 
# Il apparaît maintenant dans la liste des tableaux de la base de données :

dbListTables(DB)

# La fonction dbListFields retourne les noms de ses colonnes :

dbListFields(DB, "airlines")


## Pour importer un tableau de MySQL dans R, on utilise dbReadTable :

dbReadTable(DB, "airlines") # get database table as dataframe

## Exécuter une requête SQL

## La librairie RMySQL fournit deux fonctions pour exécuter une requête SQL sur un tableau : 
## dbSendQuery et dbGetQuery

## Fonction dbGetQuery : La fonction dbGetQuery envoie une requête et retourne le résultat dans R                

dbGetQuery(DB, "DESCRIBE airlines")


## Ou pour une reqûete SELECT :

dbGetQuery(DB, "SELECT * FROM airlines WHERE Name='Mesa Airlines Inc.';")

## Cela n’a pas altéré le tableau :

dbReadTable(DB, "airlines")


## La fonction dbGetQuery ne retourne rien lorsqu’on l’utilise pour exécuter une requête 
## qui altère le tableau, telle que ALTER ou UPDATE :



dbGetQuery(DB, "ALTER TABLE `airlines` DROP COLUMN `name`")
## data frame with 0 columns and 0 rows

dbGetQuery(DB, "UPDATE `airlines` SET `carrier` = `carrier`")
## data frame with 0 columns and 0 rows


## Ces requêtes ont bien été exécutées :
dbReadTable(DB, "airlines")

## Il vaut mieux utiliser la fonction dbSendQuery pour exécuter ces requêtes.

## Fonction dbSendQuery

# Avant d’illustrer la fonction dbSendQuery, nous restaurons le tableau initial. 
# Il faut d’abord l’effacer avec dbRemoveTable :

dbRemoveTable(DB, "airlines")
## [1] TRUE
dbWriteTable(DB, "airlines", airlines, row.names = FALSE) 
## [1] TRUE

## Exécutons la requête ALTER comme précédemment :


dbSendQuery(DB, "ALTER TABLE `airlines` DROP COLUMN `name`")


# La fonction dbGetInfo retourne toute une liste d’informations.

# La fonction dbGetQuery vue précédemment est en fait équivalente à dbSendQuery suivie 

# de dbFetch :



res <- dbSendQuery(DB, "SELECT * FROM airlines WHERE carrier = 'AA';") 
dbColumnInfo(res)

dbFetch(res)


# Déconnexion

dbDisconnect(DB)