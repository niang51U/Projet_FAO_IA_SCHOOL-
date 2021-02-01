#********************************PROJET FAO****************************************************

## Import des données des bilans alimentaires

animal <- read.csv("FAOSTAT_data_Animals_Products.csv", sep = ",", dec = ".")

vegetal_2014 <- read.csv("FAOSTAT_data_vegetal_products_2014.csv", sep = ",", dec = ".")

vegetal_2015 <- read.csv("FAOSTAT_data_vegetal_products_2015.csv", sep = ",", dec = ".")

vegetal_2016 <- read.csv("FAOSTAT_data_vegetal_products_2016.csv", sep = ",", dec = ".")

vegetal_2017 <- read.csv("FAOSTAT_data_vegetal_products_2017.csv", sep = ",", dec = ".")

vegetal <- rbind(vegetal_2014, vegetal_2015, vegetal_2016, vegetal_2017)

#ajout de la variable origin

animal$origin <- 'animal'
vegetal$origin <- 'vegetal'

# on regroupe animal et vegetal en un unique dataframe, via une union

temp <- merge(animal,vegetal,all=TRUE)

# suppressio des dataframe animal et vegetal

remove(animal, vegetal)
remove(vegetal_2014, vegetal_2015, vegetal_2016, vegetal_2017)

# on renomme les colonnes de temp

names(temp) <- c("xx", "xx2", "country_code", "country", "xx3", "element", 
                 "item_code", "item", "xx4", "year", "unit", "value", "xx5", "xx6",
                 "origin")

# Transformation de temp en table pivot

result = dcast(temp, country_code + country + item_code + item + year + origin
               ~ element, value.var="value", fun.aggregate=sum)
remove(temp)

names(result) <- c("code_pays", "pays", "code_produit", "produit", "année", "origin",
                   "domestic_supply_quantity", "export_quantity", "fat_supply_quantity_gcapitaday",
                   "feed", "food", "food_supply_kcalcapitaday", "food_supply_quantity_kgcapitayr",
                   "import_quantity", "losses", "other_uses", "processing", "production",
                   "protein_supply_quantity_gcapita_day","residuals","seed",
                   "stock_variation", "tourist_consumption")

result <- result %>%
  filter(code_pays != 351) %>%
  distinct

# Base de données

#**********************************************************************************
#1 Table Population 
#**********************************************************************************

population <- read.csv("FAOSTAT_data_population.csv", sep = ",", dec = ".")

names(population) <- c("xx1", "xx2", "country_code", "country", "element_code",
                       "element", "item_code", "item", "year_code", "year",
                       "unit", "population", "xx3", "xx4")

Population <- population %>%
  select(country_code, country, year, population)

remove(population)

## calculer la taille de la population mondiale (pour chaque année)
Population_mondiale <- Population %>%
  group_by(year) %>%
  summarise(taille = sum(population))

### Exclure le code_pays 351 il correspond à l'aggrégation des pays : 41, 96, 214 et 128
### execute a pattern-matching function on your data to create an index vector
ndx <- grep("China", Population$country, perl=T)
### use this index vector to extract the rows you want from the data frame:
selected_rows = Population[ndx,]
selected_rows
### Exclure le code_pays 351
population <- Population %>% 
  filter(country_code != 351)
names(population) <- c("code_pays", "pays", "année", "population")


remove(Population)
remove(Population_mondiale)

Population_mondiale <- population %>%
  group_by(année) %>%
  summarise(Total = sum(population))



#**********************************************************************************
#2 Table dispo_alim
#**********************************************************************************

dispo_alim <- result %>%
  filter(code_pays != 351) %>%
  select(pays, code_pays, année, produit, code_produit, origin, food,
         food_supply_kcalcapitaday,
         protein_supply_quantity_gcapita_day,
         fat_supply_quantity_gcapitaday) %>%
  distinct(pays, code_pays, année, produit, code_produit, .keep_all = TRUE)

names(dispo_alim) <- c("pays", "code_pays", "année", "produit", "code_produit", "origin",
                               "dispo_alim_tonnes", "dispo_alim_kcal_p_j",
                               "dispo_prot_g_p_j", "dispo_mat_gr_g_p_j")

## 2.1 Produis considérés comme céréales

cereals <- read.csv("FAOSTAT_data_céréales.csv", sep = ",", dec = ".")

cereals <- cereals %>%
  select(Item.Code, Item, Year, Value)
names(cereals) <- c("code_produit", "produit", "année", "value")


## 2.1.1 Ajout de la variable is_cereal dans la table disp_alim et dans la table result

dispo_alim$is_cereal <- ifelse(dispo_alim$produit == "Barley and products" |
                                dispo_alim$produit == "Cereals, Other" |
                                  dispo_alim$produit == "Maize and products" |
                                   dispo_alim$produit == "Millet and products" |
                                    dispo_alim$produit == "Oats" |
                                     dispo_alim$produit == "Rice and products" |
                                      dispo_alim$produit == "Rye and products" |
                                       dispo_alim$produit == "Sorghum and products" |
                                        dispo_alim$produit == "Wheat and products", 
                                     "OUI", "NON")

result$is_cereal <- ifelse(result$produit == "Barley and products" |
                            result$produit == "Cereals, Other" |
                              result$produit == "Maize and products" |
                                result$produit == "Millet and products" |
                                  result$produit == "Oats" |
                                    result$produit == "Rice and products" |
                                      result$produit == "Rye and products" |
                                       result$produit == "Sorghum and products" |
                                        result$produit == "Wheat and products", 
                               "OUI", "NON")

## modifier l'ordre des colonnes de la table disp_alim et de la table result
dispo_alim <- dispo_alim[, c(2, 1, 3, 4, 5, 6, 11, 7, 8, 9, 10)]

result <- result[,c(1,2,3,4,5,6,24,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)]

## 2.1.2 Proportion des céréales pour l’alimentation animale

### En ne prenant en compte que les céréales destinées à l'alimentation (humaine et animale), 
### quelle proportion (en termes de poids) est destinée à l'alimentation animale

cereals_for_feed_food <- result %>%
  filter(is_cereal == "OUI", feed != 0, food != 0) %>%
  select(produit, année, feed, food) %>%
  group_by(année) %>%
  summarise(prop_animale = round(100 * sum(feed) / (sum(feed) + sum(food)),2))

## 2.2 Comment mesure-t-on la disponibilité alimentaire ?

### 2.2.1 Dispo alim par pays et par produit ?
### Calculer (pour chaque pays et chaque produit) la disponibilité alimentaire en kcal puis en kg de protéines ?

dispo_alim <- dispo_alim %>% left_join(population)

dispo_alim <- dispo_alim %>%
  mutate(food_supply_kcal = dispo_alim$dispo_alim_kcal_p_j * dispo_alim$population * 365,
         food_supply_kgprotein = dispo_alim$dispo_prot_g_p_j * 0.001 * dispo_alim$population * 365)

### 2.2.2 Q : Ratio "énergie/poids" ? et 2.2.3 Q : % de protéines de chaque produit ?

### A partir de ces dernières informations, et à partir du poids de la disponibilité alimentaire
### (pour chaque pays et chaque produit), calculez pour chaque produit le ratio "énergie/poids",
### que vous donnerez en kcal ?

### Indice : La disponibilité alimentaire en kcal/personne/jour est calculée par la FAO en 
### multipliant la quantité Nouriture (Food) par le ratio énergie/poids (en kcal/kg), 
### puis en le divisant par la population du pays puis par 365

# Colonne contenant le poids de la disponibilité alimentaire pour chaque pays et item (en kg)

dispo_alim$food_supply_kg <- dispo_alim$dispo_alim_tonnes * 1000000

dispo_alim <- dispo_alim %>%
  mutate(`ratio_kcal/kg` = food_supply_kcal / food_supply_kg,
         `protein_%` = 100 * food_supply_kgprotein / food_supply_kg )

## 2.3 Top 20 des aliments

### 2.3.1 Q : les plus caloriques
### Citez 5 aliments parmi les 20 aliments les plus caloriques, en utilisant 
### le ratio énergie/poids ?
### 2.3.2 Q : les plus protéinés
### Citez 5 aliments parmi les 20 aliments les plus riches en protéines ?

# omit rows where either 'ratio_Kcal/Kg' or 'protein_%' have missing values
dispo_alim <- na.omit(dispo_alim, cols=c("ratio_Kcal/Kg", "protein_%"))
# garder les lignes sans Inf
dispo_alim <- dispo_alim %>%
  filter(is.finite(`ratio_kcal/kg`), is.finite(`protein_%`))

# En 2014
recherche_aliments_plus_caloriques_ou__plus_proteines_2014 <- 
  dispo_alim %>%
  filter(`ratio_kcal/kg` != 0.0, `protein_%` != 0.000000, année == 2014) %>%
  group_by(produit) %>%
  summarise(`ratio_kcal/kg` = mean(`ratio_kcal/kg`, na.rm = TRUE),
            `protein_%` = mean(`protein_%`, na.rm = TRUE)) %>%
  arrange(desc(`ratio_kcal/kg`))
recherche_aliments_plus_caloriques_ou__plus_proteines_2014$année <- 2014

# En 2015
recherche_aliments_plus_caloriques_ou__plus_proteines_2015 <- 
  dispo_alim %>%
  filter(`ratio_kcal/kg` != 0.0, `protein_%` != 0.000000, année == 2015) %>%
  group_by(produit) %>%
  summarise(`ratio_kcal/kg` = mean(`ratio_kcal/kg`, na.rm = TRUE),
            `protein_%` = mean(`protein_%`, na.rm = TRUE)) %>%
  arrange(desc(`ratio_kcal/kg`))
recherche_aliments_plus_caloriques_ou__plus_proteines_2015$année <- 2015

#En 2016
recherche_aliments_plus_caloriques_ou__plus_proteines_2016 <- 
  dispo_alim %>%
  filter(`ratio_kcal/kg` != 0.0, `protein_%` != 0.000000, année == 2016) %>%
  group_by(produit) %>%
  summarise(`ratio_kcal/kg` = mean(`ratio_kcal/kg`, na.rm = TRUE),
            `protein_%` = mean(`protein_%`, na.rm = TRUE)) %>%
  arrange(desc(`ratio_kcal/kg`))
recherche_aliments_plus_caloriques_ou__plus_proteines_2016$année <- 2016

# En 2017
recherche_aliments_plus_caloriques_ou__plus_proteines_2017 <- 
  dispo_alim %>%
  filter(`ratio_kcal/kg` != 0.0, `protein_%` != 0.000000, année == 2017) %>%
  group_by(produit) %>%
  summarise(`ratio_kcal/kg` = mean(`ratio_kcal/kg`, na.rm = TRUE),
            `protein_%` = mean(`protein_%`, na.rm = TRUE)) %>%
  arrange(desc(`ratio_kcal/kg`))
recherche_aliments_plus_caloriques_ou__plus_proteines_2017$année <- 2017

aliments_plus_caloriques_ou_plus_proteines <- rbind(recherche_aliments_plus_caloriques_ou__plus_proteines_2014,
                                                    recherche_aliments_plus_caloriques_ou__plus_proteines_2015,
                                                    recherche_aliments_plus_caloriques_ou__plus_proteines_2016,
                                                    recherche_aliments_plus_caloriques_ou__plus_proteines_2017
                                                    )
aliments_plus_caloriques_ou_plus_proteines <- 
  aliments_plus_caloriques_ou_plus_proteines[, c(4, 1, 2,3)]

remove(recherche_aliments_plus_caloriques_ou__plus_proteines_2014,
       recherche_aliments_plus_caloriques_ou__plus_proteines_2015,
       recherche_aliments_plus_caloriques_ou__plus_proteines_2016,
       recherche_aliments_plus_caloriques_ou__plus_proteines_2017)

### 2.4 Disponibilité mondiale
### 2.4.1 Q : En végétaux uniquement

### Calculer, pour les produits végétaux uniquement, la disponibilité intérieure mondiale 
### exprimée en kcal et en Kg protéines pour chaque année et tracer la viz correspondante

# Préliminaires
result <- result %>% left_join(population)

result <- result %>%
  mutate(food_supply_kcal = result$food_supply_kcalcapitaday * result$population * 365,
         food_supply_kgprotein = result$protein_supply_quantity_gcapita_day * 0.001 * result$population * 365)

result$food_supply_kg <- result$food * 1000000

result <- result %>%
  mutate(`ratio_kcal/kg` = food_supply_kcal / food_supply_kg,
         `protein_%` = 100 * food_supply_kgprotein / food_supply_kg )

# omit rows where either 'ratio_Kcal/Kg' or 'protein_%' have missing values
result <- na.omit(result, cols=c("ratio_Kcal/Kg", "protein_%"))
# garder les lignes sans Inf
result <- result %>%
  filter(is.finite(`ratio_kcal/kg`), is.finite(`protein_%`))

# Ajout de la disponibilité intérieure en kcal et en kg de proteines
dispo_int_vegetal <- result %>%
  filter(origin == "vegetal") %>%
  mutate(dom_sup_kcal = domestic_supply_quantity * 1000000 * `ratio_kcal/kg`,
         dom_sup_kgprot = domestic_supply_quantity * 1000000 * `protein_%` * .01)

# Calcul de la dispo. int. par année

dispo_int_vegetal <- dispo_int_vegetal %>%
  group_by(année) %>%
  summarise(dom_sup_Kcal = sum(dom_sup_kcal),
            dom_sup_kgprot = sum(dom_sup_kgprot))

# visualization
ggplot(
  dispo_int_vegetal,
  aes(x = année)
) +
  geom_line(aes(y = dom_sup_Kcal, color = "dom_sup_Kcal")) +
  geom_line(aes(y = dom_sup_kgprot, color = "dom_sup_kgprot")) +
  scale_colour_manual("", 
                      breaks = c("dom_sup_Kcal", "dom_sup_kgprot"), 
                      values = c("dom_sup_Kcal"="green", "dom_sup_kgprot"="blue"))+
  scale_x_continuous(name="year") +
  scale_y_continuous(name="domestic supply") +
  ggtitle("global domestic availability")

### 2.4.2 Q : Tous végétariens ?

# Combien d'humains pourraient être nourris si toute la disponibilité intérieure mondiale 
# de produits végétaux était utilisée pour de la nourriture ? Donnez les résultats en termes 
# de calories, puis de protéines, et exprimez ensuite ces 2 résultats en pourcentage de la 
# population mondiale.

# Renseignez-vous sur la recommandation (de la FAO si possible) concernant le nombre de calories
# ainsi que sur le nombre de protéines par jour nécessaire à un être humain 
# (on recherche sur internet ! ;) ).

# Nombre de calories par jour et par personne
NB_KCAL_PER_CAPITA_PER_DAY = 2500

# Poids moyen d'un humain : 62kg (https://en.wikipedia.org/wiki/Human_body_weight)
# Besoin en protéines moyens pour un humain : 0.9 g/kg/jour
KG_PROT_PER_CAPITA_PER_DAY = 62 * .9 * .001

dispo_int_vegetal <- dispo_int_vegetal %>% 
  left_join(Population_mondiale)
dispo_int_vegetal <- dispo_int_vegetal[,c(1,4,2,3)]
names(dispo_int_vegetal) <- c("année", "total_pop", "dom_sup_Kcal", "dom_sup_kgprot")

# résultats en termes de calories
nb_humains <- dispo_int_vegetal$dom_sup_Kcal / 365 / NB_KCAL_PER_CAPITA_PER_DAY

print(paste0("Population potentiellement nourrie par la disponibilité intérieure en produits issus de végétaux (en termes calorifiques) : ",
             round(nb_humains/1000000,2), " Millards, soit ", round(100*nb_humains/dispo_int_vegetal$total_pop, 1), " % de la population mondiale"))

# résultats en termes de protéines
nb_humains <- dispo_int_vegetal$dom_sup_kgprot / 365 / KG_PROT_PER_CAPITA_PER_DAY

print(paste0("Population potentiellement nourrie par la disponibilité intérieure en produits issus de végétaux (en termes de protéines) : ",
             round(nb_humains/1000000,2), " Millards, soit ", round(100*nb_humains/dispo_int_vegetal$total_pop, 1), " % de la population mondiale"))

### 2.4.3 Q : Rien ne se pert, tout se transforme

### Combien d'humains pourraient être nourris si toute la disponibilité alimentaire en produits
### végétaux (Food), la nourriture végétale destinée aux animaux (Feed) et les pertes de 
### produits végétaux (Waste) étaient utilisés pour de la nourriture ? 
### Donnez les résultats en termes de calories, puis de protéines, et 
### exprimez ensuite ces 2 résultats en pourcentage de la population mondiale

# Ajout de la disponibilité alimentaire en kcal et en kgprot
dispo_alim_food_feed_waste <- result %>%
  filter(origin == "vegetal") %>%
  mutate(food_feed_losses_supply_kcal = 1000000 * `ratio_kcal/kg` * (food + feed + losses),
         food_feed_losses_supply_kgprot = 1000000 * `protein_%` * (food + feed + losses) * .01)
  
# Calcul de la dispo. alim. nourriture animale et pertes de produits végétaux par année
dispo_alim_food_feed_waste <- dispo_alim_food_feed_waste %>%
  group_by(année) %>%
  summarise(food_feed_losses_supply_kcal = sum(food_feed_losses_supply_kcal),
            food_feed_losses_supply_kgprot = sum(food_feed_losses_supply_kgprot))

dispo_alim_food_feed_waste <- dispo_alim_food_feed_waste %>% 
  left_join(Population_mondiale)
dispo_alim_food_feed_waste <- dispo_alim_food_feed_waste[,c(1,4,2,3)]

names(dispo_alim_food_feed_waste) <- c("année", "total_pop", "food_feed_losses_supply_kcal", 
                                     "food_feed_losses_supply_kgprot")

# résultats en termes de calories
nb_humains <- dispo_alim_food_feed_waste$food_feed_losses_supply_kcal / 365 / NB_KCAL_PER_CAPITA_PER_DAY

print(paste0("Population potentiellement nourrie par la disponibilité alimentaire, la nourriture animale et les pertes de produits végétaux (en termes calorifiques) : ",
             round(nb_humains/1000000,2), " Millards, soit ", round(100*nb_humains/dispo_alim_food_feed_waste$total_pop, 1), " % de la population mondiale"))

# résultats en termes de protéines
nb_humains = dispo_alim_food_feed_waste$food_feed_losses_supply_kgprot / 365 / KG_PROT_PER_CAPITA_PER_DAY

print(paste0("Population potentiellement nourrie par la disponibilité alimentaire, la nourriture animale et les pertes de produits végétaux (en termes de protéines) : ",
             round(nb_humains/1000000,2), " Millards, soit ", round(100*nb_humains/dispo_alim_food_feed_waste$total_pop, 1), " % de la population mondiale"))

### 2.4.4 Q : Tous bien nourris ?

# Combien d'humains pourraient être nourris avec la disponibilité alimentaire mondiale ? 
# Donnez les résultats en termes de calories, puis de protéines, et exprimez ensuite ces 
# 2 résultats en pourcentage de la population mondiale.

dispo_alim_mondiale <- dispo_alim %>%
  group_by(année) %>%
  summarise(food_supply_kcal = sum(food_supply_kcal),
            food_supply_kgprotein = sum(food_supply_kgprotein))
  
dispo_alim_mondiale <- dispo_alim_mondiale %>%
  left_join(Population_mondiale)
dispo_alim_mondiale <- dispo_alim_mondiale[,c(1,4,2,3)]

names(dispo_alim_mondiale) <- c("année", "total_pop", "food_supply_kcal", 
                                       "food_supply_kgprotein")

# résultats en termes de calories
nb_humains <- dispo_alim_mondiale$food_supply_kcal / 365 / NB_KCAL_PER_CAPITA_PER_DAY

print(paste0("Population potentiellement nourrie par la disponibilité alimentaire mondiale (en termes calorifiques) : ",
             round(nb_humains/1000000,2), " Millards, soit ", round(100*nb_humains/dispo_alim_mondiale$total_pop, 1), " % de la population mondiale"))

# résultats en termes de protéines
nb_humains = dispo_alim_mondiale$food_supply_kgprotein / 365 / KG_PROT_PER_CAPITA_PER_DAY

print(paste0("Population potentiellement nourrie par la disponibilité alimentaire mondiale (en termes de protéines) : ",
             round(nb_humains/1000000,2), " Millards, soit ", round(100*nb_humains/dispo_alim_mondiale$total_pop, 1), " % de la population mondiale"))


#**********************************************************************************
# 3 Table sous_nutrition
#**********************************************************************************

temp2 <- read.csv("FAOSTAT_data_people_undernourished.csv", sep = ",", dec = ".")

names(temp2) <- c("xx1", "xx2", "country_code", "country", "element_code",
                           "element", "item_code", "item", "year_code", "year",
                           "unit", "nb_persons", "xx3", "xx4", "xx5")
Sous_nutrition <- temp2 %>%
  select(country_code, country, year, nb_persons) %>%
  distinct

remove(temp2)

### 3.1 Q : Proportion de la pop en sous-nutrition ?
### Quelle proportion de la population mondiale est considérée comme étant en sous-nutrition ?
# Calcul du nombre de personnes en sous-nutrition par année

    #  Remplacer les cellules vides par des NA
Sous_nutrition[Sous_nutrition==""] <- NA

temp <- subset(Sous_nutrition,  nb_persons != "<0.1")
temp$nb_persons <- as.numeric(temp$nb_persons)

nb_persons_undernourished <- temp %>%
  group_by(year) %>%
  summarise(personnes_en_sous_nutrition = sum(nb_persons) * 1000000)

### 3.2 Q : Liste des pays en sous-nutrition

### Sélectionnez parmi les données des bilans alimentaires les informations relatives 
### aux pays dans lesquels la FAO recense des personnes en sous-nutrition.

liste_pays_en_sous_nutrition <- Sous_nutrition %>%
  filter(!is.na(nb_persons)) %>%
  select(country_code, country) %>%
  distinct
names(liste_pays_en_sous_nutrition) <- c("code_pays", "pays")

### 3.3 Q : Liste des produits les plus exportés

### Repérer les 15 produits les plus exportés par ce groupe de pays.

result_Exportation <- result %>%
  select(code_pays, pays, produit, export_quantity) %>%
  distinct

Liste_15_produits_plus_exportes <- liste_pays_en_sous_nutrition %>%
  left_join(result_Exportation) %>%
  filter(export_quantity != 0) %>%
  group_by(produit) %>%
  summarise(nb_Export = n()) %>%
  arrange(desc(nb_Export), .by_group = TRUE) %>%
  head(15)

### 3.4 Q : Les plus grandes importations ?

### Parmi les données des bilans alimentaires au niveau mondial, sélectionner les 200 plus 
### grandes importations de ces produits (1 importation = une quantité d'un produit donné 
### importée par un pays donné sur l'année choisie)

Importations <- liste_pays_en_sous_nutrition %>%
  left_join(result) %>%
  inner_join(Liste_15_produits_plus_exportes, by = "produit") %>% 
  distinct(code_pays, pays, code_produit, produit, année, .keep_all = TRUE)

Importations <- Importations[,-31]

Importations <- Importations %>%
  group_by(code_pays, pays, code_produit, produit) %>%
  arrange(desc(import_quantity), .by_group = TRUE)
  
# Remarque importante : arrange, qui par défaut trie la table sans tenir compte des groupes. 
#          Pour obtenir un tri par groupe, il faut lui ajouter l’argument .by_group = TRUE
  

### 3.5 Q : Regrouper les importations par produit

### Grouper ces importations par produit, afin d'avoir une table contenant 1 ligne 
### pour chacun des 15 produits
### Ensuite, calculer pour chaque produit les 2 quantités suivantes :

### Le ratio entre la quantité destinés aux "Autres utilisations" (Other uses) et la disponibilité intérieure.

### Le ratio entre la quantité destinée à la nourriture animale et la quantité destinée à la nourriture (animale + humaine)

Importations <- Importations %>%
  filter(domestic_supply_quantity != 0) %>%
  mutate(other_Uses_vs_dom_Supply = other_uses / domestic_supply_quantity,
         feed_vs_food = feed / (feed + food))

Importations <- na.omit(Importations, cols=c("other_Uses_vs_dom_Supply", "feed_vs_food"))

Importations_par_produit <- Importations %>%
  group_by(produit) %>%
  summarise(other_Uses_vs_dom_Supply = sum(other_Uses_vs_dom_Supply),
            feed_vs_food = sum(feed_vs_food))

### 3.6 Q : Top 3 produits

# Donnez les 3 produits qui ont la plus grande valeur pour chacun des 2 ratios
# (vous aurez donc 6 produits à citer)

Top_3_ratio_feed_vs_food <- Importations_par_produit %>%
  select(produit, feed_vs_food) %>%
  arrange(desc(feed_vs_food)) %>%
  head(3)

Top_3_ratio_Other_Uses_vs_domestic_supply <- Importations_par_produit %>%
  select(produit, other_Uses_vs_dom_Supply) %>%
  arrange(desc(other_Uses_vs_dom_Supply)) %>%
  head(3)

### 3.7 Q :

# Combien de tonnes de céréales pourraient être libérées si les USA diminuaient 
# leur production de produits animaux de 10% ?


### 3.8 Q :
### En Thaïlande, quelle proportion de manioc est exportée ? 

Exportations_en_Thaïlande_2014 <- result %>%
  distinct %>%
  filter(pays == "Thailand", année == 2014, produit == "Cassava and products") %>%
  select(produit, année, export_quantity) %>%
  group_by(produit)

Exportations_en_Thaïlande_2015 <- result %>%
  distinct %>%
  filter(pays == "Thailand", année == 2015, produit == "Cassava and products") %>%
  select(produit, année, export_quantity) %>%
  group_by(produit)

Exportations_en_Thaïlande_2016 <- result %>%
  distinct %>%
  filter(pays == "Thailand", année == 2016, produit == "Cassava and products") %>%
  select(produit, année, export_quantity) %>%
  group_by(produit)

Exportations_en_Thaïlande_2017 <- result %>%
  distinct %>%
  filter(pays == "Thailand", année == 2017, produit == "Cassava and products") %>%
  select(produit, année, export_quantity) %>%
  group_by(produit)

Exportations_Manioc_en_Thaïlande <- rbind(Exportations_en_Thaïlande_2014,
                                   Exportations_en_Thaïlande_2015,
                                   Exportations_en_Thaïlande_2016,
                                   Exportations_en_Thaïlande_2017)
Exportations_Manioc_en_Thaïlande <- Exportations_Manioc_en_Thaïlande[,c(2,1,3)]

remove(Exportations_en_Thaïlande_2014,
       Exportations_en_Thaïlande_2015,
       Exportations_en_Thaïlande_2016,
       Exportations_en_Thaïlande_2017)

Total_Export_en_Thaïlande_par_annee <- result %>%
  distinct %>%
  filter(pays == "Thailand") %>%
  group_by(année) %>%
  summarise(Total_Export_Quantity = sum(export_quantity))

Exportations_Manioc_en_Thaïlande <- Exportations_Manioc_en_Thaïlande %>%
  left_join(Total_Export_en_Thaïlande_par_annee) %>%
  mutate(`Proportion_%` = round(100 * export_quantity / Total_Export_Quantity, 2))

### Quelle est la proportion de personnes en sous-nutrition ?
Sous_nutrition_en_Thaïlande <- Sous_nutrition %>%
  filter(country == "Thailand")


#**********************************************************************************
# 4 Table equilibre_prod
#**********************************************************************************

equilibre_prod <- result %>%
  select(code_pays, pays, code_produit, produit, année,
         domestic_supply_quantity,
         feed,
         seed,
         losses,
         processing,
         food,
         other_uses) %>%
  distinct(code_pays, pays, code_produit, produit, année, .keep_all = TRUE)

names(equilibre_prod) = c("code_pays", "pays", "code_produit", "produit",
                          "année", "dispo_int", "alim_ani", "semences",
                          "pertes", "transfo", "nourriture", "autres_utilisations")


#**********************************************************************************
#5 Q : Query SQL
#**********************************************************************************

## connect to database (don't forget to disconnect)
DB <- dbConnect(MySQL(), user="root", host="localhost",
                password="Traore160410@", dbname="projet_fao")

#La base de données que nous venons de créer est vide :
dbListTables(DB) # list tables

dbSendQuery(DB, "SET GLOBAL local_infile = true;") # <--- Added this

#Nous allons y ajouter le tableau de données suivant :

dbWriteTable(DB, "population", population, row.names = FALSE) # write dataframe to the database
dbWriteTable(DB, "dispo_alim", dispo_alim, row.names = FALSE)
dbWriteTable(DB, "Sous_nutrition", Sous_nutrition, row.names = FALSE)
dbWriteTable(DB, "equilibre_prod", equilibre_prod, row.names = FALSE)
dbWriteTable(DB, "result", result, row.names = FALSE)
dbWriteTable(DB, "Population_mondiale", Population_mondiale, row.names = FALSE)
dbWriteTable(DB, "cereals", cereals, row.names = FALSE)

# Déconnexion

dbDisconnect(DB)






