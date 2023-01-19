#### Projet analyse de données Statistiques S2 M1 BEE ####
library(ggplot2)
rm(list=ls()) # nettoyer espace de travail
data_brute <- read.csv("Rdata_exposition_larvaire.csv", header=TRUE, sep="\t")  # importation des données


### Ajout de colonnes 
data_brute$adultes_vivants <- data_brute$Nombre_femelles + data_brute$Nombre_males
data_brute$sex_ratio <- data_brute$Nombre_males / data_brute$Nombre_femelles
data_brute$taux_survie <- (data_brute$adultes_vivants)/(data_brute$Nombre_larves + data_brute$Nombre_cocons + data_brute$adultes_vivants)

data_brute$Devenir_v2 <- ifelse(data_brute$Devenir=="chrysalide", "pas de parasitoide", 
                                ifelse(data_brute$Devenir=="morte", "pas de parasitoide",
                                       ifelse(data_brute$Devenir=="parasitoide", "parasitoide", NA)))


data <- data_brute


### Barplots et tests du khi deux
par(mfrow=c(1,2)) # on va afficher 2 plots en même temps
nom_etiquettes <- c("5°C","10°C", "15°C","20°C","27°C")   # nom des étiquettes pour les plots

# Barplot longue exposition
data_chen_para_C <- subset(data, data$Manip=="C") # on prend que les chenilles exposées 50 jours
tab_cont_C <- table(data_chen_para_C$Devenir_v2, data_chen_para_C$Temperature_exposition)  # tableau de contingence TempXdevenir
#tab_cont_C_vector <- as.vector(tab_cont_C)  # on convertit de table à vecteur pour le plot
tab_cont_C
df_C <- as.data.frame.table(tab_cont_C)
colnames(df_C)[1] <- "Devenir"
colnames(df_C)[2] <- "Température"
colnames(df_C)[3] <- "Effectifs"
df_C$Total_Temp=c(0,0,0,0,0,0,0,0,0,0)

# Calcul de la somme pour chaque température pour déterminer la proportion
df_C[1,4]=df_C[1,3]+df_C[2,3]
df_C[2,4]=df_C[1,3]+df_C[2,3]
df_C[3,4]=df_C[3,3]+df_C[4,3]
df_C[4,4]=df_C[3,3]+df_C[4,3]
df_C[5,4]=df_C[5,3]+df_C[6,3]
df_C[6,4]=df_C[5,3]+df_C[6,3]
df_C[7,4]=df_C[7,3]+df_C[8,3]
df_C[8,4]=df_C[7,3]+df_C[8,3]
df_C[9,4]=df_C[9,3]+df_C[10,3]
df_C[10,4]=df_C[9,3]+df_C[10,3]
  
df_C$Fréquence <- df_C$Effectifs/df_C$Total_Temp

ggplot(data=df_C, aes(x=Température, y=Fréquence, fill=Devenir)) +
  geom_bar(stat="identity")

chisq.test(tab_cont_C)  # test du khid deux


# Barplot courte exposition
data_chen_para_L <- subset(data, data$Manip=="L")
tab_cont_L=table(data_chen_para_L$Devenir_v2, data_chen_para_L$Temperature_exposition)
df_L <- as.data.frame.table(tab_cont_L)

colnames(df_L)[1] <- "Devenir"
colnames(df_L)[2] <- "Température"
colnames(df_L)[3] <- "Effectifs"
df_L$Total_Temp=c(0,0,0,0,0,0,0,0)

# Calcul de la somme pour chaque température pour déterminer la proportion
df_L[1,4]=df_L[1,3]+df_L[2,3]
df_L[2,4]=df_L[1,3]+df_L[2,3]
df_L[3,4]=df_L[3,3]+df_L[4,3]
df_L[4,4]=df_L[3,3]+df_L[4,3]
df_L[5,4]=df_L[5,3]+df_L[6,3]
df_L[6,4]=df_L[5,3]+df_L[6,3]
df_L[7,4]=df_L[7,3]+df_L[8,3]
df_L[8,4]=df_L[7,3]+df_L[8,3]


df_L$Fréquence <- df_L$Effectifs/df_L$Total_Temp

ggplot(data=df_L, aes(x=Température, y=Fréquence, fill=Devenir)) +
  geom_bar(stat="identity")

chisq.test(tab_cont_L)

