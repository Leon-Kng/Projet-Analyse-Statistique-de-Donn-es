
rm(list=ls()) # nettoyer espace de travail
tab1<-read.table("Rdata_exposition_larvaire.csv",header=TRUE,sep="\t")
library(car) 
library(FactoMineR)
library(ggplot2)

##cr?ation de la table du jeux de donn?e
clean_data<-tab1
##suppression des NA
clean_data<-na.omit(clean_data)

##creation des colonnes supl?mentaire necessaire
##nombre total d'individu sois la somme des larves et des cocons
clean_data$nbr_tot_indiv<-clean_data$Nombre_larves+clean_data$Nombre_cocons

##Taux de survie sois le nombre d'adulte diviser par le nombre total d'individue calculer plus haut
clean_data$taux_survie<-(clean_data$Nombre_femelles+clean_data$Nombre_males)/clean_data$nbr_tot_indiv

##transformation des donn?e pour essayer de suivre une loi normal
clean_data$transfo<-asin(sqrt(clean_data$taux_survie))
#arcsin squar root transformation

#BOXPLOT
##creation data pour le plot
Manip<-as.factor(clean_data$Manip)
Temperature<-as.factor(clean_data$Temperature_exposition)
taux_survie<-clean_data$taux_survie
duree <- as.factor(clean_data$duree_temp)

data_plot<-data.frame(Manip,Temperature,taux_survie)

boxplot_brut<-ggplot(data_plot, aes(x=Temperature, y=taux_survie, fill=Manip)) + geom_boxplot()
boxplot_brut<-boxplot_brut + ggtitle("Effet de la temp?rature et de la dur?e d'exposition \n sur le taux de survie des parasite") + theme(plot.title = element_text(hjust=0.5))
boxplot_brut

##test anova
#test sur donn?e brut
anov_brut=lm(taux_survie~Temperature*duree)
Test_taux_survie<-Anova(anov_brut,type="II")
Test_taux_survie
#plot
par(mfrow=c(1,3))
plot_taux_survie<-plot(anov_brut,which=c(1,2))
plot_taux_survie
Taux_de_survie<-clean_data$taux_survie
hist(Taux_de_survie)



#test sur donn?e transformer
anov_transfo=lm(transfo~as.factor(Temperature_exposition)*as.factor(duree_temp),data=clean_data)
Test_transfo<-Anova(anov_transfo,type="II")
Test_transfo
#plot
par(mfrow=c(1,3))
plot_transfo<-plot(anov_transfo,which=c(1,2))
plot_transfo
Donn?es_transformer<-clean_data$transfo
hist(Donn?es_transformer)



clean_data$log_taux<- log(clean_data$taux_survie)
clean_data_log<-clean_data[-47,]

#test sur donn?e log
anov_log=lm(log_taux~as.factor(Temperature_exposition)*as.factor(duree_temp),data=clean_data_log)
Test_log<-Anova(anov_log,type="II")
Test_log
#plot
par(mfrow=c(1,3))
plot_log<-plot(anov_log,which=c(1,2))
plot_log
logarithme_taux_survie<-clean_data_log$log_taux
hist(logarithme_taux_survie)


##represnetation des donn?es en histogramme
hist(clean_data$taux_survie)
hist(clean_data$transfo)
hist(clean_data_log$log_taux)

