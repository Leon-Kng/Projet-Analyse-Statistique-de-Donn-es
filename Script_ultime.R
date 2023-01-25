
### OUTILS ET MANIPULATION DES DONNEES ###
rm(list=ls())  # supprime les elements de l'environnement
dataframe<-read.table("color.csv",header=TRUE,sep="\t",stringsAsFactors = TRUE) # ouvre les donnees d'un CSV avec une tabulation en tant que separateur et traite les string comme des facteurs
dataframe<-read_excel("chemin fichier.xlsx") # ouvre donnees d'un excel (necessite library "readxl")
load("projet.RData") # ouvre un projet en .RData avec des varibales dans l'environnement

head(dataframe) # affiche les 1eres lignes 
summary(df$colonne) # calcule stats utile sur la variable
mean(df$colonne)# calcule la moyenne des valeurs de la colonne desiree
var(df$colonne) # calcule la variance
sd(df$colonne) # standard error = erreur type de la moyenne = (ecart type de la pop / sqrt(taille echantillon) 
length(df$col) # donne la longueur de la colonne/ le nb de valeurs
sqrt(x) # fonction racine carrée
echantillon<-sample(df$col,50) # permet de creer un echantillon a partir d'une population, ici 50 tirages
df_splited=split(df, df$col) # va spliter le df en fonction des valeurs dans variable col par exemple si SEXE alors split F et M 
c(1,2,3) # permet de concater 1, 2 et 3 en un seul vecteur
rep("test",100) # cree un vecteur avec "test" 100 fois
df$var<-as.factor(df$var) # transforme la variable en facteur 
df2 <- df[1:3,4:7] # permet de creer un nouveau df avec les lignes 1 a 3 et les colonnes 4 a 7 du 1er df, remplacer les lignes par un c(1,4,8) permet de choisir exactement celles voulues, si aucun nombre pour les lignes alors prend toutes les lignes
df2<-df[df$col1 %in% c(1,4,"test"),] # creation d'un df2 avec toutes les lignes de df dans lesquel la variable de la colonne col1 contiennent 1 ou 4 ou "test"
valeurs_col1_ech1<-df$col1[df$col2=="test"] # stocke dans un vecteur les valeurs que prend la variable de la colonne 1 lorsque qu'une autre la valeur d'une autre variable est "test"
df2$col1<-droplevels(tab2$col1)  # diminue le nombre de niveau au nombre de niveau qu'il y a reellement


### TESTS STATISTIQUES ###

# Test de correlation de Pearson
cor.test(x2,y2,  method = "pearson")

# Test du Khi deux (Test si effet d'une variable qualitative sur une autre variable qualitative) :
table_contingence=table(inventory$espece,inventory$habitat)
expected_frequencies=prop.table(table_contingence) # pas sur du truc !!
expected_counts=round(expected_frequencies * nrow(inventory)) # pariel bizarre, donne juste la table de contingence
chisq.test(table_contingence) # test sur le tableau de contingence
chisq.test(table_contingence)$expected  #affichage des effectifs theoriques -> test si pour chaque classe >5 car condition de test
chisq.test(table_contingence)$resid # donne les residus 
seuil=qchisq(p=0.95,df=12)  #df = degree of freedom = (nb de lignes -1) * (nb de colonnes -1)
# on compare les effectifs attendus aux effectifs observes pour la conclusion biologique


# Test t de Student (comparaison de la valeur moyenne d'une variable dans 2 échantillons) :
morphometrie_splited=split(morphometrie, morphometrie$sexe)
hist(morphometrie_splited$F$lgr, xlab="Longueur femur", main="Femelles")
qqnorm(morphometrie_splited$F$lgr, main = "Q-Q Plot Femelles", xlab = "Quantiles theoriques", ylab = "Quantiles d'echantillon") # test que chaque echantillon suive bien une loi normale avec un graphique quantile-quantile
qqline(morphometrie_splited$F$lgr)
qqnorm(morphometrie_splited$M$lgr, main = "Q-Q Plot Males", xlab = "Quantiles theoriques", ylab = "Quantiles d'echantillon")
qqline(morphometrie_splited$M$lgr)
boxplot(var1~var2,data=df,xlab="Nom variable 1",ylab="Nom variable 2")
# On considère que la variance de la variable etudiee est la meme pour les 2 echantillons -> condition du test
t.test(morphometrie_splited$M$lgr,morphometrie_splited$F$lgr,var.equal=TRUE, alternative="greater") # test de Student entre les valeurs de la variable de chaque échantillon, alternative ="two.sided" si on cherche a savoir si la valeur moyenne d'un des echantillon est plus grande ou moins grande que l'autre et alternative="less" si on veut savoir si elle est plus petite
qt(0.95,ddl)


# ACP - Analyse en composantes principales :
library(corrplot)
library(FactoMineR)
library(scatterplot3d)
# Formation de la matrice de correlations
mat <- cor(df[,1:6])  # choix des colonnes etudiees 
corrplot(mat)
corrplot(mat, type="upper", order="hclust", tl.col="black", tl.srt=45)
scatterplot3d(df[,1:3], pch=16) # plot en 3D
acp <- PCA(df, quali=7:8) # stocke les resultats de l'APC et on precise que colonne 7 a 8 sont qualitatives et pas quantitatives
barplot(acp$eig[,1]) # donne le graphe des eboulis/histogramme des valeurs propres, on prend que la 1ere col car elle contient les eigenvalue=valeurs propres
plot.PCA(acp, choix="var", axes=c(2,3)) # cercle des correlations, ici on prend les dimensions 2 et 3 pour faire le cerc
plot.PCA(acp, choix ="ind", habillage=8, invisible="quali", label="none") # graphe des individus, habillage=8 signifie que la colonne 8 du df originiel determine les categories, par ex espece

### VALEUR SEUIL ###
qt(0.95, 10, lower.tail = TRUE) # quantile 0.95 pour la loi de Student, de base lower.tail=TRUE
# Si lower.tail=TRUE alors on regarde la valeur de x pour laquelle on a 95% de la surface sous la courbe à gauche et si lower.tail=FALSE alors les 95% sont à droite de la valeur seuil



### GRAPHIQUES ###
par(mfrow=c(x,y)) # determine l'affichage des graphiques par la suite, ils seront affiches avec x lignes et y colonnes
plot(df$var1,df$var2,pch=19) #plot basique
boxplot(var1~var2,data=df,xlab="Nom variable 1",ylab="Nom variable 2")
hist(df$col1) # histogramme de la variable de la colonne 1, permet de voir la distribution 

### PHRASES TYPES RESULTATS ###
# Rejet de H0 -> On rejette H0 et on accepte l'hypothese alternative N1 avec une confiance de 95% car la probabilite d'observer de tel resultats est trop faible dans le cas d'H0.
# Pas de rejet de H0 -> On ne rejette pas H0 car la probabilité d'observer de tel résultats dans le cas de cette hypothèse est trop importante.
