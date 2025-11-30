

####-------------- Importation --------------------------
library(readr)
library(dplyr)
library(tidyr)
library(moments)
library(ggplot2)

df <- read_csv("C:/Users/DELL/OneDrive/Desktop/INSSEDS/Small Project Econometric Quanti/delai_livraison.csv")
print(df)
str(df)

####------------------ Formatage--------------------------

df <- df[, !names(df) %in% "Order_ID"]

df$Distance_km <- as.numeric(df$Distance_km)
df$Preparation_Time_min <- as.integer(df$Preparation_Time_min)
df$Courier_Experience_yrs <- as.integer(df$Courier_Experience_yrs)
df$Delivery_Time_min <- as.integer(df$Delivery_Time_min)

df$Weather <- as.factor(df$Weather)
df$Time_of_Day <- as.factor(df$Time_of_Day)
df$Traffic_Level <- as.factor(df$Traffic_Level)
df$Vehicle_Type <- as.factor(df$Vehicle_Type)
str(df)


####--------------- Traitement des valeurs manquantes -------------------

library(visdat)
library(naniar)
library(Amelia)
summary(df)
nrow(df[!complete.cases(df),])   # Nbre de ligne contenant NA
mean(!complete.cases(df)) * 100  # % de lignes avec au moins un NA

colSums(is.na(df))              # Nbre de NA par colonne
colMeans(is.na(df)) * 100       # % de NA par colonne
     
cat("\n Nombre total de NA =", sum(is.na(df)))   # Nbre Total de NA dans tout le dataframe
sum(is.na(df)) / (nrow(df) * ncol(df)) * 100    # % total de NA dans tout le dataframe

####----------------- Visualisation -----------------------------

gg_miss_var(df)
vis_miss(df)
missmap(df, main = "Carte des valeurs manquantes", col = c("red", "green"))



####-------------------- Imputation KNN -------------------------
# Charger le package

library(VIM)

dataframe <- kNN(df, k = 10, imp_var = FALSE)

cat("\n Nombre total de NA après traitement =", sum(is.na(dataframe)))

vis_dat(dataframe)
missmap(dataframe, main = "Carte des valeurs manquantes après traitement", col = c("red", "green"))


####------------------ Traitement des valeurs aberrantes ------------------

#Visualisation
library(rpart)
extrême_value <- boxplot(dataframe$Delivery_Time_min)

#Traitement
library(DescTools)
dataframe$Delivery_Time_min <- Winsorize(dataframe$Delivery_Time_min)
boxplot(dataframe$Delivery_Time_min)

#### Export du jeu de donnée propre
write.csv(dataframe,"Delivery_Time_Cleaned.csv")


####---------------- Analyse univariée pour var quanti---------------------

# Variables quantitatives
quant_cols <- c("Distance_km",
                "Preparation_Time_min",
                "Courier_Experience_yrs",
                "Delivery_Time_min")

# 1. Statistiques descriptives
summary(dataframe[, quant_cols])

# 2. Skewness et Kurtosis
for (col in quant_cols) {
  cat("\n", col, ":\n")
  cat("  Skewness  =", skewness(dataframe[[col]]), "\n")
  cat("  Kurtosis  =", kurtosis(dataframe[[col]]), "\n")
  cat("-------------------------------------------\n")
}

# 3. Histogrammes
par(mfrow = c(2,2))
for (col in quant_cols) {
  hist(dataframe[[col]],
       breaks = 20,
       col = "skyblue",
       border = "black",
       main = paste("Histogramme de", col),
       xlab = col)
}
par(mfrow = c(1,1))

# 4. Q-Q Plots
par(mfrow = c(2,2))
for (col in quant_cols) {
  qqnorm(dataframe[[col]], main = paste("Q-Q Plot de", col))
  qqline(dataframe[[col]], col = "red", lwd = 2)
}
par(mfrow = c(1,1))


####---------------- Analyse univariée pour var quali ----------------

cat_cols <- c("Weather", "Traffic_Level", "Time_of_Day", "Vehicle_Type")

# Stockage des tableaux
tables <- list()

# 1. Tableaux effectifs + pourcentages
for (col in cat_cols) {
  
  counts <- table(dataframe[[col]])
  percentages <- round(prop.table(counts) * 100, 2)
  
  df_table <- data.frame(
    Modalité = names(counts),
    Effectif = as.numeric(counts),
    `Frequence (%)` = as.numeric(percentages)
  )
  
  tables[[col]] <- df_table
  
  cat("\n--- Analyse univariée de la variable '", col, "' ---\n", sep = "")
  print(df_table)
}

# 2. Diagrammes en barres
palette_couleur <- c("red", "blue", "green3", "purple", "orange", "brown", "pink", "gray")

par(mfrow = c(2, 2))

for (col in cat_cols) {
  
  counts <- table(dataframe[[col]])
  percentages <- round(prop.table(counts) * 100, 2)
  
  bar_centers <- barplot(
    percentages,
    col = palette_couleur[1:length(percentages)],
    main = paste("Répartition de", col),
    ylim = c(0, max(percentages) + 10),
    xlab = col,
    ylab = "Pourcentage (%)"
  )
  
  text(
    x = bar_centers,
    y = percentages,
    labels = paste0(percentages, "%"),
    pos = 3,
    cex = 0.8
  )
}

par(mfrow = c(1, 1))


####---------------- Analyse bivariée entre var quanti ----------------

quantitative_vars <- c("Distance_km", "Preparation_Time_min", "Courier_Experience_yrs")
r2_results <- c()

for (var in quantitative_vars) {
  model <- lm(dataframe$Delivery_Time_min ~ dataframe[[var]])
  r2_results[var] <- summary(model)$r.squared
}

r2_df <- data.frame(
  Variable = names(r2_results),
  R2 = round(as.numeric(r2_results), 3)
)

print(r2_df)

# Heatmap R² avec ggplot2
library(ggplot2)

ggplot(r2_df, aes(x = "Delivery_Time_min", y = Variable, fill = R2)) +
  geom_tile(color = "white") +
  geom_text(aes(label = R2), color = "black", size = 5) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(
    title = "R² entre Delivery_Time_min et les variables quantitatives",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0),
    panel.grid = element_blank()
  )

####---------------- Analyse bivariée entre une var quanti et une quali ------------------

# Fonction Eta²
eta_squared <- function(quantitative, qualitative) {
  
  overall_mean <- mean(quantitative)
  
  group_means <- tapply(quantitative, qualitative, mean)
  group_sizes <- tapply(quantitative, qualitative, length)
  
  between_group <- sum(group_sizes * (group_means - overall_mean)^2)
  total_variance <- sum((quantitative - overall_mean)^2)
  
  eta2 <- between_group / total_variance
  return(eta2)
}

# Calcul de Eta² 
qualitative_vars <- c("Weather", "Traffic_Level", "Time_of_Day", "Vehicle_Type")

eta_results <- c()

for (var in qualitative_vars) {
  eta_results[var] <- eta_squared(dataframe$Delivery_Time_min,
                                  dataframe[[var]])
}

eta_df <- data.frame(
  Variable = names(eta_results),
  Eta2 = round(as.numeric(eta_results), 3)
)

print(eta_df)

# Heatmap en ggplot2

ggplot(eta_df, aes(x = "Eta²", y = Variable, fill = Eta2)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Eta2), size = 5) +
  scale_fill_gradient(low = "lightyellow", high = "brown") +
  labs(
    title = "Rapport de corrélation (Eta²) entre Delivery_Time_min et variables qualitatives",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0),
    panel.grid = element_blank()
  )


####---------------------- Analyse bivariée pour var quali---------------------

# ✅ Calcul des V de Cramer des paires de variables explicatives 
# Charger les bibliothèques nécessaires
library(questionr)
library(corrplot)

# Sélectionner uniquement les variables qualitatives
df_cats <- dataframe[, sapply(dataframe, is.factor)]

# Convertir en facteur si nécessaire
#df_cats[] <- lapply(df_cats, as.factor)

# Initialiser la matrice de V de Cramer
n <- ncol(df_cats)
cramer <- matrix(NA, n, n)
colnames(cramer) <- rownames(cramer) <- colnames(df_cats)

# Calcul du V de Cramer
for (i in 1:n) {
  for (j in 1:n) {
    cramer[i, j] <- CramerV(table(df_cats[[i]], df_cats[[j]]))
  }
}

# Arrondir la matrice pour affichage
cramer_matrix <- round(cramer, 2)
cramer_matrix


# ✅ Visualisation avec seulement les noms des variables qualitatives
corrplot(cramer_matrix,
         method = "circle",
         type = "lower",
         tl.col = "red",     # couleur des étiquettes
         tl.cex = 0.9,       # taille du texte
         tl.srt = 45,        # angle des étiquettes
         is.corr = FALSE)    # important pour que les valeurs entre 0 et 1 soient acceptées



####------------------------ ACP -------------------------
library(FactoMineR)
library(factoextra)

# Sélectionner uniquement les variables quantitatives pour l'ACP
df_num <- dataframe[, sapply(dataframe, is.numeric)]

# Vérifie le nombre de variables
ncol(df_num)

# faire l'ACP sur ces variables
res.pca <- PCA(df_num, scale.unit = TRUE, ncp = 5, graph = TRUE)

# voir les valeurs propres
res.pca$eig

# Description des dimensions
dimdesc(res.pca)


# Graphe des valeurs propres 
barplot(res.pca$eig[,1], main="Valeurs 
propres",names.arg=paste("dim",1:nrow(res.pca$eig)))

# visualisation des inerties 
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 30)) 

#Graphe sur les dimensions 3 et 4 
plot(res.pca,choix="ind",cex=0.8,axes=3:4)              

plot(res.pca,choix="var",cex=0.8,habillage="SEXE",axes=3:4) 


####------------------------ ACM -----------------------------
#chargement du package FactoMineR 
library(FactoMineR)

# Selection des var categorielles
df_cats <- dataframe[, sapply(dataframe, is.factor)]
# Réalisation l'ACM
res.MCA <-MCA(df_cats)

#--------------------------- Graphe sur dimension 3 & 4
plot(res.MCA, axes = 3:4)

#------------------------------------- Graphe et ellipses de confiance 
plotellipses(res.MCA) 

#-------------------------- Voir les valeurs propres
res.MCA$eig

#----------------------------- Graphe des valeurs propres 
barplot(res.MCA$eig[,1], main="Valeurs 
propres",names.arg=paste("dim",1:nrow(res.MCA$eig)))

plot(res.MCA$eig[,1],
     type = "b",
     xlab = "Dimension",
     ylab = "Valeur propre",
     main = "Éboulis des valeurs propres (scree plot)")


#---------------------- visualisation des inerties 
library("factoextra") 
# Extraire les valeurs propres
eig.val <- get_eigenvalue(res.MCA)
fviz_eig(res.MCA, addlabels = TRUE, ylim = c(0, 30)) 


#------------------- Description des dimensions
dimdesc(res.MCA)


####---------------- Choix des variables ------------------------------

# regsubsets pour une régression linéaire
modele_subset <- regsubsets(Delivery_Time_min ~ ., 
                            data = df_encoded, 
                            nbest = 1,        # meilleur modèle pour chaque taille de subset
                            nvmax = NULL,     # nombre max de variables à tester (NULL = toutes)
                            method = "exhaustive")  # ou "forward", "backward"

summary_subset <- summary(modele_subset)
summary_subset

#--------------------- Visualisation Graphique
plot(modele_subset, scale="bic")

####---------------- Régression Modèle final ---------------------------------------
reg.fin <- lm( Delivery_Time_min ~ Distance_km + Preparation_Time_min +
               Courier_Experience_yrs + Traffic_Level +
                 Weather, data = dataframe)
summary(reg.fin)
anova(reg.fin)


####----------------- Analyse des résidus --------------------------------------------------
res.m<-rstudent(reg.fin)  
plot(res.m,pch=15,cex=.5,ylab="Residus",ylim=c(-3,3)) 
abline(h=c(-2,0,2),lty=c(2,1,2))

# Pourcentage de résidus contenu dans l'intervalle [-2,2]
cat("Pourcentage de résidus contenu dans l'intervalle [-2,2] est",sum(as.numeric(abs(res.m)<=2))/nrow(df)*100, "%")  


####--------------------Test de validité du modèle -------------------------
#LES DONNEES 
residus<-residuals(reg.fin) 
res.normalise<-rstudent(reg.fin) 
val.estimees<-fitted.values(reg.fin)


####----------------- Test de multicolinéarité
library(car) 
vif(reg.fin)
library(olsrr) 
ols_vif_tol(reg.fin)


####-------------------------------- Test de linéarité
#------------ Test graphique

quant_vars <- c("Distance_km", "Preparation_Time_min", "Courier_Experience_yrs",
                "Traffic_Level", "Weather")

for (var in quant_vars) {
  p <- ggplot(dataframe, aes_string(x = var, y = "Delivery_Time_min")) +
    geom_point(color = "skyblue", alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    labs(title = paste("Régression linéaire entre Delivery_Time_min et", var),
         x = var, y = "Delivery_Time_min") +
    theme_minimal()
  
  print(p)
}


#-------------- Tous les test graphique sur une même figure

library(ggplot2)
library(tidyr)

# Nom de la variable dépendante
var_dep <- "Delivery_Time_min"

# Identifier toutes les colonnes numériques
vars_num <- c("Distance_km", "Preparation_Time_min", "Courier_Experience_yrs",
              "Traffic_Level", "Weather")

# Garder la variable dépendante + les variables explicatives numériques
vars_keep <- c(var_dep, setdiff(vars_num, var_dep))

# Mettre au format long
df_long <- df_encoded[, vars_keep] %>%
  pivot_longer(
    cols = setdiff(vars_keep, var_dep),
    names_to = "Variable",
    values_to = "Valeur"
  )

# Graphique unique avec facettes
ggplot(df_long, aes(x = Valeur, y = .data[[var_dep]])) +
  geom_point(color = "skyblue", alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  facet_wrap(~ Variable, scales = "free_x") +
  labs(title = paste("Régression linéaire de", var_dep, "avec toutes les variables quantitatives"),
       x = "Variable explicative",
       y = var_dep
  ) +
  theme_minimal()

#------------------- Test de Rainbow
# H0 : Le modèle est linéaire
# H1 : Le modèle n'est pas linéaire
library(lmtest)  
raintest(reg.fin) 


#======================================= Correction de l'hypothèse de linéarité

#----------------- Régression polynomiale

modele_poly <- lm(Delivery_Time_min ~ poly(Distance_km, 2) +
                    poly(Preparation_Time_min, 2) +
                    Traffic_Level +
                    Weather,
                  data = dataframe)

raintest(modele_poly)   
crPlots(modele_poly)



#----------------- Régression Spline
library(splines)
modele_spline <- lm(Delivery_Time_min ~ 
                      bs(Distance_km, df = 4) +
                      bs(Preparation_Time_min, df = 4) +
                      Traffic_Level +
                      Weather,
                    data = dataframe)


raintest(modele_spline) 
crPlots(modele_spline)



#--------------- Avec une régression additive généralisée (GAM)
library(mgcv)
modele_gam <- gam(Delivery_Time_min ~ 
                    s(Distance_km) +
                    s(Preparation_Time_min) +
                    Traffic_Level+
                    Weather,
                  data = dataframe)
summary(modele_gam)
raintest(modele_gam)     

plot(modele_gam)
crPlots(modele_gam)



#----------------------- Modèle de machine learning

library(randomForest)

set.seed(123)  # Pour la reproductibilité

# Séparer jeu d'entraînement et de test
n <- nrow(df_encoded)
index_train <- sample(1:n, size = 0.8 * n)

train_data <- df_encoded[index_train, ]
test_data  <- df_encoded[-index_train, ]


# Modèle Random Forest
rf_model <- randomForest(
  Delivery_Time_min ~ ., 
  data = train_data,
  ntree = 500,       # nombre d'arbres
  mtry = floor(sqrt(ncol(train_data) - 1)), # nb de variables testées à chaque split
  importance = TRUE
)

# Résumé du modèle
print(rf_model)

# Importance des variables
importance(rf_model)
varImpPlot(rf_model)

# Prédictions sur le test set
pred_rf <- predict(rf_model, newdata = test_data)

# Performance : RMSE et R²
library(ModelMetrics)

# RMSE
rmse_rf <- rmse(test_data$Delivery_Time_min, pred_rf)

# R² (calcul manuel)
r2_rf <- cor(test_data$Delivery_Time_min, pred_rf)^2

cat("RMSE =", rmse_rf, "\nR² =", r2_rf, "\n")

raintest(rf_model) 


#----------------- 𝑽(𝜺𝒕) = 𝑬(𝜺𝒕 𝟐) = 𝝈𝜺 𝟐   (Hypothèse d’homoscédasticité des erreurs)
plot(reg.fin,1)


#TEST DE BREUSCH-PANGAN
# H0∶ 𝑖𝑙 𝑦 𝑎 ℎ𝑜𝑚𝑜𝑠𝑐é𝑑𝑎𝑠𝑡𝑖𝑐𝑖𝑡é  
# H1∶ 𝑖𝑙 𝑦 𝑎 ℎé𝑡é𝑟𝑜𝑠𝑐é𝑑𝑎𝑠𝑡𝑖𝑐𝑖𝑡é    
bptest(reg.fin)


#----------------- Test d'autocorrelation des erreures : 𝑪𝒐𝒗(𝜺𝒕,𝜺𝒔) = 𝟎 
res.m<-rstudent(reg.fin) 
plot(res.m,pch=15,cex=.5,ylab="Residus",ylim=c(-3,3),type="b") 
abline(h=c(-2,0,2),lty=c(2,1,2)) 

acf(residus,plot = FALSE)

acf(residus,plot = TRUE)

dwtest(reg.fin) # DURBAN-WASTON
bgtest(reg.fin) # BREUSCH-GODFREY 


#----------- Test de centralité des erreurs : 𝑬(𝜺𝒕)= 0
mean(residus)


#------------ Test d'indépendance des erreurs : 𝑪𝒐𝒗(𝑿,𝜺)= 0
plot(reg.fin,1)


#----------- Test de normalité des residus

# Test graphique

#Méthode 1
qqnorm(residus, main = "QQ-Plot des résidus")
qqline(residus, col = "red", lwd = 2)


#Méthode 2
residus_df <- data.frame(residus = residus)

ggplot(residus_df, aes(sample = residus)) +
  stat_qq(size = 2, color = "black") +
  stat_qq_line(color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "QQ-Plot des résidus",
       x = "Quantiles théoriques",
       y = "Quantiles observés") +
  theme_minimal(base_size = 14)

# Test d'hypothèse
shapiro.test(residus)
library(tseries)
jarque.bera.test(residus)


####------------------------------- Prevision --------------------------

# Créer un nouveau jeu de données pour la commande
nouvelle_commande <- data.frame(
  Distance_km = 3,
  Preparation_Time_min = 12,
  Courier_Experience_yrs = 3,
  Traffic_Level = factor("Medium", levels = levels(dataframe$Traffic_Level)),
  Weather = factor("Snowy", levels = levels(dataframe$Weather))
)

# Prédiction de délai de livraison
prediction <- predict(reg.fin, newdata = nouvelle_commande, interval = "prediction")
prediction


# Tâche 2 : Modélisation du ’’délai de livraison’’ 
# en fonction de la Météo par un modèle d’ANOVA


# 1. Boxplot du délai de livraison par météo
library(ggplot2)

ggplot(dataframe, aes(x = Weather, y = Delivery_Time_min, fill = Weather)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 16) +
  labs(title = "Délai de livraison selon la météo",
       x = "Météo",
       y = "Délai de livraison (min)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

dataframe$Weather <- as.factor(dataframe$Weather)
levels(dataframe$Weather)

# 2. Ajustement du modèle ANOVA
anova_model <- aov(Delivery_Time_min ~ Weather, data = dataframe)

# 3. Résumé du modèle
summary(anova_model)
# -> Test global : si p-value < 0.05, la météo influence significativement le délai

# 4. Vérification des hypothèses
# Normalité des résidus
shapiro.test(residuals(anova_model))

# Homogénéité des variances (test de Levene)
library(car)
leveneTest(Delivery_Time_min ~ Weather, data = dataframe)

# 5. Test post-hoc (si effet significatif)
TukeyHSD(anova_model)

# 6. Analyse des résidus
library(ggplot2)

# 1. Calcul des résidus studentisés
res.m.anova <- rstudent(anova_model)
res_data <- data.frame(
  Index = 1:length(res.m.anova),
  Residus = res.m.anova
)

# 2. Visualisation avec ggplot
ggplot(res_data, aes(x = Index, y = Residus)) +
  geom_point(shape = 15, size = 2, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", color = "red") +
  labs(title = "Analyse des résidus studentisés du modèle ANOVA",
       x = "Index des observations",
       y = "Résidus studentisés") +
  theme_minimal(base_size = 14)


# Pourcentage de résidus contenu dans l'intervalle [-2,2]
cat("Pourcentage de résidus contenu dans l'intervalle [-2,2] est",sum(as.numeric(abs(res.m.anova)<=2))/nrow(df)*100, "%")  
anova(anova_model)




