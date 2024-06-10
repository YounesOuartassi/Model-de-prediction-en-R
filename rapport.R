# Installer les packages si nécessaire
# install.packages("DBI")
# install.packages("RMySQL")
# install.packages("dplyr")
# install.packages("ggplot2")

# Charger les packages
library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)

# Connexion à la base de données MySQL
con <- dbConnect(RMySQL::MySQL(), 
                 dbname = "soccer",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = "")

# Vérifiez que la connexion est valide
if (!dbIsValid(con)) {
  stop("La connexion à la base de données a échoué.")
} else {
  print("Connexion réussie!")
}

# Importer les tables 'player' et 'player_attributes'
player_data <- dbGetQuery(con, "SELECT * FROM player")
player_attributes_data <- dbGetQuery(con, "SELECT * FROM player_attributes")

# Vérifier les premières lignes et la structure des données
head(player_data)
str(player_data)
head(player_attributes_data)
str(player_attributes_data)
sum(is.na(player_data))
sum(is.na(player_attributes_data))

# Fusionner les deux tables sur l'ID du joueur
merged_data <- merge(player_data, player_attributes_data, by = "player_api_id")

# Convertir la colonne birthday en un objet de type Date
merged_data$birthday <- as.Date(merged_data$birthday, format = "%Y-%m-%d")

# Calculer la différence entre la date actuelle et la date de naissance
merged_data$age <- as.numeric(difftime(Sys.Date(), merged_data$birthday, units = "days") / 365.25)

# Visualisation avancée avec ggplot2

# 1. Scatter plot pour voir la relation entre la taille, le poids et le rating global
ggplot(merged_data, aes(x = height, y = weight, color = overall_rating)) +
  geom_point() +
  labs(title = "Relation entre la taille, le poids et le rating global", x = "Taille (cm)", y = "Poids (kg)")

# 2. Boxplot pour comparer les distributions de taille et de poids
ggplot(merged_data, aes(x = factor(0), y = height)) +
  geom_boxplot() +
  labs(title = "Boîte à moustaches de la taille", x = "Joueurs", y = "Taille (cm)")

ggplot(merged_data, aes(x = factor(0), y = weight)) +
  geom_boxplot() +
  labs(title = "Boîte à moustaches du poids", x = "Joueurs", y = "Poids (kg)")

# 3. Histogramme avec ggplot2 pour les distributions de taille, poids et âge
ggplot(merged_data, aes(x = height)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Histogramme de la taille", x = "Taille (cm)", y = "Fréquence")

ggplot(merged_data, aes(x = weight)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Histogramme du poids", x = "Poids (kg)", y = "Fréquence")

ggplot(merged_data, aes(x = age)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Histogramme de l'âge", x = "Âge (ans)", y = "Fréquence")

# 4. Diagramme en secteurs pour une variable catégorielle (par exemple, la overall_rating des joueurs)
  ggplot(merged_data, aes(x = "", fill = as.factor(overall_rating))) +
    geom_bar(width = 1) +
    coord_polar("y") +
    labs(title = "Répartition du rating des joueurs", x = "", y = "", fill = "overall_rating")
 #6
  player_attributes_data$date <- as.Date(player_attributes_data$date)
  
  ggplot(player_attributes_data, aes(x = date, y = overall_rating)) +
    geom_line() +
    labs(title = "Évolution de l'overall_rating des joueurs au fil du temps", x = "Date", y = "Overall Rating")
#analyse de correlation
  # Convertir la colonne de date en un format de date
  player_attributes_data$date <- as.Date(player_attributes_data$date)
  
  # Calculer la corrélation entre "overall_rating" et la date
  correlation <- cor(player_attributes_data$overall_rating, as.numeric(player_attributes_data$date))
  
  # Afficher la corrélation
  correlation

#7
  # Charger le package de clustering
  library(stats)
  
  
  # Sélectionner les données pour le clustering
  clustering_data <- player_attributes_data[, c("overall_rating")]
  
  # Conversion en matrice si nécessaire
  clustering_matrix <- as.matrix(clustering_data)
  
  # 1. Prétraitement des données (si nécessaire)
  # Pas nécessaire dans ce cas
  
  # 2. Sélection du nombre optimal de clusters avec la méthode de l'elbow
  wss <- numeric(10) # Vecteur pour stocker l'inertie intra-cluster
  for (i in 1:10) {
    kmeans_result <- kmeans(clustering_matrix, centers = i)
    wss[i] <- kmeans_result$tot.withinss
  }
  
  # Tracer la courbe de l'inertie en fonction du nombre de clusters
  plot(1:10, wss, type = "b", xlab = "Nombre de clusters", ylab = "Inertie intra-cluster")
  
  # 3. Application de K-means avec le nombre optimal de clusters
  optimal_k <- 3 # Choix arbitraire, ajustez en fonction de la courbe de l'elbow
  kmeans_result <- kmeans(clustering_matrix, centers = optimal_k)
  
  # 4. Visualisation des clusters
  # Pour visualiser les clusters, vous pouvez utiliser une méthode appropriée selon vos données.
  # Par exemple, si vous avez des données unidimensionnelles comme ici, vous pouvez les tracer comme suit :
  plot(clustering_matrix, col = kmeans_result$cluster, main = "Clustering avec K-means")
  points(kmeans_result$centers, col = 1:optimal_k, pch = 8, cex = 2) # Ajouter les centres des clusters
  
  
#7.2
  install.packages("randomForest")
  install.packages("caret")
  library(randomForest)
  library(caret)
  
  # Convertir la colonne overall_rating en type numérique
  data$overall_rating <- as.numeric(data$overall_rating)
  
  # Fusionner les deux tables sur l'ID du joueur
  merged_data <- merge(player_data, player_attributes_data, by = "player_api_id")
  
  # Convertir la colonne date en un objet de type Date
  merged_data$date <- as.Date(merged_data$date)
  
  # Sélectionner les variables pertinentes pour la prédiction (en supprimant les colonnes non numériques)
  selected_variables <- c("potential", "crossing", "finishing", "heading_accuracy", "short_passing", "volleys",
                          "dribbling", "curve", "free_kick_accuracy", "long_passing", "ball_control",
                          "acceleration", "sprint_speed", "agility", "reactions", "balance", "shot_power",
                          "jumping", "stamina", "strength", "long_shots", "aggression", "interceptions",
                          "positioning", "vision", "penalties", "marking", "standing_tackle", "sliding_tackle",
                          "gk_diving", "gk_handling", "gk_kicking", "gk_positioning", "gk_reflexes")
  
  # Filtrer les données pour les colonnes sélectionnées et la variable cible
  data <- merged_data %>% select(all_of(selected_variables), overall_rating)
  
  # Convertir toutes les colonnes en type numérique
  data[] <- lapply(data, as.numeric)
  
  # Vérifier les types de colonnes
  str(data)
  
  # Gérer les valeurs manquantes en les remplaçant par la moyenne des colonnes correspondantes (pour les colonnes numériques)
  num_cols <- sapply(data, is.numeric)
  data[num_cols] <- lapply(data[num_cols], function(x) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    return(x)
  })
  
  # Vérifier s'il reste des valeurs manquantes
  sum(is.na(data))
  
  # Vérifier qu'il n'y a que des colonnes numériques
  str(data)
  
  # Diviser les données en ensembles d'entraînement et de test
  set.seed(123)
  trainIndex <- createDataPartition(data$overall_rating, p = .8, 
                                    list = FALSE, 
                                    times = 1)
  data_train <- data[trainIndex,]
  data_test <- data[-trainIndex,]
  
  # Entraîner le modèle de forêt aléatoire
  model_rf <- randomForest(overall_rating ~ ., data = data_train, ntree = 100)
  
  # Prédire les valeurs de l'ensemble de test
  predictions <- predict(model_rf, data_test)
  
  # Évaluer les performances du modèle
  mse <- mean((predictions - data_test$overall_rating)^2)
  print(paste("Mean Squared Error:", mse))
  
  # Importance des variables
  importance <- importance(model_rf)
  var_importance <- data.frame(Variables = rownames(importance), Importance = importance[, 1])
  
  # Visualiser l'importance des variables
  ggplot(var_importance, aes(x = reorder(Variables, Importance), y = Importance)) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    labs(title = "Importance des variables", x = "Variables", y = "Importance")
#8
  install.packages("boot")
  library(boot)
  
  # Créer une fonction pour calculer le RMSE
  rmse <- function(actual, predicted) {
    sqrt(mean((actual - predicted)^2))
  }
  
  # Définir le modèle
  model <- randomForest(overall_rating ~ ., data = data_train, ntree = 100)
  
  # Validation croisée avec 5 plis
  cv_results <- cv.glm(data_train, model, K = 5)
  
  # Afficher les résultats de la validation croisée
  print(cv_results)
  
  # Calculer le RMSE moyen sur les plis de validation croisée
  mean_rmse <- mean(cv_results$delta)
  print(paste("Mean RMSE:", mean_rmse))
  
#####
  # Division en ensembles d'entraînement et de test
  set.seed(123)
  train_test <- createDataPartition(data$overall_rating, p = .8, list = FALSE)
  data_train <- data[train_test,]
  data_test <- data[-train_test,]
  
  
  
  # Prédire les valeurs de l'ensemble de test
  predictions <- predict(model_rf, data_test)
  
  # Calculer le Mean Squared Error (MSE)
  mse <- mean((predictions - data_test$overall_rating)^2)
  print(paste("Mean Squared Error:", mse))
  
  # Calculer le coefficient de détermination (R²)
  ss_total <- sum((data_test$overall_rating - mean(data_test$overall_rating))^2)
  ss_res <- sum((data_test$overall_rating - predictions)^2)
  r_squared <- 1 - (ss_res / ss_total)
  print(paste("R-squared:", r_squared))
  
  
  # Prédire les valeurs de l'ensemble de test
  predictions <- predict(model_rf, data_test)
  
  # Évaluer les performances du modèle
  mse <- mean((predictions - data_test$overall_rating)^2)
  print(paste("Mean Squared Error:", mse))
  
  