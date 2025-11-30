**Analyse du délai de livraison**

## 1. Contexte

Ce projet s’inscrit dans le cadre du cours d’Économétrie Quantitative à l’INSSEDS.
Il porte sur l’étude des facteurs influençant le **délai de livraison** à partir d’un dataset opérationnel contenant des informations sur la distance, la météo, le trafic, l’expérience du coursier et le temps de préparation.

## 2. Objectif

L’objectif principal est de :

* nettoyer et préparer le jeu de données,
* réaliser une analyse exploratoire (univariée et bivariée),
* identifier les variables influençant significativement le délai de livraison,
* construire un **modèle économétrique explicatif** (ANOVA et régression linéaire multiple),
* générer un modèle capable de prédire le délai pour de nouvelles commandes.


## 3. Compétences mobilisées

**Statistique & économétrie**

* Analyse descriptive
* Tests statistiques (normalité, ANOVA…)
* Sélection de variables (regsubsets)
* Régression linéaire multiple et validation du modèle
* Gestion des valeurs manquantes (KNN)

**Data Science**

* Nettoyage avancé des données
* Imputation et traitement des outliers
* Encodage et transformation des variables
* Visualisation avec ggplot2
* Calcul d’indicateurs : Skewness, Kurtosis, R², Eta², V de Cramer

**Programmation**

* R (tidyverse, FactoMineR, VIM, car)


## 4. Résultats clés

* Le **délai de livraison** est fortement influencé par :
  ✔ la météo
  ✔ le niveau de trafic
  ✔ la distance
  ✔ le temps de préparation
* Les conditions **pluie/neige + trafic élevé** augmentent significativement le délai.
* L’expérience du coursier a un effet modéré mais positif.
* Le modèle final de régression est globalement **significatif, cohérent et interprétable**.
* Le modèle permet également d’estimer le délai pour de nouvelles commandes.


## 5. Auteur

**DAGNOGO Safiatou**
INSSEDS – Projet d’Économétrie Quantitative
2025
