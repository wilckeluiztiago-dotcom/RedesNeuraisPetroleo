#'Módulo 18: Ensemble Learning - Random Forest e Extreme Gradient Boosting
#'Autor: Luiz Tiago Wilcke
#'Descrição: Este módulo implementa modelos de ensemble baseados em árvores de decisão 
#'           para capturar interações complexas entre indicadores físicos e financeiros.

# Bibliotecas Requeridas
suppressMessages({
  library(tidyverse)
  library(randomForest) # Florestas Aleatórias
  library(xgboost)      # Extreme Gradient Boosting
  library(pdp)          # Partial Dependence Plots
})

#' Função: ajustar_xgboost_petroleo
#' Descrição: Treina um modelo XGBoost com DMatrix e objetivo de regressão linear.
executar_xgboost_total <- function(X_train, y_train) {
  message("Iniciando treinamento XGBoost...")
  
  dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
  
  params <- list(
    objective = "reg:squarederror",
    eta = 0.1,
    max_depth = 6
  )
  
  modelo <- xgb.train(params = params, data = dtrain, nrounds = 100)
  
  return(modelo)
}

# FIM DO MÓDULO 18
# Luiz Tiago Wilcke - 2026
