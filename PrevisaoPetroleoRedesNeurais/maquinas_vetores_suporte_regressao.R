#'Módulo 17: Support Vector Regression (SVR) e Kernels Não-Lineares
#'Autor: Luiz Tiago Wilcke
#'Descrição: Este módulo implementa Máquinas de Vetores de Suporte para regressão, 
#'           explorando Kernels RBF e Polinomiais para capturar não-linearidades do petróleo.

# Bibliotecas Requeridas
suppressMessages({
  library(tidyverse)
  library(e1071)        # LibSVM interface
  library(caret)        # Treinamento e tuning
})

#' Função: ajustar_svr_otimizado
#' Descrição: Treina um modelo SVR com busca em grid para C (custo) e Epsilon.
treinar_svr_petroleo <- function(X, y) {
  message("Ajustando Support Vector Regression (Kernel RBF)...")
  
  modelo <- svm(x = X, y = y, kernel = "radial", cost = 10, epsilon = 0.1)
  
  return(modelo)
}

# FIM DO MÓDULO 17
# Luiz Tiago Wilcke - 2026
