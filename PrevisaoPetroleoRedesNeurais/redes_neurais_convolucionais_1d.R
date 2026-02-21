#'Módulo 25: Redes Neurais Convolucionais 1D (CNN)
#'Autor: Luiz Tiago Wilcke
#'Descrição: Este módulo utiliza convoluções unidimensionais para extrair padrões 
#'           morfológicos escondidos na série temporal de preços.

# Bibliotecas Requeridas
suppressMessages({
  library(tidyverse)
  library(keras)
})

#' Função: construir_cnn_1d
#' Descrição: Arquitetura com camadas Conv1D e GlobalAveragePooling.
criar_cnn_petroleo <- function(input_shape) {
  modelo <- keras_model_sequential() %>%
    layer_conv_1d(filters = 64, kernel_size = 3, activation = "relu", input_shape = input_shape) %>%
    layer_max_pooling_1d(pool_size = 2) %>%
    layer_flatten() %>%
    layer_dense(units = 1)
  return(modelo)
}

# FIM DO MÓDULO 25
# Luiz Tiago Wilcke - 2026
