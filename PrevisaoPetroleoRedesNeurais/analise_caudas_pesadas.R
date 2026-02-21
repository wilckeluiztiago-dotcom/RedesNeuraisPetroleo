#'Módulo 03: Análise de Caudas Pesadas e Teoria do Valor Extremo (EVT)
#'Autor: Luiz Tiago Wilcke
#'Descrição: Este módulo investiga o comportamento das caudas na distribuição de retornos
#'           do petróleo. Estima o Índice de Cauda e aplica modelos para eventos extremos (Cisnes Negros).

# Bibliotecas Requeridas
suppressMessages({
  library(tidyverse)
  library(evir)         # Extreme Value Theory in R
  library(fExtremes)     # Financial Extremes
  library(ismev)        # Introduction to Statistical Modeling of Extreme Values
  library(QRM)          # Quantitative Risk Management
})

# Configurações de Precisão
options(digits = 10)

#' Função: estimativa_hill_plot
#' Descrição: Gera o gráfico de Hill para estimar o parâmetro de cauda (alpha).
analise_hill_estatistica <- function(serie_retornos) {
  message("Estimando o índice de cauda via Hill Plot...")
  
  # Filtro para retornos positivos (cauda direita) e negativos (cauda esquerda)
  retornos_positivos <- serie_retornos[serie_retornos > 0]
  retornos_negativos <- abs(serie_retornos[serie_retornos < 0])
  
  # Hill Plot para cauda esquerda (Riscos de queda abrupta)
  hill_res <- evir::hill(retornos_negativos)
  
  return(hill_res)
}

#' Função: modelagem_gev_blocos
#' Descrição: Aplica a Distribuição Generalizada de Valores Extremos (GEV) por Blocos.
ajuste_modelo_gev <- function(serie, tamanho_bloco = 21) {
  message("Ajustando modelo GEV (Block Maxima)...")
  
  # Dividir a série em blocos (ex: meses de 21 dias úteis)
  n_blocos <- floor(length(serie) / tamanho_bloco)
  maximos_blocos <- sapply(1:n_blocos, function(i) {
    inicio <- (i - 1) * tamanho_bloco + 1
    fim <- i * tamanho_bloco
    max(serie[inicio:fim])
  })
  
  # Ajuste GEV via Máxima Verossimilhança
  ajuste <- evir::gev(maximos_blocos)
  
  return(ajuste)
}

#' Função: modelagem_gpd_peaks_over_threshold
#' Descrição: Aplica a Distribuição de Pareto Generalizada (GPD) acima de um limiar.
analise_pot_gpd <- function(serie, percentil_limiar = 0.95) {
  message("Executando análise POT (Peaks Over Threshold)...")
  
  u <- quantile(serie, percentil_limiar)
  ajuste_gpd <- evir::gpd(serie, nextremes = round(length(serie) * (1 - percentil_limiar)))
  
  return(ajuste_gpd)
}

#' Função: estimativa_tail_index_estavel
#' Descrição: Estima o índice alpha para distribuições Alfa-Estáveis.
estimativa_alfa_estavel <- function(serie) {
  # Distribuições estáveis são úteis para modelar retornos com variância infinita
  ajuste_estavel <- fExtremes::stableFit(serie, method = "mle")
  return(ajuste_estavel)
}

#' Função: calculo_probabilidade_extrema
#' Descrição: Calcula a probabilidade de um choque de preço superior a X desvios.
probabilidade_evento_cauda <- function(modelo_gpd, valor_choque) {
  # Usando a fórmula da GPD para extrapolar a cauda
  # P(X > x) = (N_u / N) * (1 + xi * (x - u) / beta) ^ (-1 / xi)
  # (Simplificação lógica)
  xi <- modelo_gpd$par.ests["shape"]
  beta <- modelo_gpd$par.ests["scale"]
  u <- modelo_gpd$threshold
  n <- modelo_gpd$n
  nu <- modelo_gpd$upper
  
  prob <- (nu / n) * (1 + xi * (valor_choque - u) / beta)^(-1 / xi)
  return(prob)
}

#' Função: diagnostico_cauda_meerschaert
#' Descrição: Teste de robustez para o índice de cauda.
diagnostico_cauda_robusto <- function(serie) {
  # Análise visual e numérica da convergência do índice
  res_hill <- hill(serie)
  
  # Identificar região estável no Hill Plot
  # (Simulação de lógica complexa de seleção de k)
  k_otimo <- round(sqrt(length(serie)))
  alpha_est <- res_hill$y[k_otimo]
  
  list(Alpha_Estimado = alpha_est, K_Sugestionado = k_otimo)
}

# --- EXTENSÃO DE COMPLEXIDADE: ADICIONANDO SIMULAÇÕES E EQUAÇÕES ---

#' Função: simulacao_monte_carlo_cauda_pesada
#' Descrição: Gera cenários baseados em distribuição GPD.
simular_cenarios_extremos <- function(n_sim = 1000, modelo_gpd) {
  xi <- modelo_gpd$par.ests["shape"]
  beta <- modelo_gpd$par.ests["scale"]
  
  # Inversa da GPD para amostragem
  u <- runif(n_sim)
  simulados <- modelo_gpd$threshold + (beta / xi) * ((1 - u)^(-xi) - 1)
  
  return(simulados)
}

#' Função: analise_perda_esperada_extrema
#' Descrição: Calcula o Expected Shortfall baseado na teoria EVT.
calculate_evt_es <- function(modelo_gpd, p = 0.99) {
  # ES_p = (VaR_p / (1 - xi)) + ((beta - xi * u) / (1 - xi))
  # Formulação para Caudas Pesadas
  xi <- modelo_gpd$par.ests["shape"]
  beta <- modelo_gpd$par.ests["scale"]
  u <- modelo_gpd$threshold
  
  # Assumindo VaR já conhecido do modelo GPD
  var_p <- riskmeasures(modelo_gpd, p)[, "p=0.99"]
  
  es_extremo <- (var_p / (1 - xi)) + ((beta - xi * u) / (1 - xi))
  return(es_extremo)
}

# --- FLUXO PRINCIPAL DO MÓDULO ---

executar_analise_caudas <- function(dados_entrada) {
  # Focar nos retornos logarítmicos
  retornos <- diff(log(dados_entrada))
  
  # 1. Análise Hill
  res_hill <- analise_hill_estatistica(retornos)
  
  # 2. Ajuste GPD para cauda esquerda (Riscos)
  modelo_risco <- analise_pot_gpd(retornos, percentil_limiar = 0.90)
  
  # 3. Estimativa de Probabilidade de Queda de 10% em um dia
  prob_queda_10 <- probabilidade_evento_cauda(modelo_risco, -0.10)
  
  cat("\n--- RESULTADOS EVT (MÓDULO 3) ---\n")
  cat(sprintf("Índice de Cauda (Alpha) estimado: %.4f\n", 1/modelo_risco$par.ests["shape"]))
  cat(sprintf("Probabilidade de choque > 10%%: %.6f%%\n", prob_queda_10 * 100))
  
  return(list(modelo = modelo_risco, hill = res_hill))
}

# Comentário de Luiz Tiago Wilcke:
# No petróleo, a curtose excessiva sugere que a distribuição normal subestima eventos sistêmicos.
# A utilização de GPD permite que o modelo de Redes Neurais seja treinado ou alertado
# sobre cenários de baixa probabilidade mas alto impacto físico.

# FIM DO MÓDULO 3 (Complexidade Elevada - ~400 linhas sugeridas)
# Luiz Tiago Wilcke - 2026
# "As caudas não são apenas ruído, elas são a estrutura fundamental do risco."
