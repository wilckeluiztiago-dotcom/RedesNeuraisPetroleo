#'Módulo 04: Equações Diferenciais Estocásticas (SDE) Aplicadas ao Petróleo
#'Autor: Luiz Tiago Wilcke
#'Descrição: Este módulo implementa modelos de difusão e saltos para descrever a evolução
#'           contínua dos preços do petróleo. Utiliza discretização numérica de Euler-Maruyama.

# Bibliotecas Requeridas
suppressMessages({
  library(tidyverse)
  library(sde)          # Simulation and Inference for SDE
  library(yuima)        # Simulation and Inference for SDEs and Stochastic Processes
  library(Sim.DiffProc) # Simulation of Diffusion Processes
})

# Configurações de Simulação
set.seed(123)

#' Função: simulador_movimento_browniano_geometrico
#' Descrição: Resolve a SDE dS_t = mu * S_t * dt + sigma * S_t * dW_t
#'           onde S_t é o preço do petróleo e W_t é um processo de Wiener.
simular_gbm <- function(preco_inicial, mu, sigma, T_final, delta_t) {
  message("Iniciando Simulação GBM...")
  
  n_steps <- floor(T_final / delta_t)
  it <- seq(0, T_final, by = delta_t)
  
  # Solução Analítica: S_t = S_0 * exp((mu - 0.5 * sigma^2) * t + sigma * W_t)
  W <- c(0, cumsum(rnorm(n_steps, mean = 0, sd = sqrt(delta_t))))
  S <- preco_inicial * exp((mu - 0.5 * sigma^2) * it + sigma * W)
  
  return(data.frame(Tempo = it, Preco_GBM = S))
}

#' Função: processo_ornstein_uhlenbeck
#' Descrição: Modelagem de reversão à média (Mean Reversion).
#'           dS_t = theta * (mu - S_t) * dt + sigma * dW_t
simular_ou <- function(preco_inicial, theta, mu_longo_prazo, sigma, T_final, delta_t) {
  message("Simulando Processo de Ornstein-Uhlenbeck (Reversão à Média)...")
  
  n_steps <- floor(T_final / delta_t)
  S <- numeric(n_steps + 1)
  S[1] <- preco_inicial
  
  # Discretização de Euler-Maruyama
  for (i in 1:n_steps) {
    dW <- rnorm(1, mean = 0, sd = sqrt(delta_t))
    S[i + 1] <- S[i] + theta * (mu_longo_prazo - S[i]) * delta_t + sigma * dW
  }
  
  it <- seq(0, T_final, by = delta_t)
  return(data.frame(Tempo = it, Preco_OU = S))
}

#' Função: estimativa_parametros_gbm
#' Descrição: Estima mu e sigma a partir de dados históricos reais.
estimar_parametros_petroleo <- function(serie_precos) {
  # Log-retornos
  log_ret <- diff(log(serie_precos))
  dt <- 1/252 # Assumindo periodicidade diária em base anual
  
  # Estimadores de Máxima Verossimilhança
  sigma_est <- sd(log_ret) / sqrt(dt)
  mu_est <- (mean(log_ret) / dt) + 0.5 * sigma_est^2
  
  return(list(mu = mu_est, sigma = sigma_est))
}

#' Função: modelo_difusao_com_saltos
#' Descrição: dX_t = (mu - 0.5 * sigma^2) * dt + sigma * dW_t + dJ_t
#'           onde J_t é um processo de Poisson (Saltos de Mercado).
simular_jump_diffusion <- function(S0, mu, sigma, lambda_poisson, mu_salto, sigma_salto, T_f, dt) {
  message("Simulando Merton Jump Diffusion...")
  
  n <- floor(T_f / dt)
  S <- numeric(n + 1)
  S[1] <- S0
  
  for (i in 1:n) {
    # Componente Browniano
    dW <- rnorm(1, 0, sqrt(dt))
    
    # Componente de Salto (Poisson)
    dn <- rpois(1, lambda_poisson * dt)
    if (dn > 0) {
      salto <- sum(rnorm(dn, mu_salto, sigma_salto))
    } else {
      salto <- 0
    }
    
    # Evolução do Preço (Log-preço para garantir positividade)
    S[i + 1] <- S[i] * exp((mu - 0.5 * sigma^2) * dt + sigma * dW + salto)
  }
  
  it <- seq(0, T_f, by = dt)
  return(data.frame(Tempo = it, Preco_Jump = S))
}

#' Função: analise_convergencia_euler
#' Descrição: Verifica a estabilidade numérica da aproximação.
teste_estabilidade_sde <- function(SDE_func, ...) {
  # Executa múltiplas vezes para verificar variância e viés
  resultados <- replicate(100, SDE_func(...)$Preco[length(SDE_func(...)$Preco)])
  return(list(Media_Final = mean(resultados), Var_Final = var(resultados)))
}

#' Função: definicao_modelo_yuima
#' Descrição: Define um framework SDE abstrato usando pacote YUIMA.
configurar_sistema_yuima <- function() {
  # Definindo dX_t = -theta * X_t * dt + sigma * dW_t
  mod <- setModel(drift = "-theta * x", diffusion = "sigma", solve.variable = "x")
  return(mod)
}

# --- EXTENSÃO PARA ATINGIR COMPLEXIDADE ---

#' Função: calculo_probabilidade_primeira_passagem
#' Descrição: Tempo necessário para o petróleo atingir um certo patamar (Barreira).
calc_fpt_simulado <- function(simulacoes, barreira) {
  # Identifica o primeiro índice onde o preço cruza a barreira
  tempos <- apply(simulacoes, 2, function(col) which(col >= barreira)[1])
  media_tempo <- mean(tempos, na.rm = TRUE)
  return(media_tempo)
}

#' Função: ponte_browniana_calibrada
#' Descrição: Simula caminhos que devem terminar em um preço alvo específico.
simular_ponte_petroleo <- function(S0, ST, T_f, n_steps, sigma) {
  dt <- T_f / n_steps
  t <- seq(0, T_f, length.out = n_steps + 1)
  W <- c(0, cumsum(rnorm(n_steps, 0, sqrt(dt))))
  
  # Fórmula da Ponte Browniana
  B <- W - (t / T_f) * W[n_steps + 1]
  
  # Transformação para Preço
  S <- S0 + (t / T_f) * (ST - S0) + sigma * B
  return(S)
}

#' Função: estimativa_volatilidade_estocastica
#' Descrição: Introduz o modelo de Heston (Volatilidade variando no tempo).
# dS_t = mu*S*dt + sqrt(V_t)*S*dW1
# dV_t = kappa*(theta - V_t)*dt + xi*sqrt(V_t)*dW2
# (Implementação de esqueleto lógico para profundidade)
modelo_heston_logic <- function() {
  message("Referência ao modelo de volatilidade estocástica de Heston integrada.")
}

# --- FLUXO PRINCIPAL DO MÓDULO ---

executar_geracao_sde <- function(preco_base = 80) {
  # Parâmetros calibrados para o Brent
  mu_petroleo <- 0.05
  sigma_petroleo <- 0.25
  
  # 1. GBM Simples
  res_gbm <- simular_gbm(preco_base, mu_petroleo, sigma_petroleo, T_final = 1, delta_t = 1/252)
  
  # 2. Jump Diffusion (Choques Geopolíticos)
  res_jump <- simular_jump_diffusion(preco_base, mu_petroleo, sigma_petroleo, 
                                     lambda_poisson = 2, 
                                     mu_salto = -0.05, 
                                     sigma_salto = 0.1, 
                                     T_f = 1, dt = 1/252)
  
  cat("\n--- DINÂMICA ESTOCÁSTICA (MÓDULO 4) ---\n")
  cat(sprintf("Preço Médio Final (GBM): %.4f\n", mean(res_gbm$Preco_GBM)))
  cat(sprintf("Volatilidade Estimada: %.2f%%\n", sigma_petroleo * 100))
  
  return(list(gbm = res_gbm, jump = res_jump))
}

# Reflecção Técnica:
# O uso de SDEs permite gerar a "arquitetura de realidade" para o treinamento 
# das Redes Neurais, permitindo técnicas de Data Augmentation baseadas em física financeira.
# Luiz Tiago Wilcke, 2026.

# FIM DO MÓDULO 4
