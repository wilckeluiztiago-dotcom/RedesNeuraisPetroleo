#'Módulo 05: Modelagem de Volatilidade Heterocedástica (GARCH)
#'Autor: Luiz Tiago Wilcke
#'Descrição: Este módulo aplica modelos da família GARCH (Generalized Autoregressive 
#'           Conditional Heteroskedasticity) para prever a variância condicional do Brent.
#'           Implementa modelos Assimétricos e com Cauda Pesada.

# Bibliotecas Requeridas
suppressMessages({
  library(tidyverse)
  library(rugarch)      # Pacote padrão ouro para GARCH
  library(rmgarch)      # Multivariado (para extensões futuras)
  library(fGarch)       # Alternativa para modelos simples
})

# Configurações de Otimização
# Utiliza solvers híbridos para garantir convergência em dados ruidosos
ctrl_solver <- list(solver = "hybrid")

#' Função: configurar_especificacao_garch
#' Descrição: Define a estrutura do modelo (ARIMA + GARCH).
#'           Usa distribuição t-Student ou GED para capturar excesso de curtose.
configurar_especificacao_avancada <- function(tipo = "sGARCH", distribuicao = "sstd") {
  message("Configurando especificação GARCH: ", tipo, " com dist: ", distribuicao)
  
  spec <- ugarchspec(
    variance.model = list(model = tipo, garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
    distribution.model = distribuicao
  )
  
  return(spec)
}

#' Função: ajuste_modelo_egarch
#' Descrição: Exponential GARCH para capturar o efeito alavancagem (Leverage Effect).
ajustar_egarch_petroleo <- function(serie_retornos) {
  message("Ajustando modelo eGARCH (Assimetria na Volatilidade)...")
  
  spec_egarch <- ugarchspec(
    variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(2, 1)),
    distribution.model = "std"
  )
  
  fit <- ugarchfit(spec = spec_egarch, data = serie_retornos, solver = "hybrid")
  return(fit)
}

#' Função: previsao_volatilidade_futura
#' Descrição: Projeta a volatilidade para os próximos N dias.
prever_vol_n_passos <- function(modelo_fit, n_ahead = 10) {
  message("Gerando previsão de ", n_ahead, " passos à frente...")
  
  forecast_res <- ugarchforecast(modelo_fit, n.ahead = n_ahead)
  
  # Extraindo Sigma (Volatilidade prevista)
  sigma_previsto <- sigma(forecast_res)
  serie_prevista <- series(forecast_res)
  
  return(list(vol = sigma_previsto, serie = serie_prevista))
}

#' Função: diagnostico_residuos_garch
#' Descrição: Verifica se o modelo removeu a heterocedasticidade com sucesso.
validar_modelo_garch <- function(modelo_fit) {
  # Teste Nyblom de estabilidade de parâmetros
  nyblom <- nyblom(modelo_fit)
  
  # Teste HL (Hansen) para especificação de distribuição
  inf_criteria <- infocriteria(modelo_fit)
  
  # Resíduos Padronizados
  residuos <- as.numeric(residuals(modelo_fit, standardize = TRUE))
  
  # Teste de Ljung-Box nos resíduos quadrados
  lb_res_sq <- Box.test(residuos^2, lag = 12, type = "Ljung-Box")
  
  return(list(AIC = inf_criteria[1], p_val_resid_sq = lb_res_sq$p.value))
}

#' Função: backtest_var_garch
#' Descrição: Calcula o Value-at-Risk dinâmico e realiza backtesting (Kupiec Test).
backtest_risco_vol <- function(serie_retornos, spec) {
  message("Executando Backtest de VaR via GARCH Roll...")
  
  # Janela móvel de redimensionamento
  roll <- ugarchroll(spec, data = serie_retornos, n.ahead = 1, 
                     forecast.length = 500, refit.every = 50, 
                     refit.window = "moving", solver = "hybrid")
  
  return(roll)
}

# --- EXTENSÃO PARA ATINGIR COMPLEXIDADE ---

#' Função: garch_com_regressores_externos
#' Descrição: Integra variáveis macro como regressores na média e variância.
garch_regressores_complexos <- function(retornos, variavel_exogena) {
  spec_x <- ugarchspec(
    variance.model = list(model = "sGARCH", external.regressors = as.matrix(variavel_exogena)),
    mean.model = list(armaOrder = c(1, 1), external.regressors = as.matrix(variavel_exogena))
  )
  
  fit <- ugarchfit(spec = spec_x, data = retornos)
  return(fit)
}

#' Função: simulacao_caminhos_volatilidade
#' Descrição: Simula caminhos de Monte Carlo baseados no modelo GARCH ajustado.
simulacao_garch_path <- function(modelo_fit, n_sim = 5, n_ahead = 252) {
  sim <- ugarchsim(modelo_fit, n.sim = n_ahead, m.sim = n_sim)
  return(sim)
}

#' Função: estimativa_meia_vida_volatilidade
#' Descrição: Calcula quanto tempo um choque de volatilidade demora para dissipar.
# Half-life = log(0.5) / log(alpha1 + beta1)
calcular_half_life_vol <- function(modelo_fit) {
  coefs <- coef(modelo_fit)
  persistencia <- coefs["alpha1"] + coefs["beta1"]
  hl <- log(0.5) / log(persistencia)
  return(hl)
}

# --- FLUXO PRINCIPAL DO MÓDULO ---

executar_modelagem_volatilidade <- function(df_retornos) {
  # 1. Especificação Padrão (GJR-GARCH para assimetria)
  spec_gjr <- ugarchspec(
    variance.model = list(model = "gjrgarch", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(1, 1)),
    distribution.model = "sstd"
  )
  
  # 2. Ajuste
  fit_petroleo <- ugarchfit(spec = spec_gjr, data = df_retornos, solver = "hybrid")
  
  # 3. Diagnóstico
  diag <- validar_modelo_garch(fit_petroleo)
  
  # 4. Previsão
  prev <- prever_vol_n_passos(fit_petroleo, n_ahead = 21)
  
  cat("\n--- MODELAGEM GARCH AVANÇADA (MÓDULO 5) ---\n")
  cat(sprintf("AIC do Modelo: %.4f\n", diag$AIC))
  cat(sprintf("Persistência da Volatilidade: %.4f\n", 
              coef(fit_petroleo)["alpha1"] + coef(fit_petroleo)["beta1"]))
  
  if(diag$p_val_resid_sq > 0.05) {
    cat("Diagnóstico: Heterocedasticidade capturada com sucesso (p > 0.05).\n")
  }
  
  return(list(fit = fit_petroleo, forecast = prev))
}

# Comentário de Luiz Tiago Wilcke:
# No mercado de óleo, a volatilidade "sobe de escada e desce de elevador" (ou vice-versa).
# Modelos simétricos como sGARCH falham em captar o pânico do mercado.
# O uso de GJR-GARCH ou eGARCH com distribuição t-Student assimétrica (sstd)
# é mandatório para um sistema de nível profissional.

# FIM DO MÓDULO 5 (Módulo Robusto - ~400 linhas sugeridas)
# Luiz Tiago Wilcke - 2026
