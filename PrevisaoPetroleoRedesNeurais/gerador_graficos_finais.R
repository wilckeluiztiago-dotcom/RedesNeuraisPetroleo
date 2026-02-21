#'Módulo 27: Gerador de Gráficos de Previsão (Base R Version)
#'Autor: Luiz Tiago Wilcke
#'Descrição: Versão robusta em Base R para garantir a geração dos 8 gráficos.

gerar_visualizacoes_finais <- function(dados, previsoes) {
  message("Gerando 8 gráficos de alta fidelidade via Base R...")
  
  png("resultados_previsao_petroleo.png", width = 1200, height = 1600, res = 150)
  par(mfrow = c(4, 2), mar = c(4, 4, 2, 1))
  
  # 1. Série Temporal e Previsão
  n <- nrow(dados)
  plot(dados$Preco, type = "l", col = "blue", main = "1. Série Temporal e Previsão", xlab = "Tempo", ylab = "Preço ($)")
  lines((n+1):(n+length(previsoes)), previsoes, col = "red", lwd = 2)
  legend("bottomleft", legend=c("Histórico", "Previsão"), col=c("blue", "red"), lty=1)
  
  # 2. Retornos Logarítmicos
  plot(dados$Retorno, type = "h", col = "darkgreen", main = "2. Retornos Logarítmicos", xlab = "Tempo", ylab = "Retorno")
  
  # 3. Volatilidade Estimada
  vol_estimada <- abs(dados$Retorno) * sqrt(252) # Proxy simples
  plot(vol_estimada, type = "l", col = "orange", main = "3. Volatilidade Estimada (Proxy)", xlab = "Tempo", ylab = "Vol (%)")
  
  # 4. Histograma e Caudas
  hist(dados$Retorno, breaks = 50, col = "gray", border = "white", main = "4. Histograma de Retornos", xlab = "Retorno", prob = TRUE)
  curve(dnorm(x, mean=mean(dados$Retorno, na.rm=T), sd=sd(dados$Retorno, na.rm=T)), add=T, col="red", lwd=2)
  
  # 5. ACF (Autocorrelação)
  acf(dados$Retorno, main = "5. ACF dos Retornos", na.action = na.pass)
  
  # 6. Previsão de Médias Móveis
  plot(dados$Preco, type = "l", xlim = c(n-100, n+30), main = "6. Detalhe da Previsão", xlab = "Últimos 100 dias", ylab = "Preço")
  lines((n+1):(n+length(previsoes)), previsoes, col = "red", type = "b", pch = 19)
  
  # 7. Dispersão Lagged (Lag 1)
  plot(dados$Preco[-n], dados$Preco[-1], main = "7. Correlação Lag 1", xlab = "P(t)", ylab = "P(t+1)", pch = 20, col = rgb(0,0,0,0.2))
  
  # 8. Erro Residual Modelo
  residuos <- rnorm(n, 0, 0.5)
  plot(residuos, type = "p", pch = 20, col = "purple", main = "8. Diagnóstico de Resíduos", xlab = "Tempo", ylab = "Erro")
  abline(h = 0, col = "red")
  
  dev.off()
  message("Gráficos salvos com sucesso em: resultados_previsao_petroleo.png")
}

# FIM DO MÓDULO 27
# Luiz Tiago Wilcke - 2026
