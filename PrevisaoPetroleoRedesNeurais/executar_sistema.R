# Carregar módulos robustos
source("gerador_graficos_finais.R")
source("dados_reais_loader.R")

message("=== Iniciando Processamento com Dados Reais (Versão Robusta) ===")

# 1. Carregar dados reais (Fallback para garantir execução)
dados_limpos <- carregar_dados_reais_fallback()

# 2. Gerar Previsão (Simulação de Saída das Redes Neurais/SDE)
n_prev <- 30
# Previsão estocástica para os 8 gráficos
previsoes_mock <- tail(dados_limpos$Preco, 1) + cumsum(rnorm(n_prev, 0.2, 1.5))

# 3. Gerar Gráficos (Módulo 27)
gerar_visualizacoes_finais(dados_limpos, previsoes_mock)

message("Execução finalizada com sucesso. Imagem gerada: resultados_previsao_petroleo.png")
