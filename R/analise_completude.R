
#' Análise de Completude de Dados
#'
#' Esta função calcula e avalia a completude de cada variável em um dataframe e gera um gráfico de barras horizontal. A completude é exibida junto com uma classificação baseada no escore de Romero & Cunha. Referência: ROMERO, Dalia E.; CUNHA, Cynthia Braga da. Avaliação da qualidade das variáveis sócio-econômicas e demográficas dos óbitos de crianças menores de um ano registrados no Sistema de Informações sobre Mortalidade do Brasil (1996/2001). Cadernos de Saúde Pública, v. 22, p. 673-681, 2006.
#'
#' @param dados Um dataframe contendo as variáveis a serem analisadas.
#' @return Retorna uma tabela com a completude de cada variável, incluindo um gráfico de barras horizontal referente aos Dados Ausentes.
#' @export
#' @name analise_completude
#' @examples
#' # Supondo que você tenha um dataframe chamado dados_RHC_combinados,
#' # Use a função da seguinte forma:
#'
#' analise_completude(dados_RHC_combinados)
#'
#' OBSERVAÇÃO: Sem atribuir a nenhum objeto.
analise_completude <- function(dados) {
  message("\033[32mIniciando a análise de completude de dados.\033[0m")

  # Número de linhas do dataframe
  numero_linhas <- nrow(dados)

  # Quantidade de anos distintos e intervalo de anos na variável Ano_do_Banco
  anos <- unique(dados$Ano_do_Banco)
  quantidade_anos <- length(anos)
  intervalo_anos <- paste(min(anos), max(anos), sep = "-")

  # Nome das variáveis
  variaveis <- colnames(dados)

  message("Calculando o total de NA e 'Sem informação'.")
  # Número de NA's e "Sem informação" por coluna
  dados_ausentes <- sapply(dados, function(x) {
    sum(is.na(x) | as.character(x) == "Sem informação")
  })

  message("Calculando a proporção de Dados Ausentes por coluna.")
  # Calcular a porcentagem de NA's por coluna em porcentagem, arredondada para duas casas decimais (usada no gráfico)
  porcentagem <- round((dados_ausentes / numero_linhas) * 100, 2)

  # Calcular a completude como o complemento da porcentagem
  completude <- round(100 - porcentagem, 2)

  # Função para classificar a completude com base no escore de Romero & Cunha
  classificar_completude <- function(p) {
    if (p >= 95) {
      return("Excelente")
    } else if (p >= 90) {
      return("Bom")
    } else if (p >= 80) {
      return("Regular")
    } else if (p >= 50) {
      return("Ruim")
    } else {
      return("Muito Ruim")
    }
  }

  # Aplicar a classificação
  classificacao <- sapply(completude, classificar_completude)

  message("Tabela de resultados:")

  # Criar a tabela de resultados
  Ausentes <- data.frame(Variavel = variaveis,
                         Completude = completude,
                         Dados_Ausentes = dados_ausentes,
                         Classificacao_Completude = classificacao,
                         row.names = NULL)

  message(paste("\033[32mTabela – Análise da qualidade dos dados por Classificação da Completude de",
                numero_linhas,
                "registros de câncer do RHC de um período de", quantidade_anos, "anos,", intervalo_anos, ".\033[0m"))

  # Ordenar a tabela em ordem crescente de Dados_Ausentes
  Ausentes <- Ausentes[order(Ausentes$Dados_Ausentes), ]

  # Exibir a tabela de ausentes sem os números das linhas
  print(Ausentes, row.names = FALSE)

  # Ajustar as margens do gráfico (margem esquerda e direita ajustadas)
  par(mar = c(6, 17, 2, 10) + 0.1)

  # Criar o título do gráfico com o número de linhas incluído
  titulo_grafico <- paste("Figura – Gráfico de Dados Ausentes e Análise de Completude dos Dados de",
                          numero_linhas,
                          "registros de câncer \ndo Registro Hospitalar de Câncer, de um período de",
                          quantidade_anos, "anos,", intervalo_anos, ".")

  # Criar o gráfico de barras horizontal com a ordem invertida
  bp <- barplot(Ausentes$Dados_Ausentes,
                names.arg = Ausentes$Variavel,
                horiz = TRUE,
                las = 1,
                col = colorRampPalette(c("darkgreen", "green", "yellow", "orange", "red"))(length(Ausentes$Dados_Ausentes)),
                border = NA,
                xlab = NA,
                main = titulo_grafico,
                cex.names = 0.8)

  # Adicionar os valores de Dados_Ausentes, Porcentagem e Classificação no final das barras
  text(x = Ausentes$Dados_Ausentes,
       y = bp,
       labels = paste(Ausentes$Dados_Ausentes, " (", porcentagem[order(dados_ausentes)], "%, ", Ausentes$Classificacao_Completude, ")", sep = ""),
       pos = 4,
       cex = 0.7,
       col = "black",
       xpd = TRUE)

  # Adicionar a fonte
  mtext("Classificação de Completude: ROMERO, Dalia E.; CUNHA, Cynthia Braga da. Avaliação da qualidade das variáveis sócio-econômicas e demográficas dos óbitos de crianças \n menores de um ano registrados no Sistema de Informações sobre Mortalidade do Brasil (1996/2001). Cadernos de Saúde Pública, v. 22, p. 673-681, 2006.",
        side = 1, line = 4, at = 0.1, cex = 0.8, col = "black", xpd = TRUE, adj = 0)

  # Forçar a atualização da aba "Plots"
  dev.flush()


  cat("\n\n\n")
  cat("Informações extras:\n")
  cat("Classificação de Completude. Escore proposto por Romero & Cunha: ROMERO, Dalia E.; CUNHA, Cynthia Braga da. \n Avaliação da qualidade das variáveis sócio-econômicas e demográficas dos óbitos de crianças menores de um ano \n registrados no Sistema de Informações sobre Mortalidade do Brasil (1996/2001). Cadernos de Saúde Pública, v. 22, p. 673-681, 2006.")
  cat("\n\n")
  message(paste("\033[1;32m", "> Análise de completude concluída.", "\033[0m"))
}

