
#' Análise de Completude de Dados por Ano
#'
#' Esta função calcula e avalia a completude de cada variável por ano, distribuindo a proporção de completude pela variável Ano_do_Banco. A completude é exibida junto com uma classificação baseada no escore de Romero & Cunha. Referência: ROMERO, Dalia E.; CUNHA, Cynthia Braga da. Avaliação da qualidade das variáveis sócio-econômicas e demográficas dos óbitos de crianças menores de um ano registrados no Sistema de Informações sobre Mortalidade do Brasil (1996/2001). Cadernos de Saúde Pública, v. 22, p. 673-681, 2006."
#'
#' @param dados Um dataframe contendo as variáveis a serem analisadas.
#' @return Retorna uma tabela com a completude de cada variável por ano, incluindo uma legenda com a classificação de completude.
#' @export
#' @name analise_completude_ano
#' @examples
#' # Supondo que você tenha um dataframe chamado dados_RHC_combinados,
#' # Use a função da seguinte forma:
#'
#' Nome_da_Sua_Tabela_de_Completud_ano <- analise_completude_ano(dados_RHC_combinados)
analise_completude_ano <- function(dados) {
  message("Iniciando a análise de completude de dados por ano.")

  # Verifica se a coluna Ano_do_Banco existe antes de tentar acessá-la
  if (!"Ano_do_Banco" %in% names(dados)) {
    stop("\033[1;31mA coluna 'Ano_do_Banco' não foi encontrada no dataframe. Função interrompida.\033[0m")
  }

  # Nome das variáveis
  variaveis <- colnames(dados)

  # Identificar os anos únicos na coluna Ano_do_Banco
  anos <- sort(unique(dados$Ano_do_Banco))

  # Função para classificar a completude com base no escore de Romero & Cunha
  classificar_completude <- function(p) {
    if (p >= 95) {
      return("E")  # Excelente
    } else if (p >= 90) {
      return("B")  # Bom
    } else if (p >= 80) {
      return("R")  # Regular
    } else if (p >= 50) {
      return("RU")  # Ruim
    } else {
      return("MR")  # Muito Ruim
    }
  }

  # Inicializar uma lista para armazenar a completude por variável e ano
  completude_lista <- list()

  # Calcular a completude por variável e ano
  for (ano in anos) {
    message(paste("Processando dados para o ano:", ano))
    dados_ano <- dados[dados$Ano_do_Banco == ano, ]
    if (nrow(dados_ano) == 0) {
      stop(paste("\033[1;31mNenhum dado encontrado para o ano", ano, ". Função interrompida.\033[0m"))
    }
    dados_ausentes <- sapply(dados_ano, function(x) {
      sum(is.na(x) | as.character(x) == "Sem informação")
    })
    completude <- round(100 - (dados_ausentes / nrow(dados_ano) * 100), 1)
    classificacao <- sapply(completude, classificar_completude)
    completude_lista[[as.character(ano)]] <- paste0(completude, "% (", classificacao, ")")
  }

  message("Criando a tabela de resultados.")
  # Criar a tabela de resultados como data frame
  Completude <- data.frame(Variavel = variaveis)

  for (ano in anos) {
    Completude[[as.character(ano)]] <- completude_lista[[as.character(ano)]]
  }

  # Adicionar a legenda explicando as classificações
  cat("\nLegenda de Classificação de Completude:\n")
  cat("E  - Excelente (>= 95%)\n")
  cat("B  - Bom (90% a 94.9%)\n")
  cat("R  - Regular (80% a 89.9%)\n")
  cat("RU - Ruim (50% a 79.9%)\n")
  cat("MR - Muito Ruim (< 50%)\n")
  cat("Classificação de Completude. Escore proposto por Romero & Cunha: ROMERO, Dalia E.; CUNHA, Cynthia Braga da. \n Avaliação da qualidade das variáveis sócio-econômicas e demográficas dos óbitos de crianças menores de um ano \n registrados no Sistema de Informações sobre Mortalidade do Brasil (1996/2001). Cadernos de Saúde Pública, v. 22, p. 673-681, 2006.\n")

  message(paste("\033[1;32m", "> Análise de completude por ano concluída. Veja o dataframe criado.", "\033[0m"))

  # Retornar a tabela de completude
  return(Completude)
}


