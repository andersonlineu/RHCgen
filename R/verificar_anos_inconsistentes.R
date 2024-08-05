#' Verifica Inconsistências Entre Anos em um Dataframe
#'
#' Esta função verifica se há inconsistências na ordem cronológica dos anos fornecidos em um dataframe. As verificações incluem:
#' - "Ano_Primeiro_Diagnostico" deve ser igual ou posterior a "Ano_Primeira_Consulta"
#' - "Ano_Primeiro_Diagnostico" deve ser igual ou posterior a "Ano_Triagem"
#' - "Ano_Inicio_Tratamento" deve ser igual ou posterior a "Ano_Primeira_Consulta"
#' - "Ano_Inicio_Tratamento" deve ser igual ou posterior a "Ano_Triagem"
#' - "Ano_Inicio_Tratamento" deve ser igual ou posterior a "Ano_Primeiro_Diagnostico"
#' A função retorna um dataframe com o quantitativo e a proporção de inconsistências por verificação.
#'
#' @param data Um dataframe contendo as colunas "Ano_Primeiro_Diagnostico", "Ano_Primeira_Consulta", "Ano_Triagem" e "Ano_Inicio_Tratamento".
#' @return Retorna um dataframe com o quantitativo e a proporção de inconsistências por verificação.
#' @export
#' @name verificar_anos_inconsistentes
#' @examples
#' # Escreva o nome do dataframe e execute a função. Se seu dataframe for "dados_RHC_combinados", use a função como:
#'
#' resultados_anos_inconsistentes <- verificar_anos_inconsistentes(dados_RHC_combinados)
verificar_anos_inconsistentes <- function(data) {
  required_columns <- c("Ano_Primeiro_Diagnostico", "Ano_Primeira_Consulta", "Ano_Triagem", "Ano_Inicio_Tratamento")

  # Verificar se todas as colunas necessárias estão presentes
  for (col in required_columns) {
    if (!col %in% colnames(data)) {
      message(paste("\033[1;31mErro: Coluna", col, "ausente no dataframe.\033[0m"))
      return(NULL)
    }
  }

  message("Iniciando a verificação de inconsistências entre os anos.")

  # Função para calcular inconsistências
  calcular_inconsistencias <- function(condicao, label) {
    inconsistencias <- sum(condicao, na.rm = TRUE)
    proporcao <- paste0(round((inconsistencias / nrow(data)) * 100, 2), "%")
    if (inconsistencias > 0) {
      message(paste("Inconsistência encontrada, Variável", label, ":\033[1;32m", inconsistencias, "(", proporcao, ")\033[0m"))
    } else {
      message(paste("Nenhuma inconsistência encontrada para", label, "."))
    }
    return(data.frame(
      Descricao_das_Inconsistencias = label,
      Quantitativo_Inconsistencias = inconsistencias,
      Proporcao_Inconsistencias = proporcao,
      stringsAsFactors = FALSE
    ))
  }

  # Verificações
  resultados <- list()
  resultados[[1]] <- calcular_inconsistencias(data$Ano_Primeiro_Diagnostico < data$Ano_Primeira_Consulta, "Ano_Primeiro_Diagnostico < Ano_Primeira_Consulta")
  resultados[[2]] <- calcular_inconsistencias(data$Ano_Primeiro_Diagnostico < data$Ano_Triagem, "Ano_Primeiro_Diagnostico < Ano_Triagem")
  resultados[[3]] <- calcular_inconsistencias(data$Ano_Inicio_Tratamento < data$Ano_Primeira_Consulta, "Ano_Inicio_Tratamento < Ano_Primeira_Consulta")
  resultados[[4]] <- calcular_inconsistencias(data$Ano_Inicio_Tratamento < data$Ano_Triagem, "Ano_Inicio_Tratamento < Ano_Triagem")
  resultados[[5]] <- calcular_inconsistencias(data$Ano_Inicio_Tratamento < data$Ano_Primeiro_Diagnostico, "Ano_Inicio_Tratamento < Ano_Primeiro_Diagnostico")

  resultados_df <- do.call(rbind, resultados)

  message("\033[1;32mVerificação de inconsistências entre os anos concluída.\033[0m")

  return(resultados_df)
}

