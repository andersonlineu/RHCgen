#' Verifica Inconsistências Entre Datas em um Dataframe
#'
#' Esta função verifica se há inconsistências na ordem cronológica das datas fornecidas em um dataframe. As verificações incluem:
#' - "Data_Diagnostico" deve ser posterior a "Data_Primeira_Consulta"
#' - "Data_Diagnostico" deve ser posterior a "Data_Triagem"
#' - "Data_Inicio_Primeiro_Tratamento" deve ser posterior a "Data_Primeira_Consulta"
#' - "Data_Inicio_Primeiro_Tratamento" deve ser posterior a "Data_Triagem"
#' - "Data_Inicio_Primeiro_Tratamento" deve ser posterior a "Data_Diagnostico"
#' - "Data_Obito" deve ser posterior a "Data_Primeira_Consulta"
#' - "Data_Obito" deve ser posterior a "Data_Triagem"
#' - "Data_Obito" deve ser posterior a "Data_Diagnostico"
#' - "Data_Obito" deve ser posterior a "Data_Inicio_Primeiro_Tratamento"
#' A função considera uma diferença mínima de 12 meses (365 dias) para ser considerada inconsistente.
#'
#' @param data Um dataframe contendo as colunas "Data_Diagnostico", "Data_Primeira_Consulta", "Data_Triagem", "Data_Inicio_Primeiro_Tratamento" e "Data_Obito".
#' @return Retorna um dataframe com o quantitativo e a proporção de inconsistências por verificação.
#' @export
#' @name verificar_datas_inconsistentes
#' @examples
#' # Escreva o nome do dataframe e execute a função. Se seu dataframe for "dados_RHC_combinados", use a função como:
#'
#' resultados_datas_inconsistentes <- verificar_datas_inconsistentes(dados_RHC_combinados)
verificar_datas_inconsistentes <- function(data) {
  required_columns <- c("Data_Diagnostico", "Data_Primeira_Consulta", "Data_Triagem", "Data_Inicio_Primeiro_Tratamento", "Data_Obito")

  # Verificar se todas as colunas necessárias estão presentes
  for (col in required_columns) {
    if (!col %in% colnames(data)) {
      message(paste("\033[1;31mErro: Coluna", col, "ausente no dataframe.\033[0m"))
      return(NULL)
    }
  }

  message("\033[32mIniciando a verificação de inconsistências entre as datas.\033[0m")

  # Converter colunas de data para Date
  for (col in required_columns) {
    data[[col]] <- as.Date(data[[col]], format="%Y-%m-%d")
  }

  # Função para calcular inconsistências
  calcular_inconsistencias <- function(condicao, label) {
    inconsistencias <- sum(condicao, na.rm = TRUE)
    proporcao <- paste0(round((inconsistencias / nrow(data)) * 100, 2), "%")
    if (inconsistencias > 0) {
      message(paste("Inconsistência encontrada, variável", label, ":\033[1;32m", inconsistencias, "(", proporcao, ")\033[0m"))
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

  # Verificações com diferença mínima de 12 meses (365 dias)
  resultados <- list()
  resultados[[1]] <- calcular_inconsistencias(data$Data_Diagnostico < data$Data_Primeira_Consulta - 365, "Data_Diagnostico < Data_Primeira_Consulta - por mais de 12 meses")
  resultados[[2]] <- calcular_inconsistencias(data$Data_Diagnostico < data$Data_Triagem - 365, "Data_Diagnostico < Data_Triagem - por mais de 12 meses")
  resultados[[3]] <- calcular_inconsistencias(data$Data_Inicio_Primeiro_Tratamento < data$Data_Primeira_Consulta - 365, "Data_Inicio_Primeiro_Tratamento < Data_Primeira_Consulta - por mais de 12 meses")
  resultados[[4]] <- calcular_inconsistencias(data$Data_Inicio_Primeiro_Tratamento < data$Data_Triagem - 365, "Data_Inicio_Primeiro_Tratamento < Data_Triagem - por mais de 12 meses")
  resultados[[5]] <- calcular_inconsistencias(data$Data_Inicio_Primeiro_Tratamento < data$Data_Diagnostico - 365, "Data_Inicio_Primeiro_Tratamento < Data_Diagnostico - por mais de 12 meses")

  # Verificações adicionais para Data_Obito
  if ("Data_Obito" %in% colnames(data)) {
    resultados[[6]] <- calcular_inconsistencias(data$Data_Obito < data$Data_Primeira_Consulta - 365, "Data_Obito < Data_Primeira_Consulta - por mais de 12 meses")
    resultados[[7]] <- calcular_inconsistencias(data$Data_Obito < data$Data_Triagem - 365, "Data_Obito < Data_Triagem - por mais de 12 meses")
    resultados[[8]] <- calcular_inconsistencias(data$Data_Obito < data$Data_Diagnostico - 365, "Data_Obito < Data_Diagnostico - por mais de 12 meses")
    resultados[[9]] <- calcular_inconsistencias(data$Data_Obito < data$Data_Inicio_Primeiro_Tratamento - 365, "Data_Obito < Data_Inicio_Primeiro_Tratamento - por mais de 12 meses")
  }

  resultados_df <- do.call(rbind, resultados)

  message("\033[32mVerificação de inconsistências entre as datas concluída.\033[0m")

  return(resultados_df)
}
