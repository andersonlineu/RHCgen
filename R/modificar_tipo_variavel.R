
#' Modifica os Tipos de Variáveis em um Dataframe
#'
#' Esta função converte todas as variáveis de um dataframe para o tipo character, exceto as colunas de data, que são convertidas primeiro para character e depois para data.
#' A coluna `Idade` é convertida para o tipo numérico.
#'
#' @param data Um dataframe que contém as variáveis a serem convertidas.
#' @return Retorna um dataframe com as variáveis convertidas para os tipos apropriados conforme definido.
#' @export
#' @name modificar_tipo_variavel
#' @examples
#' # Escreva o nome do dataframe e execute a função. Se seu dataframe for "dados_RHC_combinados", use a funçao como:
#'
#' dados_RHC_combinados <- modificar_tipo_variavel(dados_RHC_combinados)
modificar_tipo_variavel <- function(data) {
  message("Iniciando a conversão dos tipos de variáveis.")

  # Lista de colunas de data
  column_data <- c("Data_Diagnostico", "Data_Triagem", "Data_Primeira_Consulta",
                   "Data_Inicio_Primeiro_Tratamento", "Data_Obito")

  # Converter todas as variáveis para character, exceto a coluna Idade
  for (column in names(data)) {
    if (column != "Idade") {
      data[[column]] <- as.character(data[[column]])
      message(paste("Convertendo a variável:", column, "para character."))
    }
  }

  # Converter a coluna Idade para numérico
  if ("Idade" %in% names(data)) {
    data$Idade <- as.numeric(as.character(data$Idade))
    message("Convertendo a variável 'Idade' para numérico.")
  }

  # Contador de variáveis convertidas para date
  contador_datas_convertidas <- 0

  # Converter colunas de data para date
  for (column in column_data) {
    if (column %in% names(data)) {
      message(paste("Convertendo a variável:", column, "para data."))
      data[[column]] <- as.Date(data[[column]], format = "%d/%m/%Y")
      if (all(is.na(data[[column]]))) {
        message(paste("\033[1;31mAviso: A conversão da variável", column, "resultou em NAs. Verifique o formato da data.\033[0m"))
      } else {
        contador_datas_convertidas <- contador_datas_convertidas + 1
      }
    }
  }

  message(paste("\033[1;32m", "> Conversão de tipos de variáveis concluída com sucesso.", "\033[0m"))

  return(data)
}

