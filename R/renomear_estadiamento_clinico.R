#' Converte os Códigos do Estadiamento Clínico de Um Dataframe
#'
#' Esta função renomeia o Estadiamento Clínico para a Classificação mais geral. Os nomes do Estadiamento Clínico são adicionados em uma nova coluna `Nome_Estadiamento_Clinico`.
#'
#' @param dados Um dataframe contendo a coluna `Estadiamento_Clinico`.
#' @return Retorna um dataframe com os nomes do Estadiamento Clínico gerais adicionados em uma nova coluna.
#' @export
#' @name renomear_estadiamento_clinico
#' @examples
#' # Escreva o nome do dataframe e execute a função. Se seu dataframe for "dados_RHC_combinados", use a função como:
#'
#' dados_RHC_combinados <- renomear_estadiamento_clinico(dados_RHC_combinados)
renomear_estadiamento_clinico <- function(dados) {
  message("Iniciando o ajuste do Estadiamento Clínico para classificação geral.")

  # Definição do dataframe de mapeamento de Estadiamento Clínico para nomes
  estadiamento_names <- data.frame(
    Codigo = c("0", "A", "1A", "2A", "1B", "B", "2B", "C", "1C", "2C", "D",
               "01", "02", "03", "04", "1", "2", "3", "4", "I", "II", "III",
               "IV", "3A", "3B", "3C", "4A", "4B", "4C"),
    Nome = c("Estádio 0", "Estádio I", "Estádio I", "Estádio II", "Estádio II",
             "Estádio II", "Estádio II", "Estádio III", "Estádio III",
             "Estádio III", "Estádio IV", "Estádio I", "Estádio II",
             "Estádio III", "Estádio IV", "Estádio I", "Estádio II",
             "Estádio III", "Estádio IV", "Estádio II", "Estádio II",
             "Estádio III", "Estádio IV", "Estádio III", "Estádio III",
             "Estádio III", "Estádio IV", "Estádio IV", "Estádio IV")
  )

  # Verifica se a coluna Estadiamento_Clinico existe antes de tentar acessá-la
  if (!"Estadiamento_Clinico" %in% names(dados)) {
    stop("\033[1;31mA coluna 'Estadiamento_Clinico' não foi encontrada no dataframe. Função interrompida.\033[0m")
  }

  message("Lendo o Estadiamento Clínico e reclassificando.")
  # Mapeamento dos códigos de Estadiamento Clínico para nomes usando um vetor nomeado
  map <- setNames(estadiamento_names$Nome, estadiamento_names$Codigo)
  dados$Nome_Estadiamento_Clinico <- map[dados$Estadiamento_Clinico]
  message("Nomes do Estadiamento Clínico adicionados.")

  message(paste("\033[1;32m", "> Ajuste do Estadiamento Clínico concluído com sucesso. Foi adicionada uma coluna no dataframe, chamada Nome_Estadiamento_Clinico.", "\033[0m"))

  return(dados)
}
