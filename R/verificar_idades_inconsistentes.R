#' Verifica Idades Inconsistentes em um Dataframe
#'
#' Esta função verifica se há idades inválidas (menores que zero ou maiores que 150 anos) na coluna `Idade` de um dataframe. A função retorna um dataframe com o quantitativo e a proporção de idades inválidas.
#'
#' @param data Um dataframe contendo a coluna `Idade`.
#' @return Retorna um dataframe com o quantitativo e a proporção de idades inválidas.
#' @export
#' @name verificar_idades_inconsistentes
#' @examples
#' # Escreva o nome do dataframe e execute a função. Se seu dataframe for "dados_RHC_combinados", use a função como:
#'
#' resultados_idades_invalidas <- verificar_idades_inconsistentes(dados_RHC_combinados)
verificar_idades_inconsistentes <- function(data) {
  if (!"Idade" %in% colnames(data)) {
    message("\033[1;31mErro: Coluna 'Idade' ausente no dataframe.\033[0m")
    return(NULL)
  }

  message("Iniciando a verificação de idades inconsistentes na coluna 'Idade'.")

  # Filtrar valores não NA na coluna Idade
  nao_na <- !is.na(data$Idade)

  # Verificar idades inválidas (menores que 0 ou maiores que 150)
  idades_invalidas_valores <- data$Idade[nao_na] < 0 | data$Idade[nao_na] > 150

  # Verificar idades com zeros à esquerda (representadas como 01, 07, etc.)
  idades_com_zero_esquerda <- data$Idade[nao_na] %% 1 == 0 & grepl("^0[1-9]", sprintf("%02d", data$Idade[nao_na]))

  # Combinar todas as inconsistências
  idades_invalidas <- idades_invalidas_valores | idades_com_zero_esquerda
  qtd_idades_invalidas <- sum(idades_invalidas, na.rm = TRUE)
  proporcao_idades_invalidas <- paste0(round((qtd_idades_invalidas / sum(nao_na)) * 100, 2), "%")

  # Mensagens de processo
  if (qtd_idades_invalidas > 0) {
    message(paste("\033[1;31mIdades inconsistentes encontradas:", qtd_idades_invalidas, "(", proporcao_idades_invalidas, ")\033[0m"))
  } else {
    message("Nenhuma idade inconsistente encontrada.")
  }

  # Criar dataframe de resultados
  resultados <- data.frame(
    Variavel = "Idade",
    Quantitativo_Idades_Inconsistentes = qtd_idades_invalidas,
    Proporcao_Idades_Inconsistentes = proporcao_idades_invalidas,
    stringsAsFactors = FALSE
  )

  message("\033[1;32mVerificação de idades inconsistentes concluída.\033[0m")

  return(resultados)
}
