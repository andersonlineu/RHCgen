#' Análise de Qualidade dos Dados
#'
#' Esta função executa uma análise completa de qualidade dos dados, chamando várias funções que verificam completude, códigos inexistentes, idades inconsistentes, anos inconsistentes, datas inconsistentes e inconsistências entre câncer e sexo.
#'
#' @param data Um dataframe contendo os dados a serem analisados.
#' @return Retorna uma mensagem indicando a conclusão da análise.
#' @export
#' @name analise_qualidade_dos_dados
#' @examples
#' # Exemplo de uso:
#' # analise_qualidade_dos_dados(dados_RHC_combinados)
analise_qualidade_dos_dados <- function(data) {
  cat("\n\n\n")
  # Análise de Completude
  message("\033[1;32mEXECUTANDO A ANÁLISE DE COMPLETUDE...\033[0m")
  analise_completude(data)

  # Verificação de Inexistências
  cat("\n\n\n\n")
  message("\033[1;32mVERIFICANDO CASOS COM CÓDIGOS INCONSISTENTES\033[0m")
  tryCatch({
    tabela_inconsistencias(data)
  }, error = function(e) {
    message("\033[1;31mERRO NA VERIFICAÇÃO DE CÓDIGOS INCONSISTENTES:\033[0m ", toupper(e$message))
  })

  cat("\n\n\n")
  message("\033[1;32mANÁLISE DE QUALIDADE DOS DADOS CONCLUÍDA.\033[0m")
}


