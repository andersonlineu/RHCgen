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

  # Verificação de Códigos Inexistentes
  cat("\n\n\n\n")
  message("\033[1;32mVERIFICANDO CASOS COM CÓDIGOS INCONSISTENTES\033[0m")
  tryCatch({
    verificar_codigos_inconsistentes(data)
  }, error = function(e) {
    message("\033[1;31mERRO NA VERIFICAÇÃO DE CÓDIGOS INCONSISTENTES:\033[0m ", toupper(e$message))
  })

  # Verificação de Idades Inconsistentes
  cat("\n\n\n\n")
  message("\033[1;32mVERIFICANDO IDADES INCONSISTENTES...\033[0m")
  tryCatch({
    verificar_idades_inconsistentes(data)
  }, error = function(e) {
    message("\033[1;31mERRO NA VERIFICAÇÃO DE IDADES INCONSISTENTES:\033[0m ", toupper(e$message))
  })

  # Verificação de Anos Inconsistentes
  cat("\n\n\n\n")
  message("\033[1;32mVERIFICANDO ANOS INCONSISTENTES...\033[0m")
  tryCatch({
    verificar_anos_inconsistentes(data)
  }, error = function(e) {
    message("\033[1;31mERRO NA VERIFICAÇÃO DE ANOS INCONSISTENTES:\033[0m ", toupper(e$message))
  })

  # Verificação de Datas Inconsistentes
  cat("\n\n\n\n")
  message("\033[1;32mVERIFICANDO DATAS INCONSISTENTES...\033[0m")
  tryCatch({
    verificar_datas_inconsistentes(data)
  }, error = function(e) {
    message("\033[1;31mERRO NA VERIFICAÇÃO DE DATAS INCONSISTENTES:\033[0m ", toupper(e$message))
  })

  # Verificação de Inconsistências entre Câncer e Sexo
  cat("\n\n\n\n")
  message("\033[1;32mVERIFICANDO INCONSISTÊNCIAS ENTRE CÂNCER E SEXO...\033[0m")
  tryCatch({
    verificar_cancer_sexo_inconsistentes(data)
  }, error = function(e) {
    message("\033[1;31mERRO NA VERIFICAÇÃO DE INCONSISTÊNCIAS ENTRE CÂNCER E SEXO:\033[0m ", toupper(e$message))
  })
  cat("\n\n\n")
  message("\033[1;32mANÁLISE DE QUALIDADE DOS DADOS CONCLUÍDA.\033[0m")
}


