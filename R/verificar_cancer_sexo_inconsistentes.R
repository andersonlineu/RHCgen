#' Verifica Inconsistências Entre CID-O e Sexo em um Dataframe
#'
#' Esta função verifica se os CID-O dos cânceres relacionados ao sexo estão de acordo com a variável "Sexo". As verificações incluem:
#' - CID-O de cânceres femininos (C53, C54, C55, C56, C57, C51, C52) devem corresponder ao sexo "Feminino"
#' - CID-O de cânceres masculinos (C60, C61, C62) devem corresponder ao sexo "Masculino"
#' A função retorna um dataframe com o quantitativo e a proporção de inconsistências por verificação.
#'
#' @param data Um dataframe contendo as colunas "Localizacao_Primaria_3D" e "Sexo".
#' @return Retorna um dataframe com o quantitativo e a proporção de inconsistências por verificação.
#' @export
#' @name verificar_cancer_sexo_inconsistentes
#' @examples
#' # Escreva o nome do dataframe e execute a função. Se seu dataframe for "dados_RHC_combinados", use a função como:
#'
#' resultados_cancer_sexo_inconsistentes <- verificar_cancer_sexo_inconsistentes(dados_RHC_combinados)
verificar_cancer_sexo_inconsistentes <- function(data) {
  required_columns <- c("Localizacao_Primaria_3D", "Sexo")

  # Verificar se todas as colunas necessárias estão presentes
  for (col in required_columns) {
    if (!col %in% colnames(data)) {
      message(paste("\033[1;31mErro: Coluna", col, "ausente no dataframe.\033[0m"))
      return(NULL)
    }
  }

  message("\033[1;32mIniciando a verificação de inconsistências entre CID-O e Sexo.\033[0m")

  # Mapear os CID-O para seus nomes completos
  cid_feminino_map <- c(
    "C53" = "Neoplasia maligna do colo do útero (C53)",
    "C54" = "Neoplasia maligna do corpo do útero (C54)",
    "C55" = "Neoplasia maligna do útero, parte não especificada (C55)",
    "C56" = "Neoplasia maligna do ovário (C56)",
    "C57" = "Neoplasia maligna de outros órgãos genitais femininos e os não especificados (C57)",
    "C51" = "Neoplasia maligna da vulva (C51)",
    "C52" = "Neoplasia maligna da vagina (C52)"
  )

  cid_masculino_map <- c(
    "C60" = "Neoplasia maligna do pênis (C60)",
    "C61" = "Neoplasia maligna da próstata (C61)",
    "C62" = "Neoplasia maligna dos testículos (C62)"
  )

  # Verificar inconsistências para cânceres femininos
  condicao_feminino <- data$Localizacao_Primaria_3D %in% names(cid_feminino_map) & data$Sexo != "Feminino"
  inconsistencias_feminino <- sum(condicao_feminino, na.rm = TRUE)
  proporcao_feminino <- paste0(round((inconsistencias_feminino / nrow(data)) * 100, 2), "%")

  if (inconsistencias_feminino > 0) {
    cids_inconsistentes_femininos <- unique(data$Localizacao_Primaria_3D[condicao_feminino & !is.na(data$Localizacao_Primaria_3D)])
    nomes_inconsistentes_femininos <- paste(cid_feminino_map[cids_inconsistentes_femininos[!is.na(cids_inconsistentes_femininos)]], collapse = "\n")
    message(paste("Inconsistências encontradas para cânceres femininos relacionados ao sexo:\n",
                  nomes_inconsistentes_femininos, ":\n\033[1;31mTotal de Inconsistências:", inconsistencias_feminino, "(", proporcao_feminino, ")\033[0m"))
  } else {
    message("Nenhuma inconsistência encontrada para cânceres femininos relacionados ao sexo.")
  }

  # Verificar inconsistências para cânceres masculinos
  condicao_masculino <- data$Localizacao_Primaria_3D %in% names(cid_masculino_map) & data$Sexo != "Masculino"
  inconsistencias_masculino <- sum(condicao_masculino, na.rm = TRUE)
  proporcao_masculino <- paste0(round((inconsistencias_masculino / nrow(data)) * 100, 2), "%")

  if (inconsistencias_masculino > 0) {
    cids_inconsistentes_masculinos <- unique(data$Localizacao_Primaria_3D[condicao_masculino & !is.na(data$Localizacao_Primaria_3D)])
    nomes_inconsistentes_masculinos <- paste(cid_masculino_map[cids_inconsistentes_masculinos[!is.na(cids_inconsistentes_masculinos)]], collapse = "\n")
    message(paste("Inconsistências encontradas para cânceres masculinos relacionados ao sexo:\n",
                  nomes_inconsistentes_masculinos, ":\n\033[1;31mTotal de Inconsistências:", inconsistencias_masculino, "(", proporcao_masculino, ")\033[0m"))
  } else {
    message("Nenhuma inconsistência encontrada para cânceres masculinos relacionados ao sexo.")
  }

  # Criar dataframe de resultados
  resultados <- data.frame(
    Verificacao = c("Cânceres Femininos", "Cânceres Masculinos"),
    Quantitativo_Inconsistencias = c(inconsistencias_feminino, inconsistencias_masculino),
    Proporcao_Inconsistencias = c(proporcao_feminino, proporcao_masculino),
    stringsAsFactors = FALSE
  )

  message("\033[1;32mVerificação de inconsistências entre CID-O e Sexo concluída.\033[0m")

  return(resultados)
}
