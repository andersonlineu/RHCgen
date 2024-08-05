#' Classifica as Neoplasias Malignas baseada na localização anatômica e tipo histológico conforme o CID-O
#'
#' Esta função cria uma nova coluna em um dataframe com a classificação das neoplasias malignas baseada na localização anatômica e tipo histológico conforme o CID-O.
#'
#'A Classificação Internacional de Doenças para Oncologia (CID-O) e a Classificação Internacional de Doenças (CID-10) agrupam os cânceres com base na localização anatômica e no tipo histológico.
#'
#' @param data Um dataframe contendo a coluna `Localizacao_Primaria_3D` com os códigos CID-0.
#' @return Retorna um dataframe com uma nova coluna `Classificacao_CID_0` indicando a categoria da neoplasia maligna.
#' @export
#' @name classificacao_CID_0
#' @examples
#' # Exemplo de uso:
#' # dados_RHC_combinados <- classificacao_CID_0(dados_RHC_combinados)
classificacao_CID_0 <- function(data) {
  if (!"Localizacao_Primaria_3D" %in% colnames(data)) {
    message("\033[1;31mErro: Coluna 'Localizacao_Primaria_3D' ausente no dataframe.\033[0m")
    return(NULL)
  }

  message("Iniciando a classificação das neoplasias malignas conforme o CID-O.")

  # Mapa de CID-10 para suas classificações
  cid_classificacao <- c(
    "C00" = "Neoplasias Malignas da Cavidade Oral e Faringe (C00-C14)",
    "C01" = "Neoplasias Malignas da Cavidade Oral e Faringe (C00-C14)",
    "C02" = "Neoplasias Malignas da Cavidade Oral e Faringe (C00-C14)",
    "C03" = "Neoplasias Malignas da Cavidade Oral e Faringe (C00-C14)",
    "C04" = "Neoplasias Malignas da Cavidade Oral e Faringe (C00-C14)",
    "C05" = "Neoplasias Malignas da Cavidade Oral e Faringe (C00-C14)",
    "C06" = "Neoplasias Malignas da Cavidade Oral e Faringe (C00-C14)",
    "C07" = "Neoplasias Malignas da Cavidade Oral e Faringe (C00-C14)",
    "C08" = "Neoplasias Malignas da Cavidade Oral e Faringe (C00-C14)",
    "C09" = "Neoplasias Malignas da Cavidade Oral e Faringe (C00-C14)",
    "C10" = "Neoplasias Malignas da Cavidade Oral e Faringe (C00-C14)",
    "C11" = "Neoplasias Malignas da Cavidade Oral e Faringe (C00-C14)",
    "C12" = "Neoplasias Malignas da Cavidade Oral e Faringe (C00-C14)",
    "C13" = "Neoplasias Malignas da Cavidade Oral e Faringe (C00-C14)",
    "C14" = "Neoplasias Malignas da Cavidade Oral e Faringe (C00-C14)",
    "C15" = "Neoplasias Malignas dos Órgãos Digestivos (C15-C26)",
    "C16" = "Neoplasias Malignas dos Órgãos Digestivos (C15-C26)",
    "C17" = "Neoplasias Malignas dos Órgãos Digestivos (C15-C26)",
    "C18" = "Neoplasias Malignas dos Órgãos Digestivos (C15-C26)",
    "C19" = "Neoplasias Malignas dos Órgãos Digestivos (C15-C26)",
    "C20" = "Neoplasias Malignas dos Órgãos Digestivos (C15-C26)",
    "C21" = "Neoplasias Malignas dos Órgãos Digestivos (C15-C26)",
    "C22" = "Neoplasias Malignas dos Órgãos Digestivos (C15-C26)",
    "C23" = "Neoplasias Malignas dos Órgãos Digestivos (C15-C26)",
    "C24" = "Neoplasias Malignas dos Órgãos Digestivos (C15-C26)",
    "C25" = "Neoplasias Malignas dos Órgãos Digestivos (C15-C26)",
    "C26" = "Neoplasias Malignas dos Órgãos Digestivos (C15-C26)",
    "C30" = "Neoplasias Malignas dos Órgãos Respiratórios e Intratorácicos (C30-C39)",
    "C31" = "Neoplasias Malignas dos Órgãos Respiratórios e Intratorácicos (C30-C39)",
    "C32" = "Neoplasias Malignas dos Órgãos Respiratórios e Intratorácicos (C30-C39)",
    "C33" = "Neoplasias Malignas dos Órgãos Respiratórios e Intratorácicos (C30-C39)",
    "C34" = "Neoplasias Malignas dos Órgãos Respiratórios e Intratorácicos (C30-C39)",
    "C37" = "Neoplasias Malignas dos Órgãos Respiratórios e Intratorácicos (C30-C39)",
    "C38" = "Neoplasias Malignas dos Órgãos Respiratórios e Intratorácicos (C30-C39)",
    "C39" = "Neoplasias Malignas dos Órgãos Respiratórios e Intratorácicos (C30-C39)",
    "C40" = "Neoplasias Malignas dos Ossos e Cartilagens Articulares (C40-C41)",
    "C41" = "Neoplasias Malignas dos Ossos e Cartilagens Articulares (C40-C41)",
    "C43" = "Neoplasias Malignas da Pele (C43-C44)",
    "C44" = "Neoplasias Malignas da Pele (C43-C44)",
    "C45" = "Neoplasias Malignas dos Tecidos Moles (C45-C49)",
    "C46" = "Neoplasias Malignas dos Tecidos Moles (C45-C49)",
    "C47" = "Neoplasias Malignas dos Tecidos Moles (C45-C49)",
    "C48" = "Neoplasias Malignas dos Tecidos Moles (C45-C49)",
    "C49" = "Neoplasias Malignas dos Tecidos Moles (C45-C49)",
    "C50" = "Neoplasias Malignas da Mama (C50)",
    "C51" = "Neoplasias Malignas dos Órgãos Genitais Femininos (C51-C58)",
    "C52" = "Neoplasias Malignas dos Órgãos Genitais Femininos (C51-C58)",
    "C53" = "Neoplasias Malignas dos Órgãos Genitais Femininos (C51-C58)",
    "C54" = "Neoplasias Malignas dos Órgãos Genitais Femininos (C51-C58)",
    "C55" = "Neoplasias Malignas dos Órgãos Genitais Femininos (C51-C58)",
    "C56" = "Neoplasias Malignas dos Órgãos Genitais Femininos (C51-C58)",
    "C57" = "Neoplasias Malignas dos Órgãos Genitais Femininos (C51-C58)",
    "C58" = "Neoplasias Malignas dos Órgãos Genitais Femininos (C51-C58)",
    "C60" = "Neoplasias Malignas dos Órgãos Genitais Masculinos (C60-C63)",
    "C61" = "Neoplasias Malignas dos Órgãos Genitais Masculinos (C60-C63)",
    "C62" = "Neoplasias Malignas dos Órgãos Genitais Masculinos (C60-C63)",
    "C63" = "Neoplasias Malignas dos Órgãos Genitais Masculinos (C60-C63)",
    "C64" = "Neoplasias Malignas dos Órgãos Urinários (C64-C68)",
    "C65" = "Neoplasias Malignas dos Órgãos Urinários (C64-C68)",
    "C66" = "Neoplasias Malignas dos Órgãos Urinários (C64-C68)",
    "C67" = "Neoplasias Malignas dos Órgãos Urinários (C64-C68)",
    "C68" = "Neoplasias Malignas dos Órgãos Urinários (C64-C68)",
    "C69" = "Neoplasias Malignas do Olho, Encéfalo e Outras Partes do Sistema Nervoso Central (C69-C72)",
    "C70" = "Neoplasias Malignas do Olho, Encéfalo e Outras Partes do Sistema Nervoso Central (C69-C72)",
    "C71" = "Neoplasias Malignas do Olho, Encéfalo e Outras Partes do Sistema Nervoso Central (C69-C72)",
    "C72" = "Neoplasias Malignas do Olho, Encéfalo e Outras Partes do Sistema Nervoso Central (C69-C72)",
    "C73" = "Neoplasias Malignas das Glândulas Endócrinas e Estruturas Relacionadas (C73-C75)",
    "C74" = "Neoplasias Malignas das Glândulas Endócrinas e Estruturas Relacionadas (C73-C75)",
    "C75" = "Neoplasias Malignas das Glândulas Endócrinas e Estruturas Relacionadas (C73-C75)",
    "C76" = "Neoplasias Malignas de Outras Localizações e de Localizações Mal Definidas (C76-C80)",
    "C77" = "Neoplasias Malignas de Outras Localizações e de Localizações Mal Definidas (C76-C80)",
    "C78" = "Neoplasias Malignas de Outras Localizações e de Localizações Mal Definidas (C76-C80)",
    "C79" = "Neoplasias Malignas de Outras Localizações e de Localizações Mal Definidas (C76-C80)",
    "C80" = "Neoplasias Malignas de Outras Localizações e de Localizações Mal Definidas (C76-C80)",
    "C81" = "Neoplasias Malignas dos Tecidos Linfático, Hematopoético e Tecidos Relacionados (C81-C96)",
    "C82" = "Neoplasias Malignas dos Tecidos Linfático, Hematopoético e Tecidos Relacionados (C81-C96)",
    "C83" = "Neoplasias Malignas dos Tecidos Linfático, Hematopoético e Tecidos Relacionados (C81-C96)",
    "C84" = "Neoplasias Malignas dos Tecidos Linfático, Hematopoético e Tecidos Relacionados (C81-C96)",
    "C85" = "Neoplasias Malignas dos Tecidos Linfático, Hematopoético e Tecidos Relacionados (C81-C96)",
    "C88" = "Neoplasias Malignas dos Tecidos Linfático, Hematopoético e Tecidos Relacionados (C81-C96)",
    "C90" = "Neoplasias Malignas dos Tecidos Linfático, Hematopoético e Tecidos Relacionados (C81-C96)",
    "C91" = "Neoplasias Malignas dos Tecidos Linfático, Hematopoético e Tecidos Relacionados (C81-C96)",
    "C92" = "Neoplasias Malignas dos Tecidos Linfático, Hematopoético e Tecidos Relacionados (C81-C96)",
    "C93" = "Neoplasias Malignas dos Tecidos Linfático, Hematopoético e Tecidos Relacionados (C81-C96)",
    "C94" = "Neoplasias Malignas dos Tecidos Linfático, Hematopoético e Tecidos Relacionados (C81-C96)",
    "C95" = "Neoplasias Malignas dos Tecidos Linfático, Hematopoético e Tecidos Relacionados (C81-C96)",
    "C96" = "Neoplasias Malignas dos Tecidos Linfático, Hematopoético e Tecidos Relacionados (C81-C96)",
    "C97" = "Neoplasias Malignas de Localizações Múltiplas (C97)"
  )

  data$Classificacao_CID_O <- sapply(data$Localizacao_Primaria_3D, function(cid) {
    if (is.na(cid)) {
      return(NA)
    } else if (cid %in% names(cid_classificacao)) {
      return(cid_classificacao[cid])
    } else {
      return("Outros")
    }
  })

  message("\033[1;32mClassificação das neoplasias malignas conforme o CID-O concluída. Foi adicionado uma coluna no dataframe final.\033[0m")

  return(data)
}

