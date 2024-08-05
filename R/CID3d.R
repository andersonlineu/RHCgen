#' Converte os CID-O de 3 Dígitos de Um Dataframe
#'
#' Esta função mapeia os códigos CID-O de 3 dígitos para seus nomes completos na coluna `Localizacao_Primaria_3D` de um dataframe. Os nomes completos dos CID-O de 3 dígitos são adicionados em uma nova coluna `CID3d`.
#'
#' @param dados Um dataframe contendo a coluna `Localizacao_Primaria_3D`.
#' @return Retorna um dataframe com os nomes completos dos CID-O de 3 dígitos adicionados em uma nova coluna.
#' @export
#' @name renomear_CID_3digitos
#' @examples
#' # Escreva o nome do dataframe e execute a função. Se seu dataframe for "dados_RHC_combinados", use a funçao como:
#'
#' dados_RHC_combinados <- renomear_CID_3digitos(dados_RHC_combinados)
renomear_CID_3digitos <- function(dados) {
  message("Iniciando o ajuste dos códigos CID-O de 3 dígitos para nomes completos.")

  # Definição do dataframe de mapeamento de CID para nomes
  cid_names <- data.frame(
    CID = c("C00", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09",
            "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19",
            "C20", "C21", "C22", "C23", "C24", "C25", "C26", "C30", "C31", "C32",
            "C33", "C34", "C37", "C38", "C39", "C40", "C41", "C43", "C44", "C45",
            "C46", "C47", "C48", "C49", "C50", "C51", "C52", "C53", "C54", "C55",
            "C56", "C57", "C58", "C60", "C61", "C62", "C63", "C64", "C65", "C66",
            "C67", "C68", "C69", "C70", "C71", "C72", "C73", "C74", "C75", "C76",
            "C77", "C78", "C79", "C80", "C81", "C82", "C83", "C84", "C85", "C88",
            "C90", "C91", "C92", "C93", "C94", "C95", "C96", "C97", "C42"),
    NOME = c("Neoplasia maligna do lábio", "Neoplasia maligna da base da língua",
             "Neoplasia maligna de outras partes e de partes não especificadas da língua",
             "Neoplasia maligna da gengiva", "Neoplasia maligna do assoalho da boca",
             "Neoplasia maligna do palato", "Neoplasia maligna de outras partes e de partes não especificadas da boca",
             "Neoplasia maligna da glândula parótida", "Neoplasia maligna de outras glândulas salivares maiores e as não especificadas",
             "Neoplasia maligna da amígdala", "Neoplasia maligna da orofaringe", "Neoplasia maligna da nasofaringe",
             "Neoplasia maligna do seio piriforme", "Neoplasia maligna da hipofaringe",
             "Neoplasia maligna de outras localizações e de localizações mal definida, do lábio, cavidade oral e faringe",
             "Neoplasia maligna do esôfago", "Neoplasia maligna do estômago", "Neoplasia maligna do intestino delgado",
             "Neoplasia maligna do cólon", "Neoplasia maligna da junção retossigmóide",
             "Neoplasia maligna do reto", "Neoplasia maligna do ânus e do canal anal",
             "Neoplasia maligna do fígado e das vias biliares intra-hepáticas", "Neoplasia maligna da vesícula biliar",
             "Neoplasia maligna de outras partes, e de partes não especificadas das vias biliares", "Neoplasia maligna do pâncreas",
             "Neoplasia maligna de outros órgãos digestivos e de localizações mal definidas no aparelho digestivo",
             "Neoplasia maligna da cavidade nasal e do ouvido médio", "Neoplasia maligna dos seios da face",
             "Neoplasia maligna da laringe", "Neoplasia maligna da traquéia", "Neoplasia maligna dos brônquios e dos pulmões",
             "Neoplasia maligna do timo", "Neoplasia maligna do coração, mediastino e pleura",
             "Neoplasia maligna de outras localizações e de localizações mal definidas do aparelho respiratório e dos órgãos intratorácicos",
             "Neoplasia maligna dos ossos e cartilagens articulares dos membros",
             "Neoplasia maligna dos ossos e das cartilagens articulares de outras localizações e de localizações não especificadas",
             "Melanoma maligno da pele", "Outras neoplasias malignas da pele", "Mesotelioma",
             "Sarcoma de Kaposi", "Neoplasia maligna dos nervos periféricos e do sistema nervoso autônomo",
             "Neoplasia maligna dos tecidos moles do retroperitônio e do peritônio",
             "Neoplasia maligna do tecido conjuntivo e de outros tecidos moles", "Neoplasia maligna da mama",
             "Neoplasia maligna da vulva", "Neoplasia maligna da vagina",
             "Neoplasia maligna do colo do útero", "Neoplasia maligna do corpo do útero",
             "Neoplasia maligna do útero, porção não especificada", "Neoplasia maligna do ovário",
             "Neoplasia maligna de outros órgãos genitais femininos e dos não especificados",
             "Neoplasia maligna da placenta", "Neoplasia maligna do pênis",
             "Neoplasia maligna da próstata", "Neoplasia maligna dos testículos",
             "Neoplasia maligna de outros órgãos genitais masculinos e dos não especificados",
             "Neoplasia maligna do rim, exceto pelve renal", "Neoplasia maligna da pelve renal",
             "Neoplasia maligna dos ureteres", "Neoplasia maligna da bexiga",
             "Neoplasia maligna de outros órgãos urinários e dos não especificados",
             "Neoplasia maligna do olho e anexos", "Neoplasia maligna das meninges",
             "Neoplasia maligna do encéfalo", "Neoplasia maligna da medula espinhal, dos nervos cranianos e de outras partes do sistema nervoso central",
             "Neoplasia maligna da glândula tireóide", "Neoplasia maligna da glândula supra-renal [Glândula adrenal]",
             "Neoplasia maligna de outras glândulas endócrinas e de estruturas relacionadas",
             "Neoplasia maligna de outras localizações e de localizações mal definidas",
             "Neoplasia maligna secundária e não especificada dos gânglios linfáticos",
             "Neoplasia maligna secundária dos órgãos respiratórios e digestivos",
             "Neoplasia maligna secundária de outras localizações", "Neoplasia maligna, sem especificação de localização",
             "Doença de Hodgkin", "Linfoma não-Hodgkin, folicular (nodular)",
             "Linfoma não-Hodgkin difuso", "Linfomas de células T cutâneas e periféricas",
             "Linfoma não-Hodgkin de outros tipos e de tipo não especificado", "Doenças imunoproliferativas malignas",
             "Mieloma múltiplo e neoplasias malignas de plasmócitos", "Leucemia linfóide",
             "Leucemia mielóide", "Leucemia monocítica", "Outras leucemias de células de tipo especificado",
             "Leucemia de tipo celular não especificado", "Outras neoplasias malignas e as não especificadas dos tecidos linfático, hematopoético e tecidos correlatos",
             "Neoplasias malignas de localizações múltiplas independentes (primárias)",
             "Sistema hematopoético/Reticuloendotelial")
  )

  # Verifica se a coluna Localizacao_Primaria_3D existe antes de tentar acessá-la
  if (!"Localizacao_Primaria_3D" %in% names(dados)) {
    stop("\033[1;31mA coluna 'Localizacao_Primaria_3D' não foi encontrada no dataframe. Função interrompida.\033[0m")
  }

  message("Mapeando os códigos CID-O de 3 dígitos para nomes completos.")
  # Mapeamento dos códigos CID para nomes usando um vetor nomeado
  map <- setNames(cid_names$NOME, cid_names$CID)
  dados$CID3d <- map[dados$Localizacao_Primaria_3D]
  message("Nomes completos dos CID-O de 3 dígitos adicionados.")

  message(paste("\033[1;32m", "> Ajuste dos códigos CID-3 dígitos concluído com sucesso. Foi adicionada uma coluna no dataframe, chamada CID3d.", "\033[0m"))

  return(dados)
}

