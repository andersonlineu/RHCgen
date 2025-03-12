
#' Renomeia as Colunas de um DataFrame
#'
#' Esta função renomeia as colunas de um dataframe utilizando uma lista predefinida de mapeamentos entre os nomes originais das colunas e seus novos nomes descritivos. O objetivo é melhorar a legibilidade. A função verifica cada coluna listada no mapeamento e, se encontrada no dataframe, substitui seu nome pelo novo nome especificado. As colunas que não estão presentes no dataframe são ignoradas, e uma mensagem é exibida para cada coluna processada, indicando se a renomeação foi bem-sucedida ou não.
#'
#' @param data Um dataframe que contém as colunas a serem renomeadas.
#' @return Um dataframe com as colunas renomeadas conforme especificado na lista de mapeamentos.
#' @export
#' @name renomear_colunas
renomear_colunas <- function(data) {
  message("Iniciando a renomeação das colunas do dataframe.")

  # Lista de colunas originais e seus novos nomes
  new_names <- list(
    TPCASO = "Tipo_de_Caso",
    SEXO = "Sexo",
    IDADE = "Idade",
    LOCALNAS = "Local_Nascimento",
    RACACOR = "Raca_Cor",
    INSTRUC = "Escolaridade",
    CLIATEN = "Clinica_Atendimento",
    CLITRAT = "Clinica_Tratamento",
    HISTFAMC = "Historico_Familiar_Cancer",
    ALCOOLIS = "Consumo_Alcool",
    TABAGISM = "Tabagismo",
    ESTADRES = "Estado_Residencia",
    PROCEDEN = "Codigo_Municipio_Residencia",
    ANOPRIDI = "Ano_Primeiro_Diagnostico",
    ORIENC = "Origem_do_Encaminhamento",
    EXDIAG = "Exames_Relevantes_para_Diagnostico",
    ESTCONJ = "Estado_conjugal",
    ANTRI = "Ano_Triagem",
    DTPRICON = "Ano_Primeira_Consulta",
    DIAGANT = "Diagnosticos_e_Tratamentos_Anterior",
    BASMAIMP = "Base_Mais_Importante_Diagnostico", ####
    LOCTUDET = "Localizacao_Primaria_3D",
    LOCTUPRI = "Localizacao_Primaria_4D",
    TIPOHIST = "Tipo_Histologico",
    LATERALI = "Lateralidade",
    LOCTUPRO = "Local_Provavel_Tumor",
    MAISUMTU = "Mais_de_Um_Tumor",
    TNM = "Classificacao_TNM",
    ESTADIAM = "Estadiamento_Clinico",
    OUTROESTA = "Outro_Estadiamento_Clinico",
    PTNM = "TNM_Patologico",
    RZNTR = "Razao_Nao_Tratamento",
    DTINITRT = "Ano_Inicio_Tratamento",
    PRITRATH = "Primeiro_Tratamento_Hospital",
    ESTDFIMT = "Estado_Doenca_Final_Tratamento",
    DTTRIAGE = "Data_Triagem",
    DATAPRICON = "Data_Primeira_Consulta",
    BASDIAGSP = "Base_Mais_Importante_Diagnostico_Sem_Patologicas", ###
    CNES = "CNES_Hospital",
    UFUH = "UF_Unidade_Hospital",
    MUUH = "Municipio_Unidade_Hospital",
    OCUPACAO = "Ocupacao_Paciente",
    DTDIAGNO = "Data_Diagnostico",
    DATAINITRT = "Data_Inicio_Primeiro_Tratamento",
    DATAOBITO = "Data_Obito",
    VALOR_TOT = "Valor_Total",
    ESTADIAG = "Estadiamento_Clinico_TNM_grupo_1985_1999"
  )

  # Contador de colunas renomeadas
  contador_renomeadas = 0

  # Verificar e renomear colunas que existem no dataframe
  for (original_col in names(new_names)) {
    message(paste("Verificando a existência da coluna:", original_col))
    if (original_col %in% names(data)) {
      message(paste("Renomeando coluna:", original_col, "para:", new_names[[original_col]]))
      names(data)[names(data) == original_col] <- new_names[[original_col]]
      contador_renomeadas <- contador_renomeadas + 1
    } else {
      message(paste("A coluna:", original_col, "não existe no dataframe e será ignorada."))
    }
  }

  message(paste("\033[1;32m", "Renomeação de colunas concluída com sucesso. Total:", contador_renomeadas, "\033[0m"))

  return(data)
}

