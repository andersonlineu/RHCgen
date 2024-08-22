#' Ajusta Colunas em uma Ordem Específica
#'
#' Esta função ajusta as colunas do dataframe fornecido em uma ordem específica.
#'
#' @param dados Um dataframe contendo as colunas a serem ajustadas.
#' @return Retorna um dataframe com as colunas ajustadas na ordem especificada.
#' @export
#' @name ordenando_colunas
#' @examples
#' # Escreva o nome do dataframe e execute a função. Se seu dataframe for "dados_RHC_combinados", use a função como:
#'
#' dados_RHC_combinados <- ordenando_colunas(dados_RHC_combinados)
ordenando_colunas <- function(dados) {
  colunas <- c(
    "Tipo_de_Caso", "Sexo", "Idade", "Local_Nascimento", "Raca_Cor", "Escolaridade",
    "Clinica_Atendimento", "Nome_Clinica_Atendimento", "Clinica_Tratamento", "Nome_Clinica_Tratamento", "Historico_Familiar_Cancer", "Consumo_Alcool",
    "Tabagismo", "Estado_Residencia", "Nome_Estado_Residencia", "Codigo_Municipio_Residencia", "Nome_Municipio_Residencia", "Ano_Primeiro_Diagnostico",
    "Origem_do_Encaminhamento", "Exames_Relevantes_para_Diagnostico", "Estado_conjugal", "Ano_Triagem",
    "Ano_Primeira_Consulta", "Diagnosticos_e_Tratamentos_Anterior", "Base_Mais_Importante_Diagnostico",
    "Localizacao_Primaria_3D", "CID3d", "Classificacao_CID_O", "Localizacao_Primaria_4D", "CID4d", "Tipo_Histologico", "Tipo_Histologico_Completo",
    "Lateralidade", "Local_Provavel_Tumor", "Mais_de_Um_Tumor", "Classificacao_TNM", "Estadiamento_Clinico", "Nome_Estadiamento_Clinico",
    "Outro_Estadiamento_Clinico", "TNM_Patologico", "Razao_Nao_Tratamento", "Ano_Inicio_Tratamento",
    "Primeiro_Tratamento_Hospital", "Estado_Doenca_Final_Tratamento", "CNES_Hospital", "Estabelecimento_Hospitalar",
    "UF_Unidade_Hospital", "Nome_Estado_Hospital", "Municipio_Unidade_Hospital", "Nome_Municipio_Hospital", "Ocupacao_Paciente", "Data_Diagnostico", "Data_Triagem",
    "Data_Primeira_Consulta", "Data_Inicio_Primeiro_Tratamento", "Data_Obito", "Valor_Total",
    "Base_Mais_Importante_Diagnostico_Sem_Patologicas", "Estadiamento_Clinico_TNM_grupo_1985_1999", "Ano_do_Banco"
  )

  colunas_ausentes <- setdiff(colunas, names(dados))

  if (length(colunas_ausentes) > 0) {
    message("As seguintes colunas estão ausentes no dataframe:")
    message(paste(colunas_ausentes, collapse = ", "))
  } else {
    message("Todas as colunas necessárias estão presentes no dataframe.")
  }

  colunas_presentes <- intersect(colunas, names(dados))
  message(paste("\033[1;32m", "> Ajuste da ordem das colunas concluído com sucesso.", "\033[0m"))
  dados <- dados[, colunas_presentes, drop = FALSE]

  return(dados)
}
