

#' Ler e Juntar Arquivo DBF
#'
#' @description Esta função ler e junta arquivos DBF do RHC e adiciona uma nova coluna: Ano_do_Banco, a partir do ano do arquivo DBF.
#'
#' @param none Não são necessários parâmetros externos, pois a função opera com base no conteúdo do diretório especificado.
#' @return Retorna um dataframe que junta todos os arquivos DBF no diretório, ou NULL se nenhum arquivo válido for encontrado ou se eles não puderem ser combinados devido a inconsistências nas estruturas de colunas.
#' É fundamental que todos os arquivos tenham o mesmo nome que vieram do IntegradorRHC e estejam todos na pasta de trabalho.
#' @export
#' @name leraquivoDBF
#' @examples
#' # Supondo que você tenha no diretório de trabalho arquivos DBF do RHC que permanecem com o mesmo nome que vieram do Integrador (Exemplo: rhc20.dbf, nome padrão do RHC inciando que o ano do banco contidono arquvio é 2020).
#' # Use a função da seguinte forma:
#'
#' dados_RHC_combinados <- leraquivoDBF()
#' # Se nenhum arquivo for encontrado ou se houver erros ao juntar os arquivos, dados_combinados será NULL.
#' # Caso contrário, você terá um dataframe com todos os dados combinados dos arquivos DBF.
leraquivoDBF <- function() {
  # Carregar o pacote foreign se necessário
  if (!require(foreign, quietly = TRUE)) {
    install.packages("foreign")
    library(foreign)
  }

  # Listar todos os arquivos .dbf que começam com "rhc"
  arquivosRHC <- list.files(pattern = "^rhc.*\\.dbf$", ignore.case = TRUE, full.names = TRUE)

  # Verificar se existem arquivos para processar
  if (length(arquivosRHC) == 0) {
    message(paste("\033[1;31m", "> Nenhum arquivo 'rhc*.dbf' encontrado.", "\033[0m"))

    return(NULL)
  }

  message("Iniciando Tarefa...")

  # Criar lista para armazenar os data frames
  lista_dataframes <- lapply(arquivosRHC, function(arquivo) {
    message(paste("Processando arquivo:", arquivo))
    tryCatch({
      data <- foreign::read.dbf(arquivo)
      if (nrow(data) == 0) {
        message(paste("\033[1;33m", "> Aviso: O arquivo está vazio.", "\033[0m"))

        return(NULL)
      }

      # Extrair o ano do nome do arquivo
      ano_str <- sub("^rhc(.*)(\\d{2})\\.dbf$", "\\2", basename(arquivo), ignore.case = TRUE)
      ano <- as.numeric(ano_str)
      if (!is.na(ano)) {
        if (ano < 50) {
          ano <- 2000 + ano  # Assumindo anos de 2000 a 2049
        } else {
          ano <- 1900 + ano  # Assumindo anos de 1950 a 1999
        }
      }
      message("Criando coluna Ano_do_Banco, a partir do ano do arquivo DBF.")
      # Adicionar coluna Ano_do_Banco
      data$Ano_do_Banco <- ano

      message("Leitura finalizada!")
      return(data)
    }, error = function(e) {
      message(paste("\033[1;31m", "Erro ao ler o arquivo:", arquivo, "\033[0m"))
      message(paste("\033[1;31m", "Detalhe do erro:", e$message, "\033[0m"))
      return(NULL)
    })
  })

  # Remover NULLs da lista de dataframes
  lista_dataframes <- Filter(Negate(is.null), lista_dataframes)

  # Identificar todas as colunas presentes em todos os dataframes
  todas_colunas <- unique(unlist(lapply(lista_dataframes, names)))

  # Adicionar colunas ausentes com NA e garantir que todas as colunas tenham o mesmo tamanho
  lista_dataframes <- lapply(lista_dataframes, function(data) {
    colunas_faltando <- setdiff(todas_colunas, names(data))
    for (coluna in colunas_faltando) {
      data[[coluna]] <- NA
    }
    return(data)
  })

  # Verificar se todas as colunas têm o mesmo tamanho
  tamanhos_colunas <- sapply(lista_dataframes, ncol)
  if (length(unique(tamanhos_colunas)) > 1) {
    stop("\033[1;31mErro: Os dataframes não têm o mesmo número de colunas após a adição de colunas ausentes.\033[0m")
      }

  # Unir os data frames usando rbind
  if (length(lista_dataframes) > 0) {
    dataRHCCombinados <- do.call(rbind, lista_dataframes)
    message(paste("\033[1;32m", "> Carregamento dos arquivos e estruturação do dataframe finalizados com sucesso! Foi adicionada uma coluna chamada Ano_do_Banco no final do dataframe.", "\033[0m"))

    return(dataRHCCombinados)
  } else {
    message("\033[1;31mNenhum dataframe válido para combinar.\033[0m")
    return(NULL)
  }
}


#' Renomear Colunas de um DataFrame
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
    ESTADIAG = "Estadiamento_Clinico_TNM_grupo(1985-1999)"
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



#' Modificar Tipos de Variáveis em um DataFrame
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
    data$Idade <- as.numeric(data$Idade)
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



#' Recodificar Variáveis em um DataFrame
#'
#' Esta função recodifica variáveis categóricas em um dataframe com base no dicionário de variáveis predefinido pelo RHC. As variáveis são transformadas em fatores com níveis específicos para melhorar a legibilidade e consistência dos dados.
#'
#' @param data Um dataframe que contém as variáveis a serem recodificadas.
#' @return Retorna um dataframe com as variáveis recodificadas.
#' @export
#' @name recodificar_variaveis
#' @examples
#' # Escreva o nome do dataframe e execute a função. Se seu dataframe for "dados_RHC_combinados", use a função como:
#'
#' dados_RHC_combinados <- recodificar_variaveis(dados_RHC_combinados)
recodificar_variaveis <- function(data) {
  message("Iniciando a recodificação das variáveis.")

  # Lista de variáveis esperadas
  variaveis_esperadas <- c("Tipo_de_Caso", "Sexo", "Raca_Cor", "Escolaridade",
                           "Historico_Familiar_Cancer", "Consumo_Alcool", "Tabagismo",
                           "Origem_do_Encaminhamento", "Exames_Relevantes_para_Diagnostico",
                           "Estado_conjugal", "Diagnosticos_e_Tratamentos_Anterior",
                           "Base_Mais_Importante_Diagnostico", "Lateralidade", "Mais_de_Um_Tumor",
                           "Razao_Nao_Tratamento", "Primeiro_Tratamento_Hospital",
                           "Estado_Doenca_Final_Tratamento", "Base_Mais_Importante_Diagnostico_Sem_Patologicas")

  # Verificar variáveis presentes e ausentes
  variaveis_presentes <- variaveis_esperadas[variaveis_esperadas %in% names(data)]
  variaveis_ausentes <- variaveis_esperadas[!variaveis_esperadas %in% names(data)]

  if (length(variaveis_ausentes) > 0) {
    message(paste("As seguintes variáveis estão ausentes no dataframe:", paste(variaveis_ausentes, collapse = ", ")))
  }

  # Contador de variáveis recodificadas
  contador_recodificacoes <- 0
  variaveis_recodificadas <- c()

  # 'Tipo_de_Caso'
  if ("Tipo_de_Caso" %in% names(data)) {
    message("Recodificando 'Tipo_de_Caso'.")
    data$Tipo_de_Caso <- ifelse(data$Tipo_de_Caso == "1", "Analítico",
                                ifelse(data$Tipo_de_Caso == "2", "Não Analítico", data$Tipo_de_Caso))
    data$Tipo_de_Caso <- factor(data$Tipo_de_Caso, levels = c("Analítico", "Não Analítico", "Outro"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Tipo_de_Caso")
  }

  # 'Sexo'
  if ("Sexo" %in% names(data)) {
    message("Recodificando 'Sexo'.")
    data$Sexo <- ifelse(data$Sexo == "1", "Masculino",
                        ifelse(data$Sexo == "2", "Feminino", data$Sexo))
    data$Sexo <- factor(data$Sexo, levels = c("Masculino", "Feminino", "Outro"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Sexo")
  }

  # 'Raca_Cor'
  if ("Raca_Cor" %in% names(data)) {
    message("Recodificando 'Raca_Cor'.")
    data$Raca_Cor <- ifelse(data$Raca_Cor == "1", "Branca",
                            ifelse(data$Raca_Cor == "2", "Preta",
                                   ifelse(data$Raca_Cor == "3", "Amarela",
                                          ifelse(data$Raca_Cor == "4", "Parda",
                                                 ifelse(data$Raca_Cor == "5", "Indígena",
                                                        ifelse(data$Raca_Cor == "9", "Sem informação", data$Raca_Cor))))))
    data$Raca_Cor <- factor(data$Raca_Cor, levels = c("Branca", "Preta", "Amarela", "Parda", "Indígena", "Sem informação"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Raca_Cor")
  }

  # 'Escolaridade'
  if ("Escolaridade" %in% names(data)) {
    message("Recodificando 'Escolaridade'.")
    data$Escolaridade <- ifelse(data$Escolaridade == "1", "Nenhuma",
                                ifelse(data$Escolaridade == "2", "Fundamental incompleto",
                                       ifelse(data$Escolaridade == "3", "Fundamental completo",
                                              ifelse(data$Escolaridade == "4", "Nível médio",
                                                     ifelse(data$Escolaridade == "5", "Nível superior incompleto",
                                                            ifelse(data$Escolaridade == "6", "Nível superior completo",
                                                                   ifelse(data$Escolaridade == "9", "Sem informação", data$Escolaridade)))))))
    data$Escolaridade <- factor(data$Escolaridade, levels = c("Nenhuma", "Fundamental incompleto", "Fundamental completo", "Nível médio", "Nível superior incompleto", "Nível superior completo", "Sem informação"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Escolaridade")
  }

  # 'Historico_Familiar_Cancer'
  if ("Historico_Familiar_Cancer" %in% names(data)) {
    message("Recodificando 'Historico_Familiar_Cancer'.")
    data$Historico_Familiar_Cancer <- ifelse(data$Historico_Familiar_Cancer == "1", "Sim",
                                             ifelse(data$Historico_Familiar_Cancer == "2", "Não",
                                                    ifelse(data$Historico_Familiar_Cancer == "9", "Sem informação", data$Historico_Familiar_Cancer)))
    data$Historico_Familiar_Cancer <- factor(data$Historico_Familiar_Cancer, levels = c("Sim", "Não", "Sem informação"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Historico_Familiar_Cancer")
  }

  # 'Consumo_Alcool'
  if ("Consumo_Alcool" %in% names(data)) {
    message("Recodificando 'Consumo_Alcool'.")
    data$Consumo_Alcool <- ifelse(data$Consumo_Alcool == "1", "Nunca",
                                  ifelse(data$Consumo_Alcool == "2", "Ex-consumidor",
                                         ifelse(data$Consumo_Alcool == "3", "Sim",
                                                ifelse(data$Consumo_Alcool == "4", "Não avaliado",
                                                       ifelse(data$Consumo_Alcool == "8", "Não se aplica",
                                                              ifelse(data$Consumo_Alcool == "9", "Sem informação", data$Consumo_Alcool))))))
    data$Consumo_Alcool <- factor(data$Consumo_Alcool, levels = c("Nunca", "Ex-consumidor", "Sim", "Não avaliado", "Não se aplica", "Sem informação"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Consumo_Alcool")
  }

  # 'Tabagismo'
  if ("Tabagismo" %in% names(data)) {
    message("Recodificando 'Tabagismo'.")
    data$Tabagismo <- ifelse(data$Tabagismo == "1", "Nunca",
                             ifelse(data$Tabagismo == "2", "Ex-consumidor",
                                    ifelse(data$Tabagismo == "3", "Sim",
                                           ifelse(data$Tabagismo == "4", "Não avaliado",
                                                  ifelse(data$Tabagismo == "8", "Não se aplica",
                                                         ifelse(data$Tabagismo == "9", "Sem informação", data$Tabagismo))))))
    data$Tabagismo <- factor(data$Tabagismo, levels = c("Nunca", "Ex-consumidor", "Sim", "Não avaliado", "Não se aplica", "Sem informação"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Tabagismo")
  }

  # 'Origem_do_Encaminhamento'
  if ("Origem_do_Encaminhamento" %in% names(data)) {
    message("Recodificando 'Origem_do_Encaminhamento'.")
    data$Origem_do_Encaminhamento <- ifelse(data$Origem_do_Encaminhamento == "1", "SUS",
                                            ifelse(data$Origem_do_Encaminhamento == "2", "Não SUS",
                                                   ifelse(data$Origem_do_Encaminhamento == "3", "Veio por conta própria",
                                                          ifelse(data$Origem_do_Encaminhamento == "8", "Não se aplica",
                                                                 ifelse(data$Origem_do_Encaminhamento == "9", "Sem informação", data$Origem_do_Encaminhamento)))))
    data$Origem_do_Encaminhamento <- factor(data$Origem_do_Encaminhamento, levels = c("SUS", "Não SUS", "Veio por conta própria", "Não se aplica", "Sem informação"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Origem_do_Encaminhamento")
  }

  # 'Exames_Relevantes_para_Diagnostico'
  if ("Exames_Relevantes_para_Diagnostico" %in% names(data)) {
    message("Recodificando 'Exames_Relevantes_para_Diagnostico'.")
    data$Exames_Relevantes_para_Diagnostico <- ifelse(data$Exames_Relevantes_para_Diagnostico == "1", "Exame clínico e patologia clínica",
                                                      ifelse(data$Exames_Relevantes_para_Diagnostico == "2", "Exames por imagem",
                                                             ifelse(data$Exames_Relevantes_para_Diagnostico == "3", "Endoscopia e cirurgia exploradora",
                                                                    ifelse(data$Exames_Relevantes_para_Diagnostico == "4", "Anatomia patológica",
                                                                           ifelse(data$Exames_Relevantes_para_Diagnostico == "5", "Marcadores tumorais",
                                                                                  ifelse(data$Exames_Relevantes_para_Diagnostico == "8", "Não se aplica",
                                                                                         ifelse(data$Exames_Relevantes_para_Diagnostico == "9", "Sem informação", data$Exames_Relevantes_para_Diagnostico)))))))
    data$Exames_Relevantes_para_Diagnostico <- factor(data$Exames_Relevantes_para_Diagnostico, levels = c("Exame clínico e patologia clínica", "Exames por imagem", "Endoscopia e cirurgia exploradora", "Anatomia patológica", "Marcadores tumorais", "Não se aplica", "Sem informação"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Exames_Relevantes_para_Diagnostico")
  }

  # 'Estado_conjugal'
  if ("Estado_conjugal" %in% names(data)) {
    message("Recodificando 'Estado_conjugal'.")
    data$Estado_conjugal <- ifelse(data$Estado_conjugal == "1", "Solteiro",
                                   ifelse(data$Estado_conjugal == "2", "Casado",
                                          ifelse(data$Estado_conjugal == "3", "Viúvo",
                                                 ifelse(data$Estado_conjugal == "4", "Separado judicialmente",
                                                        ifelse(data$Estado_conjugal == "5", "União consensual",
                                                               ifelse(data$Estado_conjugal == "9", "Sem informação", data$Estado_conjugal))))))
    data$Estado_conjugal <- factor(data$Estado_conjugal, levels = c("Solteiro", "Casado", "Viúvo", "Separado judicialmente", "União consensual", "Sem informação"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Estado_conjugal")
  }

  # 'Diagnosticos_e_Tratamentos_Anterior'
  if ("Diagnosticos_e_Tratamentos_Anterior" %in% names(data)) {
    message("Recodificando 'Diagnosticos_e_Tratamentos_Anterior'.")
    data$Diagnosticos_e_Tratamentos_Anterior <- ifelse(data$Diagnosticos_e_Tratamentos_Anterior == "1", "Sem diagnóstico/Sem tratamento",
                                                       ifelse(data$Diagnosticos_e_Tratamentos_Anterior == "2", "Com diagnóstico/Sem tratamento",
                                                              ifelse(data$Diagnosticos_e_Tratamentos_Anterior == "3", "Com diagnóstico/Com tratamento",
                                                                     ifelse(data$Diagnosticos_e_Tratamentos_Anterior == "4", "Outros",
                                                                            ifelse(data$Diagnosticos_e_Tratamentos_Anterior == "9", "Sem informação", data$Diagnosticos_e_Tratamentos_Anterior)))))
    data$Diagnosticos_e_Tratamentos_Anterior <- factor(data$Diagnosticos_e_Tratamentos_Anterior, levels = c("Sem diagnóstico/Sem tratamento", "Com diagnóstico/Sem tratamento", "Com diagnóstico/Com tratamento", "Outros", "Sem informação"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Diagnosticos_e_Tratamentos_Anterior")
  }

  # 'Base_Mais_Importante_Diagnostico'
  if ("Base_Mais_Importante_Diagnostico" %in% names(data)) {
    message("Recodificando 'Base_Mais_Importante_Diagnostico'.")
    data$Base_Mais_Importante_Diagnostico <- ifelse(data$Base_Mais_Importante_Diagnostico == "1", "Clínica",
                                                    ifelse(data$Base_Mais_Importante_Diagnostico == "2", "Pesquisa clínica",
                                                           ifelse(data$Base_Mais_Importante_Diagnostico == "3", "Exame por imagem",
                                                                  ifelse(data$Base_Mais_Importante_Diagnostico == "4", "Marcadores tumorais",
                                                                         ifelse(data$Base_Mais_Importante_Diagnostico == "5", "Citologia",
                                                                                ifelse(data$Base_Mais_Importante_Diagnostico == "6", "Histologia da metástase",
                                                                                       ifelse(data$Base_Mais_Importante_Diagnostico == "7", "Histologia do tumor primário",
                                                                                              ifelse(data$Base_Mais_Importante_Diagnostico == "9", "Sem informação", data$Base_Mais_Importante_Diagnostico))))))))
    data$Base_Mais_Importante_Diagnostico <- factor(data$Base_Mais_Importante_Diagnostico, levels = c("Clínica", "Pesquisa clínica", "Exame por imagem", "Marcadores tumorais", "Citologia", "Histologia da metástase", "Histologia do tumor primário", "Sem informação"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Base_Mais_Importante_Diagnostico")
  }

  # 'Lateralidade'
  if ("Lateralidade" %in% names(data)) {
    message("Recodificando 'Lateralidade'.")
    data$Lateralidade <- ifelse(data$Lateralidade == "1", "Direita",
                                ifelse(data$Lateralidade == "2", "Esquerda",
                                       ifelse(data$Lateralidade == "3", "Bilateral",
                                              ifelse(data$Lateralidade == "8", "Não se aplica",
                                                     ifelse(data$Lateralidade == "9", "Sem informação", data$Lateralidade)))))
    data$Lateralidade <- factor(data$Lateralidade, levels = c("Direita", "Esquerda", "Bilateral", "Não se aplica", "Sem informação"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Lateralidade")
  }

  # 'Mais_de_Um_Tumor'
  if ("Mais_de_Um_Tumor" %in% names(data)) {
    message("Recodificando 'Mais_de_Um_Tumor'.")
    data$Mais_de_Um_Tumor <- ifelse(data$Mais_de_Um_Tumor == "1", "Não",
                                    ifelse(data$Mais_de_Um_Tumor == "2", "Sim",
                                           ifelse(data$Mais_de_Um_Tumor == "3", "Duvidoso", data$Mais_de_Um_Tumor)))
    data$Mais_de_Um_Tumor <- factor(data$Mais_de_Um_Tumor, levels = c("Sim", "Não", "Duvidoso"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Mais_de_Um_Tumor")
  }

  # 'Razao_Nao_Tratamento'
  if ("Razao_Nao_Tratamento" %in% names(data)) {
    message("Recodificando 'Razao_Nao_Tratamento'.")
    data$Razao_Nao_Tratamento <- ifelse(data$Razao_Nao_Tratamento == "1", "Recusa do tratamento",
                                        ifelse(data$Razao_Nao_Tratamento == "2", "Tratamento realizado fora",
                                               ifelse(data$Razao_Nao_Tratamento == "3", "Doença avançada, falta de condições clínicas ou outras doenças associadas",
                                                      ifelse(data$Razao_Nao_Tratamento == "4", "Abandono do tratamento",
                                                             ifelse(data$Razao_Nao_Tratamento == "5", "Complicações de tratamento",
                                                                    ifelse(data$Razao_Nao_Tratamento == "6", "Óbito",
                                                                           ifelse(data$Razao_Nao_Tratamento == "7", "Outras razões",
                                                                                  ifelse(data$Razao_Nao_Tratamento == "8", "Não se aplica",
                                                                                         ifelse(data$Razao_Nao_Tratamento == "9", "Sem informação", data$Razao_Nao_Tratamento)))))))))
    data$Razao_Nao_Tratamento <- factor(data$Razao_Nao_Tratamento, levels = c("Recusa do tratamento", "Tratamento realizado fora", "Doença avançada, falta de condições clínicas ou outras doenças associadas", "Abandono do tratamento", "Complicações de tratamento", "Óbito", "Outras razões", "Não se aplica", "Sem informação"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Razao_Nao_Tratamento")
  }

  # 'Primeiro_Tratamento_Hospital'
  if ("Primeiro_Tratamento_Hospital" %in% names(data)) {
    message("Recodificando 'Primeiro_Tratamento_Hospital'.")
    data$Primeiro_Tratamento_Hospital <- ifelse(data$Primeiro_Tratamento_Hospital == "1", "Nenhum",
                                                ifelse(data$Primeiro_Tratamento_Hospital == "2", "Cirurgia",
                                                       ifelse(data$Primeiro_Tratamento_Hospital == "3", "Radioterapia",
                                                              ifelse(data$Primeiro_Tratamento_Hospital == "4", "Quimioterapia",
                                                                     ifelse(data$Primeiro_Tratamento_Hospital == "5", "Hormonioterapia",
                                                                            ifelse(data$Primeiro_Tratamento_Hospital == "6", "Transplante de medula óssea",
                                                                                   ifelse(data$Primeiro_Tratamento_Hospital == "7", "Imunoterapia",
                                                                                          ifelse(data$Primeiro_Tratamento_Hospital == "8", "Outras",
                                                                                                 ifelse(data$Primeiro_Tratamento_Hospital == "9", "Sem informação", data$Primeiro_Tratamento_Hospital)))))))))
    data$Primeiro_Tratamento_Hospital <- factor(data$Primeiro_Tratamento_Hospital, levels = c("Nenhum", "Cirurgia", "Radioterapia", "Quimioterapia", "Hormonioterapia", "Transplante de medula óssea", "Imunoterapia", "Outras", "Sem informação"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Primeiro_Tratamento_Hospital")
  }

  # 'Estado_Doenca_Final_Tratamento'
  if ("Estado_Doenca_Final_Tratamento" %in% names(data)) {
    message("Recodificando 'Estado_Doenca_Final_Tratamento'.")
    data$Estado_Doenca_Final_Tratamento <- ifelse(data$Estado_Doenca_Final_Tratamento == "1", "Sem evidência da doença (remissão completa)",
                                                  ifelse(data$Estado_Doenca_Final_Tratamento == "2", "Remissão parcial",
                                                         ifelse(data$Estado_Doenca_Final_Tratamento == "3", "Doença estável",
                                                                ifelse(data$Estado_Doenca_Final_Tratamento == "4", "Doença em progressão",
                                                                       ifelse(data$Estado_Doenca_Final_Tratamento == "5", "Suporte terapêutico oncológico",
                                                                              ifelse(data$Estado_Doenca_Final_Tratamento == "6", "Óbito",
                                                                                     ifelse(data$Estado_Doenca_Final_Tratamento == "8", "Não se aplica",
                                                                                            ifelse(data$Estado_Doenca_Final_Tratamento == "9", "Sem informação", data$Estado_Doenca_Final_Tratamento))))))))
    data$Estado_Doenca_Final_Tratamento <- factor(data$Estado_Doenca_Final_Tratamento, levels = c("Sem evidência da doença (remissão completa)", "Remissão parcial", "Doença estável", "Doença em progressão", "Suporte terapêutico oncológico", "Óbito", "Não se aplica", "Sem informação"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Estado_Doenca_Final_Tratamento")
  }

  # 'Base_Mais_Importante_Diagnostico_Sem_Patologicas'
  if ("Base_Mais_Importante_Diagnostico_Sem_Patologicas" %in% names(data)) {
    message("Recodificando 'Base_Mais_Importante_Diagnostico_Sem_Patologicas'.")
    data$Base_Mais_Importante_Diagnostico_Sem_Patologicas <- ifelse(
      data$Base_Mais_Importante_Diagnostico_Sem_Patologicas == "1", "Exame clínico",
      ifelse(data$Base_Mais_Importante_Diagnostico_Sem_Patologicas == "2", "Recursos auxiliares não microscópicos",
             ifelse(data$Base_Mais_Importante_Diagnostico_Sem_Patologicas == "3", "Confirmação microscópica",
                    ifelse(data$Base_Mais_Importante_Diagnostico_Sem_Patologicas == "4", "Sem informação", data$Base_Mais_Importante_Diagnostico_Sem_Patologicas)
             )
      )
    )
    data$Base_Mais_Importante_Diagnostico_Sem_Patologicas <- factor(
      data$Base_Mais_Importante_Diagnostico_Sem_Patologicas,
      levels = c("Exame clínico", "Recursos auxiliares não microscópicos", "Confirmação microscópica", "Sem informação")
    )
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Base_Mais_Importante_Diagnostico_Sem_Patologicas")
  }




  if (length(variaveis_ausentes) > 0) {
    message(paste("Variáveis ausentes:", paste(variaveis_ausentes, collapse = ", ")))
  }

  message(paste("Variáveis recodificadas:", paste(variaveis_recodificadas, collapse = ", ")))

  message(paste("\033[1;32m", "> Recodificação das variáveis concluída. Total:", contador_recodificacoes, "com sucesso \033[0m"))

  return(data)
}





#' Renomear Siglas de Estados para Nomes Completos
#'
#' Esta função utiliza as siglas dos estados brasileiros das variáveis `Estado_Residencia` e `UF_Unidade_Hospital` de um dataframe e cria novas colunas com os seus nomes completos. Os nomes completos dos estados são adicionados em novas colunas `Nome_Estado_Residencia` e `Nome_Estado_Hospital`.
#'
#' @param dados Um dataframe contendo as colunas `Estado_Residencia` e `UF_Unidade_Hospital`.
#' @return Retorna um dataframe com os nomes completos dos estados adicionados em novas colunas.
#' @export
#' @name renomear_siglas_estados
#' @examples
#' # Escreva o nome do dataframe e execute a função. Se seu dataframe for "dados_RHC_combinados", use a funçao como:
#'
#' dados_RHC_combinados <- renomear_siglas_estados(dados_RHC_combinados)
renomear_siglas_estados <- function(dados) {
  message("Iniciando o mapeamento das siglas dos estados para nomes completos.")

  # Mapeamento de siglas para nomes de estados
  state_map  <- c(
    AC = "Acre", AL = "Alagoas", AP = "Amapá", AM = "Amazonas",
    BA = "Bahia", CE = "Ceará", DF = "Distrito Federal", ES = "Espírito Santo",
    GO = "Goiás", MA = "Maranhão", MT = "Mato Grosso", MS = "Mato Grosso do Sul",
    MG = "Minas Gerais", PA = "Pará", PB = "Paraíba", PR = "Paraná",
    PE = "Pernambuco", PI = "Piauí", RJ = "Rio de Janeiro", RN = "Rio Grande do Norte",
    RS = "Rio Grande do Sul", RO = "Rondônia", RR = "Roraima", SC = "Santa Catarina",
    SP = "São Paulo", SE = "Sergipe", TO = "Tocantins"
  )

  colunas_adicionadas <- 0

  # Verifica se as colunas existem antes de tentar acessá-las
  if ("Estado_Residencia" %in% names(dados)) {
    dados$Nome_Estado_Residencia <- state_map[dados$Estado_Residencia]
    message("Nomes completos dos estados para 'Estado_Residencia' adicionados.")
    colunas_adicionadas <- colunas_adicionadas + 1
  } else {
    message("\033[1;31mA coluna 'Estado_Residencia' não foi encontrada no dataframe.\033[0m")
  }

  if ("UF_Unidade_Hospital" %in% names(dados)) {
    dados$Nome_Estado_Hospital <- state_map[dados$UF_Unidade_Hospital]
    message("Nomes completos dos estados para 'UF_Unidade_Hospital' adicionados.")
    colunas_adicionadas <- colunas_adicionadas + 1
  } else {
    message("\033[1;31mA coluna 'UF_Unidade_Hospital' não foi encontrada no dataframe.\033[0m")
  }

  if (colunas_adicionadas == 0) {
    stop("\033[1;31mNenhuma das colunas necessárias ('Estado_Residencia' ou 'UF_Unidade_Hospital') foi encontrada no dataframe. Função interrompida.\033[0m")
  }

  message(paste("\033[1;32m", "> Mapeamento das siglas dos estados concluído. Foram adicionadas", colunas_adicionadas, "coluna(s) no final do dataframe.", "\033[0m"))

  return(dados)
}




#' Renomear CID-O de 3 Dígitos
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

  message(paste("\033[1;32m", "> Ajuste dos códigos CID-3 dígitos concluído com sucesso. Foi adicionada uma coluna no final do dataframe, chamada CID3d.", "\033[0m"))

  return(dados)
}





#' Renomear CID  de 4 Dígitos em um DataFrame
#'
#' Esta função mapeia os códigos CID  de 4 dígitos para seus nomes completos na coluna `Localizacao_Primaria_4D` de um dataframe. Os nomes completos dos CID-O de 4 dígitos são adicionados em uma nova coluna `CID4d`.
#'
#' @param dados Um dataframe contendo a coluna `Localizacao_Primaria_4D`.
#' @return Retorna um dataframe com os nomes completos dos CID-O  de 4 dígitos adicionados em uma nova coluna.
#' @export
#' @name renomear_CID_4digitos
#' @examples
#' # Escreva o nome do dataframe e execute a função. Se seu dataframe for "dados_RHC_combinados", use a funçao como:
#'
#' dados_RHC_combinados <- renomear_CID_4digitos(dados_RHC_combinados)
renomear_CID_4digitos <- function(dados) {
  message("Iniciando o ajuste dos códigos CID-O de 4 dígitos para nomes completos.")

  # Definição do dataframe de mapeamento de CID para nomes
  cid_names <- data.frame(
    CID = c("C00.0", "C00.1", "C00.2", "C00.3", "C00.4", "C00.5", "C00.6", "C00.8", "C00.9", "C01", "C02.0", "C02.1", "C02.2", "C02.3", "C02.4", "C02.8", "C02.9", "C03.0", "C03.1", "C03.9", "C04.0", "C04.1", "C04.8", "C04.9", "C05.0", "C05.1", "C05.2", "C05.8", "C05.9", "C06.0",
            "C06.1", "C06.2", "C06.8", "C06.9", "C07", "C08.0", "C08.1", "C08.8", "C08.9", "C09.0", "C09.1", "C09.8", "C09.9", "C10.0", "C10.1", "C10.2", "C10.3", "C10.4", "C10.8", "C10.9", "C11.0", "C11.1", "C11.2", "C11.3", "C11.8", "C11.9", "C12", "C13.0", "C13.1", "C13.2",
            "C13.8", "C13.9", "C14.0", "C14.2", "C14.8", "C15.0", "C15.1", "C15.2", "C15.3", "C15.4", "C15.5", "C15.8", "C15.9", "C16.0", "C16.1", "C16.2", "C16.3", "C16.4", "C16.5", "C16.6", "C16.8", "C16.9", "C17.0", "C17.1", "C17.2", "C17.3", "C17.8", "C17.9", "C18.0", "C18.1",
            "C18.2", "C18.3", "C18.4", "C18.5", "C18.6", "C18.7", "C18.8", "C18.9", "C19", "C20", "C21.0", "C21.1", "C21.2", "C21.8", "C22.0", "C22.1", "C22.2", "C22.3", "C22.4", "C22.7", "C22.9", "C23", "C24.0", "C24.1", "C24.8", "C24.9", "C25.0", "C25.1", "C25.2", "C25.3",
            "C25.4", "C25.7", "C25.8", "C25.9", "C26.0", "C26.1", "C26.8", "C26.9", "C30.0", "C30.1", "C31.0", "C31.1", "C31.2", "C31.3", "C31.8", "C31.9", "C32.0", "C32.1", "C32.2", "C32.3", "C32.8", "C32.9", "C33", "C34.0", "C34.1", "C34.2", "C34.3", "C34.8", "C34.9", "C37",
            "C38.0", "C38.1", "C38.2", "C38.3", "C38.4", "C38.8", "C39.0", "C39.8", "C39.9", "C40.0", "C40.1", "C40.2", "C40.3", "C40.8", "C40.9", "C41.0", "C41.1", "C41.2", "C41.3", "C41.4", "C41.8", "C41.9", "C42.1", "C43.0", "C43.1", "C43.2", "C43.3", "C43.4", "C43.5", "C43.6",
            "C43.7", "C43.8", "C43.9", "C44.0", "C44.1", "C44.2", "C44.3", "C44.4", "C44.5", "C44.6", "C44.7", "C44.8", "C44.9", "C45.0", "C45.1", "C45.2", "C45.7", "C45.9", "C46.0", "C46.1", "C46.2", "C46.3", "C46.7", "C46.8", "C46.9", "C47.0", "C47.1", "C47.2", "C47.3", "C47.4",
            "C47.5", "C47.6", "C47.8", "C47.9", "C48.0", "C48.1", "C48.2", "C48.8", "C49.0", "C49.1", "C49.2", "C49.3", "C49.4", "C49.5", "C49.6", "C49.8", "C49.9", "C50.0", "C50.1", "C50.2", "C50.3", "C50.4", "C50.5", "C50.6", "C50.8", "C50.9", "C51.0", "C51.1", "C51.2", "C51.8",
            "C51.9", "C52", "C53.0", "C53.1", "C53.8", "C53.9", "C54.0", "C54.1", "C54.2", "C54.3", "C54.8", "C54.9", "C55", "C56", "C57.0", "C57.1", "C57.2", "C57.3", "C57.4", "C57.7", "C57.8", "C57.9", "C58", "C60.0", "C60.1", "C60.2", "C60.8", "C60.9", "C61", "C62.0",
            "C62.1", "C62.9", "C63.0", "C63.1", "C63.2", "C63.7", "C63.8", "C63.9", "C64", "C65", "C66", "C67.0", "C67.1", "C67.2", "C67.3", "C67.4", "C67.5", "C67.6", "C67.7", "C67.8", "C67.9", "C68.0", "C68.1", "C68.8", "C68.9", "C69.0", "C69.1", "C69.2", "C69.3", "C69.4",
            "C69.5", "C69.6", "C69.8", "C69.9", "C70.0", "C70.1", "C70.9", "C71.0", "C71.1", "C71.2", "C71.3", "C71.4", "C71.5", "C71.6", "C71.7", "C71.8", "C71.9", "C72.0", "C72.1", "C72.2", "C72.3", "C72.4", "C72.5", "C72.8", "C72.9", "C73", "C74.0", "C74.1", "C74.9", "C75.0",
            "C75.1", "C75.2", "C75.3", "C75.4", "C75.5", "C75.8", "C75.9", "C76.0", "C76.1", "C76.2", "C76.3", "C76.4", "C76.5", "C76.7", "C76.8", "C77.0", "C77.1", "C77.2", "C77.3", "C77.4", "C77.5", "C77.8", "C77.9", "C78.0", "C78.1", "C78.2", "C78.3", "C78.4", "C78.5", "C78.6",
            "C78.7", "C78.8", "C79.0", "C79.1", "C79.2", "C79.3", "C79.4", "C79.5", "C79.6", "C79.7", "C79.8", "C80", "C81.0", "C81.1", "C81.2", "C81.3", "C81.7", "C81.9", "C82.0", "C82.1", "C82.2", "C82.7", "C82.9", "C83.0", "C83.1", "C83.2", "C83.3", "C83.4", "C83.5", "C83.6",
            "C83.7", "C83.8", "C83.9", "C84.0", "C84.1", "C84.2", "C84.3", "C84.4", "C84.5", "C85.0", "C85.1", "C85.7", "C85.9", "C88.0", "C88.1", "C88.2", "C88.3", "C88.7", "C88.9", "C90.0", "C90.1", "C90.2", "C91.0", "C91.1", "C91.2", "C91.3", "C91.4", "C91.5", "C91.7", "C91.9",
            "C92.0", "C92.1", "C92.2", "C92.3", "C92.4", "C92.5", "C92.7", "C92.9", "C93.0", "C93.1", "C93.2", "C93.7", "C93.9", "C94.0", "C94.1", "C94.2", "C94.3", "C94.4", "C94.5", "C94.7", "C95.0", "C95.1", "C95.2", "C95.7", "C95.9", "C96.0", "C96.1", "C96.2", "C96.3", "C96.7",
            "C96.9", "C97", "D00.0", "D00.1", "D00.2", "D01.0", "D01.1", "D01.2", "D01.3", "D01.4", "D01.5", "D01.7", "D01.9", "D02.0", "D02.1", "D02.2", "D02.3", "D02.9", "D03.0", "D03.1", "D03.2", "D03.3", "D03.4", "D03.5", "D03.6", "D03.7", "D03.8", "D03.9", "D04.0", "D04.1",
            "D04.2", "D04.3", "D04.4", "D04.5", "D04.6", "D04.7", "D04.8", "D04.9", "D05.0", "D05.1", "D05.7", "D05.9", "D06.0", "D06.1", "D06.7", "D06.9", "D07.0", "D07.1", "D07.2", "D07.3", "D07.4", "D07.5", "D07.6", "D09.0", "D09.1", "D09.2", "D09.3", "D09.7", "D09.9", "D10.0",
            "D10.1", "D10.2", "D10.3", "D10.4", "D10.5", "D10.6", "D10.7", "D10.9", "D11.0", "D11.7", "D11.9", "D12.0", "D12.1", "D12.2", "D12.3", "D12.4", "D12.5", "D12.6", "D12.7", "D12.8", "D12.9", "D13.0", "D13.1", "D13.2", "D13.3", "D13.4", "D13.5", "D13.6", "D13.7", "D13.9",
            "D14.0", "D14.1", "D14.2", "D14.3", "D14.4", "D15.0", "D15.1", "D15.2", "D15.7", "D15.9", "D16.0", "D16.1", "D16.2", "D16.3", "D16.4", "D16.5", "D16.6", "D16.7", "D16.8", "D16.9", "D17.0", "D17.1", "D17.2", "D17.3", "D17.4", "D17.5", "D17.6", "D17.7", "D17.9", "D18.0", "D18.1",
            "D19.0", "D19.1", "D19.7", "D19.9", "D20.0", "D20.1", "D21.0", "D21.1", "D21.2", "D21.3", "D21.4", "D21.5", "D21.6", "D21.9", "D22.0", "D22.1", "D22.2", "D22.3", "D22.4", "D22.5", "D22.6", "D22.7", "D22.9", "D23.0", "D23.1", "D23.2", "D23.3", "D23.4", "D23.5", "D23.6",
            "D23.7", "D23.9", "D24", "D25.0", "D25.1", "D25.2", "D25.9", "D26.0", "D26.1", "D26.7", "D26.9", "D27", "D28.0", "D28.1", "D28.2", "D28.7", "D28.9", "D29.0", "D29.1", "D29.2", "D29.3", "D29.4", "D29.7", "D29.9", "D30.0", "D30.1", "D30.2", "D30.3", "D30.4", "D30.7",
            "D30.9", "D31.0", "D31.1", "D31.2", "D31.3", "D31.4", "D31.5", "D31.6", "D31.9", "D32.0", "D32.1", "D32.9", "D33.0", "D33.1", "D33.2", "D33.3", "D33.4", "D33.7", "D33.9", "D34", "D35.0", "D35.1", "D35.2", "D35.3", "D35.4", "D35.5", "D35.6", "D35.7", "D35.8", "D35.9",
            "D36.0", "D36.1", "D36.7", "D36.9", "D37.0", "D37.1", "D37.2", "D37.3", "D37.4", "D37.5", "D37.6", "D37.7", "D37.9", "D38.0", "D38.1", "D38.2", "D38.3", "D38.4", "D38.5", "D38.6", "D39.0", "D39.1", "D39.2", "D39.7", "D39.9", "D40.0", "D40.1", "D40.7", "D40.9", "D41.0",
            "D41.1", "D41.2", "D41.3", "D41.4", "D41.7", "D41.9", "D42.0", "D42.1", "D42.9", "D43.0", "D43.1", "D43.2", "D43.3", "D43.4", "D43.7", "D43.9", "D44.0", "D44.1", "D44.2", "D44.3", "D44.4", "D44.5", "D44.6", "D44.7", "D44.8", "D44.9", "D45", "D46.0", "D46.1", "D46.2",
            "D46.3", "D46.4", "D46.7", "D46.9", "D47.0", "D47.1", "D47.2", "D47.3", "D47.7", "D47.9", "D48.0", "D48.1", "D48.2", "D48.3", "D48.4", "D48.5", "D48.6", "D48.7", "D48.9",
            "C65.9", "C19.9", "C12.9", "C07.9", "C01.9", "C37.9", "C23.9", "C20.9", "C52.9", "C55.9", "C56.0", "C56.9", "C80.9", "C73.9", "C66.9", "C61.9", "C61.2", "C61.", "C64.9", "C64.", "C62", "C42.4", "C42.2", "C42.0", "C42.3", "C58.9"),
    NOME = c("Neoplasia maligna do lábio superior externo",
             "Neoplasia maligna do lábio inferior externo",
             "Neoplasia maligna do lábio externo, não especificado",
             "Neoplasia maligna do lábio superior, face interna",
             "Neoplasia maligna do lábio inferior, face interna",
             "Neoplasia maligna do lábio, sem especificação, face interna",
             "Neoplasia maligna da comissura labial",
             "Neoplasia maligna do lábio com lesão invasiva",
             "Neoplasia maligna do lábio, não especificado",
             "Neoplasia maligna da base da língua",
             "Neoplasia maligna da face dorsal da língua",
             "Neoplasia maligna da borda da língua",
             "Neoplasia maligna da face ventral da língua",
             "Neoplasia maligna de dois terços anteriores da língua, parte não especificada",
             "Neoplasia maligna da amígdala lingual",
             "Neoplasia maligna da língua com lesão invasiva",
             "Neoplasia maligna da língua, não especificada",
             "Neoplasia maligna da gengiva superior",
             "Neoplasia maligna da gengiva inferior",
             "Neoplasia maligna da gengiva, não especificada",
             "Neoplasia maligna do assoalho anterior da boca",
             "Neoplasia maligna do assoalho lateral da boca",
             "Neoplasia maligna do assoalho da boca com lesão invasiva",
             "Neoplasia maligna do assoalho da boca, não especificado",
             "Neoplasia maligna do palato duro",
             "Neoplasia maligna do palato mole",
             "Neoplasia maligna da úvula",
             "Neoplasia maligna do palato com lesão invasiva",
             "Neoplasia maligna do palato, não especificado",
             "Neoplasia maligna da mucosa oral",
             "Neoplasia maligna do vestíbulo da boca",
             "Neoplasia maligna da área retromolar",
             "Neoplasia maligna de outras partes e de partes não especificadas da boca com lesão invasiva",
             "Neoplasia maligna da boca, não especificada",
             "Neoplasia maligna da glândula parótida",
             "Neoplasia maligna da glândula submandibular",
             "Neoplasia maligna da glândula sublingual",
             "Neoplasia maligna das glândulas salivares maiores com lesão invasiva",
             "Neoplasia maligna da glândula salivar maior, não especificada",
             "Neoplasia maligna da fossa amigdaliana",
             "Neoplasia maligna do pilar amigdaliano (anterior) (posterior)",
             "Neoplasia maligna da amígdala com lesão invasiva",
             "Neoplasia maligna da amígdala, não especificada",
             "Neoplasia maligna da valécula",
             "Neoplasia maligna da face anterior da epiglote",
             "Neoplasia maligna da parede lateral da orofaringe",
             "Neoplasia maligna da parede posterior da orofaringe",
             "Neoplasia maligna da fenda branquial",
             "Neoplasia maligna da orofaringe com lesão invasiva",
             "Neoplasia maligna da orofaringe, não especificada",
             "Neoplasia maligna da parede superior da nasofaringe",
             "Neoplasia maligna da parede posterior da nasofaringe",
             "Neoplasia maligna da parede lateral da nasofaringe",
             "Neoplasia maligna da parede anterior da nasofaringe",
             "Neoplasia maligna da nasofaringe com lesão invasiva",
             "Neoplasia maligna da nasofaringe, não especificada",
             "Neoplasia maligna do seio piriforme",
             "Neoplasia maligna da região pós-cricóidea",
             "Neoplasia maligna da prega ariepiglótica, face hipofaríngea",
             "Neoplasia maligna da parede posterior da hipofaringe",
             "Neoplasia maligna da hipofaringe com lesão invasiva",
             "Neoplasia maligna da hipofaringe, não especificada",
             "Neoplasia maligna da faringe, não especificada",
             "Neoplasia maligna do anel de Waldeyer",
             "Neoplasia maligna do lábio, cavidade oral e faringe com lesão invasiva",
             "Neoplasia maligna da porção cervical do esôfago (esôfago cervical)",
             "Neoplasia maligna da porção torácica do esôfago (esôfago torácico)",
             "Neoplasia maligna da porção abdominal do esôfago (esôfago abdominal)",
             "Neoplasia maligna do terço superior do esôfago",
             "Neoplasia maligna do terço médio do esôfago",
             "Neoplasia maligna do terço inferior do esôfago",
             "Neoplasia maligna do esôfago com lesão invasiva",
             "Neoplasia maligna do esôfago, não especificado",
             "Neoplasia maligna da cárdia",
             "Neoplasia maligna do fundo do estômago",
             "Neoplasia maligna do corpo do estômago",
             "Neoplasia maligna do antro pilórico",
             "Neoplasia maligna do piloro",
             "Neoplasia maligna da pequena curvatura do estômago, não especificada",
             "Neoplasia maligna da grande curvatura do estômago, não especificada",
             "Neoplasia maligna do estômago com lesão invasiva",
             "Neoplasia maligna do estômago, não especificado",
             "Neoplasia maligna do duodeno",
             "Neoplasia maligna do jejuno",
             "Neoplasia maligna do íleo",
             "Neoplasia maligna do divertículo de Meckel",
             "Neoplasia maligna do intestino delgado com lesão invasiva",
             "Neoplasia maligna do intestino delgado, não especificado",
             "Neoplasia maligna do ceco",
             "Neoplasia maligna do apêndice (vermiforme)",
             "Neoplasia maligna do cólon ascendente",
             "Neoplasia maligna da flexura (ângulo) hepática(o)",
             "Neoplasia maligna do cólon transverso",
             "Neoplasia maligna da flexura (ângulo) esplênica(o)",
             "Neoplasia maligna do cólon descendente",
             "Neoplasia maligna do cólon sigmóide",
             "Neoplasia maligna do cólon com lesão invasiva",
             "Neoplasia maligna do cólon, não especificado",
             "Neoplasia maligna da junção retossigmóide",
             "Neoplasia maligna do reto",
             "Neoplasia maligna do ânus, não especificado",
             "Neoplasia maligna do canal anal",
             "Neoplasia maligna da zona cloacogênica",
             "Neoplasia maligna do reto, ânus e do canal anal com lesão invasiva",
             "Carcinoma de células hepáticas",
             "Carcinoma de vias biliares intra-hepáticas",
             "Hepatoblastoma",
             "Angiossarcoma do fígado",
             "Outros sarcomas do fígado",
             "Outros carcinomas especificados do fígado",
             "Neoplasia maligna do fígado, não especificada",
             "Neoplasia maligna da vesícula biliar",
             "Neoplasia maligna das vias biliares extra-hepáticas",
             "Neoplasia maligna da ampola de Vater",
             "Neoplasia maligna das vias biliares com lesão invasiva",
             "Neoplasia maligna da via biliar, não especificada",
             "Neoplasia maligna da cabeça do pâncreas",
             "Neoplasia maligna do corpo do pâncreas",
             "Neoplasia maligna da cauda do pâncreas",
             "Neoplasia maligna do canal pancreático",
             "Neoplasia maligna do pâncreas endócrino",
             "Neoplasia maligna de outras partes do pâncreas",
             "Neoplasia maligna do pâncreas com lesão invasiva",
             "Neoplasia maligna do pâncreas, não especificado",
             "Neoplasia maligna do trato intestinal, parte não especificada",
             "Neoplasia maligna do baço",
             "Neoplasia maligna do aparelho digestivo com lesão invasiva",
             "Neoplasia maligna de localizações mal definidas dentro do aparelho digestivo",
             "Neoplasia maligna da cavidade nasal",
             "Neoplasia maligna do ouvido médio",
             "Neoplasia maligna do seio maxilar",
             "Neoplasia maligna do seio etmoidal",
             "Neoplasia maligna do seio frontal",
             "Neoplasia maligna do seio esfenoidal",
             "Neoplasia maligna dos seios da face com lesão invasiva",
             "Neoplasia maligna do seio da face, não especificado",
             "Neoplasia maligna da glote",
             "Neoplasia maligna da região supraglótica",
             "Neoplasia maligna da região subglótica",
             "Neoplasia maligna das cartilagens da laringe",
             "Neoplasia maligna da laringe com lesão invasiva",
             "Neoplasia maligna da laringe, não especificada",
             "Neoplasia maligna da traquéia",
             "Neoplasia maligna do brônquio principal",
             "Neoplasia maligna do lobo superior, brônquio ou pulmão",
             "Neoplasia maligna do lobo médio, brônquio ou pulmão",
             "Neoplasia maligna do lobo inferior, brônquio ou pulmão",
             "Neoplasia maligna dos brônquios e dos pulmões com lesão invasiva",
             "Neoplasia maligna dos brônquios ou pulmões, não especificado",
             "Neoplasia maligna do timo",
             "Neoplasia maligna do coração",
             "Neoplasia maligna do mediastino anterior",
             "Neoplasia maligna do mediastino posterior",
             "Neoplasia maligna do mediastino, porção não especificada",
             "Neoplasia maligna da pleura",
             "Neoplasia maligna do coração, mediastino e pleura com lesão invasiva",
             "Neoplasia maligna do trato respiratório superior, porção não especificada",
             "Neoplasia maligna do aparelho respiratório e dos órgãos intratorácicos com lesão invasiva",
             "Neoplasia maligna de localizações mal definidas do aparelho respiratório",
             "Neoplasia maligna da omoplata [escápula] e ossos longos dos membros superiores",
             "Neoplasia maligna dos ossos curtos dos membros superiores",
             "Neoplasia maligna dos ossos longos dos membros inferiores",
             "Neoplasia maligna dos ossos curtos dos membros inferiores",
             "Neoplasia maligna dos ossos e cartilagens articulares dos membros com lesão invasiva",
             "Neoplasia maligna dos ossos e cartilagens articulares de membro não especificado",
             "Neoplasia maligna dos ossos do crânio e da face",
             "Neoplasia maligna da mandíbula",
             "Neoplasia maligna da coluna vertebral",
             "Neoplasia maligna das costelas, esterno e clavícula",
             "Neoplasia maligna dos ossos da pelve, sacro e cóccix",
             "Neoplasia maligna dos ossos e das cartilagens articulares com lesão invasiva",
             "Neoplasia maligna dos ossos e cartilagens articulares, não especificados",
             "Medula Óssea",
             "Melanoma maligno do lábio",
             "Melanoma maligno da pálpebra, incluindo as comissuras palpebrais",
             "Melanoma maligno da orelha e do conduto auditivo externo",
             "Melanoma maligno de outras partes e partes não especificadas da face",
             "Melanoma maligno do couro cabeludo e do pescoço",
             "Melanoma maligno do tronco",
             "Melanoma maligno do membro superior, incluindo ombro",
             "Melanoma maligno do membro inferior, incluindo quadril",
             "Melanoma maligno invasivo da pele",
             "Melanoma maligno de pele, não especificado",
             "Neoplasia maligna da pele do lábio",
             "Neoplasia maligna da pele da pálpebra, incluindo o canto",
             "Neoplasia maligna da pele da orelha e do conduto auditivo externo",
             "Neoplasia maligna da pele de outras partes e de partes não especificadas da face",
             "Neoplasia maligna da pele do couro cabeludo e do pescoço",
             "Neoplasia maligna da pele do tronco",
             "Neoplasia maligna da pele do membro superior, incluindo ombro",
             "Neoplasia maligna da pele do membro inferior, incluindo quadril",
             "Neoplasia maligna da pele com lesão invasiva",
             "Neoplasia maligna da pele, não especificada",
             "Mesotelioma da pleura",
             "Mesotelioma do peritônio",
             "Mesotelioma do pericárdio",
             "Mesotelioma de outras localizações",
             "Mesotelioma, não especificado",
             "Sarcoma de Kaposi da pele",
             "Sarcoma de Kaposi de tecidos moles",
             "Sarcoma de Kaposi do palato",
             "Sarcoma de Kaposi dos gânglios linfáticos",
             "Sarcoma de Kaposi de outras localizações",
             "Sarcoma de Kaposi de múltiplos órgãos",
             "Sarcoma de Kaposi, não especificado",
             "Neoplasia maligna dos nervos periféricos da cabeça, face e pescoço",
             "Neoplasia maligna dos nervos periféricos dos membros superiores, incluindo ombro",
             "Neoplasia maligna dos nervos periféricos dos membros inferiores, incluindo quadril",
             "Neoplasia maligna dos nervos periféricos do tórax",
             "Neoplasia maligna dos nervos periféricos do abdome",
             "Neoplasia maligna dos nervos periféricos da pelve",
             "Neoplasia maligna dos nervos periféricos do tronco",
             "Neoplasia maligna dos nervos periféricos e do sistema nervoso autônomo com lesão invasiva",
             "Neoplasia maligna dos nervos periféricos e sistema nervoso autônomo, não especificados",
             "Neoplasia maligna do retroperitônio",
             "Neoplasia maligna de partes especificadas do peritônio",
             "Neoplasia maligna do peritônio",
             "Neoplasia maligna dos tecidos moles do retroperitônio e do peritônio com lesão invasiva",
             "Neoplasia maligna do tecido conjuntivo e tecidos moles da cabeça, face e pescoço",
             "Neoplasia maligna do tecido conjuntivo e tecidos moles dos membros superiores, incluindo ombro",
             "Neoplasia maligna do tecido conjuntivo e tecidos moles dos membros inferiores, incluindo quadril",
             "Neoplasia maligna do tecido conjuntivo e tecidos moles do tórax",
             "Neoplasia maligna do tecido conjuntivo e tecidos moles do abdome",
             "Neoplasia maligna do tecido conjuntivo e tecidos moles da pelve",
             "Neoplasia maligna do tecido conjuntivo e tecidos moles do tronco",
             "Neoplasia maligna do tecido conjuntivo e dos tecidos moles com lesão invasiva",
             "Neoplasia maligna do tecido conjuntivo e tecidos moles, não especificados",
             "Neoplasia maligna do mamilo e aréola",
             "Neoplasia maligna da porção central da mama",
             "Neoplasia maligna do quadrante superior interno da mama",
             "Neoplasia maligna do quadrante inferior interno da mama",
             "Neoplasia maligna do quadrante superior externo da mama",
             "Neoplasia maligna do quadrante inferior externo da mama",
             "Neoplasia maligna da porção axilar da mama",
             "Neoplasia maligna da mama com lesão invasiva",
             "Neoplasia maligna da mama, não especificada",
             "Neoplasia maligna dos grandes lábios",
             "Neoplasia maligna dos pequenos lábios",
             "Neoplasia maligna do clitóris",
             "Neoplasia maligna da vulva com lesão invasiva",
             "Neoplasia maligna da vulva, não especificada",
             "Neoplasia maligna da vagina",
             "Neoplasia maligna do endocérvix",
             "Neoplasia maligna do exocérvix",
             "Neoplasia maligna do colo do útero com lesão invasiva",
             "Neoplasia maligna do colo do útero, não especificado",
             "Neoplasia maligna do istmo do útero",
             "Neoplasia maligna do endométrio",
             "Neoplasia maligna do miométrio",
             "Neoplasia maligna do fundo do útero",
             "Neoplasia maligna do corpo do útero com lesão invasiva",
             "Neoplasia maligna do corpo do útero, não especificado",
             "Neoplasia maligna do útero, porção não especificada",
             "Neoplasia maligna do ovário",
             "Neoplasia maligna da trompa de Falópio",
             "Neoplasia maligna do ligamento largo",
             "Neoplasia maligna do ligamento redondo",
             "Neoplasia maligna do paramétrio",
             "Neoplasia maligna dos anexos uterinos",
             "Neoplasia maligna de outras partes especificadas dos órgãos genitais femininos",
             "Neoplasia maligna dos órgãos genitais femininos com lesão invasiva",
             "Neoplasia maligna de órgão genital feminino, não especificado",
             "Neoplasia maligna da placenta",
             "Neoplasia maligna do prepúcio",
             "Neoplasia maligna da glande",
             "Neoplasia maligna do corpo do pênis",
             "Neoplasia maligna do pênis com lesão invasiva",
             "Neoplasia maligna do pênis, não especificado",
             "Neoplasia maligna da próstata",
             "Neoplasia maligna do testículo criptorquídico",
             "Neoplasia maligna do testículo tópico",
             "Neoplasia maligna do testículo, sem outras especificações",
             "Neoplasia maligna do epidídimo",
             "Neoplasia maligna do cordão espermático",
             "Neoplasia maligna do escroto",
             "Neoplasia maligna de outros órgãos genitais masculinos especificados",
             "Neoplasia maligna dos órgãos genitais masculinos com lesão invasiva",
             "Neoplasia maligna de órgão genital masculino, não especificado",
             "Neoplasia maligna do rim, exceto pelve renal",
             "Neoplasia maligna da pelve renal",
             "Neoplasia maligna dos ureteres",
             "Neoplasia maligna do trígono da bexiga",
             "Neoplasia maligna da cúpula da bexiga",
             "Neoplasia maligna da parede lateral da bexiga",
             "Neoplasia maligna da parede anterior da bexiga",
             "Neoplasia maligna da parede posterior da bexiga",
             "Neoplasia maligna do colo da bexiga",
             "Neoplasia maligna do orifício uretérico",
             "Neoplasia maligna do úraco",
             "Neoplasia maligna da bexiga com lesão invasiva",
             "Neoplasia maligna da bexiga, sem outra especificações",
             "Neoplasia maligna da uretra",
             "Neoplasia maligna da glândula parauretral",
             "Neoplasia maligna dos órgãos urinários com lesão invasiva",
             "Neoplasia maligna de órgão urinário, não especificado",
             "Neoplasia maligna da conjuntiva",
             "Neoplasia maligna da córnea",
             "Neoplasia maligna da retina",
             "Neoplasia maligna da coróide",
             "Neoplasia maligna do corpo ciliar",
             "Neoplasia maligna da glândula e canal lacrimal",
             "Neoplasia maligna da órbita",
             "Neoplasia maligna do olho e anexos com lesão invasiva",
             "Neoplasia maligna do olho, não especificado",
             "Neoplasia maligna das meninges cerebrais",
             "Neoplasia maligna das meninges espinhais",
             "Neoplasia maligna da meninge, não especificada",
             "Neoplasia maligna do cérebro, exceto lobos e ventrículos",
             "Neoplasia maligna do lobo frontal",
             "Neoplasia maligna do lobo temporal",
             "Neoplasia maligna do lobo parietal",
             "Neoplasia maligna do lobo occipital",
             "Neoplasia maligna do ventrículo cerebral",
             "Neoplasia maligna do cerebelo",
             "Neoplasia maligna do tronco cerebral",
             "Neoplasia maligna do encéfalo com lesão invasiva",
             "Neoplasia maligna do encéfalo, não especificado",
             "Neoplasia maligna da medula espinhal",
             "Neoplasia maligna da cauda eqüina",
             "Neoplasia maligna do nervo olfativo",
             "Neoplasia maligna do nervo óptico",
             "Neoplasia maligna do nervo acústico",
             "Neoplasia maligna de outros nervos cranianos e os não especificados",
             "Neoplasia maligna do encéfalo e de outras partes do sistema nervoso central com lesão invasiva",
             "Neoplasia maligna do sistema nervoso central, não especificado",
             "Neoplasia maligna da glândula tireóide",
             "Neoplasia maligna do córtex da supra-renal",
             "Neoplasia maligna da medula da supra-renal",
             "Neoplasia maligna da glândula supra-renal, não especificada",
             "Neoplasia maligna da glândula paratireóide",
             "Neoplasia maligna da glândula hipófise (pituitária)",
             "Neoplasia maligna do conduto craniofaríngeo",
             "Neoplasia maligna da glândula pineal",
             "Neoplasia maligna do corpo carotídeo",
             "Neoplasia maligna do corpo aórtico e outros paragânglios",
             "Neoplasia maligna com comprometimento pluriglandular, sem outra especificação",
             "Neoplasia maligna de glândula endócrina, não especificada",
             "Neoplasia maligna da cabeça, face e pescoço",
             "Neoplasia maligna do tórax",
             "Neoplasia maligna do abdome",
             "Neoplasia maligna da pelve",
             "Neoplasia maligna do membro superior",
             "Neoplasia maligna do membro inferior",
             "Neoplasia maligna de outras localizações mal definidas",
             "Neoplasia maligna de outras localizações e das mal definidas com lesão invasiva",
             "Neoplasia maligna secundária e não especificada dos gânglios linfáticos da cabeça, face e pescoço",
             "Neoplasia maligna secundária e não especificada dos gânglios linfáticos intratorácicos",
             "Neoplasia maligna secundária e não especificada dos gânglios linfáticos intra-abdominais",
             "Neoplasia maligna secundária e não especificada dos gânglios linfáticos axilares e dos membros superiores",
             "Neoplasia maligna secundária e não especificada dos gânglios linfáticos inguinais e dos membros inferiores",
             "Neoplasia maligna secundária e não especificada dos gânglios linfáticos intrapélvicos",
             "Neoplasia maligna secundária e não especificada dos gânglios linfáticos de múltiplas regiões",
             "Neoplasia maligna secundária e não especificada de gânglio linfático, não especificado",
             "Neoplasia maligna secundária dos pulmões",
             "Neoplasia maligna secundária do mediastino",
             "Neoplasia maligna secundária da pleura",
             "Neoplasia maligna secundária de outros órgãos respiratórios e não especificados",
             "Neoplasia maligna secundária do intestino delgado",
             "Neoplasia maligna secundária do intestino grosso e do reto",
             "Neoplasia maligna secundária do retroperitônio e do peritônio",
             "Neoplasia maligna secundária do fígado",
             "Neoplasia maligna secundária de outros órgãos digestivos e não especificados",
             "Neoplasia maligna secundária do rim e da pelve renal",
             "Neoplasia maligna secundária da bexiga, de outro órgão urinário e não especificado",
             "Neoplasia maligna secundária da pele",
             "Neoplasia maligna secundária do encéfalo e das meninges cerebrais",
             "Neoplasia maligna secundária de outras partes do sistema nervoso e não especificadas",
             "Neoplasia maligna secundária dos ossos e da medula óssea",
             "Neoplasia maligna secundária do ovário",
             "Neoplasia maligna secundária das glândulas supra-renais (adrenais)",
             "Neoplasia maligna secundária de outra localização especificada",
             "Neoplasia maligna, sem especificação de localização",
             "Doença de Hodgkin, predominância linfocítica",
             "Doença de Hodgkin, esclerose nodular",
             "Doença de Hodgkin, celularidade mista",
             "Doença de Hodgkin, depleção linfocítica",
             "Outra forma da doença de Hodgkin",
             "Doença de Hodgkin, não especificada",
             "Linfoma não-Hodgkin, pequenas células clivadas, folicular",
             "Linfoma não-Hodgkin, misto, de pequenas e grandes células clivadas, folicular",
             "Linfoma não-Hodgkin, grandes células, folicular",
             "Outros tipos de linfoma não-Hodgkin, folicular",
             "Linfoma não-Hodgkin, folicular, não especificado",
             "Linfoma não-Hodgkin difuso, pequenas células (difuso)",
             "Linfoma não-Hodgkin difuso, pequenas células clivadas (difuso)",
             "Linfoma não-Hodgkin difuso, misto, de pequenas e grandes células (difuso)",
             "Linfoma não-Hodgkin difuso, grandes células (difuso)",
             "Linfoma não-Hodgkin difuso, imunoblástico (difuso)",
             "Linfoma não-Hodgkin difuso, linfoblástico (difuso)",
             "Linfoma não-Hodgkin difuso, indiferenciado (difuso)",
             "Tumor de Burkitt",
             "Outros tipos de linfoma não-Hodgkin difuso",
             "Linfoma não-Hodgkin difuso, não especificado",
             "Micose fungóide",
             "Doença de Sézary",
             "Linfoma da zona T",
             "Linfoma linfoepitelióide",
             "Linfoma de células T, periférico",
             "Outros linfomas de células T e os não especificados",
             "Linfossarcoma",
             "Linfoma de células B, não especificado",
             "Outros tipos especificados de linfoma não-Hodgkin",
             "Linfoma não-Hodgkin de tipo não especificado",
             "Macroglobulinemia de Waldenström",
             "Doença de cadeia pesada alfa",
             "Doença de cadeia pesada gama",
             "Doença imunoproliferativa do intestino delgado",
             "Outras doenças imunoproliferativas malignas",
             "Doença imunoproliferativa maligna, não especificada",
             "Mieloma múltiplo",
             "Leucemia plasmocitária",
             "Plasmocitoma extramedular",
             "Leucemia linfoblástica aguda",
             "Leucemia linfocítica crônica",
             "Leucemia linfocítica subaguda",
             "Leucemia pró-linfocítica",
             "Leucemia de células pilosas",
             "Leucemia de células T do adulto",
             "Outras leucemias linfóides",
             "Leucemia linfóide, não especificada",
             "Leucemia mielóide aguda",
             "Leucemia mielóide crônica",
             "Leucemia mielóide subaguda",
             "Sarcoma mielóide",
             "Leucemia pró-mielocítica aguda",
             "Leucemia mielomonocítica aguda",
             "Outras leucemias mielóides",
             "Leucemia mielóide, não especificada",
             "Leucemia monocítica aguda",
             "Leucemia monocítica crônica",
             "Leucemia monocítica subaguda",
             "Outras leucemias monocíticas",
             "Leucemia monocítica, não especificada",
             "Eritremia e eritroleucemia agudas",
             "Eritremia crônica",
             "Leucemia megacarioblástica aguda",
             "Leucemia de mastócitos",
             "Panmielose aguda",
             "Mielofibrose aguda",
             "Outras leucemias especificadas",
             "Leucemia aguda de tipo celular não especificado",
             "Leucemia crônica de tipo celular não especificado",
             "Leucemia subaguda de tipo celular não especificado",
             "Outras leucemias de tipo celular não especificado",
             "Leucemia não especificada",
             "Doença de Letterer-Siwe",
             "Histiocitose maligna",
             "Tumor maligno de mastócitos",
             "Linfoma histiocítico verdadeiro",
             "Outras neoplasias malignas especificadas dos tecidos linfático, hematopoético e tecidos correlatos",
             "Neoplasia maligna dos tecidos linfático, hematopoético e tecidos correlatos, não especificada",
             "Neoplasias malignas de localizações múltiplas independentes (primárias)",
             "Carcinoma in situ dos lábios, cavidade oral e faringe",
             "Carcinoma in situ do esôfago",
             "Carcinoma in situ do estômago",
             "Carcinoma in situ do cólon",
             "Carcinoma in situ da junção retossigmóide",
             "Carcinoma in situ do reto",
             "Carcinoma in situ do ânus e canal anal",
             "Carcinoma in situ de outras partes do intestino e as não especificadas",
             "Carcinoma in situ do fígado, vesícula biliar e vias biliares",
             "Carcinoma in situ de outros órgãos especificados do aparelho digestivo",
             "Carcinoma in situ de órgãos digestivos, não especificado",
             "Carcinoma in situ da laringe",
             "Carcinoma in situ da traquéia",
             "Carcinoma in situ dos brônquios e pulmões",
             "Carcinoma in situ de outras partes do aparelho respiratório",
             "Carcinoma in situ do aparelho respiratório, não especificado",
             "Melanoma in situ do lábio",
             "Melanoma in situ da pálpebra, incluindo o canto",
             "Melanoma in situ da orelha e do conduto auditivo externo",
             "Melanoma in situ de outras partes, e de partes não especificadas da face",
             "Melanoma in situ do couro cabeludo e do pescoço",
             "Melanoma in situ do tronco",
             "Melanoma in situ dos membros superiores, incluindo ombro",
             "Melanoma in situ dos membros inferiores, incluindo quadril",
             "Melanoma in situ de outras localizações",
             "Melanoma in situ, não especificado",
             "Carcinoma in situ da pele do lábio",
             "Carcinoma in situ da pele da pálpebra, incluindo o canto",
             "Carcinoma in situ da pele da orelha e do conduto auditivo externo",
             "Carcinoma in situ da pele de outras partes e de partes não especificadas da face",
             "Carcinoma in situ da pele do couro cabeludo e do pescoço",
             "Carcinoma in situ da pele do tronco",
             "Carcinoma in situ da pele dos membros superiores, incluindo ombro",
             "Carcinoma in situ da pele dos membros inferiores, incluindo quadril",
             "Carcinoma in situ da pele de outras localizações",
             "Carcinoma in situ da pele, não especificada",
             "Carcinoma lobular in situ",
             "Carcinoma intraductal in situ",
             "Outros carcinomas in situ",
             "Carcinoma in situ da mama, não especificado",
             "Carcinoma in situ do endocérvix",
             "Carcinoma in situ do exocérvix",
             "Carcinoma in situ de outras partes do colo do útero",
             "Carcinoma in situ do colo do útero, não especificado",
             "Carcinoma in situ do endométrio",
             "Carcinoma in situ da vulva",
             "Carcinoma in situ da vagina",
             "Carcinoma in situ de outros órgãos genitais femininos e os não especificados",
             "Carcinoma in situ do pênis",
             "Carcinoma in situ da próstata",
             "Carcinoma in situ de outros órgãos genitais masculinos e os não especificados",
             "Carcinoma in situ da bexiga",
             "Carcinoma in situ de outros órgãos urinários e os não especificados",
             "Carcinoma in situ do olho",
             "Carcinoma in situ da tireóide e de outras glândulas endócrinas",
             "Carcinoma in situ de outras localizações especificadas",
             "Carcinoma in situ, não especificado",
             "Neoplasia benigna dos lábios",
             "Neoplasia benigna da língua",
             "Neoplasia benigna do assoalho da boca",
             "Neoplasia benigna de outras partes da boca e as não especificadas",
             "Neoplasia benigna da amígdala",
             "Neoplasia benigna de outras partes da orofaringe",
             "Neoplasia benigna da nasofaringe",
             "Neoplasia benigna da hipofaringe",
             "Neoplasia benigna da faringe, não especificada",
             "Neoplasia benigna da glândula parótida",
             "Neoplasia benigna de outras glândulas salivares maiores",
             "Neoplasia benigna da glândula salivar maior, não especificada",
             "Neoplasia benigna do ceco",
             "Neoplasia benigna do apêndice (vermiforme)",
             "Neoplasia benigna do cólon ascendente",
             "Neoplasia benigna do cólon transverso",
             "Neoplasia benigna do cólon descendente",
             "Neoplasia benigna do cólon sigmóide",
             "Neoplasia benigna do cólon, não especificada",
             "Neoplasia benigna da junção retossigmóide",
             "Neoplasia benigna do reto",
             "Neoplasia benigna do canal anal e ânus",
             "Neoplasia benigna do esôfago",
             "Neoplasia benigna do estômago",
             "Neoplasia benigna do duodeno",
             "Neoplasia benigna de outras partes e partes não especificadas do intestino delgado",
             "Neoplasia benigna do fígado",
             "Neoplasia benigna das vias biliares extra-hepáticas",
             "Neoplasia benigna do pâncreas",
             "Neoplasia benigna do pâncreas endócrino",
             "Neoplasia benigna de localizações mal definidas do aparelho digestivo",
             "Neoplasia benigna do ouvido médio, cavidade nasal e seios paranasais",
             "Neoplasia benigna da laringe",
             "Neoplasia benigna da traquéia",
             "Neoplasia benigna dos brônquios e pulmão",
             "Neoplasia benigna do aparelho respiratório, não especificado",
             "Neoplasia benigna do timo",
             "Neoplasia benigna do coração",
             "Neoplasia benigna do mediastino",
             "Neoplasia benigna de outros órgãos intratorácicos especificados",
             "Neoplasia benigna de órgão intratorácico, não especificado",
             "Neoplasia benigna da omoplata [escápula] e ossos longos dos membros superiores",
             "Neoplasia benigna dos ossos curtos dos membros superiores",
             "Neoplasia benigna dos ossos longos dos membros inferiores",
             "Neoplasia benigna dos ossos curtos dos membros inferiores",
             "Neoplasia benigna dos ossos do crânio e da face",
             "Neoplasia benigna do osso da mandíbula",
             "Neoplasia benigna da coluna vertebral",
             "Neoplasia benigna das costelas, do esterno e da clavícula",
             "Neoplasia benigna dos ossos pélvicos, sacro e cóccix",
             "Neoplasia benigna do osso e cartilagem articular, não especificado",
             "Neoplasia lipomatosa benigna da pele e do tecido subcutâneo da cabeça, face e pescoço",
             "Neoplasia lipomatosa benigna da pele e tecido subcutâneo do tronco",
             "Neoplasia lipomatosa benigna da pele e tecido subcutâneo dos membros",
             "Neoplasia lipomatosa benigna da pele e tecido subcutâneo de outras localizações e de localizações não especificadas",
             "Neoplasia lipomatosa benigna de órgãos intratorácicos",
             "Neoplasia lipomatosa benigna de órgãos intra-abdominais",
             "Neoplasia lipomatosa benigna do cordão espermático",
             "Neoplasia lipomatosa benigna de outras localizações",
             "Neoplasia lipomatosa benigna de localização não especificada",
             "Hemangioma de qualquer localização",
             "Linfangioma de qualquer localização",
             "Neoplasia benigna do tecido mesotelial da pleura",
             "Neoplasia benigna do tecido mesotelial do peritônio",
             "Neoplasia benigna do tecido mesotelial de outras localizações",
             "Neoplasia benigna do tecido mesotelial, não especificado",
             "Neoplasia benigna do retroperitônio",
             "Neoplasia benigna do peritônio",
             "Neoplasia benigna do tecido conjuntivo e outros tecidos moles da cabeça, face e pescoço",
             "Neoplasia benigna do tecido conjuntivo e outros tecidos moles dos membros superiores, incluindo ombro",
             "Neoplasia benigna do tecido conjuntivo e outros tecidos moles dos membros inferiores, incluindo quadril",
             "Neoplasia benigna do tecido conjuntivo e outros tecidos moles do tórax",
             "Neoplasia benigna do tecido conjuntivo e outros tecidos moles do abdome",
             "Neoplasia benigna do tecido conjuntivo e outros tecidos moles da pelve",
             "Neoplasia benigna do tecido conjuntivo e outros tecidos moles do tronco, não especificado",
             "Neoplasia benigna do tecido conjuntivo e outros tecidos moles, sem outra especificação",
             "Nevo melanocítico do lábio",
             "Nevo melanocítico da pálpebra, incluindo o canto",
             "Nevo melanocítico da orelha e do conduto auditivo externo",
             "Nevo melanocítico de outras partes e de partes não especificadas da face",
             "Nevo melanocítico do couro cabeludo e do pescoço",
             "Nevo melanocítico do tronco",
             "Nevo melanocítico dos membros superiores, incluindo ombro",
             "Nevo melanocítico dos membros inferiores, incluindo quadril",
             "Nevo melanocítico, não especificado",
             "Neoplasia benigna da pele dos lábios",
             "Neoplasia benigna da pele da pálpebra, incluindo o canto",
             "Neoplasia benigna da pele da orelha e do conduto auditivo externo",
             "Neoplasia benigna da pele de outras partes e de partes não especificadas da face",
             "Neoplasia benigna da pele do couro cabeludo e do pescoço",
             "Neoplasia benigna da pele do tronco",
             "Neoplasia benigna da pele dos membros superiores, incluindo o ombro",
             "Neoplasia benigna da pele dos membros inferiores, incluindo o quadril",
             "Neoplasia benigna da pele, não especificada",
             "Neoplasia benigna da mama",
             "Leiomioma submucoso do útero",
             "Leiomioma intramural do útero",
             "Leiomioma subseroso do útero",
             "Leiomioma do útero, não especificado",
             "Neoplasia benigna do colo do útero",
             "Neoplasia benigna do corpo do útero",
             "Neoplasia benigna de outras partes do útero",
             "Neoplasia benigna do útero, não especificado",
             "Neoplasia benigna do ovário",
             "Neoplasia benigna da vulva",
             "Neoplasia benigna da vagina",
             "Neoplasia benigna das trompas e ligamentos uterinos",
             "Neoplasia benigna de outros órgãos genitais femininos especificados",
             "Neoplasia benigna de órgão genital feminino, não especificado",
             "Neoplasia benigna do pênis",
             "Neoplasia benigna da próstata",
             "Neoplasia benigna dos testículos",
             "Neoplasia benigna do epidídimo",
             "Neoplasia benigna do escroto",
             "Neoplasia benigna de outros órgãos genitais masculinos",
             "Neoplasia benigna de órgão genital masculino, não especificado",
             "Neoplasia benigna do rim",
             "Neoplasia benigna da pelve renal",
             "Neoplasia benigna do ureter",
             "Neoplasia benigna da bexiga",
             "Neoplasia benigna da uretra",
             "Neoplasia benigna de outros órgãos urinários",
             "Neoplasia benigna de órgão urinário, não especificado",
             "Neoplasia benigna da conjuntiva",
             "Neoplasia benigna da córnea",
             "Neoplasia benigna da retina",
             "Neoplasia benigna da coróide",
             "Neoplasia benigna do corpo ciliar",
             "Neoplasia benigna das glândulas e dos canais lacrimais",
             "Neoplasia benigna da órbita, não especificada",
             "Neoplasia benigna do olho, não especificado",
             "Neoplasia benigna das meninges cerebrais",
             "Neoplasia benigna das meninges espinhais",
             "Neoplasia benigna das meninges, não especificada",
             "Neoplasia benigna do encéfalo, supratentorial",
             "Neoplasia benigna do encéfalo, infratentorial",
             "Neoplasia benigna do encéfalo, não especificado",
             "Neoplasia benigna dos nervos cranianos",
             "Neoplasia benigna da medula espinhal",
             "Neoplasia benigna de outras partes especificadas do sistema nervoso central",
             "Neoplasia benigna do sistema nervoso central, não especificado",
             "Neoplasia benigna da glândula tireóide",
             "Neoplasia benigna da glândula supra-renal (adrenal)",
             "Neoplasia benigna da glândula paratireóide",
             "Neoplasia benigna da glândula hipófise (pituitária)",
             "Neoplasia benigna do conduto craniofaríngeo",
             "Neoplasia benigna da glândula pineal",
             "Neoplasia benigna do corpo carotídeo",
             "Neoplasia benigna dos corpos aórticos e outros paragânglios",
             "Neoplasia benigna de outras glândulas endócrinas especificadas",
             "Neoplasia benigna com comprometimento pluriglandular",
             "Neoplasia benigna de glândula endócrina, não especificada",
             "Neoplasia benigna dos gânglios linfáticos (linfonodos)",
             "Neoplasia benigna dos nervos periféricos e sistema nervoso autônomo",
             "Neoplasia benigna de outras localizações especificadas",
             "Neoplasia benigna de localização não especificada",
             "Neoplasia de comportamento incerto ou desconhecido do lábio, cavidade oral e faringe",
             "Neoplasia de comportamento incerto ou desconhecido do estômago",
             "Neoplasia de comportamento incerto ou desconhecido do intestino delgado",
             "Neoplasia de comportamento incerto ou desconhecido do apêndice",
             "Neoplasia de comportamento incerto ou desconhecido dos cólons",
             "Neoplasia de comportamento incerto ou desconhecido do reto",
             "Neoplasia de comportamento incerto ou desconhecido do fígado, vesícula biliar e vias biliares",
             "Neoplasia de comportamento incerto ou desconhecido de outros órgãos digestivos",
             "Neoplasia de comportamento incerto ou desconhecido de órgão digestivo, não especificado",
             "Neoplasia de comportamento incerto ou desconhecido da laringe",
             "Neoplasia de comportamento incerto ou desconhecido da traquéia, brônquios e pulmão",
             "Neoplasia de comportamento incerto ou desconhecido da pleura",
             "Neoplasia de comportamento incerto ou desconhecido do mediastino",
             "Neoplasia de comportamento incerto ou desconhecido do timo",
             "Neoplasia de comportamento incerto ou desconhecido de outros órgãos respiratórios",
             "Neoplasia de comportamento incerto ou desconhecido de órgão respiratório, não especificado",
             "Neoplasia de comportamento incerto ou desconhecido do útero",
             "Neoplasia de comportamento incerto ou desconhecido do ovário",
             "Neoplasia de comportamento incerto ou desconhecido da placenta",
             "Neoplasia de comportamento incerto ou desconhecido de outros órgãos genitais femininos",
             "Neoplasia de comportamento incerto ou desconhecido de órgão genital feminino, não especificado",
             "Neoplasia de comportamento incerto ou desconhecido da próstata",
             "Neoplasia de comportamento incerto ou desconhecido do testículo",
             "Neoplasia de comportamento incerto ou desconhecido de outros órgãos genitais masculinos",
             "Neoplasia de comportamento incerto ou desconhecido de órgão genital masculino, não especificado",
             "Neoplasia de comportamento incerto ou desconhecido do rim",
             "Neoplasia de comportamento incerto ou desconhecido da pelve renal",
             "Neoplasia de comportamento incerto ou desconhecido do ureter",
             "Neoplasia de comportamento incerto ou desconhecido da uretra",
             "Neoplasia de comportamento incerto ou desconhecido da bexiga",
             "Neoplasia de comportamento incerto ou desconhecido de outros órgãos urinários",
             "Neoplasia de comportamento incerto ou desconhecido de órgão urinário, não especificado",
             "Neoplasia de comportamento incerto ou desconhecido das meninges cerebrais",
             "Neoplasia de comportamento incerto ou desconhecido das meninges espinhais",
             "Neoplasia de comportamento incerto ou desconhecido de meninges não especificadas",
             "Neoplasia de comportamento incerto ou desconhecido do encéfalo, supratentorial",
             "Neoplasia de comportamento incerto ou desconhecido do encéfalo, infratentorial",
             "Neoplasia de comportamento incerto ou desconhecido do encéfalo, não especificado",
             "Neoplasia de comportamento incerto ou desconhecido dos nervos cranianos",
             "Neoplasia de comportamento incerto ou desconhecido da medula espinhal",
             "Neoplasia de comportamento incerto ou desconhecido de outras partes do sistema nervoso central",
             "Neoplasia de comportamento incerto ou desconhecido do sistema nervoso central, não especificado",
             "Neoplasia de comportamento incerto ou desconhecido da glândula tireóide",
             "Neoplasia de comportamento incerto ou desconhecido da glândula supra-renal (adrenal)",
             "Neoplasia de comportamento incerto ou desconhecido da glândula paratireóide",
             "Neoplasia de comportamento incerto ou desconhecido da glândula hipófise (pituitária)",
             "Neoplasia de comportamento incerto ou desconhecido do conduto craniofaríngeo",
             "Neoplasia de comportamento incerto ou desconhecido da glândula pineal",
             "Neoplasia de comportamento incerto ou desconhecido do corpo carotídeo",
             "Neoplasia de comportamento incerto ou desconhecido dos corpos aórticos e outros paragânglios",
             "Neoplasia de comportamento incerto ou desconhecido com comprometimento pluriglandular",
             "Neoplasia de comportamento incerto ou desconhecido de glândula endócrina, não especificada",
             "Policitemia vera",
             "Anemia refratária sem sideroblastos",
             "Anemia refratária com sideroblastos",
             "Anemia refratária com excesso de blastos",
             "Anemia refratária com excesso de blastos com transformação",
             "Anemia refratária, não especificada",
             "Outras síndromes mielodisplásicas",
             "Síndrome mielodisplásica, não especificada",
             "Tumores de comportamento incerto ou desconhecido de mastócitos e células histiocíticas",
             "Doença mieloproliferativa crônica",
             "Gamopatia monoclonal",
             "Trombocitemia essencial (hemorrágica)",
             "Outras neoplasias especificadas de comportamento incerto ou desconhecido dos tecidos linfático, hematopoético e tecidos relacionados",
             "Neoplasia de comportamento incerto ou desconhecido dos tecidos linfático, hematopoético e tecidos correlatos, não especificada",
             "Neoplasia de comportamento incerto ou desconhecido dos ossos e cartilagens articulares",
             "Neoplasia de comportamento incerto ou desconhecido do tecido conjuntivo e outros tecidos moles",
             "Neoplasia de comportamento incerto ou desconhecido dos nervos periféricos e do sistema nervoso periférico",
             "Neoplasia de comportamento incerto ou desconhecido do retroperitônio",
             "Neoplasia de comportamento incerto ou desconhecido do peritônio",
             "Neoplasia de comportamento incerto ou desconhecido da pele",
             "Neoplasia de comportamento incerto ou desconhecido da mama",
             "Neoplasia de comportamento incerto ou desconhecido de outras localizações especificadas",
             "Neoplasia de comportamento incerto ou desconhecido sem outra especificação", "Pelve renal, não especificao", "Retossigmóide, não especificado", "Seio piriforme, não especificado",
             "Parótida, não especificado", "Base da língua, não especificado", "Timo, não especificado", "Vesícula biliar, não especificado", "Reto, não especificado", "Vagina, não especificado",
             "Útero, não especificado", "Ovário, não especificado", "Ovário, não especificado", "Local primário desconhecido", "Tireoide, não especificado", "Ureter", "Próstata, não especificado",
             "Neoplasia maligna da próstata", "Neoplasia maligna da próstata", "Rim, não especificado", "Neoplasia maligna do rim, exceto pelve renal", "Neoplasia maligna dos testículos",
             "Sistema hematopoiético, não especificado", "Baço", "Sangue", "Sistema reticuloendotelial, não especificado", "Mola hidatidiforme, não especificado")
  )

  # Verifica se a coluna Localizacao_Primaria_3D existe antes de tentar acessá-la
  if (!"Localizacao_Primaria_4D" %in% names(dados)) {
    stop("\033[1;31mA coluna 'Localizacao_Primaria_4D' não foi encontrada no dataframe. Função interrompida.\033[0m")
  }

  message("Mapeando os códigos CID-O de 4 dígitos para nomes completos.")
  # Mapeamento dos códigos CID para nomes usando um vetor nomeado
  map <- setNames(cid_names$NOME, cid_names$CID)
  dados$CID4d <- map[dados$Localizacao_Primaria_4D]
  message("Nomes completos dos CID-O de 4 dígitos adicionados.")

  message(paste("\033[1;32m", "> Ajuste dos códigos CID-O de 4 dígitos concluído com sucesso. Foi adicionada uma coluna no final do dataframe, chamada CID4d.", "\033[0m"))

  return(dados)
}






#' Renomear CNES
#'
#' Esta função mapeia os códigos CNES para os nomes completos dos estabelecimentos na coluna `CNES_Hospital` de um dataframe. Os nomes completos dos estabelecimentos são adicionados em uma nova coluna `Estabelecimento_Hospitalar`.
#'
#' @param dados Um dataframe contendo a coluna `CNES_Hospital`.
#' @return Retorna um dataframe com os nomes completos dos estabelecimentos adicionados em uma nova coluna.
#' @export
#' @name renomear_CNES
#' @examples
#' # Escreva o nome do dataframe e execute a função. Se seu dataframe for "dados_RHC_combinados", use a função como:
#'
#' dados_RHC_combinados <- renomear_CNES(dados_RHC_combinados)
renomear_CNES <- function(dados) {
  message("Iniciando o ajuste dos códigos CNES para nomes completos dos estabelecimentos.")

  # Definição do dataframe de mapeamento de CNES para nomes dos estabelecimentos
  cnes_names <- data.frame(
    CNES = c("2001586", "2005417", "2007037", "2006197", "2006448", "2020645", "2012677", "2017644", "3400557",
             "6602533", "2601680", "2772280", "2525569", "2802112", "4028155", "0003808", "0003816", "0003786",
             "0003832", "0004278", "0003859", "0003921", "2802104", "2799286", "2301318", "2402076", "2772566",
             "2407205", "2564211", "2563681", "2561492", "2723220", "2651394", "2611686", "2497654", "2528843",
             "2723190", "3021114", "0010510", "0010499", "0010456", "0010464", "0010480", "0010472", "0010502",
             "6876617", "2547821", "2448521", "2465833", "2494442", "0011738", "0011800", "0011746", "4044916",
             "2442108", "2361787", "2506815", "2338424", "2338351", "7891067", "2531348", "6497489", "2697696",
             "2726653", "2646536", "2171945", "2098938", "2200457", "2695324", "0026859", "0027049", "0027014",
             "0026840", "0026964", "2126494", "2148293", "2159252", "2118661", "2205440", "2215586", "2105780",
             "2153025", "2153114", "2153106", "2149990", "2219646", "2195453", "2775999", "9650105", "6442560",
             "2209195", "2129469", "2110075", "2111640", "2127989", "3145425", "2161354", "2206528", "2184834",
             "2165058", "2206595", "2146355", "6601804", "2761092", "0009709", "0009776", "0009725", "0009717",
             "2376334", "6201059", "6583326", "2756951", "2534444", "2659107", "2655519", "2396866", "2795671",
             "2334321", "7871902", "2332981", "5585422", "2315793", "2676060", "2399776", "2399741", "2605473",
             "3369293", "2427419", "2639009", "9262407", "0000809", "0000477", "0000434", "0000582", "2427427",
             "0000396", "4009444", "2726998", "3285391", "7445571", "2439360", "2576341", "0013633", "0013838",
             "0013846", "0014109", "2737434", "2740338", "2384299", "0015563", "0015334", "0015644", "3075516",
             "0015245", "9130780", "2591049", "5373190", "2741989", "2781859", "2577623", "2743469", "2586797",
             "2586169", "0017868", "2686953", "7205686", "7845138", "2280051", "2278286", "2287250", "2287447",
             "2287285", "2278855", "0012556", "0012505", "2275562", "2268779", "2296241", "2269988", "2269384",
             "2269880", "2295423", "2269775", "2273659", "2269899", "2295415", "2269783", "2280167", "2296616",
             "7185081", "2295067", "2273454", "2269821", "2273462", "2292386", "2273748", "25186", "3675580",
             "2371707", "2409194", "2409151", "2656930", "8003629", "2653982", "6599877", "4001303", "2515377",
             "7068336", "2319659", "2261987", "2241021", "2266474", "2232014", "2262274", "2223538", "2223546",
             "2263858", "2707918", "2261057", "2252287", "2246988", "2246929", "2252694", "2253054", "2237601",
             "2262568", "2265052", "2237253", "2237571", "2232995", "2255936", "2244306", "2254611", "2244357",
             "2259907", "2248298", "2248204", "2232022", "2227932", "2248190", "2558246", "2558254", "2537788",
             "2758164", "19445", "2691841", "19283", "2691868", "3157245", "2522691", "2306336", "2560771",
             "2436469", "6048692", "2504332", "2543044", "2568713", "2521792", "2491710", "0002283", "2816210",
             "0002534", "2078775", "2082527", "2081253", "2083094", "2083604", "2090236", "2790602", "2748223",
             "2704900", "2081482", "2082128", "2079798", "2081490", "2089327", "2084163", "2705982", "2081512",
             "2754843", "2027186", "2085194", "7066376", "2083086", "2786435", "2716801", "2081458", "2025507",
             "2083116", "2080680", "2096498", "4049020", "2077434", "2087057", "2772310", "7400926", "2755130",
             "2030705", "2082187", "2080400", "2084414", "7361289", "2082888", "2080273", "0008923", "0008753",
             "2025752", "2080354", "2079720", "2025361", "2027356", "2082594", "2080931", "2084228", "2798298",
             "2077396", "0009601", "2748029", "5869412", "2078287", "2688573", "2077574", "2088576", "2078015",
             "2077531", "2071371", "2066572", "2077523", "2688689", "2080575", "2077477", "2077485", "2077590",
             "2089696", "2080125", "6123740", "2091755", "2081695", "2092298", "2708779", "2079321", "2079828",
             "3126838", "2749319", "2080664", "2600536", "2786117", "9255400", "0004251", "0001023", "2430843",
             "3477371", "2281821", "0019402", "0009369"),
    ESTABELECIMENTO = c("Hospital da Fundação Hospitalar Estadual do Acre",
                        "Complexo Hospitalar Manoel André - CHAMA",
                        "Hospital da Santa Casa de Misericórdia de Maceió",
                        "Hospital Universitário Alberto Antunes/Universidade Federal de Alagoas",
                        "Hospital do Açúcar/Fundação da Agro-Indústria de Açúcar e do Álcool de Alagoas- Hospital veredas",
                        "Hospital de Clínicas Dr. Alberto Lima",
                        "Hospital da Fundação Centro de Controle de Oncologia/CECON",
                        "Hospital Universitário Getúlio Vargas",
                        "Instituto de Mama do Amazonas - SENSUMED",
                        "Hospital Estadual da Criança",
                        "Hospital Dom Pedro de Alcântara/Santa Casa de Misericórdia de Feira de Santana",
                        "Hospital Calixto Midlej Filho",
                        "Hospital Manoel Novaes",
                        "Hospital São José Maternidade Santa Helena/Santa Casa de Mis.",
                        "Hospital Regional de Juazeiro",
                        "Hospital São Rafael/Fundação Monte Tabor",
                        "Hospital Professor Edgard Santos/Hospital Universitário MEC - Universidade Federal da Bahia/FAPEX",
                        "Hospital Aristidez Maltez/Liga Baiana Contra o Câncer",
                        "Hospital Santa Isabel/Santa Casa de Misericórdia da Bahia",
                        "Hospital Martagão Gesteira/Liga Álvaro Bahia Contra a Mortalidade Infantil",
                        "Hospital Geral Roberto Santos",
                        "Centro Estadual de Oncologia - CICAN",
                        "Hospital Santo Antônio/Obras Sociais Irmã Dulce",
                        "Hospital Maternidade Luiz Argolo- Irmandade da Sta Casa de Miser De S Ant de Jesus",
                        "Hospital Municipal de Teixeira de Freitas/Prefeitura Municipal de T. de Freitas",
                        "Hospital Geral de Vitória da Conquista",
                        "Conquista Assistência Medica LTDA/ONCO-MED RAC",
                        "Serviço de Assistência Médica de Urgencia S. A. (SAMUR)",
                        "Hospital e Maternidade São Vicente de Paulo",
                        "Hospital Infantil Albert Sabin",
                        "Hospital Universitário Walter Cantídio",
                        "Instituto de Câncer do Ceará",
                        "Hospital da Irmandade Beneficente Santa Casa de Misericórdia de Fortaleza",
                        "Hospital Cura D'ars/Beneficência Camiliana",
                        "HGF- Hospital Geral de Fortaleza/Secretaria de Estado da Saude",
                        "Hospital Distrital Dr. Fernandes Távora/Instituto Clínico de Fortaleza",
                        "Centro Regional Integrado de Oncologia/CRIO",
                        "Hospital da Santa Casa de Misericórdia de Sobral",
                        "Hospital Universitário de Brasília/Fundação da Universidade de Brasília",
                        "Hospital Regional de Taguatinga",
                        "Hospital de Base do Distrito Federal",
                        "Hospital Regional da Asa Norte",
                        "Hospital Regional de Ceilândia",
                        "Hospital Regional do Gama",
                        "Hospital Regional de Sobradinho",
                        "Hospital da Criança de Brasília Jose Alencar - HCB",
                        "Hospital Evangélico de Cachoeiro de Itapemirim",
                        "Hospital São José",
                        "Hospital Rio Doce",
                        "Hospital Evangélico de Vila Velha",
                        "Hospital Santa Rita de Cássia/Associação Feminina Educacional de Combate ao Câncer",
                        "Hospital Infantil Nossa Senhora da Glória",
                        "Hospital da Santa Casa de Misericórdia de Vitória",
                        "Hospital Universitário Cassiano Antônio de Moraes",
                        "Hospital Evangélico Anápolis/Fundação James Fanstone",
                        "Santa Casa de Misericórdia de Anápolis/Fundação de Assistencia Social de Anápolis",
                        "Hospital Araújo Jorge/Hospital do Câncer/Associação de Combate ao Câncer em Goiás",
                        "Hospital das Clínicas da Universidade Federal Goiás",
                        "Hospital da Santa Casa de Misericórdia de Goiânia",
                        "Hospital Regional de Caxias Dr Everaldo Ferreira Aragão",
                        "Hospital São Rafael",
                        "ONCORADIUM",
                        "Instituto Maranhense de Oncologia Aldenora Belo IMOAB/Fundação Antônio Jorge Dino",
                        "Hospital Universitário Federal do Maranhão (HUUFM)",
                        "Hospital Geral Tarquínio Lopes Filho/SES",
                        "Casa de Caridade de Alfenas Nossa Senhora do Perpétuo Socorro",
                        "Hospital Ibiapaba S/A",
                        "Hospital Luxemburgo/Associação dos Amigos do Hospital Mário Penna",
                        "Hospital da Baleia/Fundação Benjamin Guimarães",
                        "Hospital Felício Rocho/Fundação Felice Rosso",
                        "Hospital das Clínicas da UFMG",
                        "Hospital da Santa Casa de Misericórdia de Belo Horizonte",
                        "Hospital São Francisco de Assis",
                        "Hospital Alberto Cavalcanti/Fundação Hospitalar do Estado de Minas Gerais",
                        "Hospital Professor Osvaldo R. Franco/Prefeitura de Betim/Fundo Municipal de Betim",
                        "Hospital Imaculada Conceição",
                        "Hospital São João de Deus/Fundação Geraldo Corrêa",
                        "Hospital Bom Samaritano/Beneficência Social Bom Samaritano",
                        "Hospital Márcio Cunha/Fundação São Francisco Xavier",
                        "Hospital Nossa Senhora das Dores",
                        "Hospital Manoel Goncalves",
                        "Hospital Maria José Baeta Reis/ASCOMCER",
                        "Hospital Dr. João Felício S/A",
                        "Instituto Oncológico",
                        "Hospital da Santa Casa de Montes Claros/Irmandade Nossa Senhora das Mercês de Montes Claros",
                        "Hospital Dílson de Quadros Godinho/Fundação Dílson de Quadros Godinho",
                        "Hospital do Câncer de Muriaé/Fundação Cristiano Varella",
                        "Hospital da Santa Casa de Misericórdia de Passos",
                        "Santa Casa de Misericórdia de Patos de Minas",
                        "Centro Oncológico AZ/Patos de Minas/MG",
                        "Hospital Santa Casa de Patrocínio",
                        "Hospital da Santa Casa de Misericórdia de Poços de Caldas",
                        "Clínica Memorial",
                        "Hospital Nossa Senhora das Dores/Irmandade Hospital N. Sra das Dores",
                        "Hospital das Clinicas Samuel Libânio",
                        "Instituto Sul Mineiro de Oncologia",
                        "Hospital da Santa Casa de Misericórdia de São João Del Rei",
                        "Hospital Nossa Senhora das Graças",
                        "Hospital Bom Samaritano",
                        "Hospital Dr. Hélio Angotti/Associação de Combate ao Câncer do Brasil Central",
                        "Hospital Escola da Universidade Federal do Triângulo Mineiro - Universidade Federal do Triângulo Mineiro",
                        "Hospital de Clínicas de Uberlândia/Universidade Federal de Uberlândia",
                        "Hospital e Maternidade Municipal Dr. Odelmo Leão Carneiro",
                        "Hospital Bom Pastor/Fundação Hospitalar do Município de Varginha",
                        "Hospital Universitário Maria Aparecida Pedrossian/UFMS",
                        "Hospital do Câncer Professor Dr. Alfredo Abrão/Fundação Carmem Prudente de Mato Grosso do Sul",
                        "Hospital Regional de Mato Grosso do Sul/Fundação Serviços de Saúde de Mato Grosso do Sul",
                        "Hospital da Santa Casa/Associação Beneficente de Campo Grande",
                        "Santa Casa de Misericórdia de Corumbá/Associação Beneficente de Corumbá",
                        "Hospital CASSEMS Unidade Dourados",
                        "Centro de Tratamento de Câncer de Dourados",
                        "Hospital Nossa Senhora Auxiliadora de Três Lagoas",
                        "Hospital do Câncer de Mato Grosso/Associação Matogrossense de Combate ao Câncer - AMCC",
                        "Hospital Geral Universitário/Associação de Proteção a Maternidade e a Infância Cuiabá",
                        "Hospital da Sociedade Beneficente Santa Casa de Misericórdia de Cuiabá",
                        "Santa Casa de Misericórdia e Maternidade de Rondonópolis",
                        "Hospital Santo Antonio/Fundação de Saúde Comunitária de Sinop",
                        "Hospital Ofir Loyola",
                        "Hospital Oncológico Infantil Octávio Lobo",
                        "Hospital Universitário João de Barros Barreto",
                        "Hospital Regional do Baixo Amazonas Dr. Waldemar Penna",
                        "Hospital da Fundação Assistência da Paraíba/FAP",
                        "Hospital Universitário Alcides Carneiro/Universidade Federal de Campina Grande",
                        "Hospital São Vicente de Paula/Instituto Walfredo Guedes Pereira",
                        "Hospital Napoleão Laureano",
                        "Hospital Regional Dep. Janduhy Carneiro",
                        "Hospital Memorial de Arcoverde",
                        "Hospital Regional do Agreste Dr. Waldemiro Ferrreira/Fund. Saude Amaury de Medeiros",
                        "Casa de Saúde e Maternidade Nossa Senhora do Perpétuo Socorro",
                        "Hospital Dom Tomas",
                        "Hospital da Fundação de Hematologia e Hemoterapia de Pernambuco/HEMOPE",
                        "Hospital Universitário Oswaldo Cruz",
                        "Instituto de Medicina Integral Prof. Fernanda Figueira - IMIP",
                        "Hospital de Câncer de Pernambuco/Sociedade Pernambucana do Combate ao Câncer",
                        "Hospital Barão de Lucena/Fundação de Saúde Amaury de Medeiros",
                        "Hospital das Clínicas/Universidade Federal de Pernambuco",
                        "Maternidade Dr. Marques Bastos e Hospital Infantil Dr. Mirócles Véras",
                        "Hospital São Marcos/Sociedade Piauiense Combate ao Câncer",
                        "Hospital Universitário da Universidade Federal do Piaui",
                        "ONCOCENTER",
                        "Hospital da Providência/Província Brasileira da Congregação Irmãs Filhas da Caridade de São Vicente de Paulo",
                        "Hospital Regional João de Freitas/Associação Norte Paranaense de Combate ao Câncer ao Câncer",
                        "Hospital Angelina Caron/Sociedade Hospitalar Angelina Caron",
                        "Hospital São Lucas",
                        "Hospital do Rocio",
                        "Hospital Santa Casa de Misericórdia/Associação Beneficiente Hospitalar Santa Casa de Misericórdia",
                        "CEONC",
                        "Hospital do Câncer de Cascavel UOPECCAN",
                        "Hospital de Clínicas/Universidade Federal do Paraná",
                        "Hospital Infantil Pequeno Príncipe/Associação Hospitalar de Proteção à Infância Dr. Raul Carneiro",
                        "Hospital Santa Casa/Irmandade da Santa Casa de Misericórdia de Curitiba",
                        "Hospital Erasto Gaertner/Liga Paranaense de Combate ao Câncer",
                        "Hospital São Vicente",
                        "Hospital Universitário Evangélico Mackenzie",
                        "Clinica RADION",
                        "Hospital Ministro Costa Cavalcante/Fundação de Saúde Itaiguapy",
                        "CEONC",
                        "Hospital de Caridade São Vicente de Paulo",
                        "Hospital Universitário Regional Norte do Paraná/Universidade Estadual de Londrina",
                        "Instituto de Câncer de Londrina",
                        "Hospital e Maternidade Santa Rita",
                        "Centro de Oncologia e Radioterapia Santana Ltda",
                        "Hospital do Câncer de Maringá/Instituto de Oncologia e Hematologia Maringá SC Ltda",
                        "Hospital Policlínica Pato Branco AS",
                        "Hospital da Santa Casa de Misericórdia de Ponta Grossa",
                        "Instituto Sul Paranense de Radioterapia",
                        "UOPECCAN - Filial Umuarama",
                        "Santa Casa de Misericórdia de Barra Mansa",
                        "Hospital Santa Isabel",
                        "Sociedade Portuguesa de Beneficiencia de Campos",
                        "Hospital Universitário Álvaro Alvim",
                        "Hospital Dr. Beda",
                        "Hospital São José do Avaí/Conferência São José do Avaí",
                        "Hospital Municipal Orêncio de Freitas",
                        "Hospital Universitário Antônio Pedro - HUAP/UFF",
                        "Hospital Alcides Carneiro",
                        "Centro de Terapia Oncológica",
                        "Hospital Regional Darcy Vargas",
                        "Hospital dos Servidores do Estado",
                        "Hospital Geral do Andaraí",
                        "Hospital Geral de Bonsucesso",
                        "Hospital Federal Cardoso Fontes",
                        "Hospital Geral de Ipanema",
                        "Hospital Geral da Lagoa",
                        "Hospital Mário Kroeff",
                        "Hospital Universitário Gaffrée e Guinle",
                        "Hospital Universitário Pedro Ernesto-HUPE/UERJ",
                        "Hospital Universitário Clementino Fraga Filho/UFRJ",
                        "Instituto de Puericultura e Pediatria Martagão Gesteira/UFRJ",
                        "Hospital Estadual Transplante Câncer e Cirurgia Infantil",
                        "Instituto Estadual de Hematologia Arthur Siqueira Cavalcanti",
                        "Instituto Nacional de Câncer/INCA - Hospital de Câncer I",
                        "Instituto Nacional de Câncer/INCA - Hospital de Câncer II",
                        "Instituto Nacional de Câncer/INCA - Hospital de Câncer III",
                        "Hospital São José/Associação Congregação de Santa Catarina",
                        "Hospital Universitário de Vassouras",
                        "Hospital Jardim Amália Ltda - HINJA",
                        "Hospital da LMECC",
                        "Hospital Wilson Rosado",
                        "Hospital Dr. Luiz Antônio/Liga Norteriograndense Contra o Câncer",
                        "Hospital Infantil Varela Santiago",
                        "Hospital Rio Grande",
                        "Hospital do Coração de Natal Ltda",
                        "Hospital Universitário Onofre Lopes - HUOL",
                        "Hospital Regional de Cacoal - HRC",
                        "Hospital de Base Dr. Ary Pinheiro/Hospital de Base Porto Velho",
                        "Instituto de Oncologia e Radioterapia São Pellegrino",
                        "Hospital de Amor da Amazônia",
                        "Hospital Geral de Roraima/HGR",
                        "Santa Casa de Caridade de Bagé",
                        "Hospital Tacchini",
                        "Hospital Caridade Beneficência Cachoeira do Sul",
                        "Hospital Nossa Senhora das Graças",
                        "Hospital de Caridade e Beneficência",
                        "Hospital Geral/Fundação UCS Hospital Geral de Caxias do Sul",
                        "Hospital Pompéia",
                        "Hospital São Vicente de Paulo/Associação das Damas de Caridade",
                        "Hospital da Fundação Hospitalar Santa Terezinha de Erechim",
                        "Hospital da Associação Hospital de Caridade de Ijuí",
                        "Hospital Bruno Born",
                        "Hospital São Vicente de Paulo",
                        "Hospital de Clínicas de Passo Fundo",
                        "Hospital Escola da UFPEL",
                        "Hospital da Santa Casa de Misericórdia de Pelotas",
                        "Hospital de Clínicas de Porto Alegre",
                        "Hospital São Lucas da PUCRS",
                        "Hospital Fêmina S/A",
                        "Hospital da Santa Casa de Misericórdia de Porto Alegre",
                        "Hospital Nossa Senhora da Conceição S/A",
                        "Santa Casa do Rio Grande",
                        "Hospital Ana Nery",
                        "Hospital Universitário de Santa Maria",
                        "Hospital Vida Saúde",
                        "Hospital de Caridade de Santiago",
                        "Hospital Santo Angelo",
                        "Hospital Ivan Goulart",
                        "Santa Casa de São Gabriel",
                        "Hospital Centenário",
                        "Hospital Bom Jesus",
                        "Santa Casa de Uruguaiana",
                        "Hospital Santa Isabel",
                        "Hospital Santo Antonio",
                        "Hospital Regional do Oeste",
                        "Hospital São José/Sociedade Caritativa Santo Agostinho",
                        "Centro de Pesquisas Oncológicas/CEPON",
                        "Hospital Governador Celso Ramos",
                        "Hospital Carmela Dutra",
                        "Hospital Infantil Joana de Gusmão",
                        "Hospital Univ. Professor Polydoro Ernani de São Thiago",
                        "Hospital e Maternidade Marieta Konder Bornhausen",
                        "Hospital São José/Sociedade Divina Providência",
                        "Hospital Universitário Santa Terezinha",
                        "Hospital Municipal São José",
                        "Hospital Materno Infantil Dr. Jesser Amarante Faria",
                        "Hospital e Maternidade Tereza Ramos",
                        "Hospital de Caridade São Braz de Porto União",
                        "Hospital Regional Alto Vale",
                        "Hospital e Maternidade Sagrada Família",
                        "Hospital Nossa Senhora da Conceição/Sociedade Divina Providência",
                        "Hospital de Cirurgia/Fundação de Beneficência Hospital de Cirurgia",
                        "Hospital Governador João Alves Filho",
                        "Hospital Universitário De Sergipe Huse",
                        "Hospital Sagrado Coração de Jesus/Santa Casa de Misericórdia de Araçatuba",
                        "Santa Casa de Araraquara",
                        "Hospital São Luiz de Araras",
                        "Hospital Regional de Assis",
                        "Santa Casa de Avaré",
                        "Fundação Pio XII Barretos",
                        "Hospital Estadual de Bauru",
                        "Hospital das Clínicas da Faculdade de Medicina de Botucatu",
                        "Hospital Universitário São Francisco na Prov. De Deus",
                        "Boldrini Campinas",
                        "Hospital e Maternidade Celso Pierro",
                        "Hospital das Clínicas da UNICAMP",
                        "Hospital Municipal Dr. Mário Gatti",
                        "Hospital Padre Albino Catanduva",
                        "Hospital Estadual de Diadema – Hospital Serraria",
                        "Santa Casa de Franca",
                        "Santa Casa de Misericórdia de Guaratinguetá",
                        "Hospital Santo Amaro",
                        "Santa Casa de Misericórdia de Itapeva",
                        "Hospital São Francisco de Assis",
                        "Hospital de Amor Jales",
                        "Hospital Amaral Carvalho",
                        "HCSVP Hospital São Vicente",
                        "GRENDACC",
                        "Santa Casa de Limeira",
                        "Hospital das Clínicas HCFAMEMA",
                        "Santa Casa de Marília",
                        "Hospital das Clínicas Luzia de Pinho Melo",
                        "Hospital Municipal Dr. Tabajara Ramos",
                        "Santa Casa de Ourinhos",
                        "Hospital Regional Dr. Leopoldo Bevilacqua",
                        "Hospital Fornecedores de Cana de Piracicaba",
                        "Santa Casa de Piracicaba",
                        "Fundação Hospital Regional do Câncer",
                        "Hospital Domingos Leonardo Cerávolo Presidente Prudente",
                        "Instituto de Radioterapia de Presidente Prudente",
                        "Hospital das Clínicas FAEPA",
                        "Hospital Imaculada Conceição",
                        "Santa Casa de Ribeirão Preto",
                        "CTR",
                        "Santa Casa de Rio Claro",
                        "Hospital Estadual Mário Covas de Santo André",
                        "Centro Hospitalar de Santo André Dr. Newton da Costa Brandão",
                        "Instituto de Radioterapia do ABC",
                        "Santa Casa de Santos",
                        "Hospital Santo Antônio Santos",
                        "Hospital Guilherme Álvaro",
                        "Hospital Anchieta",
                        "Hospital Municipal Universitário",
                        "Complexo Hospitalar Municipal",
                        "Santa Casa de São Carlos",
                        "Hospital da Santa Casa de Misericórdia Dona Carolina Malheiros",
                        "Santa Casa de Misericórdia de São José do Rio Preto",
                        "Hospital de Base de São José do Rio Preto",
                        "Hospital Pio XII",
                        "Santa Casa de Misericórdia",
                        "Centro de Tratamento Fabiana Macedo de Morais",
                        "Centro de Referência da Saúde da Mulher",
                        "Hospital Geral de Vila Nova Cachoeirinha",
                        "Conjunto Hospitalar do Mandaqui",
                        "Hosp de Transp. do Estado de SP Eurycles de Jesus Zerbini",
                        "HC da FMUSP Hopsital das Clínicas São Paulo",
                        "A C Camargo Cancer Center",
                        "Hospital Infantil Darcy Vargas UGA III São Paulo",
                        "Hospital Heliópolis",
                        "Hospital Ipiranga/Unidade de Gestão Assistencial II",
                        "Santa Casa de São Paulo Hospital Central São Paulo",
                        "Hospital BP/Real e Benemérita Associação Portuguesa de Beneficencia",
                        "Hospital Santa Marcelina São Paulo",
                        "Hospital São Paulo Hospital de Ensino da UNIFESP",
                        "IBCC",
                        "Hospital GRAACC Instituto de Oncologia Pediátrica IOP",
                        "Instituto do Câncer Arnaldo Vieira de Carvalho",
                        "Instituto do Câncer do Estado de São Paulo/SES",
                        "Hospital Geral de Vila Penteado Dr Jose Pangella São Paulo",
                        "Conjunto Hospitalar de Sorocaba",
                        "Santa Casa de Itu",
                        "Santa Casa de Sorocaba",
                        "Hospital GPACI",
                        "Hospital Geral de Pirajussara",
                        "Hospital Regional do Vale do Paraíba/Sociedade Beneficente São Camilo",
                        "Hospital Municipal Universitário de Taubaté",
                        "Santa Casa",
                        "Hospital de Regional de Araguaína",
                        "Hospital Geral de Palmas",
                        "Clínica Irradiar",
                        "Hospital Português/Real Sociedade Portuguesa de Beneficiência",
                        "Instituto de Radium e Supervoltagem Ivo Roesler/IRSIR",
                        "Instituto de Radioterapia Waldemir Miranda LTDA/IRWAM",
                        "Clínica de Radioterapia Ingá",
                        "Instituto Oncológico LTDA",
                        "Irmandade do Senhor Jesus dos Passos e Hospital de Caridade",
                        "Instituto de Radioterapia Vale do Paraíba/CENON - Centro de Oncologia Radioterápica do Vale do Paraíba")
  )

  # Verifica se a coluna CNES_Hospital existe antes de tentar acessá-la
  if ("CNES_Hospital" %in% names(dados)) {
    message("Mapeando os códigos CNES para nomes completos dos estabelecimentos.")
    # Mapeamento dos códigos CNES para nomes usando um vetor nomeado
    map <- setNames(cnes_names$ESTABELECIMENTO, cnes_names$CNES)
    dados$Estabelecimento_Hospitalar <- map[dados$CNES_Hospital]
    message("Nomes completos dos estabelecimentos adicionados.")
  } else {
    stop("\033[1;31mA coluna 'CNES_Hospital' não foi encontrada no dataframe. Função interrompida.\033[0m")
  }

  message(paste("\033[1;32m", "> Ajuste dos códigos CNES concluído com sucesso. Foi adicionada uma coluna no final do dataframe, chamada Estabelecimento_Hospitalar.", "\033[0m"))

  return(dados)
}









#' Renomear Tipo Histológico
#'
#' Esta função mapeia os códigos de Tipo Histológico para os nomes completos na coluna `Tipo_Histologico` de um dataframe. Os nomes completos dos tipos histológicos são adicionados em uma nova coluna `Tipo_Histologico_Completo`.
#'
#' @param dados Um dataframe contendo a coluna `Tipo_Histologico`.
#' @return Retorna um dataframe com os nomes completos dos tipos histológicos adicionados em uma nova coluna.
#' @export
#' @name renomear_tipo_histologico
#' @examples
#' # Escreva o nome do dataframe e execute a função. Se seu dataframe for "dados_RHC_combinados", use a função como:
#'
#' dados_RHC_combinados <- renomear_tipo_histologico(dados_RHC_combinados)
renomear_tipo_histologico <- function(dados) {
  message("Iniciando o ajuste dos códigos de Tipo Histológico para nomes completos.")

  # Definição do dataframe de mapeamento de Tipo Histológico para nomes completos
  tipo_histologico_names <- data.frame(
    Codigo_tipo_hist = c(
      "8000/0", "8000/1", "8000/3", "8000/6", "8001/0", "8001/1", "8001/3", "8002/3", "8003/3", "8004/3",
      "8010/0", "8010/2", "8010/3", "8010/6", "8011/0", "8011/3", "8012/3", "8020/3", "8021/3", "8022/3",
      "8030/3", "8031/3", "8032/3", "8033/3", "8034/3", "8040/1", "8041/3", "8042/3", "8043/3", "8044/3",
      "8045/3", "8050/0", "8050/2", "8050/3", "8051/0", "8051/3", "8052/0", "8052/3", "8053/0", "8060/0",
      "8070/2", "8070/3", "8070/6", "8071/3", "8072/3", "8073/3", "8074/3", "8075/3", "8076/2", "8076/3",

      "8077/2", "8080/2", "8081/2", "8082/3", "8090/1", "8090/3", "8091/3", "8092/3", "8093/3", "8094/3",
      "8095/3", "8096/0", "8100/0", "8101/0", "8102/0", "8110/0", "8110/3", "8120/0", "8120/1", "8120/2",
      "8120/3", "8121/0", "8121/1", "8121/3", "8122/3", "8123/3", "8124/3", "8130/3", "8140/0", "8140/1",
      "8140/2", "8140/3", "8140/6", "8141/3", "8142/3", "8143/3", "8144/3", "8145/3", "8146/0", "8147/0",
      "8147/3", "8150/0", "8150/3", "8151/0", "8151/3", "8152/0", "8152/3", "8153/1", "8153/3", "8154/3",

      "8155/3", "8160/0", "8160/3", "8161/0", "8161/3", "8162/3", "8170/0", "8170/3", "8171/3", "8180/3",
      "8190/0", "8190/3", "8191/0", "8200/0", "8200/3", "8201/3", "8202/0", "8210/0", "8210/2", "8210/3",
      "8211/0", "8211/3", "8220/0", "8220/3", "8221/0", "8221/3", "8230/3", "8231/3", "8240/1", "8240/3",
      "8241/1", "8241/3", "8243/3", "8244/3", "8245/3", "8246/3", "8247/3", "8248/1", "8250/1", "8250/3",
      "8251/0", "8251/3", "8260/0", "8260/3", "8261/1", "8261/2", "8261/3", "8262/3", "8263/0", "8263/2",

      "8263/3", "8270/0", "8270/3", "8271/0", "8280/0", "8280/3", "8281/0", "8281/3", "8290/0", "8290/3",
      "8300/0", "8300/3", "8310/0", "8310/3", "8311/1", "8312/3", "8313/0", "8314/3", "8315/3", "8320/3",
      "8321/0", "8322/0", "8322/3", "8323/0", "8323/3", "8324/0", "8330/0", "8330/3", "8331/3", "8332/3",
      "8333/0", "8334/0", "8340/3", "8350/3", "8360/1", "8361/1", "8370/0", "8370/3", "8371/0", "8372/0",
      "8373/0", "8374/0", "8375/0", "8380/0", "8380/1", "8380/3", "8381/0", "8381/1", "8381/3", "8390/0",

      "8390/3", "8400/0", "8400/1", "8400/3", "8401/0", "8401/3", "8402/0", "8403/0", "8404/0", "8405/0",
      "8406/0", "8407/0", "8408/0", "8410/0", "8410/3", "8420/0", "8420/3", "8430/1", "8430/3", "8440/0",
      "8440/3", "8441/0", "8441/3", "8442/3", "8450/0", "8450/3", "8451/3", "8452/1", "8460/0", "8460/3",
      "8461/0", "8461/3", "8462/3", "8470/0", "8470/3", "8471/0", "8471/3", "8472/3", "8473/3", "8480/0",
      "8480/3", "8480/6", "8481/3", "8490/3", "8490/6", "8500/2", "8500/3", "8501/2", "8501/3", "8502/3",

      "8503/0", "8503/2", "8503/3", "8504/0", "8504/2", "8504/3", "8505/0", "8506/0", "8510/3", "8511/3",
      "8512/3", "8520/2", "8520/3", "8521/3", "8522/2", "8522/3", "8530/3", "8540/3", "8541/3", "8542/3",
      "8543/3", "8550/0", "8550/1", "8551/0", "8560/3", "8561/0", "8562/3", "8570/3", "8571/3", "8572/3",
      "8573/3", "8580/0", "8580/3", "8590/1", "8600/0", "8600/3", "8601/3", "8602/0", "8610/0", "8620/1",
      "8620/3", "8621/1", "8622/1", "8623/1", "8630/0", "8630/1", "8630/3", "8631/0", "8632/1", "8640/0",

      "8640/3", "8641/0", "8650/0", "8650/1", "8650/3", "8660/0", "8670/0", "8671/0", "8680/1", "8680/3",
      "8681/1", "8682/1", "8683/0", "8690/1", "8691/1", "8692/1", "8693/1", "8693/3", "8700/0", "8700/3",
      "8710/3", "8711/0", "8712/3", "8713/0", "8720/0", "8720/2", "8720/3", "8721/3", "8722/0", "8722/3",
      "8723/0", "8723/3", "8724/0", "8725/0", "8726/0", "8727/0", "8730/0", "8730/3", "8740/0", "8740/3",
      "8741/2", "8741/3", "8742/2", "8742/3", "8743/3", "8744/3", "8745/3", "8750/0", "8760/0", "8761/1",

      "8761/3", "8770/0", "8770/3", "8771/0", "8771/3", "8772/0", "8772/3", "8773/3", "8774/3", "8780/0",
      "8780/3", "8790/0", "8800/0", "8800/3", "8800/6", "8801/3", "8802/3", "8803/3", "8804/3", "8810/0",
      "8810/3", "8811/0", "8811/3", "8812/0", "8812/3", "8813/0", "8813/3", "8814/3", "8820/0", "8821/1",
      "8822/1", "8823/1", "8824/1", "8830/0", "8830/1", "8830/3", "8832/0", "8832/3", "8833/3", "8840/0",
      "8840/3", "8841/1", "8850/0", "8850/3", "8851/0", "8851/3", "8852/0", "8852/3", "8853/3", "8854/0",

      "8854/3", "8855/3", "8856/0", "8857/0", "8858/3", "8860/0", "8861/0", "8870/0", "8880/0", "8881/0",
      "8890/0", "8890/1", "8890/3", "8891/0", "8892/0", "8893/0", "8894/0", "8894/3", "8895/0", "8895/3",
      "8896/3", "8897/1", "8900/0", "8900/3", "8901/3", "8902/3", "8903/0", "8904/0", "8910/3", "8920/3",
      "8930/0", "8930/3", "8931/1", "8932/0", "8933/3", "8940/0", "8940/3", "8941/3", "8950/3", "8951/3",
      "8960/1", "8960/3", "8963/3", "8964/3", "8970/3", "8971/3", "8972/3", "8980/3", "8981/3", "8982/0",

      "8990/0", "8990/1", "8990/3", "8991/3", "9000/0", "9000/1", "9000/3", "9010/0", "9011/0", "9012/0",
      "9013/0", "9014/0", "9015/0", "9016/0", "9020/0", "9020/1", "9020/3", "9030/3", "9040/0", "9040/3",
      "9041/3", "9042/3", "9043/3", "9044/3", "9050/0", "9050/3", "9051/0", "9051/3", "9052/0", "9052/3",
      "9053/0", "9053/3", "9054/0", "9055/1", "9060/3", "9061/3", "9062/3", "9063/3", "9064/3", "9070/3",
      "9071/3", "9072/3", "9073/1", "9080/0", "9080/1", "9080/3", "9081/3", "9082/3", "9083/3", "9084/0",

      "9084/3", "9085/3", "9090/0", "9090/3", "9091/1", "9100/0", "9100/1", "9100/3", "9101/3", "9102/3",
      "9103/0", "9104/1", "9110/0", "9110/1", "9110/3", "9120/0", "9120/3", "9121/0", "9122/0", "9123/0",
      "9124/3", "9125/0", "9126/0", "9130/0", "9130/1", "9130/3", "9131/0", "9132/0", "9133/1", "9133/3",
      "9134/1", "9140/3", "9141/0", "9142/0", "9150/0", "9150/1", "9150/3", "9160/0", "9161/1", "9170/0",
      "9170/3", "9171/0", "9172/0", "9173/0", "9174/0", "9174/1", "9175/0", "9180/0", "9180/3", "9181/3",

      "9182/3", "9183/3", "9184/3", "9185/3", "9190/3", "9191/0", "9200/0", "9200/1", "9210/0", "9210/1",
      "9220/0", "9220/1", "9220/3", "9221/0", "9221/3", "9230/0", "9230/3", "9231/3", "9240/3", "9241/0",
      "9250/1", "9250/3", "9251/1", "9251/3", "9260/3", "9261/3", "9262/0", "9270/0", "9270/1", "9270/3",
      "9271/0", "9272/0", "9273/0", "9274/0", "9275/0", "9280/0", "9281/0", "9282/0", "9290/0", "9290/3",
      "9300/0", "9301/0", "9302/0", "9310/0", "9310/3", "9311/0", "9312/0", "9320/0", "9321/0", "9322/0",

      "9330/0", "9330/3", "9340/3", "9350/1", "9360/1", "9361/1", "9362/3", "9363/0", "9364/3", "9370/0",
      "9380/3", "9381/3", "9382/3", "9383/1", "9384/1", "9390/0", "9390/3", "9391/3", "9392/3", "9393/1",
      "9394/1", "9400/3", "9401/3", "9410/3", "9411/3", "9420/3", "9421/3", "9422/3", "9423/3", "9424/3",
      "9430/0", "9440/3", "9441/3", "9442/3", "9443/3", "9450/3", "9451/3", "9460/3", "9470/3", "9471/3",
      "9472/3", "9473/3", "9480/3", "9481/3", "9490/0", "9490/3", "9491/3", "9500/3", "9501/3", "9502/3",

      "9503/3", "9504/3", "9505/1", "9506/0", "9507/0", "9510/3", "9511/3", "9512/3", "9520/3", "9521/3",
      "9522/3", "9523/3", "9530/0", "9530/1", "9530/3", "9531/0", "9532/0", "9533/0", "9534/0", "9535/0",
      "9536/0", "9537/0", "9538/1", "9539/3", "9540/0", "9540/1", "9540/3", "9541/0", "9550/0", "9560/0",
      "9560/1", "9560/3", "9561/3", "9562/0", "9570/0", "9580/0", "9580/3", "9581/3", "9590/3", "9591/3",
      "9592/3", "9593/3", "9594/3", "9595/3", "9650/3", "9652/3", "9653/3", "9654/3", "9655/3", "9657/3",

      "9658/3", "9659/3", "9660/3", "9661/3", "9662/3", "9663/3", "9664/3", "9665/3", "9666/3", "9667/3",
      "9670/3", "9671/3", "9672/3", "9673/3", "9674/3", "9675/3", "9676/3", "9677/3", "9680/3", "9681/3",
      "9682/3", "9683/3", "9684/3", "9685/3", "9686/3", "9687/3", "9690/3", "9691/3", "9692/3", "9693/3",
      "9694/3", "9695/3", "9696/3", "9697/3", "9698/3", "9700/3", "9701/3", "9702/3", "9703/3", "9704/3",
      "9705/3", "9706/3", "9707/3", "9709/3", "9711/3", "9712/3", "9713/3", "9714/3", "9720/3", "9722/3",

      "9723/3", "9731/3", "9732/3", "9740/1", "9740/3", "9741/3", "9760/3", "9761/3", "9762/3", "9763/3",
      "9764/3", "9765/1", "9766/1", "9767/1", "9768/1", "9800/3", "9801/3", "9802/3", "9803/3", "9804/3",
      "9820/3", "9821/3", "9822/3", "9823/3", "9824/3", "9825/3", "9826/3", "9827/3", "9830/3", "9840/3",
      "9841/3", "9842/3", "9850/3", "9860/3", "9861/3", "9862/3", "9863/3", "9864/3", "9866/3", "9867/3",
      "9868/3", "9870/3", "9880/3", "9890/3", "9891/3", "9892/3", "9893/3", "9894/3", "9900/3", "9910/3",

      "9930/3", "9931/3", "9932/3", "9940/3", "9941/3", "9950/1", "9960/1", "9961/3", "9962/1", "9970/1",
      "9980/1", "9981/1", "9982/1", "9983/1", "9984/1", "9989/1",
      "8000/9",	"8005/3",	"8010/9",	"8013/3",	"8014/3",	"8015/3",	"8035/3",	"8046/3",	"8052/2",	"8078/3",	"8083/3",	"8084/3",	"8097/3",	"8098/3",	"8102/3",	"8130/1",	"8130/2",	"8131/3",	"8148/2",	"8150/1",	"8156/1",	"8172/3",	"8173/3",	"8174/3",	"8175/3",	"8201/2",	"8214/3",	"8215/3",	"8230/2",	"8242/1",	"8242/3",	"8245/1",	"8249/3",	"8252/3",	"8253/3",	"8254/3",	"8255/3",	"8272/3",	"8313/3",	"8316/3",	"8317/3",	"8318/3",	"8319/3",	"8330/1",	"8333/3",	"8335/3",	"8337/3",	"8341/3",	"8342/3",	"8343/3",	"8344/3",	"8345/3",	"8346/3",	"8347/3",	"8382/3",	"8383/3",	"8384/3",	"8402/3",	"8403/3",	"8407/3",	"8408/3",	"8409/3",	"8413/3",	"8442/1",	"8444/1",	"8451/1",	"8452/3",	"8453/1",	"8453/2",	"8453/3",	"8462/1",	"8463/1",	"8470/2",	"8472/1",	"8473/1",	"8482/3",	"8507/2",	"8508/3",	"8513/3",	"8514/3",	"8523/3",	"8524/3",	"8525/3",	"8550/3",	"8551/3",	"8574/3",	"8575/3",	"8576/3",	"8580/1",	"8581/1",	"8581/3",	"8582/1",	"8582/3",	"8583/1",	"8583/3",	"8584/1",	"8584/3",	"8585/1",	"8585/3",	"8586/3",	"8588/3",	"8589/3",	"8591/1",	"8592/1",	"8593/1",	"8631/1",	"8631/3",	"8633/1",	"8634/3",	"8640/1",	"8642/1",	"8670/3",	"8711/3",	"8728/3",	"8744/2",	"8746/3",
      "8800/9",	"8805/3",	"8806/3",	"8810/1",	"8815/3",	"8825/1",	"8825/3",	"8835/1",	"8836/1",	"8850/1",	"8888/8",	"8891/3",	"8898/1",	"8912/3",	"8921/3",	"8931/3",	"8934/3",	"8935/1",	"8935/3",	"8936/1",	"8936/3",	"8959/1",	"8959/3",	"8973/3",	"8974/1",	"8982/3",	"9014/1",	"9014/3",	"9015/1",	"9015/3",	"9064/2",	"9065/3",	"9105/3",	"9186/3",	"9187/3",	"9192/3",	"9193/3",	"9194/3",	"9242/3",	"9243/3",	"9252/3",	"9351/1",	"9352/1",	"9365/3",	"9370/3",	"9371/3",	"9372/3",	"9390/1",	"9393/3",	"9412/1",	"9413/0",	"9421/1",	"9430/3",	"9442/1",	"9444/1",	"9474/3",	"9505/3",	"9506/1",	"9508/3",	"9513/3",	"9538/3",	"9539/1",	"9571/3",	"9596/3",	"9651/3",	"9678/3",	"9679/3",	"9689/3",	"9699/3",	"9708/3",	"9716/3",	"9717/3",	"9718/3",	"9719/3",	"9727/3",	"9728/3",	"9729/3",	"9733/3",	"9734/3",	"9742/3",	"9750/3",	"9751/1",	"9751/3",	"9752/1",	"9753/1",	"9754/3",	"9755/3",	"9756/3",	"9757/3",	"9758/3",	"9769/1",	"9805/3",	"9831/1",	"9832/3",	"9833/3",	"9834/3",	"9835/3",	"9836/3",	"9837/3",	"9871/3",	"9872/3",	"9873/3",	"9874/3",	"9875/3",	"9876/3",	"9895/3",	"9896/3",	"9897/3",	"9920/3",	"9945/3",	"9946/3",	"9948/3",	"9950/3",	"9960/3",	"9962/3",	"9963/3",
      "9964/3",	"9975/1",	"9980/3",	"9982/3",	"9983/3",	"9984/3",	"9985/3",	"9986/3",	"9987/3",	"9989/3",	"9990/3",
      "8000/0",	"8005/0",	"8152/1",	"8156/3",	"8272/0",	"8313/1",	"8408/1",	"8634/1",	"8728/1",	"8762/1",	"8825/0",	"8827/1",	"8834/1",	"8857/3",	"8936/0",	"9135/1",	"9136/1",	"9195/3",	"9342/3",	"9491/0",	"9571/0",	"9715/3",	"9961/1",	"9990/1",	"9990/9"


    ),
    Tipo_Histologico_Completo = c(
      "Neoplasia benigna", "Neoplasia de comportamento incerto se benigno ou maligno", "Neoplasia maligna", "Neoplasia metastática",
      "Células tumorais, benignas", "Células tumorais, incerto se benignas ou malignas", "Células tumorais malignas", "Tumor maligno, tipo de células pequenas",
      "Tumor maligno, tipo de células gigantes", "Tumor maligno, tipo de células fusiformes", "Tumor epitelial benigno", "Carcinoma in situ SOE",
      "Carcinoma SOE", "Carcinoma metastático SOE", "Epitelioma benigno", "Epitelioma maligno", "Carcinomas de células grandes SOE", "Carcinoma indiferenciado SOE",
      "Carcinoma anaplásico SOE", "Carcinoma pleomórfico", "Carcinoma de células gigantes e de células fusiformes", "Carcinoma de células gigantes",
      "Carcinoma de células fusiformes", "Carcinoma pseudossarcomatoso", "Carcinoma de células poligonais", "Tumorlet", "Carcinoma de células pequenas SOE",
      "Carcinoma 'oat cell'", "Carcinoma de células pequenas, fusiformes", "Carcinoma de células pequenas, intermediárias", "Carcinoma de células pequenas e de células grandes",
      "Papiloma SOE (exceto Papiloma de bexiga M8120/1)", "Carcinoma papilar in situ", "Carcinoma papilar SOE", "Papiloma verrucoso", "Carcinoma verrucoso SOE",
      "Papiloma de células escamosas", "Carcinoma papilar de células escamosas", "Papiloma invertido", "Papilomatose SOE", "Carcinoma in situ de células escamosas, SOE",
      "Carcinoma de células escamosas SOE", "Carcinoma de células escamosas, metastático, SOE", "Carcinoma de células escamosas, queratinizado, SOE",
      "Carcinoma de células escamosas, de células grandes, não queratinizado", "Carcinoma de células escamosas, de células pequenas, não queratinizado",
      "Carcinoma de células escamosas, de células fusiformes", "Carcinoma de células escamosas adenóides", "Carcinoma in situ de células escamosas com invasão questionável do estroma",
      "Carcinoma de células escamosas, microinvasivo", "Neoplasia intra-epitelial, grau III, de colo uterino, vulva e vagina", "Eritroplasia de Queyrat",

      "Doença de Bowen", "Carcinoma linfoepitelial", "Tumor de células basais", "Carcinoma de células basais SOE", "Carcinoma de células basais, multicêntrico",
      "Carcinoma de células basais, tipo morféia", "Carcinoma de células basais, fibroepitelial", "Carcinoma basoescamoso", "Carcinoma metatípico",
      "Epitelioma intra-epidérmico de Jadassohn", "Tricoepitelioma", "Tricofoliculoma", "Tricolemoma", "Pilomatrixoma SOE", "Carcinoma da pilomátrix",
      "Papiloma de células de transição, SOE", "Papiloma urotelial", "Carcinoma in situ de células transicionais", "Carcinoma de células transicionais SOE",
      "Papiloma schneideriano", "Papiloma de células transicionais, tipo invertido", "Carcinoma schneideriano", "Carcinoma de células transicionais, tipo células fusiformas",
      "Carcinoma basalóide", "Carcinoma cloacogênico", "Carcinoma papilar de células transicionais", "Adenoma SOE", "Adenoma brônquico SOE", "Adenocarcinoma in situ SOE",
      "Adenocarcinoma SOE", "Adenocarcinoma metastático SOE", "Adenocarcinoma esquirroso", "Linite plástica", "Adenocarcinoma de propagação superficial",
      "Adenocarcionama, tipo intestinal", "Carcinoma, tipo difuso", "Adenoma monomórfico", "Adenoma de células basais", "Adenocarcinoma de células basais",
      "Adenoma de células das ilhotas", "Carcinoma de células das ilhotas", "Insulinoma SOE", "Insulinoma maligno", "Glucagonoma SOE", "Glucagonoma maligno",
      "Gastrinoma SOE", "Gastrinoma maligno", "Adenocarcinoma misto, das ilhotas e exócrino", "Vipoma",

      "Adenoma de duto biliar", "Colangiocarcinoma", "Cistadenoma de dutos biliares", "Cistadenocarcinoma de dutos biliares", "Tumor de Klatskin",
      "Adenoma de células hepáticas", "Carcinoma hepatocelular SOE", "Carcinoma hepatocelular fibrolamelar", "Carcinoma hepatocelular e colangiocarcinoma combinados",
      "Adenoma trabecular", "Adenocarcinoma trabecular", "Adenoma embrionário", "Cilindroma dérmico écrino", "Carcinoma cístico adenóide", "Carcinoma cribriforme",
      "Adenoma microcístico", "Pólipo adenomatoso SOE", "Adenocarcinoma in situ em pólipo adenomatoso", "Adenocarcinoma em pólipo adenomatoso",
      "Adenoma tubular SOE", "Adenocarcinoma tubular", "Polipose adenomatosa do cólon", "Adenocarcinoma em polipose adenomatosa do cólon", "Pólipos adenomatosos múltiplos",
      "Adenocarcinoma em pólipos adenomatosos múltiplos", "Carcinoma sólido SOE", "Carcinoma simples", "Tumor carcinóide SOE da apêndice",
      "Tumor carcinóide SOE (exceto do apêndice M8240/1)", "Tumor carcinóide argentafínico SOE", "Tumor carcinóide argentafínico maligno",
      "Carcinoma de células caliciformes", "Carcinóide composto", "Tumor adenocarcinóide", "Carcinoma neuroendócrino", "Carcinoma de células de Merkel", "Apudoma",
      "Adenomatose pulmonar", "Adenocarcinoma bronquíolo-alveolar", "Adenoma alveolar", "Adenocarcinoma alveolar", "Adenoma papilar SOE", "Adenocarcinoma papilar SOE",
      "Adenoma viloso SOE", "Adenocarcinoma in situ em adenoma viloso", "Adenocarcinoma em adenoma viloso", "Adenocarcinoma viloso", "Adenoma tubuloviloso SOE",
      "Adenocarcinoma in situ em adenoma tubuloviloso", "Adenocarcinoma em adenoma tubuloviloso", "Adenoma cromófobo", "Carcinoma cromófobo",

      "Prolactinoma", "Adenoma acidófilo", "Carcinoma acidófilo", "Adenoma misto acidófilo-basófilo", "Carcinoma misto acidófilo-basófilo", "Adenoma oxifílico",
      "Adenocarcinoma oxifílico", "Adenoma basófilo", "Carcinoma basófilo", "Adenoma de células claras", "Adenocarcinoma de células claras SOE", "Tumor hipernefróide",
      "Carcinoma de células renais", "Adenofibroma de células claras", "Carcinoma rico em lípides", "Carcinoma rico em glicogênio", "Carcinoma de células granulares",
      "Adenoma de células principais", "Adenoma de células claras (water-clear cell)", "Adenocarcinoma de células claras (water-clear cell)", "Adenoma de células mistas",
      "Adenocarcinoma de células mistas", "Lipoadenoma", "Adenoma folicular", "Adenocarcinoma folicular SOE", "Adenocarcinoma folicular bem diferenciado",
      "Adenocarcinoma folicular trabecular", "Adenoma microfolicular", "Adenoma macrofolicular", "Carcinoma papilar, variante folicular", "Carcinoma esclerosante não-encapsulado",
      "Adenomas endócrinos múltiplos", "Tumor justaglomerular", "Adenoma de córtex supra-renal SOE", "Carcinoma de córtex supra-renal", "Adenoma de córtex supra-renal de células compactas",
      "Adenoma de córtex supra-renal, variante densamente pigmentada", "Adenoma de córtex supra-renal, de células claras", "Adenoma de córtex supra-renal, de células glomerulosas",
      "Adenoma de córtex supra-renal, de células mistas", "Adenoma endometrióide SOE", "Adenoma endometrióide 'borderline'", "Carcinoma endometrióide",
      "Adenofibroma endometrióide SOE", "Adenofibroma endometrióide 'borderline'", "Adenofibroma endometróide maligno", "Adenoma de apêndice cutâneo", "Carcinoma de apêndice cutâneo",
      "Adenoma de glândula sudorípara", "Tumor de glândula sudorípara SOE",

      "Adenocarcinoma de glândula sudorípara", "Adenoma apócrino", "Adenocarcinoma apócrino", "Acrospiroma écrino", "Espiradenoma écrino", "Hidrocistoma",
      "Hidradenoma papilar", "Siringadenoma papilar", "Siringoma SOE", "Adenoma papilar écrino", "Adenoma sebáceo", "Adenocarcinoma sebáceo", "Adenoma ceruminoso",
      "Adenocarcinoma ceruminoso", "Tumor mucoepidermóide", "Carcinoma mucoepidermóide", "Cistadenoma SOE", "Cistadenocarcinoma SOE", "Cistadenoma seroso SOE",
      "Cistadenocarcinoma seroso SOE", "Cistadenoma seroso 'borderline'", "Cistadenoma papilar SOE", "Cistadenocarcinoma papilar SOE", "Cistadenoma papilar 'borderline'",
      "Tumor cístico papilar", "Cistadenoma seroso papilar SOE", "Cistadenocarcinoma seroso papilar", "Papiloma seroso superficial", "Carcinoma papilar seroso superficial",
      "Cistadenoma seroso papilar 'borderline'", "Cistadenoma mucinoso SOE", "Cistadenocarcinoma mucinoso", "Cistadenoma mucinoso papilar SOE", "Cistadenocarcinoma mucinoso papilar",
      "Cistadenoma mucinoso 'borderline'", "Cistadenoma mucinoso papilar 'borderline'", "Adenoma mucinoso", "Adenocarcinoma mucinoso", "Pseudomixoma de peritônio",
      "Adenocarcinoma produtor de mucina", "Carcinoma de células em anel de sinete", "Carcinoma metastático de células em anel de sinete", "Carcinoma intraductal não infiltrante SOE",
      "Carcinoma de dutos infiltrante", "Comedocarcinoma não infiltrante", "Comedocarcinoma SOE", "Carcinoma juvenil da mama", "Papiloma intraductal",
      "Adenocarcinoma papilar intraductal não infiltrante", "Adenocarcinoma papilar intraductal com invasão", "Adenoma papilar intracístico", "Carcinoma intracístico não infiltrante",

      "Carcinoma intracístico SOE", "Papilomatose intraductal SOE", "Adenoma do mamilo", "Carcinoma medular SOE", "Carcinoma medular com estroma amilóide",
      "Carcinoma medular com estroma linfóide", "Carcinoma lobular in situ", "Carcinoma lobular SOE", "Carcinoma ductular infiltrante", "Carcinoma intraductel e carcinoma lobular in situ",
      "Carcinoma infiltrante de dutos e lobular", "Carcinoma inflamatório", "Doença mamária de Paget", "Doença de Paget e carcinoma de dutos infiltrante da mama",
      "Doença de Paget extramamária (exceto doença de Paget do osso)", "Doença de Paget e carcinoma intraductal da mama", "Adenoma de células acinares",
      "Tumor de células acinares", "Carcinoma de células acinares", "Carcinoma adenoescamoso", "Adenolinfoma", "Carcinoma epitelial-mioepitelial",
      "Adenocarcinoma com metaplasia escamosa", "Adenocarcinoma com metaplasia cartilaginosa e óssea", "Adenocarcinoma com metaplasia de células fusiformes",
      "Adenocarcinoma com metaplasia apócrina", "Timoma benigno", "Timoma maligno", "Tumor dos cordões sexuais e estroma", "Tecoma SOE", "Tecoma maligno",
      "Tecoma luteinizado", "Tumor esclerosante do estroma", "Luteoma SOE", "Tumor de células da granulosa SOE", "Tumor maligno de células da granulosa",
      "Tumor de células da granulosa e células da teca", "Tumor juvenil de células da granulosa", "Tumor dos cordões sexuais com túbulos anulares",
      "Androblastoma benigno", "Androblastoma SOE", "Androblastoma maligno", "Tumor de células de Sertoli-Leydig", "Ginandroblastoma", "Tumor de células de Sertoli SOE",

      "Carcinoma de células de Sertoli", "Tumor de células de Sertoli com depósito de lípides", "Tumor benigno de células de Leydig", "Tumor de células de Leydig SOE",
      "Tumor maligno de células de Leydig", "Tumor de células do hilo", "Tumor de células lipídicas do ovário", "Tumor de supra-renal acessório [resto adrenal]",
      "Paraganglioma SOE", "Paraganglioma maligno", "Paraganglioma simpático", "Paraganglioma parassimpático", "Paraganglioma gangliocítico", "Tumor do glomo jugular",
      "Tumor do corpo aórtico", "Tumor do corpo carotídeo", "Paraganglioma extra-supra-renal SOE", "Paraganglioma extra-supra-renal maligno", "Feocromocitoma SOE",
      "Feocromocitoma maligno", "Glomangiossarcoma", "Tumor de glomo", "Glomangioma", "Glomangiomioma", "Nevo pigmentado SOE", "Melanoma in situ",
      "Melanoma maligno SOE", "Melanoma nodular", "Nevo de células baloniformes", "Melanoma de células baloniformes", "Halo nevo", "Melanoma maligno em regressão",
      "Pápula fibrosa do nariz", "Neuronevo", "Nevo magnocelular", "Nevo displásico", "Nevo não pigmentado", "Melanoma amelanótico", "Nevo juncional SOE",
      "Melanoma maligno em nevo juncional", "Melanose pré-cancerosa SOE", "Melanoma maligno em melanose pré-cancerosa", "Sarda melanótica de Hutchinson SOE",
      "Melanoma maligno em sarda melanótica de Hutchinson", "Melanoma de propagação superficial", "Melanoma lentiginoso maligno das extremidades periféricas",
      "Melanoma desmoplástico maligno", "Nevo intradérmico", "Nevo composto", "Nevo pigmentado gigante SOE", "Melanoma maligno em nevo pigmentado gigante",
      "Nevo epitelióide e de células fusiformes", "Melanoma misto epitelióide e de células fusiformes",

      "Nevo de células epitelióides", "Melanoma de células epitelióides", "Nevo de células fusiformes", "Melanoma de células fusiformes SOE", "Melanoma de células fusiformes, tipo A",
      "Melanoma de células fusiformes, tipo B", "Nevo azul SOE", "Nevo azul maligno", "Nevo azul celular", "Tumor benigno de tecidos moles", "Sarcoma SOE",
      "Sarcomatose SOE", "Sarcoma de células fusiformes", "Sarcoma de células gigantes (exceto de osso M9250/3)", "Sarcoma de células pequenas", "Sarcoma epitelióide",
      "Fibroma SOE", "Fibrossarcoma SOE", "Fibromixoma SOE", "Firbromixossarcoma", "Fibroma periostal", "Fibrossarcoma periostal", "Fibroma de fáscia", "Fibrossarcoma de fáscia",
      "Fibrossarcoma infantil", "Elastofibroma", "Fibromatose agressiva", "Fibromatose abdominal", "Fibroma desmoplástico", "Miofibromatose", "Histiocitoma fibroso SOE",
      "Histiocitoma fibroso atípico", "Histiocitoma fibroso maligno", "Dermatofibroma SOE", "Dermatofibrossarcoma SOE", "Dermatofibrossarcoma protuberante pigmentado",
      "Mixoma SOE", "Mixossarcoma", "Angiomixoma", "Lipoma SOE", "Lipossarcoma SOE", "Fibrolipoma", "Lipossarcoma bem diferenciado", "Fibromixolipoma",
      "Lipossarcoma mixóide", "Lipossarcoma de células redondas", "Lipoma pleomórfico", "Lipossarcoma pleomórfico", "Lipossarcoma misto", "Lipoma intramuscular",
      "Lipoma de células fusiformes", "Lipossarcoma desdiferenciado", "Angiomiolipoma", "Angiolipoma SOE", "Mielolipoma", "Hibernoma", "Lipoblastomatose",

      "Leiomioma SOE", "Leiomiomatose SOE", "Leiomiossarcoma SOE", "Leiomioma epitelióide", "Leiomioma celular", "Leiomioma bizarro", "Angiomioma", "Angiomiossarcoma",
      "Mioma", "Miossarcoma", "Leiomiossarcoma mixóide", "Tumor de músculo liso SOE", "Rabdomioma SOE", "Rabdomiossarcoma SOE", "Rabdomiossarcoma pleomórfico",
      "Rabdomiossarcoma tipo misto", "Rabdomioma fetal", "Rabdomioma adulto", "Rabdomiossarcoma embrionário", "Rabdomiossarcoma alveolar", "Nódulo do estroma endometrial",
      "Sarcoma do estroma endometrial", "Miose endolinfático do estroma", "Adenomioma", "Adenossarcoma", "Adenoma pleomórfico", "Tumor misto maligno SOE",
      "Carcinoma em adenoma pleomórfico", "Tumor mulleriano misto", "Tumor mesodérmico misto", "Nefroma mesoblástico", "Nefroblastoma SOE", "Sarcoma rabdóide",
      "Sarcoma de células claras do rim", "Hepatoblastoma", "Pancreatoblastoma", "Blastoma pulmonar", "Carcinossarcoma SOE", "Carcinossarcoma embrionário",
      "Mioepitelioma", "Mesenquimoma benigno", "Mesenquimoma SOE", "Mesenquimoma maligno", "Sarcoma embrionário", "Tumor de Brenner SOE", "Tumor de Brenner 'borderline'",
      "Tumor de Brenner maligno", "Fibroadenoma SOE", "Fibroadenoma intracanalicular", "Fibroadenoma pericanalicular", "Adenofibroma SOE", "Adenofibroma seroso",
      "Adenofibroma mucinoso", "Fibroadenoma gigante", "Tumor filodes benigno", "Tumor filodes SOE", "Tumor filodes maligno", "Fibroadenoma juvenil", "Sinovioma benigno",
      "Sarcoma sinovial SOE", "Sarcoma sinovial de células fusiformes", "Sarcoma sinovial de células epitelióides", "Sarcoma sinovial bifásico",

      "Sarcoma de células claras (exceto rim M8964/3)", "Mesotelioma benigno", "Mesotelioma maligno", "Mesotelioma fibroso benigno", "Mesotelioma fibroso maligno",
      "Mesotelioma epitelióide benigno", "Mesotelioma epitelióide maligno", "Mesotelioma bifásico benigno", "Mesotelioma bifásico maligno", "Tumor adenomatóide SOE",
      "Mesotelioma cístico", "Disgerminoma", "Seminoma SOE", "Seminoma anaplástico", "Seminoma espermatocítico", "Germinoma", "Carcinoma embrionário SOE",
      "Tumor de seio endodérmico", "Poliembrioma", "Gonadoblastoma", "Teratoma benigno", "Teratoma SOE", "Teratoma maligno SOE", "Teratocarcinoma", "Teratoma maligno não diferenciado",
      "Teratoma maligno intemediário", "Cisto dermóide SOE", "Teratoma com transformação maligna", "Tumor misto de células germinativas", "Estruma ovariana SOE",
      "Estruma ovariana maligna", "Carcinóide estrumal", "Mola hidatiforme SOE", "Mola hidatiforma invasiva", "Coriocarcinoma SOE", "Coriocarcinoma combinado com outros elementos de células germinativas",
      "Teratoma maligno trofoblástico", "Mola hidatiforme parcial", "Tumor trofoblástico do sítio placentário", "Mesomefroma benigno", "Tumor mesonéfrico",
      "Mesomefroma maligno", "Hemangioma SOE", "Hemangiossarcoma", "Hemangioma cavernoso", "Hemangioma venoso", "Hemangioma racemoso", "Sarcoma das células de Kupfer",
      "Hemangioma epitelióde", "Hemangioma histiocitóide", "Hemangioendotelioma benigno", "Hemangioendotelioma SOE", "Hemangioendotelioma maligno", "Hemangioma capilar",
      "Hemangioma intramuscular", "Hemangioendotelioma epitelióide SOE", "Hemangioendotelioma epitelióide maligno", "Tumor alveolar brônquio intravascular", "Sarcoma de Kaposi",

      "Angioqueratoma", "Hemangioma queratótico verrucoso", "Hemangiopericitoma benigno", "Hemangiopericitoma SOE", "Hemangiopericitoma maligno",
      "Angiofibroma SOE", "Hemangioblastoma", "Linfangioma SOE", "Linfangiossarcoma", "Linfangioma capilar", "Linfangioma cavernoso", "Linfangioma cístico",
      "Linfangiomioma", "Linfangiomiomatose", "Hemolinfangioma", "Osteoma SOE", "Osteossarcoma SOE", "Osteossarcoma condroblástico", "Osteossarcoma fibroblástico",
      "Osteossarcoma telangiectásico", "Osteossarcoma em doença de Paget do osso", "Osteossarcoma de células pequenas", "Osteossarcoma justacortical",
      "Osteoma osteóide SOE", "Osteoblastoma SOE", "Osteoblastoma agressivo", "Osteocondroma", "Osteocondromatose SOE", "Condroma SOE", "Condromatose SOE",
      "Condrossarcoma SOE", "Condroma justacortical", "Condrossarcoma justacortical", "Condroblastoma SOE", "Condroblastoma maligno", "Condrossarcoma mixóide",
      "Condrossarcoma mesenquimal", "Fibrossarcoma condromixóide", "Tumor de células gigantes do osso, SOE", "Tumor maligno de células gigantes do osso",
      "Tumor de células gigantes de partes moles, SOE", "Tumor maligno de células gigantes de partes moles", "Sarcoma de Ewing", "Adamantinoma de ossos longos",
      "Fibroma ossificante", "Tumor odontogênico benigno", "Tumor odontogênico SOE", "Tumor odontogênico maligno", "Dentinoma", "Cementoma SOE", "Cementoblastoma benigno",

      "Fibroma cementificante", "Cementoma gigantiforme", "Odontoma SOE", "Odontoma composto", "Odontoma complexo", "Fibro-odontoma ameloblástico",
      "Odontossarcoma ameloblástico", "Tumor odontogênico adenomatóide", "Cisto odontogênico calcificante", "Tumor odontogênico de células fantasma",
      "Ameloblastoma SOE", "Ameloblastoma maligno", "Odontoameloblastoma", "Tumor odontogênico escamoso", "Mixoma odontogênico", "Fibroma odontogênico central",
      "Fibroma odontogênico periférico", "Fibroma ameloblástico", "Fibrossarcoma ameloblástico", "Tumor odontogênico epitelial calcificante", "Craniofaringioma",
      "Pinealoma", "Pineocitoma", "Pineoblastoma", "Tumor neuroectodérmico melanótico", "Tumor neuroectodérmico periférico", "Cordoma", "Glioma maligno",
      "Gliomatose cerebral", "Glioma misto", "Glioma subependimal", "Astrocitoma subependimal de células gigantes", "Papiloma de plexo coróide, SOE",
      "Papiloma maligno de plexo coróide", "Ependimoma SOE", "Ependimoma anaplástico", "Ependimoma papilar", "Ependimoma mixopapilar", "Astrocitoma SOE",
      "Astrocitoma anaplástico", "Astrocitoma protoplásmico", "Astrocitoma gemistocítico", "Astrocitoma fibrilar", "Astrocitoma pilocítico", "Espongioblastoma SOE",
      "Espongioblastoma polar", "Xantoastrocitoma pleomórfico", "Astroblastoma", "Glioblastoma SOE", "Glioblastoma de células gigantes", "Gliossarcoma",
      "Espongioblastoma polar primitivo", "Oligodendroglioma SOE", "Oligodendroglioma anaplástico", "Oligodendroblastoma", "Meduloblastoma SOE",

      "Meduloblastoma desmoplástico", "Medulomioblastoma", "Tumor neuroectodérmico primitivo", "Sarcoma cerebelar SOE", "Sarcoma monstrocelular",
      "Ganglioneuroma", "Ganglioneuroblastoma", "Ganglioneuromatose", "Neuroblastoma SOE", "Meduloepitelioma SOE", "Meduloepitelioma teratóide",
      "Neuroepitelioma SOE", "Espongioneuroblastoma", "Ganglioglioma", "Neurocitoma", "Tumor paciniano", "Retinoblastoma SOE", "Retinoblastoma diferenciado",
      "Retinoblastoma não-diferenciado", "Tumor neurogênico olfatório", "Estesioneurocitoma", "Estesioneuroblastoma", "Estesioneuroepitelioma",
      "Meningioma SOE", "Meningiomatose SOE", "Meningioma maligno", "Meningioma meningoteliomatoso", "Meningioma fibroso", "Meningioma psamomatoso",
      "Meningioma angiomatoso", "Meningioma hemangioblástico", "Meningioma hemangiopericítico", "Meningioma transicional", "Meningioma papilar",
      "Sarcomatose meníngea", "Neurofibroma SOE", "Neurofibromatose SOE", "Neurofibrossarcoma", "Neurofibroma melanótico", "Neurofibroma plexiforme",
      "Neurilemoma SOE", "Neurinomatose", "Neurilemoma maligno", "Tumor tritão maligno", "Neurotequeoma", "Neuroma SOE", "Tumores de células granulares SOE",
      "Tumor maligno de células granulares", "Sarcoma alveolar de partes moles", "Linfoma maligno SOE", "Linfoma maligno não-Hodgkin SOE", "Linfossarcoma SOE",
      "Reticulossarcoma SOE", "Microglioma", "Linfoma maligno difuso SOE", "Doença de Hodgkin SOE", "Doença de Hodgkin de celularidade mista SOE",
      "Doença de Hodgkin, de depleção linfocítica, SOE", "Doença de Hodgkin, de depleção linfocítica, com fibrose difusa", "Doença de Hodgkin, de depleção linfocítica, reticular",

      "Doença de Hodgkin, com predominância linfocítica, SOE", "Doença de Hodgkin, com predominância linfocítica, difusa", "Doença de Hodgkin, com predominância linfocítica, nodular",
      "Paragranuloma de Hodgkin SOE", "Granuloma de Hodgkin", "Sarcoma de Hodgkin", "Doença de Hodgkin, esclerose nodular, SOE", "Doença de Hodgkin, esclerose nodular, fase celular",
      "Doença de Hodgkin, esclerose nodular, predominância linfocítica", "Doença de Hodgkin, esclerose nodular, celularidade mista", "Doença de Hodgkin, esclerose nodular, depleção linfocítica",
      "Linfoma maligno, linfócitos pequenos, SOE", "Linfoma maligno, linfoplasmocítico", "Linfoma maligno de pequenas células clivadas, difuso",
      "Linfoma maligna linfocítico, diferenciação intermediária, difuso", "Linfoma maligno centrocítico", "Linfoma maligno, misto de células pequenas e grandes, difuso",
      "Linfoma maligno, centroblástico-centrocítico, difuso", "Polipose linfomatosa maligna", "Linfoma maligno de células grandes, difuso, SOE",
      "Linfoma maligno, de células grandes, clivadas, difuso", "Linfoma maligno, de células grandes, não clivadas, difuso", "Linfoma maligno, centroblástico, difuso",
      "Linfoma maligno imunoblástico SOE", "Linfoma maligno linfoblástico", "Linfoma maligno, de células pequenas não clivadas, difuso", "Linfoma de Burkitt SOE",
      "Linfoma maligno folicular SOE", "Linfoma maligno, misto de células pequenas clivadas e células grandes, folicular", "Linfoma maligno, centroblástico-centrocítico, folicular",
      "Linfoma maligno, linfocítico, bem diferenciado, nodular", "Linfoma maligno, linfocítico, diferenciação intermediária, nodular", "Linfoma maligno de células pequenas clivadas, folicular",
      "Linfoma maligno linfocítico, pouco diferenciado, nodular", "Linfoma maligno centroblástico folicular", "Linfoma maligno de células grandes, folicular, SOE",
      "Micose fungóide", "Doença de Sézary", "Linfoma de células T periférico SOE", "Linfoma de zona T", "Linfoma linfoepitelóide",

      "Linfoma de células T periféricos (linfadenopatia angio-imunoblástica com disproteinemia)", "Linfoma de células T, periférico, de células pequenas pleomórficas",
      "Linfoma de células T, periférico, pleomórfico de células médias e grandes", "Linfoma cutâneo", "Linfoma de células B monocitóides", "Angioendoteliomatose",
      "Linfoma de células T angiocêntrico", "Linfoma de células grandes (Ki-1+)", "Histiocitose maligna", "Doença de Letterer-Siwe", "Linfoma histiocítico verdadeiro",
      "Plasmocitoma SOE", "Mieloma múltiplo", "Mastocitoma SOE", "Sarcoma de mastócitos", "Mastocitose maligna", "Doença imunoproliferativa SOE",
      "Macroglobulinemia de Waldenström", "Doença da cadeia pesada alfa", "Doença da cadeia pesada gama", "Doença imunoproliferativa do intestino delgado",
      "Gamopatia monoclonal", "Lesão imunoproliferativa angiocêntrica", "Linfadenopatia angioimunoblástica", "Doença linfoproliferativa T-gama", "Leucemia SOE",
      "Leucemia aguda SOE", "Leucemia subaguda SOE", "Leucemia crônica SOE", "Leucemia aleucêmica SOE", "Leucemia linfóide SOE", "Leucemia linfoblástica aguda SOE",
      "Leucemia linfóide subaguda", "Leucemia linfocítica crônica", "Leucemia linfóide aleucêmica", "Leucemia prolinfocítica", "Leucemia de células de Burkitt",
      "Leucemia /linfoma de células T adultas", "Leucemia de plasmócitos", "Eritroleucemia", "Eritremia aguda", "Eritremia crônica", "Leucemia de células de linfossarcoma",
      "Leucemia mielóide SOE", "Leucemia mielóide aguda", "Leucemia mielóide subaguda", "Leucemia mielóide crônica", "Leucemia mielóide aleucêmica",
      "Leucemia promielocítica aguda", "Leucemia mielomonocítica aguda", "Leucemia mielomonocítica crônica", "Leucemia basófila", "Leucemia eosinofílica",

      "Leucemia monocítica SOE", "Leucemia monocítica aguda", "Leucemia monocítica subaguda", "Leucemia monocítica crônica", "Leucemia monocítica aleucêmica",
      "Leucemia de mastócitos", "Leucemia megacarioblástica aguda", "Sarcoma mielóide", "Panmielose aguda", "Mielofibrose aguda", "Leucemia 'hairy cell'",
      "Reticuloendoteliose leucêmica", "Policitemia vera", "Doença mieloproliferativa crônica", "Mieloesclerose com metaplasia mielóide", "Trombocitemia idiopática",
      "Doença linfoproliferativa SOE", "Anemia refratária SOE", "Anemia refratária sem sideroblastos", "Anemia refratária com sideroblastos", "Anemia refratária com excesso de blastos",
      "Anemia refratária com excesso de blastos com transformação", "Síndrome mielodisplásica SOE",
      " Neoplasia maligna, incerta se primária ou metastática​",	" Tumor maligno de células claras​",	" Carcinomatose​",	" Carcinoma neuroendócrino de grandes células​",	" Carcinoma de grandes células, fenótipo rabdóide​",	" Carcinoma de células 'vítreas'​",	" Carcinoma de células gigantes tipo osteoclasto​",	" Carcinoma de células não pequenas​",	" Carcinoma escamoso, papilar não invasivo​",	" Carcinoma de células escamosas corneificadas​",	" Carcinoma escamocelular, basalóide​",	" Carcinoma escamoso, tipo células claras​",	" Carcinoma basocelular nodular​",	" Carcinoma basocelular adenóide​",	" Tumor maligno de células granulares​",	" Neoplasia benigna do epitélio glandular​",	" Carcinoma in situ do epitélio glandular​",	" Carcinoma de células transicionais, micropapilífero​",	" Neoplasia glandular intraepitelial, grau III​",	" Adenoma de glândulas intestinais​",	" Somatostatinoma, SOE​",	" Carcinoma hepatocelular, tipo esclerosante​",	" Carcinoma hepatocelular, variante fusocelular​",	" Carcinoma hepatocelular, tipo células claras​",	" Carcinoma hepatocelular, tipo pleomórfico​(CIDZERO)",	" Carcinoma ductal in situ tipo cribriforme (C50.-)​",	" Adenocarcinoma de células parietais (C16.-)​",	" Adenocarcinoma de glândulas anais (C21.1)​",	" Carcinoma ductal in situ tipo sólido (C50.-)​",	" Carcinóide de células semelhantes a enterocromafinicas, SOE​",	" Tumor maligno de células semelhantes a enterocromafinicas​",	" Carcinóide tubular​",	" Tumor carcinoide atípico​",	" Carcinoma bronquíolo-alveolar não mucinoso (C34.-)​",	" Carcinoma bronquíolo-alveolar mucinoso (C34.-)​",	" Carcinoma bronquíolo-alveolar misto, mucinoso e não mucinoso (C34.-)​(CIDZERO_R)",	" Adenocarcinoma com subtipos mistos​",	" Carcinoma pituitário, SOE (C75.1)​",	" Adenocarcinoma de células claras (C56.9)​",	" Carcinoma de células renais associado a cisto (C64.9)​",	" Carcinoma renal, cromófobo (C64.9)​",	" Carcinoma renal, sarcomatoide (C64.9)​",	" Carcinoma de ductos coletores (C64.9)​",	" Adenoma folicular atípico (C73.9)​",	" Adenocarcinoma fetal (C73.9)​",	" Carcinoma folicular encapsulado (C73.9)​",	" Carcinoma insular (C73.9)​",	" Microcarcinoma papilífero (C73.9)​",	" Carcinoma papilífero, tipo células oxifílicas (C73.9)​",	" Carcinoma papilífero encapsulado (C73.9)​",	" Carcinoma papilífero, células colunares (C73.9)​",	" Carcinoma papilífero, variante de células altas (C73.9)​",	" Carcinoma misto medular e folicular (C73.9)​",	" Carcinoma misto medular papilífero (C73.9)​",	" Adenocarcinoma endometrioide, tipo secretor​",	" Adenocarcinoma endometrioide, tipo células ciliadas​",	" Adenocarcinoma tipo endocervical​",	" Tumor de Brenner maligno (C56.9)​(CIDZERO)",	" Espiradenoma écrino maligno",	" Carcinoma ductal de glândula sudorípara, esclerosante",	" Adenocarcinoma écrino papilífero",	" Poroma ecrino maligno",	" Adenocarcinoma écrino",	" Tumor seroso atípico proliferativo",	" Tumor cístico de células claras de malignidade limítrofe",	" Adenoma viloso",	" Carcinoma sólido pseudopapilífero",	" Tumor intraductal papilífero mucinoso com displasia moderada",	" Carcinoma intraductal papilífero mucinoso, não invasivo",	" Carcinoma intraductal papilífero mucinoso, invasivo",	" Tumor cístico papilífero seroso de malignidade limítrofe",	" Tumor papilífero seroso de malignidade limítrofe",	" Cistadenocarcinoma mucinoso, não invasivo",	" Tumor cístico mucinoso de malignidade limítrofe",	" Adenoma seroso de células claras, borderline",	" Adenocarcinoma mucinoso, tipo endocervical",	" Carcinoma intraductal micropapilífero",	" Carcinoma cístico hipersecretor",	" Carcinoma medular atípico",	" Carcinoma ductal, tipo desmoplásico",	" Carcinoma misto (ducto infiltrativo e de outros tipos)",
      "Carcinoma lobular infiltrativo misto com outros tipos de carcinoma",	" Adenocarcinoma polimorfo de baixo grau",	" Carcinoma tubular",	" Cistoadenocarcinoma tipo células acinares",	" Adenocarcinoma com diferenciação neuroendócrina",	" Carcinoma metaplásico, SOE",	" Adenocarcinoma hepatoide",	" Timoma, tipo A, SOE",	" Timoma, tipo A, SOE",	" Timoma, Tipo A, maligno",	" Timoma, tipo AB, SOE",	" Timoma maligno tipo AB",	" Timoma, tipo B1, SOE",	" Timoma maligno tipo B1",	" Timoma tipo B2, SOE",	" Timoma maligno tipo B2",	" Timoma tipo B3, SOE",	" Timoma maligno tipo B3",	" Timoma tipo C",	" Tumor epitelial tipo fusiforme com elementos semelhantes ao timo",	" Carcinoma semelhante a células tímicas",	" Tumor do estroma dos cordões sexuais, parcialmente diferenciado.",	" Tumor do estromal dos cordões sexuais, formas mistas",	" Tumor estromal com poucos elementos dos cordões sexuais",	" Tumor de células de Sertoli-Leydig, de diferenciação intermediária",	" Tumor de células de Sertoli-Leydig, pouco diferenciado",	" Tumor de células de Sertoli-Leydig, retiforme",	" Tumor de células de Sertoli-Leydig, pouco diferenciado, com elementos heterólogos",	" Tumor de células de Sertoli, células grandes calcificadas",	" Tumor de células de Sertoli-Leydig, pouco diferenciado",	" Tumor maligno de células produtoras de esteroides",	" Tumor glômico maligno",	" Melanoma meníngeo difuso",	" Neoplasia melanocítica intraepitelial",	" Melanoma lentiginoso acral",	" Tumor mesenquimal indiferenciado",	" Sarcoma indiferenciado",	" Tumor desmoplásico de pequenas células redondas",	" Fibroma de células grandes",	" Tumor fibroso solitário maligno",	" Tumor miofibroblástico inflamatório",	" Tumor miofibroblástico maligno",	" Tumor fibrohistiocítico plexiforme",	" Histiocitoma fibroso angiomatóide",	" Lipoma atípico",	" Tumor de malignidade indeterminada",	" Leiomiossarcoma",	" Leiomioma metastatizante",	" Rabdomiossarcoma fusocelular",	" Rabdomiossarcoma alveolar",	" Sarcoma do estroma endometrial de baixo grau",	" Carcinofibroma",	" Tumor estromal gastrointestinal, benigno",	" Tumor estromal gastrointestinal maligno",	" Tumor estromal gastrointestinal, malignidade incerta",	" Tumor estromal gastrointestinal maligno",	" Nefroblastoma cístico parcialmente diferenciado",	" Nefroma cístico maligno",	" Blastoma pleuropulmonar",	" Sialoblastoma",	" Mioepitelioma maligno",	" Adenofibroma seroso de malignidade limítrofe",	" Adenocarcinofibroma seroso",	" Adenofibroma mucinoso de malignidade limítrofe",	" Adenocarcinofibroma mucinoso",	" Neoplasia maligna intratubular de células germinativas",	" Tumor de células germinativas não seminomatoso",	" Tumor trofoblástico epitelióide",	" Osteossarcoma central",	" Osteossarcoma central bem diferenciado",	" Osteossarcoma telangiectásico",	" Osteossarcoma perióstal",	" Osteossarcoma de superfície, alto grau",	" Condrossarcoma de células claras",	" Condrossarcoma desdiferenciado",	" Tumor tenossinovial de células gigantes maligno",	" Craniofaringioma, tipo adamantinoma",	" Craniofaringioma papilífero",	" Tumor de Askin",	" Cordoma",	" Cordoma condróide",	" Cordoma desdiferenciado",	" Papiloma do plexo coróide atípico",	" Ependimoma anaplásico",	" Astrocitoma infantil desmoplásico",	" Tumor neuroepitelial disembrioplástico",	" Astrocitoma pilocítico",	" Glioblastoma multiforme",	" Gliofibroma",	" Glioma cordóide",	" Meduloblastoma de células grandes",	" Ganglioglioma anaplásico",	" Neurocitoma central",	" Tumor teratoide/rabdóide atípico",	" Retinoblastoma difuso",	" Meningioma rabdóide",	" Meningioma atípico",	" Perineuroma maligno",	" Linfoma composto, Hodgkin/não Hodgkin",	" Linfoma de Hodgkin, rico em linfócitos",	" Linfoma mediastinal primário de grandes células B",
      " Linfoma da zona marginal esplênica",	" Linfoma da zona marginal tipo células B, SOE",	" Linfoma MALT",	" Linfoma subcutâneo, tipo paniculite de células T",	" Linfoma hepatoesplênico tipo gama-delta",	" Linfoma intestinal de células T",	"Lesão linfoproliferativa cutânea primária de célula T (CD30+) (C44.-)",	" Linfoma nasal e tipo nasal de células T/NK",	" Linfoma linfoblástico de células precursoras, SOE",	" Linfoma linfoblástico de células precursoras B",	" Linfoma linfoblástico de células precursoras T",	"Leucemia de plasmocitos (C42.1)",	"Plamocitoma extramedular",	" Leucemia células mastocitárias",	"Histiocitose maligna",	" Histiocitose de células de Langerhans, SOE",	" Histiocitose de células de Langerhans, multifocal",	" Histiocitose de células de Langerhans unifocal",	" Histiocitose de células de Langerhans, poliostótica",	" Sarcoma histiocítico",	"Sarcoma histiocitico",	" Sarcoma de células de Langerhans",	" Sarcoma de células dendríticas interdigitantes",	" Sarcoma de células dendríticas foliculares",	" Doença de depósito de imunoglobulina",	" Leucemia aguda, bifenotípica",	" Leucemia linfocítica granular de células grandes tipo T",	" Leucemia prolinfocítica tipo célula B",	" Leucemia prolinfocítica tipo célula T",	" Leucemia linfoblástica de células precursoras, SOE",	" Leucemia linfoblástica de células precursoras tipo B",	" Leucemia linfoblástica de células precursoras tipo T",	"Leucemia linfoblástica de células T precursora",	" Leucemia mieloide aguda com eosinófilos anormais na medula",	" Leucemia mieloide aguda, com diferenciação mínima",	" Leucemia mieloide aguda sem maturação",	" Leucemia mieloide aguda com maturação",	" Leucemia crônica mielogênica, BCR/ABL positiva",	" Leucemia mieloide crônica atípica, BCR/ABL negativa",	" Leucemia mieloide aguda com displasia com multilinhagem",	" Leucemia mieloide aguda, t(8;21)",	" Leucemia mieloide aguda com anomalias em 11q23",	" Leucemia mieloide aguda relacionada ao tratamento",	" Leucemia mielomonocítica crônica",	" Leucemia mielomonocítica juvenil",	" Leucemia agressiva de células NK",	" Policitemia vera",	" Síndrome hipereosinofílica",	"Trombocitemia essencial",	" Citopenia refratária com displasia multilinhagem",	" Síndrome mielodisplásica relacionada ao tratamento",	"Doença mieloproliferativa crônica classificavel",	" Anemia refratária com sideroblastos em anel",	" RAEB I (Refratária Anemia com Excesso de Blastos)",	" RAEB II (Refratária Anemia com Excesso de Blastos)",	" RAEB-T (Refratária Anemia com Excesso de Blastos em Transformação)",	"Citopenia refratária com displasia multilinear (síndrome mielodisplásica)",	" Síndrome mielodisplásica com deleção 5q (5q-)",	"Síndrome mielodisplásica relacionada a terapia, SOE",	"Síndrome mielodisplásica, SOE",	"clinicamente tumor maligno (câncer)",
      "Neoplasia benigna",	"Tumor de células claras, SOE",	"Glucagonoma, SOE (C25.-)",	"Somatostatinoma maligno",	"Adenoma pituitario, SOE (C75.1)",	"Adenofibroma de células claras, malignidade limítrofe (C56.9)",	"Adenoma papilar digitiforme agressivo (C44.-)",	"Tumor de células de Sertoli-Leydig com diferenciação intermediaria e elementos heterologos",	"Melanocitoma meningeano (C70.9)",	"Lesão proliferativa dérmica em nevos congênitos (C44.-)",	"Miofibroblastoma",	"Tumor miofibroblástico peribrônquico (C34.-)",	"Fibroblastoma de células gigantes",	"Lipossarcoma fibroblástico",	"Tumor estromal gastrointestinal benigno",	"Angioendotelioma papilar endovascular",	"Hemangioendotelioma fusocelular",	"Osteossarcoma intra-cortical (C40.-, C41.-)",	"Carcinossarcoma odontogênico",	"Ganglioneuromatose",	"Perineuroma, SOE",	"Linfoma associado a tecido linfóide mucoso",	"Mieloesclerose com metaplasia mielóide",	"Clinicamente tumor, SOE",	"Base do diagnóstico não mensionado"


    )
  )

  # Verifica se a coluna Tipo_Histologico existe antes de tentar acessá-la
  if ("Tipo_Histologico" %in% names(dados)) {
    message("Mapeando os códigos de Tipo Histológico para nomes completos.")
    # Mapeamento dos códigos de Tipo Histológico para nomes usando um vetor nomeado
    map <- setNames(tipo_histologico_names$Tipo_Histologico_Completo, tipo_histologico_names$Codigo_tipo_hist)
    dados$Tipo_Histologico_Completo <- map[as.character(dados$Tipo_Histologico)]
    message("Nomes completos dos Tipos Histológicos adicionados.")
  } else {
    stop("\033[1;31mA coluna 'Tipo_Histologico' não foi encontrada no dataframe. Função interrompida.\033[0m")
  }

  message(paste("\033[1;32m", "> Ajuste dos códigos de Tipo Histológico concluído com sucesso. Foi adicionada uma coluna no final do dataframe, chamada Tipo_Histologico_Completo.", "\033[0m"))

  return(dados)

}






#' Análise de Completude de Dados
#'
#' Esta função calcula e avalia a completude de cada variável em um dataframe e gera um gráfico de barras horizontal. A completude é exibida junto com uma classificação baseada no escore de Romero & Cunha. Referência: ROMERO, Dalia E.; CUNHA, Cynthia Braga da. Avaliação da qualidade das variáveis sócio-econômicas e demográficas dos óbitos de crianças menores de um ano registrados no Sistema de Informações sobre Mortalidade do Brasil (1996/2001). Cadernos de Saúde Pública, v. 22, p. 673-681, 2006.
#'
#' @param dados Um dataframe contendo as variáveis a serem analisadas.
#' @return Retorna uma tabela com a completude de cada variável, incluindo um gráfico de barras horizontal referente aos Dados Ausentes.
#' @export
#' @name analise_completude
#' @examples
#' # Supondo que você tenha um dataframe chamado dados_RHC_combinados,
#' # Use a função da seguinte forma:
#'
#' analise_completude(dados_RHC_combinados)
#'
#' OBSERVAÇÃO: Sem atribuir a nenhum objeto.
analise_completude <- function(dados) {
  message("Iniciando a análise de completude de dados.")

  # Número de linhas do dataframe
  numero_linhas <- nrow(dados)

  # Quantidade de anos distintos e intervalo de anos na variável Ano_do_Banco
  anos <- unique(dados$Ano_do_Banco)
  quantidade_anos <- length(anos)
  intervalo_anos <- paste(min(anos), max(anos), sep = "-")

  # Nome das variáveis
  variaveis <- colnames(dados)

  message("Calculando o total de NA e 'Sem informação'.")
  # Número de NA's e "Sem informação" por coluna
  dados_ausentes <- sapply(dados, function(x) {
    sum(is.na(x) | as.character(x) == "Sem informação")
  })
  message("Calculando a proporção de Dados Ausentes por coluna")
  # Calcular a porcentagem de NA's por coluna em porcentagem, arredondada para duas casas decimais (usada no gráfico)
  porcentagem <- round((dados_ausentes / nrow(dados)) * 100, 2)

  # Calcular a completude como o complemento da porcentagem
  completude <- round(100 - porcentagem, 2)

  # Função para classificar a completude com base no escore de Romero & Cunha
  classificar_completude <- function(p) {
    if (p >= 95) {
      return("Excelente")
    } else if (p >= 90) {
      return("Bom")
    } else if (p >= 80) {
      return("Regular")
    } else if (p >= 50) {
      return("Ruim")
    } else {
      return("Muito Ruim")
    }
  }

  # Aplicar a classificação
  classificacao <- sapply(completude, classificar_completude)

  message("Tabela de resultados:")

  # Criar a tabela de resultados
  Ausentes <- data.frame(Variavel = variaveis,
                         Completude = completude,
                         Dados_Ausentes = dados_ausentes,
                         Classificacao_Completude = classificacao,
                         row.names = NULL)
  cat("\n\n")
  message(paste("Tabela – Análise da qualidade dos dados por Classificação da Completude de",
                numero_linhas,
                "registros de câncer do RHC de um período de", quantidade_anos, "anos,", intervalo_anos,"."))
  cat("\n")
  # Ordenar a tabela em ordem crescente de Dados_Ausentes
  Ausentes <- Ausentes[order(Ausentes$Dados_Ausentes), ]

  # Exibir a tabela de ausentes sem os números das linhas
  print(Ausentes, row.names = FALSE)

  # Ajustar as margens do gráfico (margem esquerda e direita ajustadas)
  par(mar = c(6, 17, 2, 10) + 0.1)

  # Criar o título do gráfico com o número de linhas incluído
  titulo_grafico <- paste("Figura – Gráfico de Dados Ausentes e Análise de Completude dos Dados de",
                          numero_linhas,
                          "registros de câncer \ndo Registro Hospitalar de Câncer, de um período de",
                          quantidade_anos, "anos,", intervalo_anos, ".")

  # Criar o gráfico de barras horizontal com a ordem invertida
  bp <- barplot(Ausentes$Dados_Ausentes,
                names.arg = Ausentes$Variavel,
                horiz = TRUE,
                las = 1,
                col = colorRampPalette(c("darkgreen", "green", "yellow", "orange", "red"))(length(Ausentes$Dados_Ausentes)),
                border = NA,
                xlab = NA,
                main = titulo_grafico,
                cex.names = 0.8)

  # Adicionar os valores de Dados_Ausentes, Porcentagem e Classificação no final das barras
  text(x = Ausentes$Dados_Ausentes,
       y = bp,
       labels = paste(Ausentes$Dados_Ausentes, " (", porcentagem[order(dados_ausentes)], "%, ", Ausentes$Classificacao_Completude, ")", sep = ""),
       pos = 4,
       cex = 0.7,
       col = "black",
       xpd = TRUE)

  # Adicionar a fonte
  mtext("Classificação de Completude: ROMERO, Dalia E.; CUNHA, Cynthia Braga da. Avaliação da qualidade das variáveis sócio-econômicas e demográficas dos óbitos de crianças \n menores de um ano registrados no Sistema de Informações sobre Mortalidade do Brasil (1996/2001). Cadernos de Saúde Pública, v. 22, p. 673-681, 2006.",
        side = 1, line = 4, at = 0.1, cex = 0.8, col = "black", xpd = TRUE, adj = 0)

  cat("\n\n\n")
  cat("Informações extras:\n")
  cat("Classificação de Completude. Escore proposto por Romero & Cunha: ROMERO, Dalia E.; CUNHA, Cynthia Braga da. \n Avaliação da qualidade das variáveis sócio-econômicas e demográficas dos óbitos de crianças menores de um ano \n registrados no Sistema de Informações sobre Mortalidade do Brasil (1996/2001). Cadernos de Saúde Pública, v. 22, p. 673-681, 2006.")
  cat("\n\n")
  message(paste("\033[1;32m", "> Análise de completude concluída.", "\033[0m"))

}









#' Análise de Completude de Dados por Ano
#'
#' Esta função calcula e avalia a completude de cada variável por ano, distribuindo a proporção de completude pela variável Ano_do_Banco. A completude é exibida junto com uma classificação baseada no escore de Romero & Cunha. Referência: ROMERO, Dalia E.; CUNHA, Cynthia Braga da. Avaliação da qualidade das variáveis sócio-econômicas e demográficas dos óbitos de crianças menores de um ano registrados no Sistema de Informações sobre Mortalidade do Brasil (1996/2001). Cadernos de Saúde Pública, v. 22, p. 673-681, 2006."
#'
#' @param dados Um dataframe contendo as variáveis a serem analisadas.
#' @return Retorna uma tabela com a completude de cada variável por ano, incluindo uma legenda com a classificação de completude.
#' @export
#' @name analise_completude_ano
#' @examples
#' # Supondo que você tenha um dataframe chamado dados_RHC_combinados,
#' # Use a função da seguinte forma:
#'
#' Nome_da_Sua_Tabela_de_Completud_ano <- analise_completude_ano(dados_RHC_combinados)
analise_completude_ano <- function(dados) {
  message("Iniciando a análise de completude de dados por ano.")

  # Verifica se a coluna Ano_do_Banco existe antes de tentar acessá-la
  if (!"Ano_do_Banco" %in% names(dados)) {
    stop("\033[1;31mA coluna 'Ano_do_Banco' não foi encontrada no dataframe. Função interrompida.\033[0m")
  }

  # Nome das variáveis
  variaveis <- colnames(dados)

  # Identificar os anos únicos na coluna Ano_do_Banco
  anos <- sort(unique(dados$Ano_do_Banco))

  # Função para classificar a completude com base no escore de Romero & Cunha
  classificar_completude <- function(p) {
    if (p >= 95) {
      return("E")  # Excelente
    } else if (p >= 90) {
      return("B")  # Bom
    } else if (p >= 80) {
      return("R")  # Regular
    } else if (p >= 50) {
      return("RU")  # Ruim
    } else {
      return("MR")  # Muito Ruim
    }
  }

  # Inicializar uma lista para armazenar a completude por variável e ano
  completude_lista <- list()

  # Calcular a completude por variável e ano
  for (ano in anos) {
    message(paste("Processando dados para o ano:", ano))
    dados_ano <- dados[dados$Ano_do_Banco == ano, ]
    if (nrow(dados_ano) == 0) {
      stop(paste("\033[1;31mNenhum dado encontrado para o ano", ano, ". Função interrompida.\033[0m"))
    }
    dados_ausentes <- sapply(dados_ano, function(x) {
      sum(is.na(x) | as.character(x) == "Sem informação")
    })
    completude <- round(100 - (dados_ausentes / nrow(dados_ano) * 100), 1)
    classificacao <- sapply(completude, classificar_completude)
    completude_lista[[as.character(ano)]] <- paste0(completude, "% (", classificacao, ")")
  }

  message("Criando a tabela de resultados.")
  # Criar a tabela de resultados como data frame
  Completude <- data.frame(Variavel = variaveis)

  for (ano in anos) {
    Completude[[as.character(ano)]] <- completude_lista[[as.character(ano)]]
  }

  # Adicionar a legenda explicando as classificações
  cat("\nLegenda de Classificação de Completude:\n")
  cat("E  - Excelente (>= 95%)\n")
  cat("B  - Bom (90% a 94.9%)\n")
  cat("R  - Regular (80% a 89.9%)\n")
  cat("RU - Ruim (50% a 79.9%)\n")
  cat("MR - Muito Ruim (< 50%)\n")
  cat("Classificação de Completude. Escore proposto por Romero & Cunha: ROMERO, Dalia E.; CUNHA, Cynthia Braga da. \n Avaliação da qualidade das variáveis sócio-econômicas e demográficas dos óbitos de crianças menores de um ano \n registrados no Sistema de Informações sobre Mortalidade do Brasil (1996/2001). Cadernos de Saúde Pública, v. 22, p. 673-681, 2006.\n")

  message(paste("\033[1;32m", "> Análise de completude por ano concluída. Veja o dataframe criado.", "\033[0m"))

  # Retornar a tabela de completude
  return(Completude)
}








#' Construir Banco de Dados Automaticamente
#'
#' Esta função aplica uma série de ajustes a um dataframe para construir um banco de dados finalizado. As etapas incluem ler e juntar os arquivo DBF, renomear colunas, modificar tipos de variáveis, recodificar variáveis, renomear siglas dos estados, renomear códigos CID de 3 e 4 dígitos, renomear códigos CNES e Tipo Histolófico e adicionar novas variáveis ao final do dataframe.
#'
#' @param none Não são necessários parâmetros externos, pois a função opera com base no conteúdo do diretório especificado.
#' @return Retorna o dataframe processado com todos os ajustes iniciais para posterior análise.
#' @examples
#' # Supondo que você tenha no diretório de trabalho arquivos DBF do RHC que permanecem com o mesmo nome que vieram do Integrador,
#' # Use a função da seguinte forma:
#'
#' dados_RHC_combinados <- construir_banco()
#' # Se nenhum arquivo for encontrado ou se houver erros ao juntar os arquivos, dados_combinados será NULL.
#' # Caso contrário, você terá um dataframe com todos os dados ajustados de forma automática.
#' @export
#' @name construir_banco
construir_banco <- function() {
  message("Carregando os dados...")

  # Iniciar medição do tempo total
  start_time_total <- Sys.time()

  # Aplicar a função leraquivoDBF
  data <- leraquivoDBF()

  # Verificação se a lista está vazia (NULL)
  if (is.null(data)) {
    message(paste("\033[1;31m", "> Nenhum dado foi encontrado. A função será interrompida.", "\033[0m"))

    return(NULL)
  }

  cat("\n\n\n\n")

  # Aplicar a função renomear_colunas
  message("Renomeando colunas...")
  data <- renomear_colunas(data)
  cat("\n\n\n\n")

  # Aplicar a função modificar_tipo_variavel
  message("Modificando tipos de variáveis...")
  data <- modificar_tipo_variavel(data)
  cat("\n\n\n\n")

  # Aplicar a função recodificar_variaveis
  message("Recodificando variáveis...")
  data <- recodificar_variaveis(data)
  cat("\n\n\n\n")

  # Aplicar a função renomear_siglas_estados
  message("Renomeando siglas dos estados...")
  data <- renomear_siglas_estados(data)
  cat("\n\n\n\n")

  # Aplicar a função renomear_CID_3digitos
  message("Ajustando códigos CID de 3 dígitos...")
  data <- renomear_CID_3digitos(data)
  cat("\n\n\n\n")

  # Aplicar a função renomear_CID_4digitos
  message("Ajustando códigos CID de 4 dígitos...")
  data <- renomear_CID_4digitos(data)
  cat("\n\n\n\n")

  # Aplicar a função renomear_CNES
  message("Ajustando códigos CNES...")
  data <- renomear_CNES(data)
  cat("\n\n\n\n")

  # Aplicar a função renomear_tipo_histologico
  message("Ajustando códigos Tipo Histológico...")
  data <- renomear_tipo_histologico(data)
  cat("\n\n\n\n")

  # Aplicar a função analise_completude
  analise_completude(data)

  cat("\n")

  cat("Relatório Final:\n")
  # Função para exibir informações finais
  exibir_informacoes_finais <- function(tempo_total, data) {
    # Calcular a quantidade de memória utilizada
    memoria_usada <- object.size(data)
    memoria_usada_mb <- round(memoria_usada / 1024^2, 2) # Convertendo para MB
    memoria_usada_gb <- round(memoria_usada / 1024^3, 2) # Convertendo para GB

    # Calcular o total de registros e variáveis
    total_registros <- nrow(data)
    total_variaveis <- ncol(data)

    # Converter o tempo total de execução para minutos
    tempo_total_minutos <- as.numeric(tempo_total, units = "mins")

    # Exibir informações finais
    message("Tempo total de execução: ", round(tempo_total_minutos, 2), " minutos.")
    message("Memória utilizada: ", memoria_usada_mb, "MB (", memoria_usada_gb, "GB)")
    message("Banco de dados final com ", total_registros, " registros de câncer e ", total_variaveis, " variáveis.")
  }

  # Medir o tempo total de execução
  end_time_total <- Sys.time()
  tempo_total <- end_time_total - start_time_total

  exibir_informacoes_finais(tempo_total, data)

  return(data)
}





#' Filtrar Dados com Base em Vários Critérios
#'
#' Esta função filtra um dataframe com base em critérios específicos fornecidos pelo usuário, incluindo códigos CID-3, códigos CID-4, anos de triagem, anos de diagnóstico, anos da primeira consulta, idade, tipo de caso, sexo, estado de residência, unidade hospitalar, primeiro tratamento hospitalar, ano de início do tratamento, origem do encaminhamento e anos do banco de dados.
#'
#' @param dados Um dataframe que contém as variáveis a serem filtradas.
#' @param cid3digitos Um vetor de códigos CID-3 para filtrar a coluna `Localizacao_Primaria_3D`. Padrão é NULL.
#' @param cid4digitos Um vetor de códigos CID-4 para filtrar a coluna `Localizacao_Primaria_4D`. Padrão é NULL.
#' @param ano_inicio_triagem Ano inicial para filtrar a coluna `Ano_Triagem`. Padrão é NULL.
#' @param ano_fim_triagem Ano final para filtrar a coluna `Ano_Triagem`. Padrão é NULL.
#' @param ano_inicio_diagnostico Ano inicial para filtrar a coluna `Ano_Primeiro_Diagnostico`. Padrão é NULL.
#' @param ano_fim_diagnostico Ano final para filtrar a coluna `Ano_Primeiro_Diagnostico`. Padrão é NULL.
#' @param ano_inicio_primeira_consulta Ano inicial para filtrar a coluna `Ano_Primeira_Consulta`. Padrão é NULL.
#' @param ano_fim_primeira_consulta Ano final para filtrar a coluna `Ano_Primeira_Consulta`. Padrão é NULL.
#' @param idade_inicio Idade inicial para filtrar a coluna `Idade`. Padrão é NULL.
#' @param idade_fim Idade final para filtrar a coluna `Idade`. Padrão é NULL.
#' @param tipo_de_caso Filtro para a coluna `Tipo_de_Caso` (valores possíveis: "Analítico", "Não Analítico"). Padrão é NULL.
#' @param sexo Filtro para a coluna `Sexo` (valores possíveis: "Masculino", "Feminino"). Padrão é NULL.
#' @param estado_residencia Filtro para a coluna `Estado_Residencia` com siglas dos estados. Padrão é NULL.
#' @param uf_unidade_hospital Filtro para a coluna `UF_Unidade_Hospital` com siglas dos estados. Padrão é NULL.
#' @param primeiro_tratamento_hospital Filtro para a coluna `Primeiro_Tratamento_Hospital`. Padrão é NULL.
#' @param ano_inicio_tratamento Ano inicial para filtrar a coluna `Ano_Inicio_Tratamento`. Padrão é NULL.
#' @param ano_fim_tratamento Ano final para filtrar a coluna `Ano_Inicio_Tratamento`. Padrão é NULL.
#' @param origem_do_encaminhamento Filtro para a coluna `Origem_do_Encaminhamento`. Padrão é NULL.
#' @param ano_do_banco_inicio Ano inicial para filtrar a coluna `Ano_do_Banco`. Padrão é NULL.
#' @param ano_do_banco_fim Ano final para filtrar a coluna `Ano_do_Banco`. Padrão é NULL.
#' @return Retorna o dataframe filtrado com base nos critérios fornecidos.
#' @examples
#' # Filtrar o banco de dados
#' DADOS_FILTRADOS <- filtrar_banco(dados = dados_RHC_combinados,
#'                                  cid3digitos = "C44",  # Filtra os dados pela coluna `Localizacao_Primaria_3D` usando o código CID-3 "C44"
#'                                  cid4digitos = "C44.0",  # Filtra os dados pela coluna `Localizacao_Primaria_4D` usando o código CID-4 "C44.0"
#'                                  ano_inicio_triagem = 2018,  # Filtra os dados pela coluna `Ano_Triagem` entre os anos de 2018 e 2020
#'                                  ano_fim_triagem = 2020,
#'                                  ano_inicio_diagnostico = 2015,  # Filtra os dados pela coluna `Ano_Primeiro_Diagnostico` entre os anos de 2015 e 2019
#'                                  ano_fim_diagnostico = 2019,
#'                                  ano_inicio_primeira_consulta = 2016,  # Filtra os dados pela coluna `Ano_Primeira_Consulta` entre os anos de 2016 e 2020
#'                                  ano_fim_primeira_consulta = 2020,
#'                                  idade_inicio = 20,  # Filtra os dados pela coluna `Idade` entre 20 e 80 anos
#'                                  idade_fim = 80,
#'                                  tipo_de_caso = "Analítico",  # Filtra os dados pela coluna `Tipo_de_Caso`, selecionando "Analítico"
#'                                  sexo = NULL,  # Não filtra pelo sexo (NULL)
#'                                  estado_residencia = c("RJ", "MG"),  # Filtra os dados pela coluna `Estado_Residencia`, selecionando "RJ" e "MG"
#'                                  uf_unidade_hospital = c("RJ", "MG"),  # Filtra os dados pela coluna `UF_Unidade_Hospital`, selecionando "RJ" e "MG"
#'                                  primeiro_tratamento_hospital = "Cirurgia",  # Filtra os dados pela coluna `Primeiro_Tratamento_Hospital`, selecionando "Cirurgia"
#'                                  ano_inicio_tratamento = 2014,  # Filtra os dados pela coluna `Ano_Inicio_Tratamento` entre os anos de 2014 e 2019
#'                                  ano_fim_tratamento = 2019,
#'                                  origem_do_encaminhamento = "SUS",  # Filtra os dados pela coluna `Origem_do_Encaminhamento`, selecionando "SUS"
#'                                  ano_do_banco_inicio = 2010,  # Filtra os dados pela coluna `Ano_do_Banco` entre os anos de 2010 e 2020
#'                                  ano_do_banco_fim = 2020)
#' @export
#' @name filtrar_banco
filtrar_banco <- function(dados = Seu_data_frame_aqui,
                          cid3digitos = NULL,
                          cid4digitos = NULL,
                          ano_inicio_triagem = NULL, ano_fim_triagem = NULL,
                          ano_inicio_diagnostico = NULL, ano_fim_diagnostico = NULL,
                          ano_inicio_primeira_consulta = NULL, ano_fim_primeira_consulta = NULL,
                          idade_inicio = NULL, idade_fim = NULL,
                          tipo_de_caso = NULL,
                          sexo = NULL,
                          estado_residencia = NULL,
                          uf_unidade_hospital = NULL,
                          primeiro_tratamento_hospital = NULL,
                          ano_inicio_tratamento = NULL, ano_fim_tratamento = NULL,
                          origem_do_encaminhamento = NULL,
                          ano_do_banco_inicio = NULL, ano_do_banco_fim = NULL)


{message("Verificando a existência dos campos necessários e validade dos valores dos parâmetros...")

  # Lista de parâmetros e seus campos correspondentes no dataframe
  parametros <- list(cid3digitos = "Localizacao_Primaria_3D",
                     cid4digitos = "Localizacao_Primaria_4D",
                     ano_inicio_triagem = "Ano_Triagem",
                     ano_fim_triagem = "Ano_Triagem",
                     ano_inicio_diagnostico = "Ano_Primeiro_Diagnostico",
                     ano_fim_diagnostico = "Ano_Primeiro_Diagnostico",
                     ano_inicio_primeira_consulta = "Ano_Primeira_Consulta",
                     ano_fim_primeira_consulta = "Ano_Primeira_Consulta",
                     idade_inicio = "Idade",
                     idade_fim = "Idade",
                     estado_residencia = "Estado_Residencia",
                     uf_unidade_hospital = "UF_Unidade_Hospital",
                     primeiro_tratamento_hospital = "Primeiro_Tratamento_Hospital",
                     ano_inicio_tratamento = "Ano_Inicio_Tratamento",
                     origem_do_encaminhamento = "Origem_do_Encaminhamento",
                     ano_do_banco_inicio = "Ano_do_Banco",
                     ano_do_banco_fim = "Ano_do_Banco")


  # Verificar se os parâmetros fornecidos existem no dataframe
  for (parametro in names(parametros)) {
    campo <- parametros[[parametro]]
    valor <- eval(parse(text = parametro))
    if (!is.null(valor)) {
      if (!campo %in% names(dados)) {
        stop(paste("\033[1;31mErro: O campo '", campo, "' não existe no dataframe.\033[0m", sep = ""))
          }
      if (!all(valor %in% dados[[campo]])) {
        stop(paste("\033[1;31mErro: O valor fornecido para '", campo, "' não existe no dataframe.\033[0m", sep = ""))

      }
    }
  }


  message("Todos os campos e valores são válidos. Iniciando a filtragem dos dados.")


  message("Iniciando a filtragem dos dados.")

  # Verificar se cid3digitos possui exatamente 3 caracteres
  if (!is.null(cid3digitos)) {
    if (any(nchar(cid3digitos) != 3)) {
      stop("\033[1;31mErro: cid3digitos deve possuir exatamente 3 caracteres. Utilize cid4digitos para códigos mais longos.\033[0m")

    }
    message("Filtrando dados com base nos códigos CID-3.")
    if (!is.vector(cid3digitos)) {
      cid3digitos <- c(cid3digitos)
    }
    dados <- subset(dados, Localizacao_Primaria_3D %in% cid3digitos)
  }

  # Verificar se cid4digitos possui exatamente 4 caracteres com um ponto entre o penúltimo e o último
  if (!is.null(cid4digitos)) {
    if (any(nchar(cid4digitos) != 5 | !grepl("\\.[A-Za-z0-9]{1}$", cid4digitos))) {
      stop("\033[1;31mErro: cid4digitos deve possuir exatamente 4 caracteres com um ponto entre o penúltimo e o último. Utilize cid3digitos para códigos mais curtos.\033[0m")

    }
    message("Filtrando dados com base nos códigos CID-4.")
    if (!is.vector(cid4digitos)) {
      cid4digitos <- c(cid4digitos)
    }
    dados <- subset(dados, Localizacao_Primaria_4D %in% cid4digitos)
  }

  # Aplicar filtragem adicional com base no Ano_Triagem, se fornecido
  if (!is.null(ano_inicio_triagem) && !is.null(ano_fim_triagem)) {
    message(paste("Filtrando dados com base no Ano_Triagem entre", ano_inicio_triagem, "e", ano_fim_triagem, "."))
    dados <- subset(dados, Ano_Triagem >= ano_inicio_triagem & Ano_Triagem <= ano_fim_triagem)
  }

  # Aplicar filtragem adicional com base no Ano_Primeiro_Diagnostico, se fornecido
  if (!is.null(ano_inicio_diagnostico) && !is.null(ano_fim_diagnostico)) {
    message(paste("Filtrando dados com base no Ano_Primeiro_Diagnostico entre", ano_inicio_diagnostico, "e", ano_fim_diagnostico, "."))
    dados <- subset(dados, Ano_Primeiro_Diagnostico >= ano_inicio_diagnostico & Ano_Primeiro_Diagnostico <= ano_fim_diagnostico)
  }

  # Aplicar filtragem adicional com base no Ano_Primeira_Consulta, se fornecido
  if (!is.null(ano_inicio_primeira_consulta) && !is.null(ano_fim_primeira_consulta)) {
    message(paste("Filtrando dados com base no Ano_Primeira_Consulta entre", ano_inicio_primeira_consulta, "e", ano_fim_primeira_consulta, "."))
    dados <- subset(dados, Ano_Primeira_Consulta >= ano_inicio_primeira_consulta & Ano_Primeira_Consulta <= ano_fim_primeira_consulta)
  }

  # Aplicar filtragem adicional com base na Idade, se fornecido
  if (!is.null(idade_inicio) && !is.null(idade_fim)) {
    message(paste("Filtrando dados com base na Idade entre", idade_inicio, "e", idade_fim, "."))
    dados <- subset(dados, Idade >= idade_inicio & Idade <= idade_fim)
  }

  # Aplicar filtragem adicional com base no Tipo_de_Caso, se fornecido
  if (!is.null(tipo_de_caso)) {
    if (!tipo_de_caso %in% c("Analítico", "Não Analítico")) {
      stop("\033[1;31mErro: tipo_de_caso deve ser 'Analítico' ou 'Não Analítico'.\033[0m")

    }
    message(paste("Filtrando dados com base no Tipo_de_Caso:", tipo_de_caso, "."))
    dados <- subset(dados, Tipo_de_Caso == tipo_de_caso)
  }

  # Aplicar filtragem adicional com base no Sexo, se fornecido
  if (!is.null(sexo)) {
    if (!sexo %in% c("Masculino", "Feminino")) {
      stop("\033[1;31mErro: sexo deve ser 'Masculino' ou 'Feminino'.\033[0m")

    }
    message(paste("Filtrando dados com base no Sexo:", sexo, "."))
    dados <- subset(dados, Sexo == sexo)
  }

  # Aplicar filtragem adicional com base no Estado_Residencia, se fornecido
  if (!is.null(estado_residencia)) {
    message(paste("Filtrando dados com base no Estado de Residência:", estado_residencia, "."))
    dados <- subset(dados, Estado_Residencia %in% estado_residencia)
  }

  # Aplicar filtragem adicional com base no UF_Unidade_Hospital, se fornecido
  if (!is.null(uf_unidade_hospital)) {
    message(paste("Filtrando dados com base na UF da Unidade Hospitalar:", uf_unidade_hospital, "."))
    dados <- subset(dados, UF_Unidade_Hospital %in% uf_unidade_hospital)
  }

  # Aplicar filtragem adicional com base no Primeiro_Tratamento_Hospital, se fornecido
  if (!is.null(primeiro_tratamento_hospital)) {
    message(paste("Filtrando dados com base no Primeiro Tratamento Hospitalar:", primeiro_tratamento_hospital, "."))
    dados <- subset(dados, Primeiro_Tratamento_Hospital == primeiro_tratamento_hospital)
  }

  # Aplicar filtragem adicional com base no Ano_Inicio_Tratamento, se fornecido
  if (!is.null(ano_inicio_tratamento) && !is.null(ano_fim_tratamento)) {
    message(paste("Filtrando dados com base no Ano_Inicio_Tratamento entre", ano_inicio_tratamento, "e", ano_fim_tratamento, "."))
    dados <- subset(dados, Ano_Inicio_Tratamento >= ano_inicio_tratamento & Ano_Inicio_Tratamento <= ano_fim_tratamento)
  }

  # Aplicar filtragem adicional com base na Origem_do_Encaminhamento, se fornecido
  if (!is.null(origem_do_encaminhamento)) {
    message(paste("Filtrando dados com base na Origem do Encaminhamento:", origem_do_encaminhamento, "."))
    dados <- subset(dados, Origem_do_Encaminhamento == origem_do_encaminhamento)
  }

  # Aplicar filtragem adicional com base no Ano_do_Banco, se fornecido
  if (!is.null(ano_do_banco_inicio) && !is.null(ano_do_banco_fim)) {
    message(paste("Filtrando dados com base no Ano_do_Banco entre", ano_do_banco_inicio, "e", ano_do_banco_fim, "."))
    dados <- subset(dados, Ano_do_Banco >= ano_do_banco_inicio & Ano_do_Banco <= ano_do_banco_fim)
  }


  message(paste("\033[1;32m", "> Filtragem dos dados concluída com sucesso.", "\033[0m"))


  # Retornar o dataframe filtrado
  return(dados)
}

