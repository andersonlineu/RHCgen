#' Ler e Junta os Arquivo DBF
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
    message(paste("\033[1;32m", "> Carregamento dos arquivos e estruturação do dataframe finalizados com sucesso! Foi adicionada uma coluna chamada Ano_do_Banco no dataframe.", "\033[0m"))

    return(dataRHCCombinados)
  } else {
    message("\033[1;31mNenhum dataframe válido para combinar.\033[0m")
    return(NULL)
  }
}


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



#' Modifica os Tipos de Variáveis em um Dataframe
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



#' Recodifica as Variáveis em um DataFrame
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

  # 'Estadiamento_Clinico_TNM_grupo_1985_1999'
  if ("Estadiamento_Clinico_TNM_grupo_1985_1999" %in% names(data)) {
    message("Recodificando 'Estadiamento_Clinico_TNM_grupo_1985_1999'.")
    data$Estadiamento_Clinico_TNM_grupo_1985_1999 <- ifelse(
      data$Estadiamento_Clinico_TNM_grupo_1985_1999 == "1", "Estádio I",
      ifelse(
        data$Estadiamento_Clinico_TNM_grupo_1985_1999 == "2", "Estádio II",
        ifelse(
          data$Estadiamento_Clinico_TNM_grupo_1985_1999 == "3", "Estádio III",
          ifelse(
            data$Estadiamento_Clinico_TNM_grupo_1985_1999 == "4", "Estádio IV",
            ifelse(
              data$Estadiamento_Clinico_TNM_grupo_1985_1999 == "8", "Não se aplica",
              ifelse(
                data$Estadiamento_Clinico_TNM_grupo_1985_1999 == "9", "Sem informação",
                data$Estadiamento_Clinico_TNM_grupo_1985_1999
              )
            )
          )
        )
      )
    )
    data$Estadiamento_Clinico_TNM_grupo_1985_1999 <- factor(
      data$Estadiamento_Clinico_TNM_grupo_1985_1999,
      levels = c("Estádio I", "Estádio II", "Estádio III", "Estádio IV", "Não se aplica", "Sem informação")
    )
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Estadiamento_Clinico_TNM_grupo_1985_1999")
  }



  if (length(variaveis_ausentes) > 0) {
    message(paste("Variáveis ausentes:", paste(variaveis_ausentes, collapse = ", ")))
  }

  message(paste("Variáveis recodificadas:", paste(variaveis_recodificadas, collapse = ", ")))

  message(paste("\033[1;32m", "> Recodificação das variáveis concluída. Total:", contador_recodificacoes, "com sucesso \033[0m"))

  return(data)
}







#' Constrói o Banco de Dados Todo Automaticamente
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

  # Aplicar a função renomear_codigo_municipio_residencia
  message("Convertendo códigos dos municípios para nomes completos...")
  data <- renomear_codigo_municipio_residencia(data)
  cat("\n\n\n\n")

  # Aplicar a função renomear_codigo_municipio_hospital
  message("Convertendo códigos dos municípios para nomes completos...")
  data <- renomear_codigo_municipio_hospital(data)
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

  # Aplicar a função ordenando_colunas
  message("Reordenando as colunas...")
  data <- ordenando_colunas(data)
  cat("\n\n\n\n")

  # Aplicar a função analise_completude
  analise_qualidade_dos_dados(data)

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
    message("Tempo total de execução: \033[1;32m", round(tempo_total_minutos, 2), " minutos.\033[0m")
    message("Memória utilizada: \033[1;32m", memoria_usada_mb, "MB (", memoria_usada_gb, "GB)\033[0m")
    message("Banco de dados final com: \033[1;32m", total_registros, " registros de câncer e ", total_variaveis, " variáveis.\033[0m")
    cat("\n\n")
  }

  # Medir o tempo total de execução
  end_time_total <- Sys.time()
  tempo_total <- end_time_total - start_time_total

  exibir_informacoes_finais(tempo_total, data)

  return(data)
}


#' Filtrar os Dados com Base em Vários Critérios
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
                          ano_do_banco_inicio = NULL, ano_do_banco_fim = NULL) {

  message("Verificando a existência dos campos necessários e validade dos valores dos parâmetros...")

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
    if (!is.null(valor) && !campo %in% names(dados)) {
      stop(paste("\033[1;31mErro: O campo '", campo, "' não existe no dataframe.\033[0m", sep = ""))
    }
  }

  message("Todos os campos são válidos. Iniciando a filtragem dos dados.")

  # Verificar se cid3digitos possui exatamente 3 caracteres
  if (!is.null(cid3digitos)) {
    if (any(nchar(cid3digitos) != 3)) {
      stop("\033[1;31mErro: cid3digitos deve possuir exatamente 3 caracteres. Utilize cid4digitos para códigos mais longos.\033[0m")
    }
    message("Filtrando dados com base nos códigos CID-3.")
    dados <- subset(dados, tolower(Localizacao_Primaria_3D) %in% tolower(cid3digitos))
    dados <- droplevels(dados)
  }

  # Verificar se cid4digitos possui exatamente 4 caracteres com um ponto entre o penúltimo e o último
  if (!is.null(cid4digitos)) {
    if (any(nchar(cid4digitos) != 5 | !grepl("\\.[A-Za-z0-9]{1}$", cid4digitos))) {
      stop("\033[1;31mErro: cid4digitos deve possuir exatamente 4 caracteres com um ponto entre o penúltimo e o último. Utilize cid3digitos para códigos mais curtos.\033[0m")
    }
    message("Filtrando dados com base nos códigos CID-4.")
    dados <- subset(dados, tolower(Localizacao_Primaria_4D) %in% tolower(cid4digitos))
    dados <- droplevels(dados)
  }

  # Aplicar filtragem adicional com base no Ano_Triagem, se fornecido
  if (!is.null(ano_inicio_triagem) && !is.null(ano_fim_triagem)) {
    message(paste("Filtrando dados com base no Ano_Triagem entre", ano_inicio_triagem, "e", ano_fim_triagem, "."))
    dados <- subset(dados, Ano_Triagem >= ano_inicio_triagem & Ano_Triagem <= ano_fim_triagem)
    dados <- droplevels(dados)
  }

  # Aplicar filtragem adicional com base no Ano_Primeiro_Diagnostico, se fornecido
  if (!is.null(ano_inicio_diagnostico) && !is.null(ano_fim_diagnostico)) {
    message(paste("Filtrando dados com base no Ano_Primeiro_Diagnostico entre", ano_inicio_diagnostico, "e", ano_fim_diagnostico, "."))
    dados <- subset(dados, Ano_Primeiro_Diagnostico >= ano_inicio_diagnostico & Ano_Primeiro_Diagnostico <= ano_fim_diagnostico)
    dados <- droplevels(dados)
  }

  # Aplicar filtragem adicional com base no Ano_Primeira_Consulta, se fornecido
  if (!is.null(ano_inicio_primeira_consulta) && !is.null(ano_fim_primeira_consulta)) {
    message(paste("Filtrando dados com base no Ano_Primeira_Consulta entre", ano_inicio_primeira_consulta, "e", ano_fim_primeira_consulta, "."))
    dados <- subset(dados, Ano_Primeira_Consulta >= ano_inicio_primeira_consulta & Ano_Primeira_Consulta <= ano_fim_primeira_consulta)
    dados <- droplevels(dados)
  }

  # Aplicar filtragem adicional com base na Idade, se fornecido
  if (!is.null(idade_inicio) && !is.null(idade_fim)) {
    message(paste("Filtrando dados com base na Idade entre", idade_inicio, "e", idade_fim, "."))
    dados <- subset(dados, Idade >= idade_inicio & Idade <= idade_fim)
    dados <- droplevels(dados)
  }

  # Aplicar filtragem adicional com base no Tipo_de_Caso, se fornecido
  if (!is.null(tipo_de_caso)) {
    tipo_de_caso <- tolower(iconv(tipo_de_caso, to = "ASCII//TRANSLIT"))
    if (!tipo_de_caso %in% c("analitico", "nao analitico")) {
      stop("\033[1;31mErro: tipo_de_caso deve ser 'Analítico' ou 'Não Analítico'.\033[0m")
    }
    message(paste("Filtrando dados com base no Tipo_de_Caso:", tipo_de_caso, "."))
    dados <- subset(dados, tolower(iconv(Tipo_de_Caso, to = "ASCII//TRANSLIT")) == tipo_de_caso)
    dados <- droplevels(dados)
  }

  # Aplicar filtragem adicional com base no Sexo, se fornecido
  if (!is.null(sexo)) {
    if (!tolower(sexo) %in% c("masculino", "feminino")) {
      stop("\033[1;31mErro: sexo deve ser 'Masculino' ou 'Feminino'.\033[0m")
    }
    message(paste("Filtrando dados com base no Sexo:", sexo, "."))
    dados <- subset(dados, tolower(Sexo) == tolower(sexo))
    dados <- droplevels(dados)
  }

  # Aplicar filtragem adicional com base no Estado_Residencia, se fornecido
  if (!is.null(estado_residencia)) {
    message(paste("Filtrando dados com base no Estado de Residência:", estado_residencia, "."))
    dados <- subset(dados, tolower(Estado_Residencia) %in% tolower(estado_residencia))
    dados <- droplevels(dados)
  }

  # Aplicar filtragem adicional com base no UF_Unidade_Hospital, se fornecido
  if (!is.null(uf_unidade_hospital)) {
    message(paste("Filtrando dados com base na UF da Unidade Hospitalar:", uf_unidade_hospital, "."))
    dados <- subset(dados, tolower(UF_Unidade_Hospital) %in% tolower(uf_unidade_hospital))
    dados <- droplevels(dados)
  }

  # Aplicar filtragem adicional com base no Primeiro_Tratamento_Hospital, se fornecido
  if (!is.null(primeiro_tratamento_hospital)) {
    message(paste("Filtrando dados com base no Primeiro Tratamento Hospitalar:", primeiro_tratamento_hospital, "."))
    dados <- subset(dados, tolower(Primeiro_Tratamento_Hospital) == tolower(primeiro_tratamento_hospital))
    dados <- droplevels(dados)
  }

  # Aplicar filtragem adicional com base no Ano_Inicio_Tratamento, se fornecido
  if (!is.null(ano_inicio_tratamento) && !is.null(ano_fim_tratamento)) {
    message(paste("Filtrando dados com base no Ano_Inicio_Tratamento entre", ano_inicio_tratamento, "e", ano_fim_tratamento, "."))
    dados <- subset(dados, Ano_Inicio_Tratamento >= ano_inicio_tratamento & Ano_Inicio_Tratamento <= ano_fim_tratamento)
    dados <- droplevels(dados)
  }

  # Aplicar filtragem adicional com base na Origem_do_Encaminhamento, se fornecido
  if (!is.null(origem_do_encaminhamento)) {
    message(paste("Filtrando dados com base na Origem do Encaminhamento:", origem_do_encaminhamento, "."))
    dados <- subset(dados, tolower(Origem_do_Encaminhamento) == tolower(origem_do_encaminhamento))
    dados <- droplevels(dados)
  }

  # Aplicar filtragem adicional com base no Ano_do_Banco, se fornecido
  if (!is.null(ano_do_banco_inicio) && !is.null(ano_do_banco_fim)) {
    message(paste("Filtrando dados com base no Ano_do_Banco entre", ano_do_banco_inicio, "e", ano_do_banco_fim, "."))
    dados <- subset(dados, Ano_do_Banco >= ano_do_banco_inicio & Ano_do_Banco <= ano_do_banco_fim)
    dados <- droplevels(dados)
  }

  message(paste("\033[1;32m", "> Filtragem dos dados concluída com sucesso.", "\033[0m"))

  # Retornar o dataframe filtrado
  return(dados)
}
