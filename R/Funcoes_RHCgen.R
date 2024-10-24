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
  # Instala e carrega o pacote foreign
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

  # Aplicar a função renomear_tipo_histologico
  message("Ajustando códigos das Clínicas...")
  data <- renomear_clinica(data)
  cat("\n\n\n\n")

  # Aplicar a função renomear_tipo_histologico
  message("Ajustando Estadiamento Clínico")
  data <- renomear_estadiamento_clinico(data)
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

