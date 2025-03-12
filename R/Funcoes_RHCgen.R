


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

  # Aplicar a função lerarquivoDBF
  data <- lerarquivoDBF()

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

  # Aplicar a função renomear_clinica
  message("Ajustando códigos das Clínicas...")
  data <- renomear_clinica(data)
  cat("\n\n\n\n")

  # Aplicar a função renomear_estadiamento_clinico
  message("Ajustando Estadiamento Clínico")
  data <- renomear_estadiamento_clinico(data)
  cat("\n\n\n\n")

  # Aplicar a função ordenando_colunas
  message("Reordenando as colunas...")
  data <- ordenando_colunas(data)
  cat("\n\n\n\n")

  # Aplicar a função analise_qualidade_dos_dados
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

