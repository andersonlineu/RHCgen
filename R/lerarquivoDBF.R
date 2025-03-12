#' Ler e Junta os Arquivo DBF
#'
#' @description Esta função ler e junta arquivos DBF do RHC e adiciona uma nova coluna: Ano_do_Banco, a partir do ano do arquivo DBF.
#'
#' @param none Não são necessários parâmetros externos, pois a função opera com base no conteúdo do diretório especificado.
#' @return Retorna um dataframe que junta todos os arquivos DBF no diretório, ou NULL se nenhum arquivo válido for encontrado ou se eles não puderem ser combinados devido a inconsistências nas estruturas de colunas.
#' É fundamental que todos os arquivos tenham o mesmo nome que vieram do IntegradorRHC e estejam todos na pasta de trabalho.
#' @export
#' @name lerarquivoDBF
#' @examples
#' # Supondo que você tenha no diretório de trabalho arquivos DBF do RHC que permanecem com o mesmo nome que vieram do Integrador (Exemplo: rhc20.dbf, nome padrão do RHC inciando que o ano do banco contidono arquvio é 2020).
#' # Use a função da seguinte forma:
#'
#' dados_RHC_combinados <- lerarquivoDBF()
#' # Se nenhum arquivo for encontrado ou se houver erros ao juntar os arquivos, dados_combinados será NULL.
#' # Caso contrário, você terá um dataframe com todos os dados combinados dos arquivos DBF.
lerarquivoDBF <- function() {
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
