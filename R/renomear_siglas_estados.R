

#' Converte as Siglas de Estados para Nomes Completos
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

  message(paste("\033[1;32m", "> Mapeamento das siglas dos estados concluído. Foram adicionadas", colunas_adicionadas, "coluna(s) no dataframe.", "\033[0m"))

  return(dados)
}


