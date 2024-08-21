
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
