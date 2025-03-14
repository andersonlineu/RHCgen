% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/verificar_codigos_inconsistentes.R
\name{verificar_codigos_inconsistentes}
\alias{verificar_codigos_inconsistentes}
\title{Verifica Códigos inconsistentes em um Dataframe}
\usage{
verificar_codigos_inconsistentes(data)
}
\arguments{
\item{data}{Um dataframe contendo as variáveis a serem verificadas.}
}
\value{
Retorna um dataframe com o quantitativo e a proporção de códigos inconsistentes por variável.
}
\description{
Esta função verifica se há códigos inconsistentes (campos preenchidos com códigos inexistentes na Ficha de Registro de Tumor ou com formatos incorretos de preenchimento) nas variáveis de um dataframe. As variáveis e seus códigos esperados são definidos dentro da função. A função retorna um dataframe com o quantitativo e a proporção de códigos inconsistentes por variável.
}
\examples{
# Escreva o nome do dataframe e execute a função. Se seu dataframe for "dados_RHC_combinados", use a função como:

resultados_codigos_inconsistentes <- verificar_codigos_inconsistentes(dados_RHC_combinados)
}
