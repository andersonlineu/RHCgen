% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lerarquivoDBF.R
\name{lerarquivoDBF}
\alias{lerarquivoDBF}
\title{Ler e Junta os Arquivo DBF}
\usage{
lerarquivoDBF()
}
\arguments{
\item{none}{Não são necessários parâmetros externos, pois a função opera com base no conteúdo do diretório especificado.}
}
\value{
Retorna um dataframe que junta todos os arquivos DBF no diretório, ou NULL se nenhum arquivo válido for encontrado ou se eles não puderem ser combinados devido a inconsistências nas estruturas de colunas.
É fundamental que todos os arquivos tenham o mesmo nome que vieram do IntegradorRHC e estejam todos na pasta de trabalho.
}
\description{
Esta função ler e junta arquivos DBF do RHC e adiciona uma nova coluna: Ano_do_Banco, a partir do ano do arquivo DBF.
}
\examples{
# Supondo que você tenha no diretório de trabalho arquivos DBF do RHC que permanecem com o mesmo nome que vieram do Integrador (Exemplo: rhc20.dbf, nome padrão do RHC inciando que o ano do banco contidono arquvio é 2020).
# Use a função da seguinte forma:

dados_RHC_combinados <- lerarquivoDBF()
# Se nenhum arquivo for encontrado ou se houver erros ao juntar os arquivos, dados_combinados será NULL.
# Caso contrário, você terá um dataframe com todos os dados combinados dos arquivos DBF.
}
