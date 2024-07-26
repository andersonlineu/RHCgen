\name{Baixar_Arquivos_DBF}
\alias{Baixar_Arquivos_DBF}
\title{Tutorial para baixar arquivos - RHC}
\description{
Para utilizar o pacote é necessário baixar os arquivos (em .dbf) com os dados de câncer do IntegradorRHC do INCA.\cr
Basta seguir o passo a passo abaixo:\cr
\cr
1. Acesse o site \url{https://irhc.inca.gov.br/RHCNet/visualizaTabNetExterno.action}.\cr
\cr
2. No menu Tabulador Hospitalar, clique em Download.\cr
\cr
3. Em seguida, uma nova janela do navegador será aberta automaticamente. Selecione Base de dados de todos os estados e clique em enviar.\cr
\cr
4. Em uma nova janela que será aberta, selecione Modelo do arquivo de definição para tabwin* (def). Escolha todos os anos que deseja incluir no banco de dados e clique em enviar.\cr
\cr
5. O download dos arquivos começará, e um banner aparecerá com a mensagem: “Seu Download está sendo gerado! Por favor, aguarde..”.\cr
\cr
6. Após o término do download, os arquivos com extensão “.dbf” serão salvos na pasta de downloads do seu computador. Estes arquivos estarão compactados. Descompacte-os e copie todos os arquivos com extensão “.dbf”. Exemplo: rhc18.dbf, rhc19.dbf, rhc20.dbf\cr
\cr
7. Cole os arquivos na mesma pasta onde está localizado o script do R. Isso é essencial para que o pacote RHCgen possa ler os arquivos corretamente.\cr
\cr
8. Abra um novo arquivo no R, selecione R Script.\cr
\cr
9. Carregue o pacote RHCgen usando o comando library(RHCgen).\cr
\cr
10. Utilize a função construir_banco() para criar automaticamente o banco de dados ajustado.\cr
}
\seealso{\code{\link{construir_banco}}}
\keyword{documentation}
