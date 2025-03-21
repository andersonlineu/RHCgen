## RHCgen
***
O RHCgen é um pacote desenvolvido para a linguagem de programação R, concebido com o objetivo de automatizar e padronizar o processamento de dados provenientes do Registro Hospitalar de Câncer (RHC) no Brasil. Este pacote foi criado para abordar os desafios inerentes à manipulação de grandes volumes de dados heterogêneos, característicos dos registros hospitalares de câncer, proporcionando uma ferramenta eficiente, reprodutível e acessível para pesquisadores e profissionais de saúde.

---


### Funções do Pacote RHCgen
| Função                          | Descrição                                                                                                                     |
|---------------------------------|--------------------------------------------------------------------------------------------------------------------------------|
| leraquivoDBF                    | Lê e combina arquivos DBF do Registro Hospitalar de Câncer (RHC) em um único dataframe, garantindo consistência das colunas.    |
| renomear_colunas                | Renomeia colunas de um dataframe com base em uma lista predefinida de mapeamentos, tornando os nomes das colunas mais legíveis. |
| modificar_tipo_variavel         | Converte os tipos de variáveis em um dataframe, garantindo que cada coluna tenha o tipo de dados apropriado.                    |
| recodificar_variaveis           | Recodifica variáveis categóricas em um dataframe conforme o dicionário de variáveis do Registro Hospitalar de Câncer (RHC).     |
| renomear_siglas_estados         | Converte siglas de estados brasileiros em nomes completos nas variáveis de estado de residência e unidade hospitalar.           |
| renomear_CID_3digitos           | Converte os códigos CID-O de 3 dígitos para seus nomes completos.                                                               |
| renomear_CID_4digitos           | Converte os códigos CID-O de 4 dígitos para seus nomes completos.                                                               |
| renomear_CNES                   | Converte os códigos CNES para os nomes completos.                                                                               |
| renomear_tipo_histologico       | Converte os códigos Tipo Histológico para os nomes completos.                                                                   |
| renomear_clinica                | Converte os códigos das Clínicas de Atendimento e Tratamento para os nomes completos.                                           |
| renomear_estadiamento_clinico   | Renomeia o Estadiamento Clínico para a classificação mais geral.                                                                |
| analise_completude              | Calcula e avalia a completude de cada variável em um dataframe, gerando uma tabela e um gráfico de completude.                  |
| analise_completude_ano          | Avalia a completude de cada variável por ano, gerando uma tabela detalhada de completude.                                       |
| verificar_codigos_inconsistentes| Verifica códigos inconsistentes em um dataframe de registros hospitalares de câncer e retorna o quantitativo e a proporção de inconsistências. |
| verificar_idades_inconsistentes | Verifica idades inválidas na coluna Idade de um dataframe de registros hospitalares de câncer e retorna o quantitativo e a proporção de inconsistências. |
| verificar_anos_inconsistentes   | Verifica inconsistências na ordem cronológica dos anos em um dataframe de registros hospitalares de câncer e retorna o quantitativo e a proporção de inconsistências. |
| verificar_datas_inconsistentes  | Verifica inconsistências na ordem cronológica das datas em um dataframe de registros hospitalares de câncer e retorna o quantitativo e a proporção de inconsistências. |
| verificar_cancer_sexo_inconsistentes | Verifica se os códigos CID-O dos cânceres estão de acordo com a variável Sexo em um dataframe de registros hospitalares de câncer e retorna o quantitativo e a proporção de inconsistências. |
| construir_banco                 | Constrói todo o banco de dados a partir de arquivos DBF, automatizando etapas como leitura, junção, renomeação de colunas, modificação de tipos de variáveis, recodificação e ajuste de códigos CID-O, CNES, tipo histológico. Finaliza com a avaliação da completude do banco de dados. |
| filtrar_banco                   | Filtra um dataframe com base em critérios como códigos CID, anos de triagem, anos de diagnóstico, idade, tipo de caso, sexo, estado de residência, unidade hospitalar, primeiro tratamento hospitalar, ano de início do tratamento, ano do banco de dados e origem do encaminhamento. |


---

### Instalação

### Versão de desenvolvimento do GitHub

```r
# install.packages("remotes")
remotes::install_github("andersonlineu/RHCgen")

Obs.: É necessário ter instalado previamente o pacote "remotes".
```

---
### Link do IntegradorRHC

- [IntegradorRHC](https://irhc.inca.gov.br/RHCNet/)

### Drive com os arquivo DBF do RHC

- [Todos os arquivos DBF do RHC 1985-1999](https://1drv.ms/f/s!Al8LbsUUcnUJiPwx8DeFbAkG_YMQfw?e=JXhSgt)

- [Todos os arquivos DBF do RHC 2000-2020](https://1drv.ms/f/s!Al8LbsUUcnUJiJsttNv4HVyHLn7sUg?e=PMWmsL)


### Dúvidas e sugestões
- Email: andersonlineu@gmail.com

- [Lattes](http://lattes.cnpq.br/9727490230028203)

- [Orcid iD](https://orcid.org/0000-0002-1703-9310)

- [Reportar Bugs](https://github.com/andersonlineu/RHCgen/issues)

