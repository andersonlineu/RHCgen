## RHCgen
***
**RHCgen** é um pacote desenvolvido em R para automatizar e padronizar o processamento de dados do Registro Hospitalar de Câncer (RHC) no Brasil. O pacote implementa um pipeline completo de preparação de dados, incluindo leitura, harmonização estrutural, padronização semântica e avaliação da qualidade dos dados.

Foi concebido para lidar com grandes volumes de dados heterogêneos, provenientes de diferentes períodos e versões do RHC, permitindo a construção reprodutível de bases analíticas.

O desenvolvimento do RHCgen é um produto que integra a minha tese de doutorado em Epidemiologia em Saúde Pública da Escola Nacional de Saúde Pública Sergio Arouca (ENSP/Fiocruz).

---

## Principais funcionalidades

O RHCgen organiza o processamento dos dados em quatro etapas principais:

### 1. Leitura e integração
- `lerarquivoDBF` — leitura e consolidação de arquivos `.dbf`

### 2. Padronização estrutural e semântica
- `renomear_colunas`
- `modificar_tipo_variavel`
- `recodificar_variaveis`
- `renomear_siglas_estados`
- `renomear_CID_3digitos`
- `renomear_CID_4digitos`
- `renomear_CNES`
- `renomear_tipo_histologico`
- `renomear_clinica`
- `renomear_estadiamento_clinico`

### 3. Avaliação da qualidade dos dados
- `analise_completude`
- `analise_completude_ano`
- `verificar_codigos_inconsistentes`
- `verificar_idades_inconsistentes`
- `verificar_anos_inconsistentes`
- `verificar_datas_inconsistentes`
- `verificar_cancer_sexo_inconsistentes`

### 4. Construção e filtragem da base analítica
- `construir_banco` — execução integrada do pipeline
- `filtrar_banco` — seleção de subconjuntos analíticos

---

### Instalação

### Versão de desenvolvimento do GitHub. RHCgen 1.0

```r
# install.packages("remotes")
remotes::install_github("andersonlineu/RHCgen")

Obs.: É necessário ter instalado previamente o pacote "remotes".
```

---
### Link do IntegradorRHC

- [IntegradorRHC](https://irhc.inca.gov.br/RHCNet/)


### Dúvidas e sugestões
- Email: andersonlineu@gmail.com

- [Lattes](http://lattes.cnpq.br/9727490230028203)

- [Orcid iD](https://orcid.org/0000-0002-1703-9310)

- [Reportar Bugs](https://github.com/andersonlineu/RHCgen/issues)

