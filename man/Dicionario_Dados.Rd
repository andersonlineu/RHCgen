\name{Dicionario_Dados}
\alias{Dicionario_Dados}
\title{Dicionário de Dados do RHC}
\description{
Este é o dicionário de dados disponibilizado pelo RHC com o acréscimo de variáveis criadas após a construção do banco.\cr
}
\section{Dicionário de Variáveis}{
\tabular{ll}{
\strong{Código RHC} \tab \strong{Nome da Variável e Descrição} \cr
TPCASO \tab Tipo_de_Caso: Tipo de caso (analítico ou não analítico). \cr
SEXO \tab Sexo: Sexo do paciente. \cr
IDADE \tab Idade: Idade do paciente. \cr
LOCALNAS \tab Local_Nascimento: Local de nascimento do paciente. \cr
RACACOR \tab Raca_Cor: Raça ou cor do paciente. \cr
INSTRUC \tab Escolaridade: Nível de educação do paciente. \cr
CLIATEN \tab Clinica_Atendimento: Clínica onde o paciente foi atendido. \cr
\tab Nome_Clinica_Atendimento: Nome da clínica onde o paciente foi atendido. \cr
CLITRAT \tab Clinica_Tratamento: Clínica onde o paciente foi tratado. \cr
\tab Nome_Clinica_Tratamento: Nome da clínica onde o paciente foi tratado. \cr
HISTFAMC \tab Historico_Familiar_Cancer: Histórico de câncer na família. \cr
ALCOOLIS \tab Consumo_Alcool: Informação sobre o consumo de álcool pelo paciente. \cr
TABAGISM \tab Tabagismo: Informação sobre o tabagismo do paciente. \cr
ESTADRES \tab Estado_Residencia: Estado de residência do paciente. \cr
\tab Nome_Estado_Residencia: Nome do estado de residência do paciente. \cr
PROCEDEN \tab Codigo_Municipio_Residencia: Código do município de residência do paciente. \cr
\tab Nome_Municipio_Residencia: Nome do município de residência do paciente. \cr
ANOPRIDI \tab Ano_Primeiro_Diagnostico: Ano do primeiro diagnóstico do paciente. \cr
ORIENC \tab Origem_do_Encaminhamento: Origem do encaminhamento para tratamento ou diagnóstico. \cr
EXDIAG \tab Exames_Relevantes_para_Diagnostico: Exames relevantes para o diagnóstico do paciente. \cr
ESTCONJ \tab Estado_conjugal: Estado conjugal do paciente. \cr
ANTRI \tab Ano_Triagem: Ano em que a triagem foi realizada. \cr
DTPRICON \tab Ano_Primeira_Consulta: Ano da primeira consulta do paciente. \cr
DIAGANT \tab Diagnosticos_e_Tratamentos_Anteriores: Diagnósticos e tratamentos anteriores. \cr
BASMAIMP \tab Base_Maior_Importancia_Diagnostico: Base de maior importância para o diagnóstico. \cr
LOCTUDET \tab Localizacao_Primaria_3D: Localização primária do tumor em 3D. \cr
\tab CID3d: Classificação Internacional de Doenças (CID) para a localização primária em 3D. \cr
\tab Classificacao_CID_O: Classificação do câncer segundo a CID-O (Classificação Internacional de Doenças Oncológicas). \cr
LOCTUPRI \tab Localizacao_Primaria_4D: Localização primária do tumor em 4D. \cr
\tab CID4d: Classificação Internacional de Doenças (CID) para a localização primária em 4D. \cr
TIPOHIST \tab Tipo_Histologico: Tipo histológico do tumor. \cr
\tab Tipo_Histologico_Completo: Descrição completa do tipo histológico do tumor. \cr
LATERALI \tab Lateralidade: Lateralidade do tumor. \cr
LOCTUPRO \tab Local_Provavel_Tumor: Local provável do tumor. \cr
MAISUMTU \tab Mais_de_Um_Tumor: Indicação de se há mais de um tumor. \cr
TNM \tab Classificacao_TNM: Classificação TNM do tumor. \cr
ESTADIAM \tab Estadiamento_Clinico: Estadiamento clínico do câncer. \cr
\tab Nome_Estadiamento_Clinico: Nome do estadiamento clínico do câncer. \cr
OUTROESTA \tab Outro_Estadiamento_Clinico: Outras informações de estadiamento clínico. \cr
PTNM \tab TNM_Patologico: TNM patológico. \cr
RZNTR \tab Razao_Nao_Tratamento: Razão pela qual o tratamento não foi realizado. \cr
DTINITRT \tab Ano_Inicio_Tratamento: Ano de início do tratamento. \cr
PRITRATH \tab Primeiro_Tratamento_Hospital: Primeiro tratamento realizado no hospital. \cr
ESTDFIMT \tab Estado_Doenca_Final_Tratamento: Estado final após o tratamento. \cr
CNES \tab CNES_Hospital: Código CNES do hospital. \cr
\tab Estabelecimento_Hospitalar: Estabelecimento hospitalar onde o paciente foi tratado. \cr
UFUH \tab UF_Unidade_Hospital: Unidade federativa do hospital. \cr
\tab Nome_Estado_Hospital: Nome do estado onde o hospital está localizado. \cr
MUUH \tab Municipio_Unidade_Hospital: Município onde o hospital está localizado. \cr
\tab Nome_Municipio_Hospital: Nome do município onde o hospital está localizado. \cr
OCUPACAO \tab Ocupacao_Paciente: Ocupação do paciente. \cr
DTDIAGNO \tab Data_Diagnostico: Data do diagnóstico. \cr
DTTRIAGE \tab Data_Triagem: Data da triagem. \cr
DATAPRICON \tab Data_Primeira_Consulta: Data da primeira consulta. \cr
DATAINITRT \tab Data_Inicio_Primeiro_Tratamento: Data de início do primeiro tratamento. \cr
DATAOBITO \tab Data_Obito: Data de óbito do paciente. \cr
VALOR_TOT \tab Valor_Total: Valor total associado ao tratamento. \cr
BASDIAGSP \tab Base_Mais_Importante_Diagnostico_Sem Patológicas: Base mais importante para o diagnóstico - Sem Patológicas. \cr
ESTADIAG \tab Variável até 1999. Representava o estadiamento no diagnóstico. \cr
\tab Ano_do_Banco: Ano em que os dados foram registrados no banco de dados. \cr
}
}
\seealso{\code{\link{construir_banco}}}
\keyword{documentation}
