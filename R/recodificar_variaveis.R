

#' Recodifica as Variáveis em um DataFrame
#'
#' Esta função recodifica variáveis categóricas em um dataframe com base no dicionário de variáveis predefinido pelo RHC. As variáveis são transformadas em fatores com níveis específicos para melhorar a legibilidade e consistência dos dados.
#'
#' @param data Um dataframe que contém as variáveis a serem recodificadas.
#' @return Retorna um dataframe com as variáveis recodificadas.
#' @export
#' @name recodificar_variaveis
#' @examples
#' # Escreva o nome do dataframe e execute a função. Se seu dataframe for "dados_RHC_combinados", use a função como:
#'
#' dados_RHC_combinados <- recodificar_variaveis(dados_RHC_combinados)
recodificar_variaveis <- function(data) {
  message("Iniciando a recodificação das variáveis.")

  # Lista de variáveis esperadas
  variaveis_esperadas <- c("Tipo_de_Caso", "Sexo", "Raca_Cor", "Escolaridade",
                           "Historico_Familiar_Cancer", "Consumo_Alcool", "Tabagismo",
                           "Origem_do_Encaminhamento", "Exames_Relevantes_para_Diagnostico",
                           "Estado_conjugal", "Diagnosticos_e_Tratamentos_Anterior",
                           "Base_Mais_Importante_Diagnostico", "Lateralidade", "Mais_de_Um_Tumor",
                           "Razao_Nao_Tratamento", "Primeiro_Tratamento_Hospital",
                           "Estado_Doenca_Final_Tratamento", "Base_Mais_Importante_Diagnostico_Sem_Patologicas")

  # Verificar variáveis presentes e ausentes
  variaveis_presentes <- variaveis_esperadas[variaveis_esperadas %in% names(data)]
  variaveis_ausentes <- variaveis_esperadas[!variaveis_esperadas %in% names(data)]

  if (length(variaveis_ausentes) > 0) {
    message(paste("As seguintes variáveis estão ausentes no dataframe:", paste(variaveis_ausentes, collapse = ", ")))
  }

  # Contador de variáveis recodificadas
  contador_recodificacoes <- 0
  variaveis_recodificadas <- c()

  # 'Tipo_de_Caso'
  if ("Tipo_de_Caso" %in% names(data)) {
    message("Recodificando 'Tipo_de_Caso'.")
    data$Tipo_de_Caso <- ifelse(data$Tipo_de_Caso == "1", "Analítico",
                                ifelse(data$Tipo_de_Caso == "2", "Não Analítico", data$Tipo_de_Caso))
    data$Tipo_de_Caso <- factor(data$Tipo_de_Caso, levels = c("Analítico", "Não Analítico", "Outro"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Tipo_de_Caso")
  }

  # 'Sexo'
  if ("Sexo" %in% names(data)) {
    message("Recodificando 'Sexo'.")
    data$Sexo <- ifelse(data$Sexo == "1", "Masculino",
                        ifelse(data$Sexo == "2", "Feminino", data$Sexo))
    data$Sexo <- factor(data$Sexo, levels = c("Masculino", "Feminino", "Outro"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Sexo")
  }

  # 'Raca_Cor'
  if ("Raca_Cor" %in% names(data)) {
    message("Recodificando 'Raca_Cor'.")
    data$Raca_Cor <- ifelse(data$Raca_Cor == "1", "Branca",
                            ifelse(data$Raca_Cor == "2", "Preta",
                                   ifelse(data$Raca_Cor == "3", "Amarela",
                                          ifelse(data$Raca_Cor == "4", "Parda",
                                                 ifelse(data$Raca_Cor == "5", "Indígena",
                                                        ifelse(data$Raca_Cor == "9", "Sem informação", data$Raca_Cor))))))
    data$Raca_Cor <- factor(data$Raca_Cor, levels = c("Branca", "Preta", "Amarela", "Parda", "Indígena", "Sem informação"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Raca_Cor")
  }

  # 'Escolaridade'
  if ("Escolaridade" %in% names(data)) {
    message("Recodificando 'Escolaridade'.")
    data$Escolaridade <- ifelse(data$Escolaridade == "1", "Nenhuma",
                                ifelse(data$Escolaridade == "2", "Fundamental incompleto",
                                       ifelse(data$Escolaridade == "3", "Fundamental completo",
                                              ifelse(data$Escolaridade == "4", "Nível médio",
                                                     ifelse(data$Escolaridade == "5", "Nível superior incompleto",
                                                            ifelse(data$Escolaridade == "6", "Nível superior completo",
                                                                   ifelse(data$Escolaridade == "9", "Sem informação", data$Escolaridade)))))))
    data$Escolaridade <- factor(data$Escolaridade, levels = c("Nenhuma", "Fundamental incompleto", "Fundamental completo", "Nível médio", "Nível superior incompleto", "Nível superior completo", "Sem informação"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Escolaridade")
  }

  # 'Historico_Familiar_Cancer'
  if ("Historico_Familiar_Cancer" %in% names(data)) {
    message("Recodificando 'Historico_Familiar_Cancer'.")
    data$Historico_Familiar_Cancer <- ifelse(data$Historico_Familiar_Cancer == "1", "Sim",
                                             ifelse(data$Historico_Familiar_Cancer == "2", "Não",
                                                    ifelse(data$Historico_Familiar_Cancer == "9", "Sem informação", data$Historico_Familiar_Cancer)))
    data$Historico_Familiar_Cancer <- factor(data$Historico_Familiar_Cancer, levels = c("Sim", "Não", "Sem informação"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Historico_Familiar_Cancer")
  }

  # 'Consumo_Alcool'
  if ("Consumo_Alcool" %in% names(data)) {
    message("Recodificando 'Consumo_Alcool'.")
    data$Consumo_Alcool <- ifelse(data$Consumo_Alcool == "1", "Nunca",
                                  ifelse(data$Consumo_Alcool == "2", "Ex-consumidor",
                                         ifelse(data$Consumo_Alcool == "3", "Sim",
                                                ifelse(data$Consumo_Alcool == "4", "Não avaliado",
                                                       ifelse(data$Consumo_Alcool == "8", "Não se aplica",
                                                              ifelse(data$Consumo_Alcool == "9", "Sem informação", data$Consumo_Alcool))))))
    data$Consumo_Alcool <- factor(data$Consumo_Alcool, levels = c("Nunca", "Ex-consumidor", "Sim", "Não avaliado", "Não se aplica", "Sem informação"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Consumo_Alcool")
  }

  # 'Tabagismo'
  if ("Tabagismo" %in% names(data)) {
    message("Recodificando 'Tabagismo'.")
    data$Tabagismo <- ifelse(data$Tabagismo == "1", "Nunca",
                             ifelse(data$Tabagismo == "2", "Ex-consumidor",
                                    ifelse(data$Tabagismo == "3", "Sim",
                                           ifelse(data$Tabagismo == "4", "Não avaliado",
                                                  ifelse(data$Tabagismo == "8", "Não se aplica",
                                                         ifelse(data$Tabagismo == "9", "Sem informação", data$Tabagismo))))))
    data$Tabagismo <- factor(data$Tabagismo, levels = c("Nunca", "Ex-consumidor", "Sim", "Não avaliado", "Não se aplica", "Sem informação"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Tabagismo")
  }

  # 'Origem_do_Encaminhamento'
  if ("Origem_do_Encaminhamento" %in% names(data)) {
    message("Recodificando 'Origem_do_Encaminhamento'.")
    data$Origem_do_Encaminhamento <- ifelse(data$Origem_do_Encaminhamento == "1", "SUS",
                                            ifelse(data$Origem_do_Encaminhamento == "2", "Não SUS",
                                                   ifelse(data$Origem_do_Encaminhamento == "3", "Veio por conta própria",
                                                          ifelse(data$Origem_do_Encaminhamento == "8", "Não se aplica",
                                                                 ifelse(data$Origem_do_Encaminhamento == "9", "Sem informação", data$Origem_do_Encaminhamento)))))
    data$Origem_do_Encaminhamento <- factor(data$Origem_do_Encaminhamento, levels = c("SUS", "Não SUS", "Veio por conta própria", "Não se aplica", "Sem informação"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Origem_do_Encaminhamento")
  }

  # 'Exames_Relevantes_para_Diagnostico'
  if ("Exames_Relevantes_para_Diagnostico" %in% names(data)) {
    message("Recodificando 'Exames_Relevantes_para_Diagnostico'.")
    data$Exames_Relevantes_para_Diagnostico <- ifelse(data$Exames_Relevantes_para_Diagnostico == "1", "Exame clínico e patologia clínica",
                                                      ifelse(data$Exames_Relevantes_para_Diagnostico == "2", "Exames por imagem",
                                                             ifelse(data$Exames_Relevantes_para_Diagnostico == "3", "Endoscopia e cirurgia exploradora",
                                                                    ifelse(data$Exames_Relevantes_para_Diagnostico == "4", "Anatomia patológica",
                                                                           ifelse(data$Exames_Relevantes_para_Diagnostico == "5", "Marcadores tumorais",
                                                                                  ifelse(data$Exames_Relevantes_para_Diagnostico == "8", "Não se aplica",
                                                                                         ifelse(data$Exames_Relevantes_para_Diagnostico == "9", "Sem informação", data$Exames_Relevantes_para_Diagnostico)))))))
    data$Exames_Relevantes_para_Diagnostico <- factor(data$Exames_Relevantes_para_Diagnostico, levels = c("Exame clínico e patologia clínica", "Exames por imagem", "Endoscopia e cirurgia exploradora", "Anatomia patológica", "Marcadores tumorais", "Não se aplica", "Sem informação"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Exames_Relevantes_para_Diagnostico")
  }

  # 'Estado_conjugal'
  if ("Estado_conjugal" %in% names(data)) {
    message("Recodificando 'Estado_conjugal'.")
    data$Estado_conjugal <- ifelse(data$Estado_conjugal == "1", "Solteiro",
                                   ifelse(data$Estado_conjugal == "2", "Casado",
                                          ifelse(data$Estado_conjugal == "3", "Viúvo",
                                                 ifelse(data$Estado_conjugal == "4", "Separado judicialmente",
                                                        ifelse(data$Estado_conjugal == "5", "União consensual",
                                                               ifelse(data$Estado_conjugal == "9", "Sem informação", data$Estado_conjugal))))))
    data$Estado_conjugal <- factor(data$Estado_conjugal, levels = c("Solteiro", "Casado", "Viúvo", "Separado judicialmente", "União consensual", "Sem informação"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Estado_conjugal")
  }

  # 'Diagnosticos_e_Tratamentos_Anterior'
  if ("Diagnosticos_e_Tratamentos_Anterior" %in% names(data)) {
    message("Recodificando 'Diagnosticos_e_Tratamentos_Anterior'.")
    data$Diagnosticos_e_Tratamentos_Anterior <- ifelse(data$Diagnosticos_e_Tratamentos_Anterior == "1", "Sem diagnóstico/Sem tratamento",
                                                       ifelse(data$Diagnosticos_e_Tratamentos_Anterior == "2", "Com diagnóstico/Sem tratamento",
                                                              ifelse(data$Diagnosticos_e_Tratamentos_Anterior == "3", "Com diagnóstico/Com tratamento",
                                                                     ifelse(data$Diagnosticos_e_Tratamentos_Anterior == "4", "Outros",
                                                                            ifelse(data$Diagnosticos_e_Tratamentos_Anterior == "9", "Sem informação", data$Diagnosticos_e_Tratamentos_Anterior)))))
    data$Diagnosticos_e_Tratamentos_Anterior <- factor(data$Diagnosticos_e_Tratamentos_Anterior, levels = c("Sem diagnóstico/Sem tratamento", "Com diagnóstico/Sem tratamento", "Com diagnóstico/Com tratamento", "Outros", "Sem informação"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Diagnosticos_e_Tratamentos_Anterior")
  }

  # 'Base_Mais_Importante_Diagnostico'
  if ("Base_Mais_Importante_Diagnostico" %in% names(data)) {
    message("Recodificando 'Base_Mais_Importante_Diagnostico'.")
    data$Base_Mais_Importante_Diagnostico <- ifelse(data$Base_Mais_Importante_Diagnostico == "1", "Clínica",
                                                    ifelse(data$Base_Mais_Importante_Diagnostico == "2", "Pesquisa clínica",
                                                           ifelse(data$Base_Mais_Importante_Diagnostico == "3", "Exame por imagem",
                                                                  ifelse(data$Base_Mais_Importante_Diagnostico == "4", "Marcadores tumorais",
                                                                         ifelse(data$Base_Mais_Importante_Diagnostico == "5", "Citologia",
                                                                                ifelse(data$Base_Mais_Importante_Diagnostico == "6", "Histologia da metástase",
                                                                                       ifelse(data$Base_Mais_Importante_Diagnostico == "7", "Histologia do tumor primário",
                                                                                              ifelse(data$Base_Mais_Importante_Diagnostico == "9", "Sem informação", data$Base_Mais_Importante_Diagnostico))))))))
    data$Base_Mais_Importante_Diagnostico <- factor(data$Base_Mais_Importante_Diagnostico, levels = c("Clínica", "Pesquisa clínica", "Exame por imagem", "Marcadores tumorais", "Citologia", "Histologia da metástase", "Histologia do tumor primário", "Sem informação"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Base_Mais_Importante_Diagnostico")
  }

  # 'Lateralidade'
  if ("Lateralidade" %in% names(data)) {
    message("Recodificando 'Lateralidade'.")
    data$Lateralidade <- ifelse(data$Lateralidade == "1", "Direita",
                                ifelse(data$Lateralidade == "2", "Esquerda",
                                       ifelse(data$Lateralidade == "3", "Bilateral",
                                              ifelse(data$Lateralidade == "8", "Não se aplica",
                                                     ifelse(data$Lateralidade == "9", "Sem informação", data$Lateralidade)))))
    data$Lateralidade <- factor(data$Lateralidade, levels = c("Direita", "Esquerda", "Bilateral", "Não se aplica", "Sem informação"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Lateralidade")
  }

  # 'Mais_de_Um_Tumor'
  if ("Mais_de_Um_Tumor" %in% names(data)) {
    message("Recodificando 'Mais_de_Um_Tumor'.")
    data$Mais_de_Um_Tumor <- ifelse(data$Mais_de_Um_Tumor == "1", "Não",
                                    ifelse(data$Mais_de_Um_Tumor == "2", "Sim",
                                           ifelse(data$Mais_de_Um_Tumor == "3", "Duvidoso", data$Mais_de_Um_Tumor)))
    data$Mais_de_Um_Tumor <- factor(data$Mais_de_Um_Tumor, levels = c("Sim", "Não", "Duvidoso"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Mais_de_Um_Tumor")
  }

  # 'Razao_Nao_Tratamento'
  if ("Razao_Nao_Tratamento" %in% names(data)) {
    message("Recodificando 'Razao_Nao_Tratamento'.")
    data$Razao_Nao_Tratamento <- ifelse(data$Razao_Nao_Tratamento == "1", "Recusa do tratamento",
                                        ifelse(data$Razao_Nao_Tratamento == "2", "Tratamento realizado fora",
                                               ifelse(data$Razao_Nao_Tratamento == "3", "Doença avançada, falta de condições clínicas ou outras doenças associadas",
                                                      ifelse(data$Razao_Nao_Tratamento == "4", "Abandono do tratamento",
                                                             ifelse(data$Razao_Nao_Tratamento == "5", "Complicações de tratamento",
                                                                    ifelse(data$Razao_Nao_Tratamento == "6", "Óbito",
                                                                           ifelse(data$Razao_Nao_Tratamento == "7", "Outras razões",
                                                                                  ifelse(data$Razao_Nao_Tratamento == "8", "Não se aplica",
                                                                                         ifelse(data$Razao_Nao_Tratamento == "9", "Sem informação", data$Razao_Nao_Tratamento)))))))))
    data$Razao_Nao_Tratamento <- factor(data$Razao_Nao_Tratamento, levels = c("Recusa do tratamento", "Tratamento realizado fora", "Doença avançada, falta de condições clínicas ou outras doenças associadas", "Abandono do tratamento", "Complicações de tratamento", "Óbito", "Outras razões", "Não se aplica", "Sem informação"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Razao_Nao_Tratamento")
  }

  # 'Primeiro_Tratamento_Hospital'
  if ("Primeiro_Tratamento_Hospital" %in% names(data)) {
    message("Recodificando 'Primeiro_Tratamento_Hospital'.")
    data$Primeiro_Tratamento_Hospital <- ifelse(data$Primeiro_Tratamento_Hospital == "1", "Nenhum",
                                                ifelse(data$Primeiro_Tratamento_Hospital == "2", "Cirurgia",
                                                       ifelse(data$Primeiro_Tratamento_Hospital == "3", "Radioterapia",
                                                              ifelse(data$Primeiro_Tratamento_Hospital == "4", "Quimioterapia",
                                                                     ifelse(data$Primeiro_Tratamento_Hospital == "5", "Hormonioterapia",
                                                                            ifelse(data$Primeiro_Tratamento_Hospital == "6", "Transplante de medula óssea",
                                                                                   ifelse(data$Primeiro_Tratamento_Hospital == "7", "Imunoterapia",
                                                                                          ifelse(data$Primeiro_Tratamento_Hospital == "8", "Outras",
                                                                                                 ifelse(data$Primeiro_Tratamento_Hospital == "9", "Sem informação", data$Primeiro_Tratamento_Hospital)))))))))
    data$Primeiro_Tratamento_Hospital <- factor(data$Primeiro_Tratamento_Hospital, levels = c("Nenhum", "Cirurgia", "Radioterapia", "Quimioterapia", "Hormonioterapia", "Transplante de medula óssea", "Imunoterapia", "Outras", "Sem informação"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Primeiro_Tratamento_Hospital")
  }

  # 'Estado_Doenca_Final_Tratamento'
  if ("Estado_Doenca_Final_Tratamento" %in% names(data)) {
    message("Recodificando 'Estado_Doenca_Final_Tratamento'.")
    data$Estado_Doenca_Final_Tratamento <- ifelse(data$Estado_Doenca_Final_Tratamento == "1", "Sem evidência da doença (remissão completa)",
                                                  ifelse(data$Estado_Doenca_Final_Tratamento == "2", "Remissão parcial",
                                                         ifelse(data$Estado_Doenca_Final_Tratamento == "3", "Doença estável",
                                                                ifelse(data$Estado_Doenca_Final_Tratamento == "4", "Doença em progressão",
                                                                       ifelse(data$Estado_Doenca_Final_Tratamento == "5", "Suporte terapêutico oncológico",
                                                                              ifelse(data$Estado_Doenca_Final_Tratamento == "6", "Óbito",
                                                                                     ifelse(data$Estado_Doenca_Final_Tratamento == "8", "Não se aplica",
                                                                                            ifelse(data$Estado_Doenca_Final_Tratamento == "9", "Sem informação", data$Estado_Doenca_Final_Tratamento))))))))
    data$Estado_Doenca_Final_Tratamento <- factor(data$Estado_Doenca_Final_Tratamento, levels = c("Sem evidência da doença (remissão completa)", "Remissão parcial", "Doença estável", "Doença em progressão", "Suporte terapêutico oncológico", "Óbito", "Não se aplica", "Sem informação"))
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Estado_Doenca_Final_Tratamento")
  }

  # 'Base_Mais_Importante_Diagnostico_Sem_Patologicas'
  if ("Base_Mais_Importante_Diagnostico_Sem_Patologicas" %in% names(data)) {
    message("Recodificando 'Base_Mais_Importante_Diagnostico_Sem_Patologicas'.")
    data$Base_Mais_Importante_Diagnostico_Sem_Patologicas <- ifelse(
      data$Base_Mais_Importante_Diagnostico_Sem_Patologicas == "1", "Exame clínico",
      ifelse(data$Base_Mais_Importante_Diagnostico_Sem_Patologicas == "2", "Recursos auxiliares não microscópicos",
             ifelse(data$Base_Mais_Importante_Diagnostico_Sem_Patologicas == "3", "Confirmação microscópica",
                    ifelse(data$Base_Mais_Importante_Diagnostico_Sem_Patologicas == "4", "Sem informação", data$Base_Mais_Importante_Diagnostico_Sem_Patologicas)
             )
      )
    )
    data$Base_Mais_Importante_Diagnostico_Sem_Patologicas <- factor(
      data$Base_Mais_Importante_Diagnostico_Sem_Patologicas,
      levels = c("Exame clínico", "Recursos auxiliares não microscópicos", "Confirmação microscópica", "Sem informação")
    )
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Base_Mais_Importante_Diagnostico_Sem_Patologicas")
  }

  # 'Estadiamento_Clinico_TNM_grupo_1985_1999'
  if ("Estadiamento_Clinico_TNM_grupo_1985_1999" %in% names(data)) {
    message("Recodificando 'Estadiamento_Clinico_TNM_grupo_1985_1999'.")
    data$Estadiamento_Clinico_TNM_grupo_1985_1999 <- ifelse(
      data$Estadiamento_Clinico_TNM_grupo_1985_1999 == "1", "Estádio I",
      ifelse(
        data$Estadiamento_Clinico_TNM_grupo_1985_1999 == "2", "Estádio II",
        ifelse(
          data$Estadiamento_Clinico_TNM_grupo_1985_1999 == "3", "Estádio III",
          ifelse(
            data$Estadiamento_Clinico_TNM_grupo_1985_1999 == "4", "Estádio IV",
            ifelse(
              data$Estadiamento_Clinico_TNM_grupo_1985_1999 == "8", "Não se aplica",
              ifelse(
                data$Estadiamento_Clinico_TNM_grupo_1985_1999 == "9", "Sem informação",
                data$Estadiamento_Clinico_TNM_grupo_1985_1999
              )
            )
          )
        )
      )
    )
    data$Estadiamento_Clinico_TNM_grupo_1985_1999 <- factor(
      data$Estadiamento_Clinico_TNM_grupo_1985_1999,
      levels = c("Estádio I", "Estádio II", "Estádio III", "Estádio IV", "Não se aplica", "Sem informação")
    )
    contador_recodificacoes <- contador_recodificacoes + 1
    variaveis_recodificadas <- c(variaveis_recodificadas, "Estadiamento_Clinico_TNM_grupo_1985_1999")
  }



  if (length(variaveis_ausentes) > 0) {
    message(paste("Variáveis ausentes:", paste(variaveis_ausentes, collapse = ", ")))
  }

  message(paste("Variáveis recodificadas:", paste(variaveis_recodificadas, collapse = ", ")))

  message(paste("\033[1;32m", "> Recodificação das variáveis concluída. Total:", contador_recodificacoes, "com sucesso \033[0m"))

  return(data)
}

