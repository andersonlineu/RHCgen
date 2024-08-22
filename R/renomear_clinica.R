#' Converte os Códigos das Clínicas de Atendimento e Tratamento de Um Dataframe
#'
#' Esta função mapeia os códigos das clínicas para seus nomes completos nas colunas `Clinica_Atendimento` e `Clinica_Tratamento` de um dataframe. Os nomes completos das clínicas são adicionados em novas colunas `Nome_Clinica_Atendimento` e `Nome_Clinica_Tratamento`.
#'
#' @param dados Um dataframe contendo as colunas `Clinica_Atendimento` e `Clinica_Tratamento`.
#' @return Retorna um dataframe com os nomes completos das clínicas de atendimento e tratamento adicionados em novas colunas.
#' @export
#' @name renomear_clinica
#' @examples
#' # Escreva o nome do dataframe e execute a função. Se seu dataframe for "dados_RHC_combinados", use a função como:
#'
#' dados_RHC_combinados <- renomear_clinica(dados_RHC_combinados)
renomear_clinica <- function(dados) {
  message("Iniciando o ajuste dos códigos das clínicas para nomes completos.")

  # Definição do dataframe de mapeamento de códigos para nomes
  clinica_names <- data.frame(
    Codigo = c(88, 99, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
               17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,
               33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 50),
    Nome = c("Não Se Aplica", "Sem Informação", "Sem Informação", "Alergia/Imunologia", "Cirurgia Cardíaca",
             "Cabeça e Pescoço", "Cirurgia Geral", "Pediatria Cirúrgica", "Cirurgia Plástica",
             "Cirurgia Torácica", "Cirurgia Vascular", "Clínica Médica", "Dermatologia",
             "Endocrinologia", "Gastrocirurgia", "Gastroenterologia", "Geriatria", "Ginecologia",
             "Ginecologia/Obstetrícia", "Hematologia Clínica", "Infectologia", "Nefrologia",
             "Neurocirurgia", "Neurologia", "Oftalmologia", "Oncologia Cirúrgica", "Oncologia Clínica",
             "Pediatria Oncológica", "Ortopedia", "Otorrinolaringologia", "Pediatria", "Pneumologia",
             "Proctologia", "Radioterapia", "Urologia", "Mastologia", "Oncologia Cutânea",
             "Cirurgia Pélvica", "Cirurgia Abdominal", "Odontologia", "Cemo", "Endoscopia",
             "Estômato - Odontologia", "Plástica Reparadora", "Tecido Ósseo e Conectivo",
             "Hemoterapia", "Medicina Nuclear", "Psiquiatria", "Triagem", "Clínica Da Dor",
             "Suporte Terapêutico Oncológico", "Abdômen Pélvica")
  )

  # Verifica se as colunas Clinica_Atendimento e Clinica_Tratamento existem antes de tentar acessá-las
  if (!"Clinica_Atendimento" %in% names(dados)) {
    stop("\033[1;31mA coluna 'Clinica_Atendimento' não foi encontrada no dataframe. Função interrompida.\033[0m")
  }
  if (!"Clinica_Tratamento" %in% names(dados)) {
    stop("\033[1;31mA coluna 'Clinica_Tratamento' não foi encontrada no dataframe. Função interrompida.\033[0m")
  }

  message("Mapeando os códigos das clínicas de atendimento e tratamento para nomes completos.")

  # Mapeamento dos códigos para nomes usando um vetor nomeado
  map <- setNames(clinica_names$Nome, clinica_names$Codigo)

  # Mapeamento para Clinica_Atendimento
  dados$Nome_Clinica_Atendimento <- map[dados$Clinica_Atendimento]

  # Mapeamento para Clinica_Tratamento
  dados$Nome_Clinica_Tratamento <- map[dados$Clinica_Tratamento]

  message("Nomes completos das clínicas de atendimento e tratamento adicionados.")

  message(paste("\033[1;32m", "> Ajuste dos códigos das clínicas concluído com sucesso. Foram adicionadas as colunas Nome_Clinica_Atendimento e Nome_Clinica_Tratamento no dataframe.", "\033[0m"))

  return(dados)
}
