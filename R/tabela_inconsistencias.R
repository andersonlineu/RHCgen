#' Executa Verificações e Salva Resultado em HTML
#'
#' Esta função executa verificações de múltiplas inconsistências em um dataframe,
#' unifica os resultados em seções e salva as tabelas em um arquivo HTML.
#'
#' @param data Um dataframe contendo as colunas requeridas para todas as verificações.
#' @param output_file Caminho completo para o arquivo HTML onde os resultados serão salvos.
#' @return Abre o arquivo HTML no navegador padrão do sistema.
#' @export
#' @name tabela_inconsistencias
#' @examples
#' tabela_inconsistencias(dados_RHC_combinados)
tabela_inconsistencias <- function(data, output_file = "resultados_verificacoes.html") {
  # Verificar inconsistências em várias dimensões
  resultados_codigos_inconsistentes <- verificar_codigos_inconsistentes(data)
  resultados_idades_invalidas <- verificar_idades_inconsistentes(data)
  resultados_datas_inconsistentes <- verificar_datas_inconsistentes(data)
  resultados_anos_inconsistentes <- verificar_anos_inconsistentes(data)
  resultados_cancer_sexo_inconsistentes <- verificar_cancer_sexo_inconsistentes(data)

  # Padronizar nomes das colunas
  colnames(resultados_codigos_inconsistentes) <- c("Descricao", "Quantidade", "Proporcao")
  colnames(resultados_idades_invalidas) <- c("Descricao", "Quantidade", "Proporcao")
  colnames(resultados_datas_inconsistentes) <- c("Descricao", "Quantidade", "Proporcao")
  colnames(resultados_anos_inconsistentes) <- c("Descricao", "Quantidade", "Proporcao")
  colnames(resultados_cancer_sexo_inconsistentes) <- c("Descricao", "Quantidade", "Proporcao")

  # Criar títulos para cada seção
  titulos <- list(
    "<h3><b>Inconsistências de Códigos</b></h3>" = resultados_codigos_inconsistentes,
    "<h3><b>Inconsistências de Idades</b></h3>" = resultados_idades_invalidas,
    "<h3><b>Inconsistências de Datas</b></h3>" = resultados_datas_inconsistentes,
    "<h3><b>Inconsistências de Anos</b></h3>" = resultados_anos_inconsistentes,
    "<h3><b>Inconsistências entre Tipo de Câncer e Sexo</b></h3>" = resultados_cancer_sexo_inconsistentes
  )

  # Criar o HTML básico
  html_content <- paste0(
    "<!DOCTYPE html>",
    "<html lang='pt-br'>",
    "<head>",
    "<meta charset='UTF-8'>",
    "<title>Resultados das Verificações</title>",
    "<style>",
    "table {border-collapse: collapse; width: 100%; margin: 20px 0;}",
    "th, td {border: 1px solid #ddd; padding: 8px; text-align: left;}",
    "th {background-color: #f4f4f4; font-weight: bold;}",
    "</style>",
    "</head>",
    "<body>",
    "<h2>Resultados das Verificações</h2>"
  )

  # Adicionar cada seção com seu título e tabela
  for (titulo in names(titulos)) {
    tabela <- titulos[[titulo]]

    # Adicionar descrição específica para códigos inconsistentes
    if (titulo == "<h3><b>Inconsistências de Códigos</b></h3>") {
      descricao_codigos <- paste0(
        "<p>Verificou-se se há códigos inconsistentes (campos preenchidos com códigos inexistentes na Ficha de Registro de Tumor ou com formatos incorretos de preenchimento)",
        " em todas as variáveis do seu dataframe.</p>"
      )
      html_content <- paste0(html_content, titulo, descricao_codigos)
    }

    # Adicionar descrição específica para idades inválidas
    if (titulo == "<h3><b>Inconsistências de Idades</b></h3>") {
      descricao_idades <- paste0(
        "<p>Verificou-se se há idades inválidas (menores que zero ou maiores que 150 anos, ou erro de preenchimento)",
        " na coluna 'Idade' do seu dataframe.</p>"
      )
      html_content <- paste0(html_content, titulo, descricao_idades)
    }

    # Adicionar descrição específica para datas inconsistentes
    if (titulo == "<h3><b>Inconsistências de Datas</b></h3>") {
      descricao_datas <- paste0(
        "<p>Verificou-se se há inconsistências na ordem cronológica das datas fornecidas no seu dataframe. As verificações incluem:</p>",
        "<ul>",
        "<li>'Data_Diagnostico' deve ser posterior a 'Data_Primeira_Consulta'.</li>",
        "<li>'Data_Diagnostico' deve ser posterior a 'Data_Triagem'.</li>",
        "<li>'Data_Inicio_Primeiro_Tratamento' deve ser posterior a 'Data_Primeira_Consulta'.</li>",
        "<li>'Data_Inicio_Primeiro_Tratamento' deve ser posterior a 'Data_Triagem'.</li>",
        "<li>'Data_Inicio_Primeiro_Tratamento' deve ser posterior a 'Data_Diagnostico'.</li>",
        "<li>'Data_Obito' deve ser posterior a 'Data_Primeira_Consulta'.</li>",
        "<li>'Data_Obito' deve ser posterior a 'Data_Triagem'.</li>",
        "<li>'Data_Obito' deve ser posterior a 'Data_Diagnostico'.</li>",
        "<li>'Data_Obito' deve ser posterior a 'Data_Inicio_Primeiro_Tratamento'.</li>",
        "</ul>",
        "<p>Foi considerada uma diferença mínima de 12 meses (365 dias) para ser inconsistente.</p>"
      )
      html_content <- paste0(html_content, titulo, descricao_datas)
    }

    # Adicionar descrição específica para anos inconsistentes
    if (titulo == "<h3><b>Inconsistências de Anos</b></h3>") {
      descricao_anos <- paste0(
        "<p>Verificou-se se há inconsistências na ordem cronológica dos anos fornecidos no seu dataframe. As verificações incluem:</p>",
        "<ul>",
        "<li>'Ano_Primeiro_Diagnostico' deve ser igual ou posterior a 'Ano_Primeira_Consulta'.</li>",
        "<li>'Ano_Primeiro_Diagnostico' deve ser igual ou posterior a 'Ano_Triagem'.</li>",
        "<li>'Ano_Inicio_Tratamento' deve ser igual ou posterior a 'Ano_Primeira_Consulta'.</li>",
        "<li>'Ano_Inicio_Tratamento' deve ser igual ou posterior a 'Ano_Triagem'.</li>",
        "<li>'Ano_Inicio_Tratamento' deve ser igual ou posterior a 'Ano_Primeiro_Diagnostico'.</li>",
        "</ul>"
      )
      html_content <- paste0(html_content, titulo, descricao_anos)
    }

    # Adicionar descrição específica para câncer e sexo
    if (titulo == "<h3><b>Inconsistências entre Tipo de Câncer e Sexo</b></h3>") {
      descricao_cancer_sexo <- paste0(
        "<p>Verificou-se no CID-O para avaliar se os cânceres relacionados ao sexo estão de acordo com a variável 'Sexo'.",
        " As verificações incluem:</p>",
        "<ul>",
        "<li>CID-O de cânceres femininos (C53, C54, C55, C56, C57, C51, C52) devem corresponder ao sexo 'Feminino'.</li>",
        "<li>CID-O de cânceres masculinos (C60, C61, C62) devem corresponder ao sexo 'Masculino'.</li>",
        "</ul>"
      )
      html_content <- paste0(html_content, titulo, descricao_cancer_sexo)
    }

    # Adicionar tabela
    html_content <- paste0(html_content, "<table><thead><tr><th>Descrição</th><th>Quantidade de Inconsistências</th><th>Proporção</th></tr></thead><tbody>")
    for (i in 1:nrow(tabela)) {
      html_content <- paste0(
        html_content,
        "<tr>",
        "<td>", tabela[i, "Descricao"], "</td>",
        "<td>", tabela[i, "Quantidade"], "</td>",
        "<td>", tabela[i, "Proporcao"], "</td>",
        "</tr>"
      )
    }
    html_content <- paste0(html_content, "</tbody></table>")
  }

  # Finalizar o HTML
  html_content <- paste0(html_content, "</body></html>")

  # Salvar o HTML temporariamente
  writeLines(html_content, con = output_file)

  # Abrir no navegador padrão
  browseURL(output_file)
}
