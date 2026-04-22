#' Executa Verificações e Salva Resultado em HTML
#'
#' Esta função executa verificações de múltiplas inconsistências em um dataframe,
#' organiza os resultados em seções analíticas e salva as tabelas em um arquivo HTML
#' mais claro, interpretável e visualmente estruturado.
#'
#' @param data Um dataframe contendo as colunas requeridas para todas as verificações.
#' @param output_file Caminho completo para o arquivo HTML onde os resultados serão salvos.
#' @return Abre o arquivo HTML no navegador padrão do sistema.
#' @export
#' @name tabela_inconsistencias2
#' @examples
#' tabela_inconsistencias2(dados_RHC_combinados)
tabela_inconsistencias <- function(data, output_file = "resultados_verificacoes.html") {

  # =========================
  # 1. Funções auxiliares
  # =========================

  formatar_n <- function(x) {
    format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
  }

  formatar_prop <- function(x) {
    if (is.numeric(x)) {
      return(sprintf("%.2f%%", x))
    }

    x <- gsub("%", "", x)
    x <- gsub(",", ".", x)
    x_num <- suppressWarnings(as.numeric(x))

    ifelse(
      is.na(x_num),
      x,
      sprintf("%.2f%%", x_num)
    )
  }

  extrair_prop_num <- function(x) {
    if (is.numeric(x)) return(x)
    x <- gsub("%", "", x)
    x <- gsub(",", ".", x)
    suppressWarnings(as.numeric(x))
  }

  classificar_gravidade <- function(p) {
    if (is.na(p)) {
      return("Não classificado")
    } else if (p < 1) {
      return("Baixo")
    } else if (p < 5) {
      return("Moderado")
    } else if (p < 20) {
      return("Alto")
    } else {
      return("Crítico")
    }
  }

  classe_css_gravidade <- function(grv) {
    dplyr::case_when(
      grv == "Baixo" ~ "baixo",
      grv == "Moderado" ~ "moderado",
      grv == "Alto" ~ "alto",
      grv == "Crítico" ~ "critico",
      TRUE ~ "na"
    )
  }

  resumo_secao <- function(df, nome_secao) {

    if (nrow(df) == 0) {
      return("<p class='interpretacao'>Nenhuma inconsistência foi identificada nesta seção.</p>")
    }

    # 🔴 CASO ESPECIAL: só uma linha (ex: Idade)
    if (nrow(df) == 1) {
      return(paste0(
        "<div class='interpretacao'>",
        "<h4>Interpretação</h4>",
        "<p>Na seção <b>", nome_secao, "</b>, ",
        df$Proporcao, " dos registros apresentaram inconsistências na variável <b>",
        df$Descricao, "</b>, correspondendo a um nível de gravidade <b>",
        df$Gravidade, "</b>.</p>",
        "</div>"
      ))
    }

    # 🟣 CASO ESPECIAL: duas linhas com mesma proporção
    if (nrow(df) == 2 && length(unique(df$Proporcao_num)) == 1) {

      return(paste0(
        "<div class='interpretacao'>",
        "<h4>Interpretação</h4>",
        "<p>Na seção <b>", nome_secao, "</b>, observou-se baixa frequência de inconsistências em ambas as categorias avaliadas, ",
        "com proporções idênticas (<b>", df$Proporcao[1], "</b>) entre <b>",
        df$Descricao[1], "</b> e <b>", df$Descricao[2],
        "</b>, indicando elevada consistência entre as variáveis analisadas.</p>",
        "</div>"
      ))
    }

    # 🟢 CASO NORMAL: múltiplas variáveis
    top1 <- df[1, , drop = FALSE]

    n_critico <- sum(df$Gravidade == "Crítico", na.rm = TRUE)
    n_alto <- sum(df$Gravidade == "Alto", na.rm = TRUE)
    n_mod <- sum(df$Gravidade == "Moderado", na.rm = TRUE)
    n_baixo <- sum(df$Gravidade == "Baixo", na.rm = TRUE)

    return(paste0(
      "<div class='interpretacao'>",
      "<h4>Interpretação</h4>",
      "<p>Na seção <b>", nome_secao, "</b>, destaca-se como principal inconsistência a variável <b>",
      top1$Descricao, "</b>, com <b>", top1$Proporcao, "</b> dos registros avaliados.</p>",
      "<p>Distribuição da gravidade: <b>", n_critico, "</b> crítica(s), <b>",
      n_alto, "</b> alta(s), <b>", n_mod, "</b> moderada(s) e <b>",
      n_baixo, "</b> baixa(s).</p>",
      "</div>"
    ))
  }

  montar_tabela_html <- function(df, incluir_n_completos = FALSE) {
    if (nrow(df) == 0) {
      return("<p>Nenhuma inconsistência identificada.</p>")
    }

    html <- "<table><thead><tr>"
    html <- paste0(
      html,
      "<th>Variável analisada</th>",
      "<th>Nº de inconsistências</th>",
      "<th>Proporção (%)</th>",
      "<th>Classificação</th>"
    )

    if (incluir_n_completos) {
      html <- paste0(html, "<th>Nº de registros completos</th>")
    }

    html <- paste0(html, "</tr></thead><tbody>")

    for (i in seq_len(nrow(df))) {
      classe <- classe_css_gravidade(df$Gravidade[i])

      html <- paste0(html, "<tr class='", classe, "'>")
      html <- paste0(html, "<td>", df$Descricao[i], "</td>")
      html <- paste0(html, "<td>", df$Quantidade[i], "</td>")
      html <- paste0(html, "<td>", df$Proporcao[i], "</td>")
      html <- paste0(html, "<td>", df$Gravidade[i], "</td>")

      if (incluir_n_completos) {
        html <- paste0(html, "<td>", df$N_Registros_Completos[i], "</td>")
      }

      html <- paste0(html, "</tr>")
    }

    html <- paste0(html, "</tbody></table>")
    html
  }

  preparar_resultado <- function(df, tem_n_completos = FALSE) {
    if (nrow(df) == 0) return(df)

    if (tem_n_completos) {
      names(df) <- c("Descricao", "Quantidade", "Proporcao", "N_Registros_Completos")
    } else {
      names(df) <- c("Descricao", "Quantidade", "Proporcao")
    }

    df$Proporcao_num <- extrair_prop_num(df$Proporcao)
    df$Proporcao <- formatar_prop(df$Proporcao)
    df$Gravidade <- vapply(df$Proporcao_num, classificar_gravidade, character(1))

    if ("Quantidade" %in% names(df)) {
      df$Quantidade <- formatar_n(df$Quantidade)
    }

    if ("N_Registros_Completos" %in% names(df)) {
      df$N_Registros_Completos <- formatar_n(df$N_Registros_Completos)
    }

    df <- df[order(-df$Proporcao_num, df$Descricao), , drop = FALSE]
    df
  }

  # =========================
  # 2. Executar verificações
  # =========================

  resultados_codigos_inconsistentes <- verificar_codigos_inconsistentes(data)
  resultados_idades_invalidas <- verificar_idades_inconsistentes(data)
  resultados_datas_inconsistentes <- verificar_datas_inconsistentes(data)
  resultados_anos_inconsistentes <- verificar_anos_inconsistentes(data)
  resultados_cancer_sexo_inconsistentes <- verificar_cancer_sexo_inconsistentes(data)

  resultados_codigos_inconsistentes <- preparar_resultado(
    resultados_codigos_inconsistentes,
    tem_n_completos = "N_Registros_Completos" %in% colnames(resultados_codigos_inconsistentes)
  )

  resultados_idades_invalidas <- preparar_resultado(
    resultados_idades_invalidas,
    tem_n_completos = FALSE
  )

  resultados_datas_inconsistentes <- preparar_resultado(
    resultados_datas_inconsistentes,
    tem_n_completos = FALSE
  )

  resultados_anos_inconsistentes <- preparar_resultado(
    resultados_anos_inconsistentes,
    tem_n_completos = FALSE
  )

  resultados_cancer_sexo_inconsistentes <- preparar_resultado(
    resultados_cancer_sexo_inconsistentes,
    tem_n_completos = "N_Registros_Completos" %in% colnames(resultados_cancer_sexo_inconsistentes)
  )

  # =========================
  # 3. Totais e títulos
  # =========================

  total_registros <- nrow(data)
  total_registros_formatado <- formatar_n(total_registros)

  if ("Idade" %in% colnames(data)) {
    n_registros_completos_idade <- sum(!is.na(data$Idade))
    n_registros_completos_idade_formatado <- formatar_n(n_registros_completos_idade)
  } else {
    n_registros_completos_idade_formatado <- "N/A"
  }

  required_date_cols <- c(
    "Data_Diagnostico",
    "Data_Primeira_Consulta",
    "Data_Triagem",
    "Data_Inicio_Primeiro_Tratamento",
    "Data_Obito"
  )

  present_date_cols <- required_date_cols[required_date_cols %in% colnames(data)]

  if (length(present_date_cols) > 0) {
    n_registros_completos_total_datas <- sum(rowSums(!is.na(data[, present_date_cols, drop = FALSE])) > 0)
    n_registros_completos_total_datas_formatado <- formatar_n(n_registros_completos_total_datas)
  } else {
    n_registros_completos_total_datas_formatado <- "N/A"
  }

  titulo_tabela1 <- paste0(
    "Tabela 1 – Frequência e proporção de inconsistências por variável na etapa de validação de códigos (n = ",
    total_registros_formatado, ")"
  )
  titulo_tabela2 <- paste0(
    "Tabela 2 – Frequência e proporção de inconsistências na variável Idade (n de registros completos = ",
    n_registros_completos_idade_formatado, ")"
  )
  titulo_tabela3 <- paste0(
    "Tabela 3 – Frequência e proporção de inconsistências na coerência lógico-temporal de datas (n de registros completos = ",
    n_registros_completos_total_datas_formatado, ")"
  )
  titulo_tabela4 <- paste0(
    "Tabela 4 – Frequência e proporção de inconsistências na coerência lógico-temporal dos anos dos eventos clínicos (n = ",
    total_registros_formatado, ")"
  )
  titulo_tabela5 <- paste0(
    "Tabela 5 – Frequência e proporção de inconsistências na coerência entre sexo e topografia tumoral (n = ",
    total_registros_formatado, ")"
  )

  # =========================
  # 4. Resumo executivo
  # =========================

  juntar_top <- function(df, secao) {
    if (nrow(df) == 0) return(NULL)
    data.frame(
      Secao = secao,
      Descricao = df$Descricao,
      Proporcao = df$Proporcao,
      Proporcao_num = df$Proporcao_num,
      Gravidade = df$Gravidade,
      stringsAsFactors = FALSE
    )
  }

  painel_resumo <- dplyr::bind_rows(
    juntar_top(resultados_codigos_inconsistentes, "Validação de códigos"),
    juntar_top(resultados_idades_invalidas, "Idade"),
    juntar_top(resultados_datas_inconsistentes, "Datas clínicas"),
    juntar_top(resultados_anos_inconsistentes, "Anos dos eventos clínicos"),
    juntar_top(resultados_cancer_sexo_inconsistentes, "Sexo e topografia tumoral")
  )

  painel_resumo <- painel_resumo[order(-painel_resumo$Proporcao_num), , drop = FALSE]

  top_resumo <- if (nrow(painel_resumo) > 0) utils::head(painel_resumo, 5) else NULL

  resumo_html <- ""
  if (!is.null(top_resumo) && nrow(top_resumo) > 0) {
    itens_resumo <- paste0(
      "<li><b>", top_resumo$Descricao, "</b> (",
      top_resumo$Secao, "): ",
      top_resumo$Proporcao, " – ",
      top_resumo$Gravidade, "</li>",
      collapse = ""
    )

    n_criticos_total <- sum(painel_resumo$Gravidade == "Crítico", na.rm = TRUE)
    n_altos_total <- sum(painel_resumo$Gravidade == "Alto", na.rm = TRUE)

    resumo_html <- paste0(
      "<div class='resumo-box'>",
      "<h3>Resumo executivo</h3>",
      "<p>Foram analisados <b>", total_registros_formatado, "</b> registros. ",
      "Este relatório apresenta a frequência e a proporção de inconsistências identificadas em diferentes dimensões da base.</p>",
      "<p>No conjunto das verificações, foram identificadas <b>", n_criticos_total, "</b> inconsistência(s) classificadas como críticas e <b>",
      n_altos_total, "</b> classificadas como altas.</p>",
      "<p><b>Principais achados:</b></p>",
      "<ul>", itens_resumo, "</ul>",
      "</div>"
    )
  }

  # =========================
  # 5. Construção do HTML
  # =========================

  html_content <- paste0(
    "<!DOCTYPE html>",
    "<html lang='pt-br'>",
    "<head>",
    "<meta charset='UTF-8'>",
    "<title>Resultados das Verificações</title>",
    "<style>",
    "body { font-family: Arial, sans-serif; line-height: 1.6; margin: 0; background: #f7f8fa; color: #222; }",
    ".container { max-width: 1200px; margin: 0 auto; background: #ffffff; padding: 30px; box-shadow: 0 2px 10px rgba(0,0,0,0.06); }",
    "h1 { text-align: center; color: #17324d; margin-bottom: 10px; }",
    "h2 { color: #17324d; border-bottom: 2px solid #e5e7eb; padding-bottom: 6px; margin-top: 32px; }",
    "h3 { color: #17324d; margin-top: 28px; }",
    "h4 { color: #264653; margin-top: 20px; }",
    "p { margin-bottom: 12px; text-align: justify; }",
    "ul { margin-left: 22px; }",
    ".resumo-box { background: #eef6ff; border-left: 6px solid #2f6fad; padding: 18px 20px; margin: 24px 0; border-radius: 6px; }",
    ".interpretacao { background: #f8fafc; border: 1px solid #e5e7eb; padding: 14px 16px; border-radius: 6px; margin: 14px 0 26px 0; }",
    ".legenda { background: #fafafa; border: 1px solid #e5e7eb; padding: 12px 16px; border-radius: 6px; margin: 20px 0; }",
    ".badge { display: inline-block; padding: 3px 8px; border-radius: 12px; font-size: 12px; font-weight: bold; margin-right: 6px; }",
    ".badge-baixo { background: #e8f5e9; color: #1b5e20; }",
    ".badge-moderado { background: #fff8e1; color: #8d6e00; }",
    ".badge-alto { background: #fff3e0; color: #e65100; }",
    ".badge-critico { background: #ffebee; color: #b71c1c; }",
    "table { border-collapse: collapse; width: 100%; margin: 18px 0; font-size: 14px; }",
    "th, td { border: 1px solid #d9dee5; padding: 10px 12px; text-align: left; vertical-align: top; }",
    "th { background-color: #1f4e79; color: white; }",
    "tr:nth-child(even) { background-color: #fafafa; }",
    "tr.baixo td { background-color: #edf7ed; }",
    "tr.moderado td { background-color: #fff8e8; }",
    "tr.alto td { background-color: #fff1e6; }",
    "tr.critico td { background-color: #fdecec; }",
    ".nota { font-size: 13px; color: #444; background: #fafafa; border-left: 4px solid #cbd5e1; padding: 10px 12px; margin-top: 8px; }",
    ".secao { margin-top: 34px; }",
    ".subtitulo-tabela { font-weight: bold; font-size: 18px; margin-top: 20px; color: #17324d; }",
    ".rodape { margin-top: 40px; font-size: 13px; color: #444; }",
    "a { color: #1d4ed8; }",
    "</style>",
    "</head>",
    "<body>",
    "<div class='container'>",
    "<h1>Análise de Consistência das Variáveis</h1>",
    "A metodologia da análise adotada baseou-se em parâmetros previamente descritos por Pinto IV, Ramos DN, Costa MCE, et al. (2012)<sup>1</sup>, ",
    "que propuseram critérios operacionais para análise de completude e consistência em registros hospitalares de câncer no Brasil.</p>",
    "<p>A avaliação de consistência considerou duas dimensões principais: ",
    "(i) o preenchimento formal e a validação de códigos; e ",
    "(ii) a coerência lógico-clínica entre variáveis.</p>",
    resumo_html,
    "<div class='legenda'>",
    "<p><b>Classificação da gravidade das inconsistências</b></p>",
    "<p>",
    "<span class='badge badge-baixo'>Baixo</span> &lt; 1% &nbsp;&nbsp;",
    "<span class='badge badge-moderado'>Moderado</span> 1% a &lt; 5% &nbsp;&nbsp;",
    "<span class='badge badge-alto'>Alto</span> 5% a &lt; 20% &nbsp;&nbsp;",
    "<span class='badge badge-critico'>Crítico</span> ≥ 20%",
    "</p>",
    "</div>"
  )

  # =========================
  # 6. Seção 1
  # =========================

  html_content <- paste0(
    html_content,
    "<div class='secao'>",
    "<h2>1. Preenchimento formal e validação de códigos</h2>",
    "<p>Esta dimensão refere-se à verificação da integridade estrutural dos dados, ou seja, se os campos foram preenchidos corretamente e com valores válidos. ",
    "Isso inclui checar se os códigos utilizados em cada variável seguem os padrões predefinidos e se não há registros com códigos inexistentes, inválidos ou em formatos incorretos. ",
    "Códigos representando informação ignorada, como “Sem informação”, “99”, “999”, “88” e “888”, foram considerados válidos.</p>",
    "<p>As variáveis com ocorrência de preenchimento inconsistente estão apresentadas a seguir, em ordem decrescente de proporção de inconsistências.</p>",
    "<div class='subtitulo-tabela'>", titulo_tabela1, "</div>",
    montar_tabela_html(resultados_codigos_inconsistentes, incluir_n_completos = TRUE),
    "<div class='nota'><b>Nota:</b> A proporção de inconsistências foi calculada como o número de registros com códigos inválidos dividido pelo total de registros disponíveis para a variável, expresso em percentual.</div>",
    resumo_secao(resultados_codigos_inconsistentes, "preenchimento formal e validação de códigos"),
    "</div>"
  )

  # =========================
  # 7. Seção 2
  # =========================

  html_content <- paste0(
    html_content,
    "<div class='secao'>",
    "<h2>2. Inconsistência lógico-clínica entre variáveis</h2>",
    "<p>Esta dimensão concentrou-se na avaliação da coerência semântica e temporal dos registros, com o objetivo de identificar contradições entre diferentes campos de dados clínicos e demográficos.</p>",

    "<h3>2.1 Plausibilidade da idade</h3>",
    "<p>A variável idade foi analisada quanto à plausibilidade dos valores registrados. Foram classificadas como inconsistentes as idades inferiores a zero, superiores a 150 anos ou aquelas com erros de preenchimento que inviabilizavam a interpretação numérica.</p>",
    "<div class='subtitulo-tabela'>", titulo_tabela2, "</div>",
    montar_tabela_html(resultados_idades_invalidas, incluir_n_completos = FALSE),
    "<div class='nota'><b>Nota:</b> A proporção de inconsistências foi calculada como o número de registros com idades inválidas dividido pelo total de registros disponíveis para a variável.</div>",
    resumo_secao(resultados_idades_invalidas, "plausibilidade da idade"),

    "<h3>2.2 Coerência entre datas clínicas</h3>",
    "<p>Foram verificadas inconsistências nos marcos temporais do cuidado oncológico, com foco nos intervalos entre a data do diagnóstico, a triagem, a primeira consulta, o início do tratamento e o óbito.</p>",
    "<p>Foram considerados inconsistentes os casos em que a sequência temporal esperada era violada ou em que o intervalo entre eventos excedia 12 meses, para mais ou para menos, conforme a regra aplicada em cada comparação.</p>",
    "<div class='subtitulo-tabela'>", titulo_tabela3, "</div>",
    montar_tabela_html(resultados_datas_inconsistentes, incluir_n_completos = FALSE),
    "<div class='nota'><b>Nota:</b> A proporção de inconsistências foi calculada como o número de registros com inconsistências cronológicas dividido pelo total de registros avaliados nesta dimensão. Devido às características estruturais da base de dados, não é possível distinguir se essa antecipação ou inconsistênicas reflete um erro de registro ou o fluxo real de pacientes que ingressam na instituição já com diagnóstico confirmado externamente, recomendando-se cautela na interpretação deste indicador como falha de qualidade.</div>",
    resumo_secao(resultados_datas_inconsistentes, "coerência entre datas clínicas"),

    "<h3>2.3 Coerência cronológica entre os anos dos eventos clínicos</h3>",
    "<p>Foi analisada a coerência entre os anos informados nos campos relacionados à trajetória assistencial, considerando a sequência esperada entre triagem, primeira consulta, diagnóstico e início do tratamento.</p>",
    "<div class='subtitulo-tabela'>", titulo_tabela4, "</div>",
    montar_tabela_html(resultados_anos_inconsistentes, incluir_n_completos = FALSE),
    "<div class='nota'><b>Nota:</b> A proporção de inconsistências foi calculada como o número de registros com inconsistências cronológicas entre anos dividido pelo número total de registros analisados no banco.</div>",
    resumo_secao(resultados_anos_inconsistentes, "coerência cronológica entre os anos dos eventos clínicos"),

    "<h3>2.4 Consistência entre sexo e topografia tumoral</h3>",
    "<p>A análise da consistência entre a variável sexo e a localização primária do tumor foi realizada com base nas diretrizes da <em>International Agency for Research on Cancer (IARC)</em><sup>2</sup>. ",
    "Foram considerados inconsistentes os registros em que o sexo informado era incompatível com a topografia tumoral associada a órgãos genitais específicos.</p>",
    "<div class='subtitulo-tabela'>", titulo_tabela5, "</div>",
    montar_tabela_html(resultados_cancer_sexo_inconsistentes, incluir_n_completos = TRUE),
    "<div class='nota'><b>Nota:</b> A proporção de inconsistências foi calculada como o número de registros com sexo incompatível com a topografia tumoral dividido pelo total de registros completos com a respectiva localização primária do tumor.</div>",
    resumo_secao(resultados_cancer_sexo_inconsistentes, "consistência entre sexo e topografia tumoral"),
    "</div>"
  )

  # =========================
  # 8. Implicações analíticas
  # =========================

  html_content <- paste0(
    html_content,
    "<div class='secao'>",
    "<h2>3. Implicações para análise </h2>",
    "<p>As inconsistências identificadas podem afetar de maneira distinta o uso analítico da base. Problemas de validação de códigos comprometem a classificação adequada das variáveis. ",
    "Inconsistências temporais podem introduzir erro na estimação de intervalos assistenciais, como tempo entre diagnóstico e tratamento. ",
    "Já incoerências entre sexo e topografia tumoral podem indicar erro de digitação, codificação inadequada ou problemas de integração entre campos clínicos.</p>",
    "<p>Por esse motivo, recomenda-se que as variáveis com maior proporção de inconsistência sejam priorizadas em etapas de revisão, recodificação e análise de sensibilidade.</p>",
    "</div>"
  )

  html_content <- paste0(html_content,
                         "<div class='secao'>",
                         "<h2>4. Limitações</h2>",
                         "<p>A análise automatizada de consistência baseia-se em regras pré-definidas, podendo não captar a complexidade dos fluxos assistenciais e das situações clínicas individuais. Os resultados refletem estritamente a qualidade e a coerência das informações registradas no banco de dados, não necessariamente correspondendo à realidade assistencial, especialmente quando consideradas informações clínicas e contextuais não capturadas nos sistemas de registro. Parte das inconsistências identificadas pode refletir registros legítimos, como diagnósticos retrospectivos ou atrasos no preenchimento, e não necessariamente erros. Adicionalmente, a qualidade dos resultados depende da completude e padronização das variáveis, podendo ocorrer sub ou superestimação das inconsistências. Dessa forma, os achados devem ser interpretados com cautela e, sempre que possível, complementados por análise contextual ou validação manual.</p>",
                         "</div>"
  )

  # =========================
  # 9. Referências
  # =========================

  html_content <- paste0(
    html_content,
    "<div class='rodape'>",
    "<h2>Referências</h2>",
    "<ul>",
    "<li>1. Pinto IV, Ramos DN, Costa MCE, et al. Completude e consistência dos dados dos registros hospitalares de câncer no Brasil. Cad Saúde Colet [Internet]. 2012. Disponível em: <a href='https://ninho.inca.gov.br/jspui/handle/123456789/10112'>https://ninho.inca.gov.br/jspui/handle/123456789/10112</a></li>",
    "<li>2. Bray F. et al. (org.). Cancer Incidence in Five Continents Volume XII. Lyon, França: International Agency for Research on Cancer, 2024. Disponível em: <a href='https://ci5.iarc.fr/ci5-xii/'>https://ci5.iarc.fr/ci5-xii/</a></li>",
    "</ul>",
    "</div>",
    "</div>",
    "</body></html>"
  )

  # =========================
  # 10. Salvar e abrir
  # =========================

  writeLines(html_content, con = output_file, useBytes = TRUE)
  browseURL(output_file)
}
