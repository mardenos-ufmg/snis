

#' Mapa interativo de Scores por Região
#'
#' Gera um mapa interativo mostrando a distribuição de um score por região,
#' colorindo os municípios de acordo com quartis do score.
#'
#' @param df Data frame contendo os dados.
#' @param var String com o nome da coluna que contém o score, como `"score médio su"`.
#' @param titulo (opcional) Título do mapa. Se `NULL`, é gerado automaticamente com base no nome da coluna de score.
#' @param quart (opcional) Lógico. Se `TRUE`, colore os municípios por **quartis** do score;
#'   se `FALSE` (padrão), usa os valores contínuos.
#'
#' @return Um objeto `tmap` interativo com os municípios coloridos por quartis do score.
#'
#' @export
#'
#' @importFrom viridis turbo
mapa_interativo <- function(df, var, quart = FALSE, titulo = NULL) {
  stopifnot(
    "df não contém todas as colunas necessárias" = all( c(var, "município", "região intermediária") %in% colnames(df))
  )

  if (is.null(titulo)) {
    titulo <- paste0("Mapa Interativo de ", var)
    if ("ano de referência" %in% colnames(df)) {
      titulo <- paste(titulo, df$`ano de referência`[1])
    }
  }

  geo_merged <-
    df %>%
    select(
      name = all_of("município"),
      Score = all_of(var),
      Grupo = all_of("região intermediária")
    ) %>%
    mutate(Score = round(Score, 4)) %>%
    distinct(name, .keep_all = TRUE) %>%
    left_join(x = mapa_MG, by = "name")

  coor = sf::st_bbox( filter(geo_merged, !is.na(Score)) )

  if (quart) {
    geo_merged <-
      geo_merged %>%
      mutate(
        Score_Quartil = cut(Score,
                            breaks = quantile(Score, probs = 0:4/4, na.rm = TRUE),
                            include.lowest = TRUE,
                            labels = c("Q1","Q2","Q3","Q4"))
      )
    fill_var <- "Score_Quartil"
    style <- "cat"
    cores <- c("#922B21", "#E67E22", "#F4D03F", "#52BE80")
  } else {
    fill_var <- "Score"
    style <- "cont"
    cores <- viridis::turbo(100, direction = -1)
  }

  #tmap::tmap_mode("view")
  tmap::tm_shape(geo_merged, bbox = coor) +
    tmap::tm_fill(
      col = fill_var,
      title = "Legenda",
      style = style,
      palette = cores,
      popup.vars = c("name", "Grupo", fill_var, "Score")
    ) +
    tmap::tm_borders(col = "gray50", lwd = 0.5) +
    tmap::tm_basemap(server = "OpenStreetMap") +
    tmap::tm_layout(
      main.title = titulo,
      main.title.position = "center",
      legend.outside = TRUE
    )
}


#' Heatmap das Cargas Fatoriais por Grupo
#'
#' Essa função gera um conjunto de heatmaps mostrando as cargas fatoriais (`loadings`)
#' de cada variável em cada fator para diferentes grupos em uma análise fatorial.
#' Cada grupo recebe um plot separado, e todos são combinados em um único grid.
#'
#' @param FA Objeto de análise fatorial criado previamente, que deve conter:
#'   - `FA$geral$scores`: tabela com scores gerais (usada para identificar os grupos);
#'   - `FA[[grupo]]$loadings$df`: data frame com as cargas fatoriais de cada variável por fator.
#'
#' @details
#' - Cada grupo é representado separadamente, com seu próprio título;
#' - As cores são escaladas individualmente para cada grupo, respeitando os valores mínimos e máximos das cargas;
#' - Os plots são organizados em um grid com número de colunas igual ao número de grupos.
#'
#' @return Um gráfico `ggplot` combinado (grid) de todos os grupos, mostrando:
#'   - Variáveis no eixo y;
#'   - Fatores no eixo x;
#'   - Cargas fatoriais codificadas por cores (azul = negativa, vermelho = positiva, branco = zero);
#'   - Valores numéricos das cargas sobre os tiles.
#'
#' @export
#'
#' @importFrom gridExtra grid.arrange
plot_loading = function(FA) {
  plot_list = list()
  grupos = colnames(FA$geral$scores)[-(1:2)]

  for (grupo in grupos) {
    df_long =
      FA[[grupo]]$loadings$df |>
      pivot_longer(
        cols = -variável
        , names_to = "fator",
        values_to = "carga"
      )

    plot_list[[grupo]] =
      ggplot(df_long, aes(x = fator, y = variável, fill = carga)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                           midpoint = 0,
                           limit = c(min(df_long$carga, na.rm = TRUE),
                                     max(df_long$carga, na.rm = TRUE))
                           ) +
      geom_text(aes(label = round(carga, 2)), color = "black", size = 3) +
      labs(title = paste("Grupo:", grupo)) + theme_minimal() +
      scale_x_discrete( labels = seq_along(unique(df_long$fator)) ) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()
            )
  }

  gridExtra::grid.arrange(grobs = plot_list, ncol = length(grupos))
}


#' Tabela dos Melhores e Piores municípios
#'
#' Exibe uma tabela cos os Melhores e Piores Municípios com base no Score Médio das categorias.
#'
#' @param df Data frame contendo os dados.
#' @param top_n Número de municípios com maiores *scores médios* a serem listados.
#' @param bottom_n Número de municípios com menores *scores médios* a serem listados.
#'
#' @return Uma lista com dois elementos:
#'   - `Melhores`: data frame com os municípios com maiores *scores médios*;
#'   - `Piores`: data frame com os municípios com menores *scores médios*.
#'
#' @export
table_top_bottom <- function(df, top_n, bottom_n) {
  df_aux <-df %>%
    group_by(município, `natureza jurídica`) %>%
    summarise(
      Sustentabilidade = mean(`score sustentabilidade`, na.rm = TRUE),
      Universalidade = mean(`score universalidade`, na.rm = TRUE),
      Score_Médio = mean(`score médio su`, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Sustentabilidade = round(Sustentabilidade, 3),
      Universalidade = round(Universalidade, 3),
      Score_Médio = round(Score_Médio, 3)
    )

  top_scores <- df_aux %>%
    arrange(desc(Score_Médio)) %>%
    slice_head(n = top_n) %>%
    rename(
      Município = município,
      Natureza_Jurídica = `natureza jurídica`
    ) %>%
    select(all_of("Município", "Score_Médio", "Sustentabilidade", "Universalidade", "Natureza_Jurídica"))

  bottom_scores <- df_aux %>%
    arrange(Score_Médio) %>%
    slice_head(n = bottom_n) %>%
    rename(
      Município = município,
      Natureza_Jurídica = `natureza jurídica`
    ) %>%
    select(all_of("Município", "Score_Médio", "Sustentabilidade", "Universalidade", "Natureza_Jurídica"))

  list(
    Melhores = top_scores,
    Piores = bottom_scores
  )
}


#' Boxplots Scores por Grupos
#'
#' Essa função gera um boxplot que mostra a distribuição de uma variável de
#' *score* em diferentes grupos (como prestadores, regiões ou naturezas jurídicas).
#'
#' @param df Data frame contendo os dados.
#'
#' @param score_cols Vetor de strings com os nomes das colunas que contêm os scores, como `score médio su`, `score médio eee`.
#' @param group_cols Vetor de strings com os nomes das colunas de agrupamento, como `natureza jurídica`.
#' @param titulo (opcional) Título do gráfico. Se `NULL`, é gerado automaticamente
#' @param cor_paleta (opcional) Paleta de cores viridis a ser usada.
#'   Opções: `"viridis"`, `"plasma"`, `"magma"`, `"cividis"`, `"inferno"`.
#'   Padrão: `"plasma"`.
#'
#' @return Um objeto `ggplot` contendo o boxplot formatado.
#'
#' @export
#'
#' @importFrom viridis viridis
plot_boxplot <- function(df, score_cols, group_cols, titulo = NULL, cor_paleta = "plasma") {

  todas_colunas <- c(score_cols, group_cols)
  faltando <- todas_colunas[!todas_colunas %in% colnames(df)]
  if (length(faltando) > 0) {
    stop(paste0("Colunas não existem no dataframe: ", paste(faltando, collapse = ", ")))
  }

  df_long <- df %>%
    unite("Grupo", all_of(group_cols), sep = " | ") %>%
    pivot_longer(
      cols = all_of(score_cols),
      names_to = "Tipo_Score",
      values_to = "Score"
    )

  if (is.null(titulo)) {
    titulo <- paste0("Distribuição de ", paste(score_cols, collapse = ", "), " por ", paste(group_cols, collapse = ", "))
  }

  cores <- viridis::viridis(length(score_cols), option = cor_paleta)
  names(cores) <- score_cols

  ggplot(df_long, aes(x = Grupo, y = Score, fill = Tipo_Score)) +
    geom_boxplot(position = position_dodge(width = 0.8), outlier.color = "gray40", outlier.alpha = 0.6, width = 0.5) +
    scale_fill_manual(values = cores, name = "Tipo de Score") +
    labs(title = titulo, x = paste(group_cols, collapse = " | "), y = "Score") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
      axis.title.x = element_text(margin = margin(t = 12)),
      axis.title.y = element_text(margin = margin(r = 12)),
      legend.position = "bottom"
    )
}


#' Gráfico de barras com a Mediana por Grupo
#'
#' Essa função cria um barplot horizontal mostrando a mediana de uma variável
#' numérica (`score_col`) por grupos (`group_col`), adicionando os valores das
#' medianas sobre as barras.
#'
#' @param df Data frame contendo os dados.
#' @param score_cols Vetor de strings com os nomes das colunas que contêm os scores, como `score médio su`, `score médio eee`.
#' @param group_cols Vetor de strings com os nomes das colunas de agrupamento, como `região intermediária`.
#' @param titulo (opcional) Título do gráfico. Se `NULL`, é gerado automaticamente
#' @param cor_paleta (opcional) Paleta de cores viridis a ser usada.
#'   Opções: `"viridis"`, `"plasma"`, `"magma"`, `"cividis"`, `"inferno"`.
#'   Padrão: `"plasma"`.
#'
#' @return Um objeto `ggplot` contendo o barplot formatado.
#'
#' @export
plot_median_barplot <- function(df, score_cols, group_cols, titulo = NULL, cor_paleta = "plasma") {

  todas_colunas <- c(score_cols, group_cols)
  faltando <- todas_colunas[!todas_colunas %in% colnames(df)]
  if (length(faltando) > 0) {
    stop(paste0("Colunas não existem no dataframe: ", paste(faltando, collapse = ", ")))
  }

  df_long <- df %>%
    tidyr::unite("Grupo", all_of(group_cols), sep = " | ") %>%
    tidyr::pivot_longer(
      cols = all_of(score_cols),
      names_to = "Tipo_Score",
      values_to = "Score"
    )

  if (is.null(titulo)) {
    titulo <- paste0("Mediana de ", paste(score_cols, collapse = ", "), " por ", paste(group_cols, collapse = ", "))
  }

  cores <- viridis::viridis(length(score_cols), option = cor_paleta)
  names(cores) <- score_cols

  order_levels <- df_long %>%
    dplyr::group_by(Grupo, Tipo_Score) %>%
    dplyr::summarise(Mediana = median(Score, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(Mediana) %>%
    dplyr::pull(Grupo) %>%
    unique()

  ggplot(df_long, aes(x = Score, y = factor(Grupo, levels = order_levels), fill = Tipo_Score)) +
    stat_summary(fun = median, geom = "bar", width = 0.7, color = "black", alpha = 0.85, position = "dodge") +
    stat_summary(fun = median, geom = "text", aes(label = round(after_stat(x), 3)), hjust = -0.1, size = 3, position = position_dodge(width = 0.7)) +
    scale_fill_manual(values = cores, name = "Tipo de Score") +
    labs(title = titulo, x = "Mediana", y = paste(group_cols, collapse = " | ")) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10))
    )
}

#' Gráfico tipo "ondas" (ridge) para múltiplos scores
#'
#' Cria um gráfico de densidade estilo "ridge plot", mostrando a distribuição
#' de diferentes scores (`score_cols`) por grupos (`group_col`).
#'
#' @param df Data frame contendo os dados.
#' @param score_cols Vetor de strings com os nomes das colunas que contêm os scores, como `score médio su`, `score médio eee`.
#' @param group_cols Vetor de strings com os nomes das colunas de agrupamento, como `região intermediária` e `natureza jurídica`.
#' @param titulo (opcional) Título do gráfico. Se `NULL`, é gerado automaticamente
#' @param cor_paleta (opcional) Paleta de cores viridis a ser usada.
#'   Opções: `"viridis"`, `"plasma"`, `"magma"`, `"cividis"`, `"inferno"`.
#'   Padrão: `"plasma"`.
#'
#' @return Um objeto `ggplot` com o ridge plot.
#'
#' @export
plot_ridge_scores <- function(df, score_cols, group_cols, titulo = NULL, cor_paleta = "plasma") {

  todas_colunas <- c(score_cols, group_cols)
  faltando <- todas_colunas[!todas_colunas %in% colnames(df)]
  if (length(faltando) > 0) {
    stop(paste0("Colunas não existem no dataframe: ", paste(faltando, collapse = ", ")))
  }

  df_long <- df %>%
    tidyr::unite("Grupo", all_of(group_cols), sep = " | ") %>%
    tidyr::pivot_longer(
      cols = all_of(score_cols),
      names_to = "Tipo_Score",
      values_to = "Score"
    )

  if (is.null(titulo)) {
    titulo <- paste0("Distribuição de Scores por ", paste(group_cols, collapse = " | "))
  }

  cores <- viridis::viridis(length(score_cols), option = cor_paleta)
  names(cores) <- score_cols

  ggplot(df_long, aes(x = Score, y = Grupo, fill = Tipo_Score)) +
    ggridges::geom_density_ridges(alpha = 0.9, scale = 1.5, rel_min_height = 0.005) +
    scale_fill_manual(values = cores, name = "Tipo de Score") +
    labs(title = titulo, x = "Score", y = paste(group_cols, collapse = " | ")) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    )
}

#' Tabela de Medianas por Grupo
#'
#' Calcula a mediana de uma ou mais colunas de score por uma ou mais colunas de agrupamento.
#'
#' @param df Data frame contendo os dados.
#' @param score_cols Vetor de strings com os nomes das colunas que contêm os scores, como `score médio su`e `score médio eee`.
#' @param group_cols Vetor de strings com os nomes das colunas de agrupamento, como `região intermediária`.
#'
#' @return Data frame com duas colunas:
#'   - `group_col`: grupos;
#'   - `Score_Mediana`: mediana do score em cada grupo.
#'
#' @export
table_median <- function(df, score_cols, group_cols) {

  todas_colunas <- c(score_cols, group_cols)
  faltando <- todas_colunas[!todas_colunas %in% colnames(df)]
  if (length(faltando) > 0) {
    stop(paste0("Colunas não existem no dataframe: ", paste(faltando, collapse = ", ")))
  }

  df_resumo <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(score_cols), ~ median(.x, na.rm = TRUE), .names = "Mediana_{.col}"),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::across(dplyr::starts_with("Mediana_")))

  return(df_resumo)
}




#' Histograma de uma variável numérica
#'
#' Cria um histograma mostrando a distribuição dos valores de uma variável numérica.
#'
#' @param df Data frame contendo os dados.
#' @param var_col String com o nome da coluna numérica a ser analisada, como `"IN023"`.
#' @param titulo (opcional) Título do gráfico. Se `NULL`, é gerado automaticamente.
#'
#' @return Um objeto `ggplot` com o histograma.
#'
#' @export
plot_hist_score <- function(df, group_col, titulo = NULL) {
  if (!group_col %in% colnames(df)) {
    stop("❌ A coluna informada não existe no dataframe.")
  }

  if (is.null(titulo)) {
    titulo <- paste0("Distribuição de ", group_col)
  }

  ggplot(df, aes(x = .data[[group_col]])) +
    geom_histogram(
      bins = 30,
      fill = viridis::viridis(1, option = "plasma"),
      color = "white",
      alpha = 0.8
    ) +
    labs(
      title = titulo,
      x = group_col,
      y = "Frequência"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
}


#' Densidade de uma variável numérica
#'
#' Cria um gráfico de densidade mostrando a distribuição suave dos valores
#' de uma variável numérica, como uma "onda".
#'
#' @param df Data frame contendo os dados.
#' @param var_col String com o nome da coluna numérica a ser analisada, como `"IN023"`.
#' @param titulo (opcional) Título do gráfico. Se `NULL`, é gerado automaticamente.
#'
#' @return Um objeto `ggplot` com o gráfico de densidade.
#'
#' @export
plot_density_score <- function(df, group_col, titulo = NULL) {
  if (!group_col %in% colnames(df)) {
    stop("❌ A coluna informada não existe no dataframe.")
  }

  if (is.null(titulo)) {
    titulo <- paste0("Distribuição de Densidade de ", group_col)
  }

  ggplot(df, aes(x = .data[[group_col]])) +
    geom_density(
      fill = viridis::viridis(1, option = "plasma"),
      color = "black",
      alpha = 0.7
    ) +
    labs(
      title = titulo,
      x = group_col,
      y = "Densidade"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
}


#' Tabela dos Rankings
#'
#' Gera uma Tabela com o Ranking dos Municípios com base no Score Médio das categorias desejadas.
#'
#' @param df Data frame contendo os dados.
#' @param top_n (opcional) Número de municípios a serem exibidos no ranking final.
#'   Se não for especificado, retorna 10 municípios.
#'
#' @return Um data frame contendo as colunas:
#'   - `município`: nome do município;
#'   - `Ranking`: posição no ranking baseado no Score Médio;
#'   - `Rank_Medio`: ranking médio de todas as dimensões;
#'   - `Diferenca`: diferença entre o Ranking e o Rank_Medio;
#'   - `Sustentabilidade`, `Universalidade`, `Score_Medio`: valores médios dos scores;
#'
#' @export
table_ranking <- function(df, top_n = 10) {

  df_score <- df %>%
    dplyr::group_by(município, `código do município`) %>%
    dplyr::summarise(
      Sustentabilidade = mean(`score sustentabilidade`, na.rm = TRUE),
      Universalidade   = mean(`score universalidade`, na.rm = TRUE),
      Score_Medio      = mean(`score médio su`, na.rm = TRUE),
      .groups = "drop"
    )

  df_score <- df_score %>%
    dplyr::arrange(desc(Score_Medio)) %>%
    dplyr::mutate(Ranking = dplyr::row_number())

  df_score <- df_score %>%
    dplyr::arrange(desc(Sustentabilidade + Universalidade)) %>%
    dplyr::mutate(Rank_Medio = dplyr::row_number()) %>%
    dplyr::arrange(Ranking)

  df_score <- df_score %>%
    dplyr::mutate(Diferenca = Ranking - Rank_Medio) %>%
    dplyr::select(município, Ranking, Rank_Medio, Diferenca,
                  Sustentabilidade, Universalidade, Score_Medio)

  if (!is.null(top_n)) {
    df_score <- head(df_score, top_n)
  }

  return(df_score)
}
