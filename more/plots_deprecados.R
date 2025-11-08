






# DEPRECAR?
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

# DEPRECAR?
#' Gráfico tipo "ondas" (ridge) para múltiplos scores
#'
#' Cria um gráfico de densidade estilo "ridge plot", mostrando a distribuição
#' de diferentes scores (`score_cols`) por grupos (`group_col`).
#'
#' @param df Data frame contendo os dados.
#' @param score_cols Vetor de strings com os nomes das colunas que contêm os scores, como `score médio su`, `score médio eee`.
#' @param group_cols Vetor de strings com os nomes das colunas de agrupamento, como `região intermediária` e `natureza jurídica`.
#' @param titulo (opcional) Título do gráfico. Se `NULL`, é gerado automaticamente
#' @param paleta (opcional) Paleta de cores viridis a ser usada.
#'   Opções: `"viridis"`, `"plasma"`, `"magma"`, `"cividis"`, `"inferno"`.
#'   Padrão: `"plasma"`.
#'
#' @return Um objeto `ggplot` com o ridge plot.
#'
#' @export
plot_ridge_scores <- function(df, score_cols, group_cols, titulo = NULL, paleta = "plasma") {

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

  cores <- viridis::viridis(length(score_cols), option = paleta)
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


# DEPRECAR
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
#' @param paleta (opcional) Paleta de cores viridis a ser usada.
#'   Opções: `"viridis"`, `"plasma"`, `"magma"`, `"cividis"`, `"inferno"`.
#'   Padrão: `"plasma"`.
#'
#' @return Um objeto `ggplot` contendo o barplot formatado.
#'
#' @export
plot_median_barplot <- function(df, score_cols, group_cols, titulo = NULL, paleta = "plasma") {

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

  cores <- viridis::viridis(length(score_cols), option = paleta)
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


# DEPRECAR OU MELHORAR TIPO SUMMARY
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

