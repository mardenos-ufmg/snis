#' Title
#'
#' @param df .
#' @param var .
#' @param quart .
#'
#' @returns ggplot
#'
#' @export
plot_map = function(df, var, quart = F) {
  df =
    df[c("código do município", var)] |>
    dplyr::mutate(`código do município` = as.numeric(`código do município`))

  legenda = colnames(df)[2]
  colnames(df)[2] = "var"

  if (quart) {
    df[[2]] = df[[2]] |> quartile()
    legenda = paste0(legenda, "\n(quartis)")
  }

  mapa =
    readRDS("data/map_MG.rds") |>
    dplyr::left_join(df, by = c("code_muni" = "código do município")) |>
    sf::st_as_sf()

  ggplot(mapa) +
    geom_sf(aes(fill = .data[["var"]]), color = NA) +
    {if (quart) {
      scale_fill_viridis_d(option = "plasma", na.value = "grey90")
    } else {
      scale_fill_viridis_c(option = "plasma", na.value = "grey90")
    }
    } +
    labs(title = "Mapa",
         fill = legenda) +
    theme_void()
}

#' Title
#'
#' @param FA .
#'
#' @returns ggplot
#'
#' @export
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
      geom_tile(color = "white") + scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                                                        midpoint = 0,
                                                        limit = c(min(df_long$carga, na.rm = TRUE),
                                                                  max(df_long$carga, na.rm = TRUE))) +
      geom_text(aes(label = round(carga, 2)), color = "black", size = 3) +
      labs(title = paste("Grupo:", grupo)) + theme_minimal() +
      scale_x_discrete( labels = seq_along(unique(df_long$fator)) ) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank())
  }

  gridExtra::grid.arrange(grobs = plot_list, ncol = length(grupos))
}

#' Title
#'
#' @param df .
#' @param top_n .
#'
#' @returns df
#'
#' @export
table_ranking <- function(df, top_n = NULL) {

  df_score <- df %>%
    dplyr::group_by(município, `código do município`) %>%
    dplyr::summarise(
      Efetividade = mean(`score efetividade`, na.rm = TRUE),
      Eficácia = mean(`score eficácia`, na.rm = TRUE),
      Eficiência = mean(`score eficiência`, na.rm = TRUE),
      Score_Medio = mean(`score médio eee`, na.rm = TRUE),
      .groups = "drop"
    )

  df_score <- df_score %>%
    dplyr::arrange(desc(Score_Medio)) %>%
    dplyr::mutate(Ranking = dplyr::row_number())

  df_score <- df_score %>%
    dplyr::arrange(desc(Efetividade + Eficácia + Eficiência)) %>%
    dplyr::mutate(Rank_Medio = dplyr::row_number()) %>%
    dplyr::arrange(Ranking)

  df_score <- df_score %>%
    dplyr::mutate(Diferenca = Ranking - Rank_Medio)

  df_score <- df_score %>%
    dplyr::select(município, Ranking, Rank_Medio, Diferenca,
                  Efetividade, Eficácia, Eficiência, Score_Medio)

  if (!is.null(top_n)) {
    df_score <- head(df_score, top_n)
  }

  return(df_score)
}

#Imprime as melhores e piores cidades com base no ranking
#' Title
#'
#' @param df .
#' @param top_n .
#' @param bottom_n .
#'
#' @returns table
#'
#' @export
table_top_bottom <- function(df, top_n, bottom_n) {
  df_aux <- df %>%
    dplyr::group_by(município, `natureza jurídica`) %>%
    dplyr::summarise(
      Efetividade = mean(`score efetividade`, na.rm = TRUE),
      Eficácia = mean(`score eficácia`, na.rm = TRUE),
      Eficiência = mean(`score eficiência`, na.rm = TRUE),
      Score_Medio = mean(`score médio eee`, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Efetividade = round(Efetividade, 3),
      Eficácia = round(Eficácia, 3),
      Eficiência = round(Eficiência, 3),
      Score_Medio = round(Score_Medio, 3)
    )

  top_scores <- df_aux %>%
    dplyr::arrange(desc(Score_Medio)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::rename(
      Município = município,
      Natureza_jurídica = `natureza jurídica`
    )

  bottom_scores <- df_aux %>%
    dplyr::arrange(Score_Medio) %>%
    dplyr::slice_head(n = bottom_n) %>%
    dplyr::rename(
      Município = município,
      Natureza_jurídica = `natureza jurídica`
    )

  list(
    Melhores = top_scores,
    Piores = bottom_scores
  )
}

#Imprime boxplot para a variavel selecionada
#' Title
#'
#' @param df .
#' @param score_col .
#' @param group_col .
#' @param titulo .
#' @param cor_paleta .
#'
#' @returns plot
#'
#' @export
plot_boxplot <- function(df, score_col, group_col, titulo = NULL, cor_paleta = "Set2") {

  if (!all(c(score_col, group_col) %in% colnames(df))) {
    stop("As colunas informadas não existem no dataframe.")
  }

  df_aux <- df %>%
    dplyr::select(
      Score = dplyr::all_of(score_col),
      Grupo = dplyr::all_of(group_col)
    )

  if (is.null(titulo)) {
    titulo <- paste0("Distribuição de ", score_col, " por ", group_col)
  }


  escala_cores <- if (n_categorias <= 8) {
    scale_fill_brewer(palette = cor_paleta)
  } else {
    scale_fill_viridis_d(option = "plasma")
  }

  ggplot(df_aux, aes(x = Grupo, y = Score, fill = Grupo)) +
    geom_boxplot(
      outlier.color = "gray40",
      outlier.alpha = 0.6,
      width = 0.5
    ) +
    escala_cores +
    labs(
      title = titulo,
      x = stringr::str_to_title(group_col),
      y = stringr::str_to_title(score_col)
    ) +
    scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
      axis.title.x = element_text(margin = margin(t = 12)),
      axis.title.y = element_text(margin = margin(r = 12)),
      legend.position = "none"
    )
}

#Faz um grafico de barras com a mediana
#' Title
#'
#' @param df .
#' @param score_col .
#' @param group_col .
#' @param titulo .
#' @param casas_decimais .
#' @param cor_paleta .
#'
#' @returns plot
#'
#' @export
plot_median_barplot <- function(df, score_col, group_col, titulo = NULL, casas_decimais = 3, cor_paleta = "Blues") {

  if (!all(c(score_col, group_col) %in% colnames(df))) {
    stop("❌ As colunas informadas não existem no dataframe.")
  }

  df_aux <- df %>%
    dplyr::select(
      Score = dplyr::all_of(score_col),
      Grupo = dplyr::all_of(group_col)
    )

  order_levels <- df_aux %>%
    dplyr::group_by(Grupo) %>%
    dplyr::summarise(Mediana_Score = median(Score, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(Mediana_Score) %>%
    dplyr::pull(Grupo)

  if (is.null(titulo)) {
    titulo <- paste0("Mediana de ", score_col, " por ", group_col)
  }

  n_categorias <- length(unique(df_aux$Grupo))
  escala_cores <- if (n_categorias <= 9) {
    scale_fill_brewer(palette = cor_paleta)
  } else {
    scale_fill_viridis_d(option = "plasma")
  }

  ggplot(df_aux, aes(
    x = Score,
    y = factor(Grupo, levels = order_levels),
    fill = Grupo
  )) +
    stat_summary(
      fun = median,
      geom = "bar",
      width = 0.7,
      color = "black",
      alpha = 0.85
    ) +
    stat_summary(
      fun = median,
      geom = "text",
      aes(label = round(after_stat(x), casas_decimais)),
      hjust = -0.1,
      size = 3
    ) +
    escala_cores +
    labs(
      title = titulo,
      x = paste0("Mediana de ", score_col),
      y = stringr::str_to_title(group_col)
    ) +
    expand_limits(x = max(df_aux$Score, na.rm = TRUE) * 1.05) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title.x = element_text(size = 12, margin = margin(t = 10)),
      axis.title.y = element_text(size = 12, margin = margin(r = 10))
    )
}

#Gara o grafico de "ondas"
#' Title
#'
#' @param df .
#' @param group_col .
#' @param score_cols .
#' @param titulo .
#'
#' @returns plot
#'
#' @export
plot_ridge_scores <- function(df, group_col, score_cols, titulo = NULL) {

  todas_colunas <- c(group_col, score_cols)
  if (!all(todas_colunas %in% colnames(df))) {
    faltando <- todas_colunas[!todas_colunas %in% colnames(df)]
    stop(paste0("❌ Algumas colunas informadas não existem no dataframe: ", paste(faltando, collapse = ", ")))
  }

  df_aux_long <- df %>%
    dplyr::select(dplyr::all_of(c(group_col, score_cols))) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(score_cols),
      names_to = "Tipo_Score",
      values_to = "Scores"
    ) %>%
    dplyr::mutate(Grupo = factor(.data[[group_col]]))

  if (is.null(titulo)) {
    titulo <- paste0("Distribuição de Scores por ", group_col)
  }

  n_scores <- length(score_cols)
  cores <- if (n_scores <= 9) {
    RColorBrewer::brewer.pal(n_scores, "Set2")
  } else {
    viridis::viridis(n_scores)
  }
  names(cores) <- score_cols

  ridge_plot <- ggplot(df_aux_long, aes(x = Scores, y = Grupo, fill = Tipo_Score)) +
    ggridges::geom_density_ridges(alpha = 0.9, scale = 1.5, rel_min_height = 0.005) +
    scale_fill_manual(values = cores, name = "Tipo de Score") +
    labs(
      title = titulo,
      x = "Valor do Score",
      y = stringr::str_to_title(group_col)
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    )

  print(ridge_plot)
}

#' Title
#'
#' @param df .
#' @param score_col .
#' @param group_col .
#'
#' @returns plot
#'
#' @export
table_median <- function(df, score_col, group_col) {

  if (!all(c(score_col, group_col) %in% colnames(df))) {
    stop(paste0("❌ Algumas colunas informadas não existem no dataframe: ",
                paste(c(score_col, group_col)[!c(score_col, group_col) %in% colnames(df)], collapse = ", ")))
  }

  df_resumo <- df %>%
    dplyr::select(dplyr::all_of(c(score_col, group_col))) %>%
    dplyr::group_by(.data[[group_col]]) %>%
    dplyr::summarise(
      Score_Mediana = median(.data[[score_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(Score_Mediana)

  return(df_resumo)
}


#Gera um mapa interativo do grupo desejado
#' Title
#'
#' @param df .
#' @param score_col .
#' @param group_col .
#' @param titulo .
#'
#' @returns tmap
#'
#' @export
plot_interactive_map <- function(df, score_col, group_col, titulo = NULL) {

  geojson_path <- "data/geojs-31-mun.json"

  if (!requireNamespace("sf", quietly = TRUE) ||
      !requireNamespace("tmap", quietly = TRUE)) {
    stop("❌ Os pacotes 'sf' e 'tmap' são necessários para esta função.")
  }


  if (!all(c(score_col, "município", group_col) %in% colnames(df))) {
    stop("❌ O dataframe não contém todas as colunas necessárias: ",
         paste(c("município", score_col, group_col), collapse = ", "))
  }

  df_aux <- df %>%
    dplyr::select(
      name = dplyr::all_of("município"),
      Score = dplyr::all_of(score_col),
      Grupo = dplyr::all_of(group_col)
    ) %>%
    dplyr::mutate(
      Score = round(Score, 4),
      Quantil = dplyr::case_when(
        Score <= 0.25 ~ "(0-25)%",
        Score > 0.25 & Score <= 0.50 ~ "(25-50)%",
        Score > 0.50 & Score <= 0.75 ~ "(50-75)%",
        TRUE ~ "(75-100)%"
      ),
      Quantil = factor(Quantil,
                       levels = c("(0-25)%", "(25-50)%", "(50-75)%", "(75-100)%"))
    ) %>%
    dplyr::distinct(name, .keep_all = TRUE)

  geo_data_sf <- sf::st_read(geojson_path, quiet = TRUE)

  geo_merged <- geo_data_sf %>%
    dplyr::left_join(df_aux, by = "name")

  if (is.null(titulo)) {
    titulo <- paste0("Mapa Interativo de ", score_col)
  }

  cores_quantil <- c("#922B21", "#E67E22", "#F4D03F", "#52BE80")

  tmap::tmap_mode("view")
  mapa <- tmap::tm_shape(geo_merged) +
    tmap::tm_fill(
      col = "Quantil",
      title = titulo,
      palette = cores_quantil,
      style = "cat",
      popup.vars = c("name", "Grupo", "Quantil", "Score")
    ) +
    tmap::tm_borders(col = "gray50", lwd = 0.5) +
    tmap::tm_basemap(server = "OpenStreetMap") +
    tmap::tm_view(set.view = c(lon = -43.98, lat = -19.84, zoom = 6)) +
    tmap::tm_layout(
      main.title = titulo,
      main.title.position = "center",
      legend.outside = TRUE
    )

  return(mapa)
}
