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
#' @import tidyverse
#' @import ggplot2
mapa_interativo <- function(df, var, quart = FALSE, titulo = NULL) {
  stopifnot(
    "df não contém todas as colunas necessárias" = all( c(var, "município", "região intermediária") %in% colnames(df))
  )

  if (is.null(titulo)) {
    titulo <- paste0("Mapa Interativo de ", var)
    if ("ano de referência" %in% colnames(df)) {
      titulo <- paste0(titulo, " em ", df$`ano de referência`[1])
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
#' @import tidyverse
#' @import ggplot2
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
#' @import tidyverse
#' @import ggplot2
table_top_bottom <- function(df, top_n, bottom_n) {
  df_aux <- df %>%
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
    select(all_of(c("Município", "Score_Médio", "Sustentabilidade", "Universalidade", "Natureza_Jurídica")))

  bottom_scores <- df_aux %>%
    arrange(Score_Médio) %>%
    slice_head(n = bottom_n) %>%
    rename(
      Município = município,
      Natureza_Jurídica = `natureza jurídica`
    ) %>%
    select(all_of(c("Município", "Score_Médio", "Sustentabilidade", "Universalidade", "Natureza_Jurídica")))

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
#' @param var Vetor de strings com os nomes das colunas que contêm os scores, como `score médio su`, `score médio eee`.
#' @param group Vetor de strings com os nomes das colunas de agrupamento, como `natureza jurídica`.
#' @param titulo (opcional) Título do gráfico. Se `NULL`, é gerado automaticamente
#' @param paleta (opcional) Paleta de cores viridis a ser usada.
#'   Opções: `"viridis"`, `"plasma"`, `"magma"`, `"cividis"`, `"inferno"`.
#'   Padrão: `"plasma"`.
#'
#' @return Um objeto `ggplot` contendo o boxplot formatado.
#'
#' @export
#'
#' @importFrom viridis viridis
#' @import tidyverse
#' @import ggplot2
plot_boxplot <- function(df, var, group, titulo = NULL, paleta = "plasma") {
  stopifnot(
    "Faltam colunas no dataframe" = all(c(var, group) %in% colnames(df)),
    "Só é possível agrupar por coluna não numérica" = !is.numeric(df[[group]])
  )

  if (is.null(titulo)) {
    titulo <- paste0(var, " vs ", group)
    if ("ano de referência" %in% colnames(df)) {
      titulo <- paste0(titulo, " em ", df$`ano de referência`[1])
    }
  }

  df_long <- df %>%
    unite("Grupo", all_of(group), sep = " | ") %>%
    pivot_longer(
      cols = all_of(var),
      names_to = "Tipo_Score",
      values_to = "Score"
    )

  cores <- viridis::viridis(length(var), option = paleta)
  names(cores) <- var

  ggplot(df_long, aes(x = Grupo, y = Score, fill = Tipo_Score)) +
    geom_boxplot(position = position_dodge(width = 0.8), outlier.color = "gray40", outlier.alpha = 0.6, width = 0.5) +
    scale_fill_manual(values = cores, name = "Tipo de Score") +
    labs(title = titulo, x = paste(group, collapse = " | "), y = var) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
      axis.title.x = element_text(margin = margin(t = 12)),
      axis.title.y = element_text(margin = margin(r = 12)),
      legend.position = "none"
    )
}


#' Histograma com curva de densidade
#'
#' Cria um histograma com a curva de densidade sobreposta,
#' permitindo visualizar simultaneamente a frequência e a distribuição suave.
#'
#' @param df Data frame contendo os dados.
#' @param var String com o nome da coluna numérica a ser analisada, como `"IN023"`.
#' @param titulo (opcional) Título do gráfico. Se `NULL`, é gerado automaticamente.
#'
#' @return Um objeto `ggplot` com o histograma e a curva de densidade.
#'
#' @export
#'
#' @import tidyverse
#' @import ggplot2
plot_hist <- function(df, var, titulo = NULL) {
  stopifnot(
    "A var informada não está no dataframe" = var %in% colnames(df)
  )

  if (is.null(titulo)) {
    titulo <- paste0("Distribuição de ", var)
    if ("ano de referência" %in% colnames(df)) {
      titulo <- paste0(titulo, " em ", df$`ano de referência`[1])
    }
  }

  ggplot(df, aes(x = .data[[var]])) +

    geom_histogram(
      aes(y = ..density..),
      bins = 30,
      fill = "#6BAED6",
      color = "white",
      alpha = 0.6
    ) +

    geom_density(
      color = "#08519C",
      alpha = 0.4,
      linewidth = 1
    ) +
    labs(
      title = titulo,
      x = var,
      y = "Densidade"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
}
