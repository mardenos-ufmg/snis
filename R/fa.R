# falta sentido
# falta rotacao = 'oblimin',
# falta normalizar_score
# falta códigos de anos passados
fa = function(
    df,
    grupos = NULL,
    delta = 0.05
    ) {
  if (is.null(grupos)) {
    grupos =
      system.file("extdata", "grupos.ods", package = "snis") |>
      readODS::read_ods()

  }

  grupos =
    grupos |>
    `colnames<-`(c("variavel", "grupo")) |>
    dplyr::mutate(
      grupo = tolower(grupo)
    )
  FA = list()

  for (grupo_nome in unique(grupos$grupo)) {

    variaveis =
      grupos |>
      dplyr::filter(.data$grupo == grupo_nome) |>
      purrr::pluck("variavel")

    dados =
      df |>
      select(all_of(variaveis)) |>
      scale() |>
      tibble::as_tibble()

    if (!Bartlett(dados)) {
      warning(
        cat("Não passou no teste de Bartlett o grupo ", grupo_nome, "\n")
      )
    }

    autovalores =
      dados |>
      cor(use = "pairwise.complete.obs") |>
      eigen() |>
      purrr::pluck("values")

    quant_fatores = sum(autovalores > 1 - delta)
    loadings = psych::fa(dados, nfactors = quant_fatores, fm = "minres")
    scores   = psych::factor.scores(dados, loadings, method = "regression")

    loadings$df =
      loadings$loadings |>
      unclass() |>
      tibble::as_tibble() |>
      dplyr::mutate(
        variável = variaveis
      ) |>
      dplyr::relocate(variável)

    scores$df =
      scores$scores |>
      tibble::as_tibble() |>
      mutate() |>
      dplyr::mutate(
        !!grupo_nome := rowSums(across(everything()), na.rm = TRUE),
        `código do município` = df$`código do município`,
        across(
          .cols = !!grupo_nome,
          .fns = ~ pnorm(., mean = mean(.), sd = sd(.))
        )
      ) |>
      dplyr::relocate(c(`código do município`, !!grupo_nome))


    FA[[grupo_nome]] =
      list(
        scores   = scores,
        loadings = loadings
      )
  }

  FA$geral$scores =
    lapply(FA, function(x) x$scores$df[,2] |> purrr::pluck(1)) |>
    tibble::as_tibble() |>
    mutate(
      `médio` = round(rowMeans(across(everything()), na.rm = TRUE), 3),
      `código do município` = df$`código do município`
      ) |>
    dplyr::relocate(c(`código do município`, `médio`))

  class(FA) = "fa-snis"

  FA
}
