# falta sentido
# rotacao = 'oblimin',
# normalizar_score = TRUE
fa2 = function(
    year,
    features = readODS::read_ods("data/features.ods"),
    delta = 0.05
    ) {
  
  df = process_data(year)
  FA = list()
  
  for (grupo_nome in unique(features$`Grupo DH`)) {
    
    variaveis =
      features |>
      dplyr::filter(.data$`Grupo DH` == grupo_nome) |>
      pull(Variável)
    
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
    loadings = fa(dados, nfactors = quant_fatores, rotate = rotacao, fm = "minres")
    scores   = psych::factor.scores(dados, loadings, method = "regression")
    
    loadings_df =
      loadings$loadings |>
      unclass() |>
      tibble::as_tibble() |>
      dplyr::mutate(
        Variável = variaveis
      )

    scores_df =
      scores$scores |>
      tibble::as_tibble() |>
      mutate() |>
      dplyr::mutate(
        !!grupo_nome := rowSums(across(everything()), na.rm = TRUE),
        `Código Município` = df$`Código do Município`,
        across(
          .cols = !!grupo_nome,
          .fns = ~ pnorm(., mean = mean(.), sd = sd(.))
        )
      )
        
    FA[[grupo_nome]] =
      list(
        scores   = scores_df,
        loadings = loadings_df
        )
  }
  
  scores =
    lapply(FA, function(x) x$scores[,ncol(x$scores)-1] |> purrr::pluck(1)) |>
    tibble::as_tibble() |>
    mutate(
      `Score Médio` = round(rowMeans(across(everything()), na.rm = TRUE), 3),
      `Código Município` = df$`Código do Município`
      )
  
  list(
    geral = scores,
    FA    = FA
    )
}
