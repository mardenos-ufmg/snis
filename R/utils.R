#Alfa é o nível de significancia aceito (padrão)
# FALSE Não rejeitamos H0, logo não faz sentido uma Análise Fatorial
# TRUE Rejeitamos H0
#O objetivo dessa função é verificar se os dados são adequados para uma Análise Fatorial.
Bartlett <- function(df, alpha = 0.05) {
  p.value =
    psych::cortest.bartlett(
      cor(df, use = "pairwise.complete.obs"),
      n = nrow(df)
      ) |>
    {\(.) .$p.value}()

  if (p.value > alpha) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}


update_df_fa = function(df, ...) {
  FAs = list(...)
  #df_score = FAs[[1]]$geral$scores["código do município"]

  for (i in 1:length(FAs)) {
    df_score_aux =
      FAs[[i]]$geral$scores |>
      select(-`código do município`)
    colnames = colnames(df_score_aux)
    sufixo = colnames(df_score_aux)[-1] |> substr(1,1) |> paste0(collapse = "")
    colnames[1] = paste(colnames[1], sufixo)
    colnames = paste("score", colnames)
    df_score_aux =
      df_score_aux |>
      `colnames<-`( colnames )
    df = cbind(df, df_score_aux)
    # colnames(df_score_aux)[-1] = paste("score", colnames(df_score_aux)[-1])
    # colnames(df_score_aux)[-1] = paste("score", colnames(df_score_aux)[-1])
    # df_score = dplyr::left_join(df_score, df_score_aux, by = "código do município")
  }

  # dplyr::left_join(df, df_score, by = "código do município")
  as_tibble(df)
}


quartile = function(vec) {
  cut(
    vec,
    breaks = quantile(vec, probs = seq(0, 1, 0.25)),
    include.lowest = TRUE,
    labels = c("Q1", "Q2", "Q3", "Q4")
  )
}
