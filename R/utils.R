#' Teste de Barlett
#'
#' O objetivo dessa função é verificar se os dados são adequados para uma Análise Fatorial, isto é,
#' se há correlação entre as variáveis.
#'
#' @section Tipo de correlação:
#'
#' Para computar a matriz de covarância escolhemos o tipo `na.or.complete`, que considera apenas
#' as linha completas (sem `NA`) de `df`. Se não houver linhas completas, o valor retornado será
#' um `NA`. Originalmente usava-se o `pairwise.complete.obs`, mas isso pode retornar matrizes de
#' correlçao com autoravalores negativos, o que não é adequado.
#' Esses métodos de computação da matriz de covariância estão disponíveis no argumento
#' `use` da função `stats::cor`.
#'
#'
#' @param df data.frame com dados
#' @param alpha número entre zero e um; nível de significancia aceito; o padrão é 0.05.
#'
#' @returns `TRUE` se há dependência entre os dados ou
#' `FALSE` se não há (logo não faz sentido uma Análise Fatorial),
#' é possível retornar `NA` se não há linhas completas.
#'
#' @export
#'
#' @importFrom psych cortest.bartlett
#' @importFrom purrr pluck
bartlett = function(df, alpha = 0.05) {
  p.value =
    df |>
    cor(use = "na.or.complete") |>
    psych::cortest.bartlett( n = sum(complete.cases(df)) ) |>
    purrr::pluck("p.value")

  return(p.value <= alpha)
}


#' Atualizar data frame com scores
#'
#' Adiciona colunas no data frame original com os valores dos scores desejados
#'
#' @param df data.frame usado para fazer análise fatorial
#' @param ... objetos do tipo `fa-snis`
#'
#' @returns data.frame atualizado
#'
#' @export
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
    # df_score = left_join(df_score, df_score_aux, by = "código do município")
  }

  # left_join(df, df_score, by = "código do município")
  as_tibble(df)
}


#' Criar vetor de quartis
#'
#' Cria um vetor que mostra em que faixa dos quartis os elementos do vetor original estão
#'
#' @param vec vetor de números a serem analisados
#'
#' @returns vetor com a faixa quartílica de cada número
#'
#' @export
quartis = function(vec) {
  cut(
    vec,
    breaks = quantile(vec, probs = seq(0, 1, 0.25)),
    include.lowest = TRUE,
    labels = c("Q1", "Q2", "Q3", "Q4")
  )
}
