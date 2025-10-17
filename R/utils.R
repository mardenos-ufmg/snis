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
