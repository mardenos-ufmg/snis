# ==============
# Função Score 
# =============
Score <- function(df, df_features, delta = 0.05, normalizar_score = TRUE, rotacao = 'oblimin', verbose = FALSE) {
  
  scores <- fatores(df, df_features, delta, rotacao, verbose)

  if (normalizar_score) scores <- normalizacao(scores)
  
  scores <- scores %>%
    mutate(Score_Medio = round(rowMeans(across(everything()), na.rm = TRUE), 3))
  
  cat(paste0(strrep("-", 50), " Scores ", strrep("-", 50), "\n"))
  print(scores)
  
  return( scores )
}
