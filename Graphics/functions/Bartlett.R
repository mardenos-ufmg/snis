# FUNÇÃO: Bartlett
library(psych)
library(dplyr)

Bartlett <- function(df, alpha = 0.05) { #Alfa é o nível de significancia aceito (padrão)
  
  cor_matrix <- cor(df, use = "pairwise.complete.obs")
  n_obs <- nrow(df)
  
  bartlett_test_result <- cortest.bartlett(cor_matrix, n = n_obs) #Aplica o teste de esfericidade de Bartlett
 #Ou seja, (H0 = É uma matriz identidade) caso h0 seja verdadeira, então não há correlação.
  
  p_value <- bartlett_test_result$p.value
  
  if (p_value > alpha) {
    return(FALSE) #Não rejeitamos H0, logo não faz sentido uma Análise Fatorial
  } else {
    return(TRUE) #Rejeitamos H0
  }
}

#O objetivo dessa função é verificar se os dados são adequados para uma Análise Fatorial.