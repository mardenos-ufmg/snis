#Função Normalização

library(dplyr)

normalizacao <- function(df) { 
  df_normalizado <- df %>%
    mutate(
      across(
        .cols = everything(), #Em todas as colunas
        .fns = ~ pnorm(., mean = mean(.), sd = sd(.)) #Calcula a média, o desvio padrão e faz a função de distribuição acumulada da Normal
      )
    )
  
  return(df_normalizado) #Retorna o banco de dados normalizado
}

#Essa função padroniza os dados em uma escala de 0 a 1 facilitando assim, a comparação dos mesmos.