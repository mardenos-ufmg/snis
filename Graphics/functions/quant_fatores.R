#Função Quant. de Fatores
library(dplyr)

quant_fatores <- function(df_x, delta = 0.05) { #Usa um padrão de tolerância de 0,05
  
  threshold <- 1 - delta #Limiar = Vou considerar como importante todo fator com autovalor maior que 0,95
  
  CorrMx <- cor(df_x, use = "pairwise.complete.obs") #Matriz de correlação em que os NA são tratados par a par sem excluir linhas inteiras. 
  
  eigen_results <- eigen(CorrMx) #Cálculo de Autovalores
  autovalores <- eigen_results$values #Cada autovalor representa a quantidade de variância explicada por um componente
  
  quant_fatores_result <- sum(autovalores > threshold) #Conta quantos autovalores são maiores que o limiar estabelecido
  
  return(quant_fatores_result)
}

#Essa função tem como objetivo determinar o número de fatores relevantes a serem considerados numa análise fatoria. Quanto maior o autovalor, mais informação aquele fator retém dos dados originais.