library(dplyr)
library(psych)
sentido <- function(STR, df_features, ROTACAO, verbose = TRUE) {
  #STR = quanto cada variável está associada a cada fator, df_features = o sentido esperado de cada variável (+/-), 
  #Rotacão = tipo de rotação fatorial usada, verbose = Se imprime os resultados antes/depois da inversão 
  
  sentido_positivo <- df_features %>%
    filter(Sentido == 1) %>%
    pull(Variavel) #Vtor com o nome de todas as variáveis cujo Sentido foi marcado como (positivo) na tabela df_features.
  
  STR_origin <- STR #é uma matriz em que cada fator (linha) está associado a uma variável (coluna)
  
  for (x in rownames(STR)) {
    linha <- unlist(STR[x, ]) #Seleciona a linha correspondente ao fator atual e transforma em um vetor simples (com as cargas daquele fator)
    max_abs <- max(abs(linha)) #maior valor em módulo absoluto
    indice <- names(linha)[which.max(abs(linha))] #nome da coluna que possui o maior modulo
    valor_principal <- linha[indice] #Pega o valor com o sinal original 
    
    deve_inverter <- (x %in% sentido_positivo && valor_principal > 0) ||
      (!(x %in% sentido_positivo) && valor_principal < 0) #Critério que decide se o sinal do fator deve ser invertido
    
    if (deve_inverter) {
      STR[x, ] <- -linha
    }
  }
  
  if (verbose) {
    cat("\n--- R: Fatores sem inverter ---\n")
    print(STR_origin)
    cat("\n--- R: Fatores invertidos ---\n")
    print(STR)
  }
  
  return(STR)
}

#Essa função tem o objetivo de ajustar o sentido dos fatores extraídos em uma análise fatorial, ou seja, sinal positivo para as variáveis que aumentam o fator, negativo para as que reduzem.
