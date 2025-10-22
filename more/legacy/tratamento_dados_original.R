library(readxl)
library(mice)
library(tidyr)
library(dplyr)
library("writexl")

dado <- read_excel("SNIS_2019_final.xlsx")
vars <- c("IN002","IN031","IN101","IN049","IN019","IN023","IN024",
          "IN055","IN056","IN057","IN075","IN076","IN084","IN046",
          "IN015","IN009","IN013","IN029","IN058","C?digo do Prestador","Prestador",
          "Natureza jur?dica","Tipo de servi?o","Nome_Mesorregi?o","Abrang?ncia","Nome_Munic?pio",
          "IN004","IN003","POP_URB","POP_TOT","AG013","AG022","IN006","IN016","IN047","ES006","ES005" )
dado_filter <- dado[,vars]

#Retirando variaveis de esgoto
dado_filter <- filter(dado_filter, dado_filter$`Tipo de servi?o`!= "Esgotos")
dado_filter$`Tipo de servi?o`


## Preencher com zero as variaveis de esgoto para prestadores de ?gua
var_esgoto <- c("IN006","IN016","IN047","IN015","Tipo de servi?o")
d_esgoto <- dado_filter[,var_esgoto]

for (i in 1:nrow(d_esgoto)){
  if (d_esgoto$`Tipo de servi?o`[i]=="?gua"){
    d_esgoto[i,][is.na(d_esgoto[i,])] <- "0"
  }
}

#Adicionando dados tratados
dado_filter_esgoto <- dado_filter[,!(names(dado_filter) %in% var_esgoto)] #retirando da original
dados_f <- cbind(dado_filter_esgoto,d_esgoto) #adicionando


##### mice ####
numerico <- c("IN002","IN031","IN101","IN049","IN019","IN023","IN024",
          "IN055","IN056","IN057","IN075","IN076","IN084","IN046",
          "IN015","IN009","IN013","IN029","IN058","IN004","IN003","POP_URB",
          "POP_TOT","AG013","IN006","IN016","IN047","AG022")
#
dados_input <- dados_f[,numerico]

dados_input[numerico] <- sapply(dados_input[numerico],as.numeric)
summary(dados_input)


#impute missing values, using all parameters as default values
dados_input.imp <- mice(dados_input,m=5,method = "cart")
summary(dados_input.imp)
a <- complete(dados_input.imp)

#### checando colinearidade
mice:::find.collinear(dados_input)
cor(dados_input, use = "pairwise.complete.obs")

#IN024 tem multicolinearidade com IN047
#AG022 tem multicolineadidade com AG013

#### Excluindo IN047 e AG013 para imputar IN024 e AG022
numerico2 <- c("IN002","IN031","IN101","IN049","IN019","IN023","IN024",
              "IN055","IN056","IN057","IN075","IN076","IN084","IN046",
              "IN015","IN009","IN013","IN029","IN058","IN004","IN003","POP_URB",
              "POP_TOT","IN006","IN016","AG022")

dados_input2 <- a[,numerico2]

dados_input2.imp <- mice(dados_input2,m=5,method = "cart")
summary(dados_input2.imp)
a2 <- complete(dados_input2.imp)

## juntando
a$IN024 <- a2$IN024
a$AG022 <- a2$AG022

for (i in 1: length(a$AG022)){
  if (a$AG022[i]>a$AG013[i]){
    a$AG022[i] <- a$AG013[i]
  }
}

###Criando colunas

#"prestador2","grau_urbanizacao","qtde_n_micromedida","Razao"
a$Tarifa <- a$IN004 / a$IN003
a$qtde_n_micromedida <- a$AG013 - a$AG022
a$grau_urbanizacao <- a$POP_URB / a$POP_TOT

#Adicionando dados tratados
dados_missing <- dados_f[,!(names(dados_f) %in% numerico)] #retirando da original
dataset_final <- cbind(a,dados_missing) #adicionando

dataset_final <- dataset_final %>% mutate(Prestador2 =
               case_when(dataset_final$`Natureza jur?dica` == "Empresa p?blica" ~ "COPANOR",
                         dataset_final$`Natureza jur?dica` == "Sociedade de economia mista com administra??o p?blica" ~ "COPASA",
                         dataset_final$`Natureza jur?dica` == "Autarquia" ~ "Autarquia",
                         dataset_final$`Natureza jur?dica` == "Administra??o p?blica direta" ~ "Prefeitura",
                         dataset_final$`Natureza jur?dica` == "Empresa privada" ~ "Empresa privada")
)

###### Resolvendo duplicados ####
municipios_duplicados <- read_excel("C:\\Users\\DELL\\Google Drive\\2021-2\\TCC\\Codigos\\municipios_duplicados_natureza_juridica.xlsx")

#retirando os duplicados
municipios_filter <- dataset_final[dataset_final$Nome_Munic?pio %in% municipios_duplicados$Nome_Munic?pio,]

#criando id
municipios_duplicados$id <- paste(municipios_duplicados$Nome_Munic?pio,"+",municipios_duplicados$Prestador2)
municipios_filter$id <- paste(municipios_filter$Nome_Munic?pio,"+",municipios_filter$Prestador2)

#Removendo ids diferentes
municipios_filter2 <- municipios_filter[municipios_filter$id %in% municipios_duplicados$id,]

#Removendo id
municipios_filter3 <- subset(municipios_filter2, select = -id )

#Adicionando dados tratados
dataset_final_2 <- dataset_final[!dataset_final$Nome_Munic?pio %in% municipios_duplicados$Nome_Munic?pio,]#retirando da original
dataset_final_3 <- rbind(dataset_final_2,municipios_filter3) #adicionando

#### Exportando ####
write_xlsx(dataset_final_3,path="C:\\Users\\DELL\\Google Drive\\2021-2\\TCC\\Codigos\\dt_missing_excel.xlsx")

"The problem with using mice for imputation here is the large number of unbalanced factor variables
in this dataset. When these are turned into dummy variables there is a high probability
that you will have one column a linear combination of another.
Since the default imputation methods involve linear regression,
this results in a X matrix that cannot be inverted.
One solution is to change the default imputation method to one that is not stochastic"

#https://stackoverflow.com/questions/36330570/mice-does-not-impute-certain-columns-but-also-does-not-give-an-error




