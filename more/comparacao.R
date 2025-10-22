library(tidyverse)
devtools::load_all()


df_new = process_data(2019) |> select(-all_of(c("código do município","código da região intermediária")))

dado =
  read(2019)

vars <- c("IN002","IN031","IN101","IN049","IN019","IN023","IN024",
          "IN055","IN056","IN057","IN075","IN076","IN084","IN046",
          "IN015","IN009","IN013","IN029","IN058","código do prestador","prestador",
          "natureza jurídica","tipo de serviço","região intermediária","abrangência","município",
          "IN004","IN003","POP_URB","POP_TOT","AG013","AG022","IN006","IN016","IN047","ES006","ES005" )
dado_filter <- dado[,vars]

#Retirando variaveis de esgoto
dado_filter <- filter(dado_filter, dado_filter$`tipo de serviço`!= "Esgotos")

## Preencher com zero as variaveis de esgoto para prestadores de ?gua
var_esgoto <- c("IN006","IN016","IN047","IN015","tipo de serviço")
d_esgoto <- dado_filter[,var_esgoto]

for (i in 1:nrow(d_esgoto)){
  if (d_esgoto$`tipo de serviço`[i]=="?gua"){
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


#impute missing values, using all parameters as default values
#set.seed(12345)
dados_input.imp <- mice::mice(dados_input,m=5,method = "cart", seed = 1)
a <- complete(dados_input.imp)

#### checando colinearidade
# mice:::find.collinear(dados_input)
# cor(dados_input, use = "pairwise.complete.obs")

#IN024 tem multicolinearidade com IN047
#AG022 tem multicolineadidade com AG013

#### Excluindo IN047 e AG013 para imputar IN024 e AG022
numerico2 <- c("IN002","IN031","IN101","IN049","IN019","IN023","IN024",
              "IN055","IN056","IN057","IN075","IN076","IN084","IN046",
              "IN015","IN009","IN013","IN029","IN058","IN004","IN003","POP_URB",
              "POP_TOT","IN006","IN016","AG022")
dados_input2 <- a[,numerico2]

#set.seed(12345)
dados_input2.imp <- mice::mice(dados_input2,m=5,method = "cart", seed = 1)
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

#"prestador2","urbanização","micromedida","Razao"
a$tarifa <- a$IN004 / a$IN003
a$micromedida <- a$AG013 - a$AG022
a$urbanização <- a$POP_URB / a$POP_TOT

#Adicionando dados tratados
dados_missing <- dados_f[,!(names(dados_f) %in% numerico)] #retirando da original
dataset_final <- cbind(a,dados_missing) #adicionando

dataset_final <- dataset_final %>% mutate(prestador2 =
               case_when(dataset_final$`natureza jurídica` == "Empresa pública" ~ "COPANOR",
                         dataset_final$`natureza jurídica` == "Sociedade de economia mista com administração pública" ~ "COPASA",
                         dataset_final$`natureza jurídica` == "Autarquia" ~ "Autarquia",
                         dataset_final$`natureza jurídica` == "Administração pública direta" ~ "Prefeitura",
                         dataset_final$`natureza jurídica` == "Empresa privada" ~ "Empresa privada")
)

###### Resolvendo duplicados ####
municipios_duplicados <- readODS::read_ods("data/duplicatas.ods")

#retirando os duplicados
municipios_filter <- dataset_final[dataset_final$município %in% municipios_duplicados$município,]

#criando id
municipios_duplicados$id <- paste(municipios_duplicados$município,"+",municipios_duplicados$prestador2)
municipios_filter$id <- paste(municipios_filter$município,"+",municipios_filter$prestador2)

#Removendo ids diferentes
municipios_filter2 <- municipios_filter[municipios_filter$id %in% municipios_duplicados$id,]

#Removendo id
municipios_filter3 <- subset(municipios_filter2, select = -id )

#Adicionando dados tratados
dataset_final_2 <- dataset_final[!dataset_final$município %in% municipios_duplicados$município,]#retirando da original
df_original <-
  rbind(dataset_final_2,municipios_filter3) |>
  as_tibble() |>
  relocate(colnames(df_new))




all.equal(df_original, df_new)
