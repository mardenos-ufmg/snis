
####################
####  ler base  ####
####################

read = function(file) {
  file |>
    file(encoding = "UTF-16LE") |>
    readLines() |>
    {\(.) sub(";\\s*$", "", .)}() |>
    {\(.)
      read.csv2(
        text = .,
        dec = ",",
        header = TRUE,
        stringsAsFactors = FALSE,
        check.names = FALSE
        )
    }() |>
    {\(.) `colnames<-`(., sub(" - .*", "", colnames(.) ))}() |>
    tibble::as_tibble() |>
    {\(.) dplyr::mutate(., across(where(is.character),
                                  ~ na_if(., "")))
      }() |>
    {\(.) {
      if(  grepl("TOTAL DA AMOSTRA", toupper(tail(.[[1]], 1)))  ) {
        . = head(., -1)
      }
      .
    }}() |>
    {\(.) {
      col_sem_letras = names(.)[
        sapply(., function(col) is.character(col) && all(!grepl("[A-Za-z]", col)))
      ]
      col_sem_letras = setdiff(col_sem_letras, c("Código do Prestador", "Código do Município"))
      mutate(., across(all_of(col_sem_letras),
                       ~ readr::parse_number(., locale = readr::locale(decimal_mark = ",", grouping_mark = "."))
                       ))
    }}() |>
    mutate(across(where(is.character), stringr::str_trim))
}

df = read("data/Agregado-2018.csv")
df = read("data/Agregado-completo.csv")
df = read("data/Desagregado-2018.csv")
df = read("data/Desagregado-2022-2021.csv")

readr::guess_encoding("data/Agregado-20251012180348.csv")

dados = read_excel("data/SNIS_COMPLETO_2021.xlsx")

readr::guess_encoding("data/Desagregado-20251012183018.csv")

vars = c(
  "Natureza jurídica","Tipo de serviço","Abrangência","Município","Código do Prestador","Prestador","POP_URB","POP_TOT",
  "IN002","IN031","IN101","IN049","IN019","IN023","IN024",
  "IN055","IN056","IN057","IN075","IN076","IN084","IN046",
  "IN015","IN009","IN013","IN029","IN058","IN004","IN003",
  "AG013","AG022","IN006","IN016","IN047","ES006","ES005"
  )

sum(!(vars %in% colnames(df)))  # há 7 vars que não estão no agregado, todas estão no desagregado
# Falta "Nome_Mesorregi?o"
vars[!(vars %in% colnames(df))]

read.csv(file("data/RELATORIO_DTB_BRASIL_2024_MUNICIPIOS.ods", encoding = "ASCII"))


# Tipo de Serviço AGREGADO
# Tipo de serviço DESAGREGADO

#################################
####  tratar para aplicação  ####
#################################

filter_data = function(file) {
  df = read(file)
  
  ibge =
    readxl::read_xls("data/RELATORIO_DTB_BRASIL_2024_MUNICIPIOS.xls", skip = 6) |>
    #readxl::read_xls("data/RELATORIO_DTB_BRASIL_2024_MUNICIPIOS.xls", skip = 6, range = "A7:I9999") |>
    dplyr::select(c("Código Município Completo",
                    "Região Geográfica Intermediária",
                    "Nome Região Geográfica Intermediária",
                    "Região Geográfica Imediata",
                    "Nome Região Geográfica Imediata")) |>
    `colnames<-`(c("Código do Município",
                   "Código da Região Intermediária",
                   "Região Intermediária",
                   "Código da Região Imediata",
                   "Região Imediata")) |>
    dplyr::mutate(
      "Código do Município" = substr(.data$"Código do Município", 1, 6)
    )
  
  df =
    df |>
    dplyr::left_join(ibge, by = "Código do Município") |>
    dplyr::select(
      all_of(c(
        "Natureza jurídica","Tipo de serviço","Abrangência","Município",
        "Região Intermediária", "Código do Prestador","Prestador","POP_URB","POP_TOT",
        "IN002","IN031","IN101","IN049","IN019","IN023","IN024",
        "IN055","IN056","IN057","IN075","IN076","IN084","IN046",
        "IN015","IN009","IN013","IN029","IN058","IN004","IN003",
        "AG013","AG022","IN006","IN016","IN047","ES006","ES005"
      ))
    ) |>
    dplyr::filter(.data$"Tipo de serviço" != "Esgotos") |>
    mutate(
      across(c("IN006","IN016","IN047","IN015"),
             ~ case_when(
               is.na(.) & .data$`Tipo de serviço` == "Água" ~ 0, 
               TRUE ~ .
             )
      )
    )
  
  #IN024 tem multicolinearidade com IN047 
  #AG022 tem multicolineadidade com AG013
  #### Excluindo IN047 e AG013 para imputar IN024 e AG022
  numerico = c("IN002","IN031","IN101","IN049","IN019","IN023","IN024",
                "IN055","IN056","IN057","IN075","IN076","IN084","IN046",
                "IN015","IN009","IN013","IN029","IN058","IN004","IN003","POP_URB",
                "POP_TOT","AG013","IN006","IN016","IN047","AG022")
  numerico2 = c("IN002","IN031","IN101","IN049","IN019","IN023","IN024",
                 "IN055","IN056","IN057","IN075","IN076","IN084","IN046",
                 "IN015","IN009","IN013","IN029","IN058","IN004","IN003","POP_URB",
                 "POP_TOT","IN006","IN016","AG022")
  dados_f = df
  dados_input <- dados_f[,numerico]
  dados_input[numerico] <- sapply(dados_input[numerico],as.numeric)
  dados_input.imp <- mice(dados_input,m=5,method = "cart", printFlag = FALSE)
  a <- complete(dados_input.imp)
  dados_input2 <- a[,numerico2]
  dados_input2.imp <- mice(dados_input2,m=5,method = "cart", printFlag = FALSE)
  a2 <- complete(dados_input2.imp)
  
  input =
    # df |>
    # select(all_of(numerico)) |>
    # mice::mice(m = 5, method = "cart", printFlag = FALSE) |>
    # mice::complete() |>
    a |>
    mutate(
      IN024 = a2$IN024,
      AG022 = a2$AG022,
      AG022 = case_when(
        .data$AG022 > .data$AG013 ~ .data$AG013,
        TRUE ~ .data$AG022
      ),
      Tarifa = .data$IN004 / .data$IN003,
      Micromedida = .data$AG013 - .data$AG022,
      Urbanização   = .data$POP_URB / .data$POP_TOT
    )
  
  # qtde_n_micromedida  VIROU Micromedida
  # grau_urbanizacao    VIROU Urbanização
  
  df =
    df[,!(names(dados_f) %in% numerico)] |>
    cbind(input) |>
    mutate(
      Prestador2 =
        case_when(
          .data$`Natureza jurídica` == "Empresa pública" ~ "COPANOR",
          .data$`Natureza jurídica` == "Sociedade de economia mista com administração pública" ~ "COPASA",
          .data$`Natureza jurídica` == "Autarquia" ~ "Autarquia",
          .data$`Natureza jurídica` == "Administração pública direta" ~ "Prefeitura",
          .data$`Natureza jurídica` == "Empresa privada" ~ "Empresa privada"
        )
    )
    
  df
}
 
# precisa "Região Imediata"
# Começar com nome de ... ?
# Em 2017, o IBGE reformulou a divisão regional do país, substituindo as mesorregiões pelas novas regiões geográficas intermediárias, e as microrregiões pelas regiões geográficas imediatas. 
# site do SNIS ainda fala 

#any(duplicated(substr(ibge$"Código do Município", 1, 6)))


dado <- read("data/Desagregado-2019.csv")
dado <- filter_data("data/Desagregado-2019.csv")

dado <- read_excel("data/SNIS_2019_final.xlsx")
dado <- read_excel("data/SNIS_COMPLETO_2021.xlsx")





#################################
####  Resolvendo duplicados  ####
#################################



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

