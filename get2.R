library(dplyr)
library(mice)

df = read("data/Agregado-2018.csv")
df = read("data/Agregado-completo.csv")
df = read("data/Desagregado-2018.csv")
df = read("data/Desagregado-2022-2021.csv")



numerico2 = c(
  "POP_URB","POP_TOT","IN006","IN016","AG022",
  "IN002","IN031","IN101","IN049","IN019","IN023","IN024",
  "IN055","IN056","IN057","IN075","IN076","IN084","IN046",
  "IN015","IN009","IN013","IN029","IN058","IN004","IN003"
)

numerico = c(numerico2, "AG013", "IN047")

input1 = df |> select(all_of(numerico))  |> mice::mice(m = 5, method = "cart", printFlag = FALSE) |> mice::complete()
input2 = df |> select(all_of(numerico2)) |> mice::mice(m = 5, method = "cart", printFlag = FALSE) |> mice::complete()

df =
  df |>
  select(-all_of(numerico)) |>
  bind_cols(input1) |>
  mutate(
    IN024 = input2$IN024,
    AG022 = pmin(input2$AG022, .data$AG013, na.rm = TRUE),
    Tarifa       = .data$IN004 / .data$IN003,
    Micromedida  = .data$AG013 - .data$AG022,
    Urbanização  = .data$POP_URB / .data$POP_TOT,
    Prestador2   = case_when(
      `Natureza jurídica` == "Empresa pública" ~ "COPANOR",
      `Natureza jurídica` == "Sociedade de economia mista com administração pública" ~ "COPASA",
      `Natureza jurídica` == "Autarquia" ~ "Autarquia",
      `Natureza jurídica` == "Administração pública direta" ~ "Prefeitura",
      `Natureza jurídica` == "Empresa privada" ~ "Empresa privada"
    )
  )

df_ = filter_data("data/Desagregado-2022-2021.csv")
df_ = filter_data("data/Desagregado-2018.csv")
identical(df, df_)






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
    a |>
    mutate(
      IN024 = a2$IN024,
      Tarifa = .data$IN004 / .data$IN003,
      Urbanização   = .data$POP_URB / .data$POP_TOT
    )

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
