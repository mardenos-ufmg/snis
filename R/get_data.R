
####################
####  ler base  ####
####################

read = function(year) {
  stopifnot("Ano deve estar entre 2010 e 2022" = year %in% 2010:2022)
  # dados retirados de https://app4.cidades.gov.br/serieHistorica/
  
  ibge =
    readxl::read_xls("data/RELATORIO_DTB_BRASIL_2024_MUNICIPIOS.xls", skip = 6) |>
    #read.csv(file("data/RELATORIO_DTB_BRASIL_2024_MUNICIPIOS.ods", encoding = "ASCII")) |>
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
      prefix = substr(.data$"Código do Município", 1, 6)
    ) |>
    suppressWarnings() |>
    suppressMessages()
  
  paste0("data/Desagregado-", year, ".csv") |>
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
    mutate(across(where(is.character), stringr::str_trim)) |>
    dplyr::left_join(ibge, by = c("Código do Município" = "prefix")) |>
    dplyr::select(-all_of("Código do Município")) |>
    dplyr::rename("Código do Município" = "Código do Município.y") |>
    dplyr::relocate("Código do Município")
}


#################################
####  tratar para aplicação  ####
#################################

process_data = function(year) {
  df =
    read(year) |>
    dplyr::select(
      all_of(c(
        "Natureza jurídica","Tipo de serviço","Abrangência","Município", "Código do Município",
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
  numerico2 = c(
    "POP_URB","POP_TOT","IN006","IN016","AG022",
    "IN002","IN031","IN101","IN049","IN019","IN023","IN024",
    "IN055","IN056","IN057","IN075","IN076","IN084","IN046",
    "IN015","IN009","IN013","IN029","IN058","IN004","IN003"
  )
  numerico = c(numerico2, "AG013", "IN047")
  
  input1 = mice::mice(df[,numerico],  m = 5, method = "cart", printFlag = FALSE) |> complete() |> as_tibble() |> suppressWarnings()
  input2 = mice::mice(df[,numerico2], m = 5, method = "cart", printFlag = FALSE) |> complete() |> as_tibble() |>suppressWarnings()
  
  df_input =
    input1 |>
    mutate(
      IN024 = input2$IN024,
      AG022 = input2$AG022,
      AG022 = case_when(
        .data$AG022 > .data$AG013 ~ .data$AG013,
        TRUE ~ .data$AG022
      ),
      Tarifa      = .data$IN004 / .data$IN003,
      Micromedida = .data$AG013 - .data$AG022,
      Urbanização = .data$POP_URB / .data$POP_TOT
    )
  
  # qtde_n_micromedida  VIROU Micromedida
  # grau_urbanizacao    VIROU Urbanização
  
  df =
    df[,!(names(df) %in% numerico)] |>
    cbind(df_input) |>
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
    
  municipios_duplicados =
    read_excel("data2/municipios_duplicados_natureza_juridica.xlsx") |>
    mutate(
      id = paste(.data$Nome_Município, "+", .data$Prestador2)
    )
  
  municipios_filter =
    df |>
    filter(.data$Município %in% municipios_duplicados$Nome_Município) |>
    mutate(
      id = paste(.data$Município,"+",.data$Prestador2)
    ) |>
    filter(
      .data$id %in% municipios_duplicados$id
    ) |>
    select(-all_of("id"))
  
  df =
    df |>
    dplyr::filter(!(.data$Município %in% municipios_duplicados$Nome_Município)) |>
    rbind(municipios_filter) |>
    tibble::as_tibble()
  
  df
}
