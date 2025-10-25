#' Ler CSV
#'
#' @param ano integer between 2010&
#'
#' @returns tibble::tibble
#'
#' @export
#'
# dados retirados de https://app4.cidades.gov.br/serieHistorica/
read = function(ano) {
  stopifnot("Ano deve estar entre 2015 e 2022" = ano %in% 2010:2022)

  ibge =
    readODS::read_ods("data/RELATORIO_DTB_BRASIL_2024_MUNICIPIOS.ods", skip = 6) |>
    select(c("Código Município Completo",
                    "Região Geográfica Intermediária",
                    "Nome Região Geográfica Intermediária",
                    "Região Geográfica Imediata",
                    "Nome Região Geográfica Imediata")) |>
    `colnames<-`(c("Código do Município",
                   "Código da Região Intermediária",
                   "Região Intermediária",
                   "Código da Região Imediata",
                   "Região Imediata")) |>
    mutate(
      prefix = substr(.data$"Código do Município", 1, 6) |> as.numeric(),
      across(starts_with("Código"), as.numeric)
    ) |>
    suppressWarnings() |>
    suppressMessages()

  colnames =
    paste0("data/Desagregado-", ano, ".csv") |>
    file(encoding = "UTF-16LE") |>
    readLines(1) |>
    strsplit(";") |>
    purrr::pluck(1) |>
    {\(.) gsub("\"", "", .)}() |>
    {\(.) gsub("\\\\", "", .)}() |>
    {\(.) sub(" - .*", "", .) }()

  df =
    paste0("data/Desagregado-", ano, ".csv") |>
    file(encoding = "UTF-16LE") |>
    readLines() |>
    {\(.) .[2:(length(.)-1)] }() |>
    {\(.) substr(., 1, nchar(.) - 1) }() |>
    {\(.)
      read.csv2(
        text = .,
        header = FALSE,
        dec = ",",
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    }() |>
    tibble::as_tibble() |>
    `colnames<-`(colnames) |>
    {\(.) mutate(.,
                        across(
                          where(is.character),
                          ~ na_if(., "")
    ))}() |>
    {\(.) {
      col_sem_letras = names(.)[  sapply(., function(col) is.character(col) && all(!grepl("[A-Za-z]", col)))  ]
      mutate(., across(all_of(col_sem_letras),
                       ~ readr::parse_number(., locale = readr::locale(decimal_mark = ",", grouping_mark = "."))
      ))
    }}() |>
    mutate(across(where(is.character), stringr::str_trim)) |>
    left_join(ibge, by = c("Código do Município" = "prefix")) |>
    select(-all_of("Código do Município")) |>
    rename("Código do Município" = "Código do Município.y") |>
    relocate("Código do Município") |>
    relocate(c("Código da Região Intermediária", "Região Intermediária", "Código da Região Imediata", "Região Imediata"), .after = "Natureza jurídica") |>
    relocate(c("POP_TOT", "POP_URB"), .after = "Região Imediata") |>
    rename_with(tolower, .cols = 1:14) |>
    mutate(
      across(c("prestador", "sigla do prestador", "abrangência", "tipo de serviço", "natureza jurídica", "região intermediária","região imediata"),
             as.factor)
    )

  df
}



#' Processar dados
#'
#' @param ano inteiro
#'
#' @returns tibble::tibble
#' @export
process_data = function(ano) {
  cols = c(
    "ano de referência","natureza jurídica","tipo de serviço","abrangência","município",
    "código do município","código da região intermediária", "região intermediária",
    "código do prestador","prestador","POP_URB","POP_TOT",
    "IN002","IN031","IN101","IN049","IN019","IN023","IN024",
    "IN055","IN056","IN057","IN075","IN076","IN084","IN046",
    "IN015","IN009","IN013","IN029","IN058","IN004","IN003",
    "AG013","AG022","IN006","IN016","IN047","ES006","ES005"
  )

  df =
    read(ano) |>
    select(all_of(cols)) |>
    filter(.data$"tipo de serviço" != "Esgotos") |>
    mutate(
      across(c("IN006","IN016","IN047","IN015"),
             ~ case_when(
               is.na(.) & .data$`tipo de serviço` == "Água" ~ 0,
               TRUE ~ .
    )))

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

  input1 = mice::mice(df[,numerico],  m = 5, method = "cart", printFlag = FALSE, seed = 1) |> complete() |> as_tibble() |> suppressWarnings()
  input2 = mice::mice(df[,numerico2], m = 5, method = "cart", printFlag = FALSE, seed = 1) |> complete() |> as_tibble() |>suppressWarnings()

  df_input =
    input1 |>
    mutate(
      IN024 = input2$IN024,
      AG022 = input2$AG022,
      AG022 = case_when(
        .data$AG022 > .data$AG013 ~ .data$AG013,
        TRUE ~ .data$AG022
      ),
      tarifa      = .data$IN004 / .data$IN003,
      micromedida = .data$AG013 - .data$AG022,
      urbanização = .data$POP_URB / .data$POP_TOT
    )

  # qtde_n_micromedida  VIROU micromedida
  # grau_urbanizacao    VIROU urbanização

  df =
    df[,!(names(df) %in% numerico)] |>
    cbind(df_input) |>
    mutate(
      prestador2 =
        case_when(
          .data$`natureza jurídica` == "Empresa pública" ~ "COPANOR",
          .data$`natureza jurídica` == "Sociedade de economia mista com administração pública" ~ "COPASA",
          .data$`natureza jurídica` == "Autarquia" ~ "Autarquia",
          .data$`natureza jurídica` == "Administração pública direta" ~ "Prefeitura",
          .data$`natureza jurídica` == "Empresa privada" ~ "Empresa privada"
        )
    )

  municipios_duplicados =
    readODS::read_ods("data/duplicatas.ods") |>
    mutate(
      id = paste(.data$município, "+", .data$prestador2)
    )

  municipios_filter =
    df |>
    filter(.data$município %in% municipios_duplicados$município) |>
    mutate(
      id = paste(.data$município,"+",.data$prestador2)
    ) |>
    filter(
      .data$id %in% municipios_duplicados$id
    ) |>
    select(-all_of("id"))

  df =
    df |>
    filter(!(.data$município %in% municipios_duplicados$município)) |>
    rbind(municipios_filter) |>
    tibble::as_tibble() |>
    relocate(c("tarifa", "micromedida", "urbanização", "prestador2"))

  df
}
