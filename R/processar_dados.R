#' Ler CSV do SNIS
#'
#' @section Origem dos dados:
#'
#' Os dados foram retirados de `https://app4.cidades.gov.br/serieHistorica/` com as seguintes opções:
#' - Informações e indicadores desagregados;
#' - Ano de referẽncia: `ano`;
#' - Região: `Sudeste`;
#' - Estado: `Minas Gerais`;
#' - Demais filtros: `Marcar todos`.
#'
#'
#' @section Estrutura do CSV:
#'
#' Por meio da função `readr::guess_encoding()` podemos ver que a codificação dos CSVs é `UTF-16LE`.
#'
#' As colunas são separadas por `;` e possuem um `;` extra ao final de cada linha, o que pode produzir uma coluna
#' extra a depender da forma como esse arquivo é lido. Isso foi evitado aqui por meio da exclusão do último caractere
#' de cada durante a leitra do CSV.
#'
#' A última linha - que originalmente continha valores totais das variáveis da amostra - foi excluída.
#'
#' Para os números, o separador decimal original era a vírgula e o de centenas, o ponto.
#'
#'
#' @section Estilo:
#'
#' Optamos por manter acentos e diacríticos (como o til) no nome das colunas, mas o nome de
#' todas as colunas foi passada para caracteres minúsculos. Originalmente, a descrição de cada variável
#' estava no pŕoprio nome da coluna, após um travessão `-`, mas aqui optamos por excluir tudo o que
#' vinha depois disso. A descrição das variáveis está disponível na vinheta
#' \href{https://mardenos-ufmg.github.io/snis/articles/dados.html}{`dados`}.
#'
#'
#' @section IBGE:
#'
#' Originalmente, o `código do município` no CSV não continha o último dígito de verificação.
#' Optamos por colocar esse último dígito, conforme padrão do IBGE.
#' Ainda de acordo com o IBGE, as colunas referentes à `região intermediária` e à `região imediata`
#' foram adicionadas.
#'
#'
#' @param ano inteiro entre 2000 e 2022
#'
#' @returns tibble
#'
#' @export
#'
#' @importFrom purrr pluck
#' @importFrom readODS read_ods
#' @importFrom readr parse_number locale
#' @import tidyverse
read = function(ano) {
  stopifnot("Ano deve estar entre 2000 e 2022" = ano %in% 2000:2022)

  ibge =
    system.file("extdata", "RELATORIO_DTB_BRASIL_2024_MUNICIPIOS.ods", package = "snis") |>
    readODS::read_ods(skip = 6) |>
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
    system.file("extdata",
                paste0("Desagregado-", ano, ".csv"),
                package = "snis") |>
    file(encoding = "UTF-16LE") |>
    readLines(1) |>
    strsplit(";") |>
    purrr::pluck(1) |>
    {\(.) gsub("\"", "", .)}() |>
    {\(.) gsub("\\\\", "", .)}() |>
    {\(.) sub(" - .*", "", .) }()

  df =
    system.file("extdata",
                paste0("Desagregado-", ano, ".csv"),
                package = "snis") |>
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
    as_tibble() |>
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


#' Processar dados para análise fatorial
#'
#' Rotina de processamento (e criação) de variáveis para a aplicação de análise fatorial
#'
#' @section Inputação:
#'
#' A inputação foi feita pelo pacote `mice` em duas etapas. Detectamos que as seguintes variáveis têm
#' forte correlação entre si: IN024 com IN047; AG022 com AG013. Como essa forte multicolinearidade piora
#' a qualidade das previsões feitas pelo `mice`, decidimos excluindo IN047 e AG013 para a primeira
#' fase de imputação (em que IN024 e AG022 são imputadas). Em seguida, usamos os valores imputados
#' para prever IN047 e AG013.
#'
#' O pacote `mice` usa alguma aleatoriedade para suas previsões, por isso decidimos fixar a semente
#' das previsões para que os mesmos resultados sejam retornados em cada ano.
#'
#' Os valores de algumas variáveis foram imputadas por simples lógica, como as que se seguem:
#'
#' - `IN003`: ;
#'
#'
#' @section Variáveis criadas:
#'
#' Algumas variáveis foram criadas com as seguintes fórmulas:
#'
#' - `tarifa`: ;
#' - `micromedida`: ;
#' - `urbanização`: ;
#' - `prestador2`: ;
#'
#'
#'
#' @inheritParams read
#'
#' @returns tibble
#'
#' @export
#'
#' @importFrom mice mice
#' @importFrom readODS read_ods
#' @import tidyverse
processar_dados = function(ano) {
  cols = c(
    "ano de referência","município","código do município",
    "natureza jurídica","tipo de serviço","abrangência",
    "código do prestador","prestador",
    "código da região intermediária","região intermediária","código da região imediata", "região imediata",
    "POP_URB","POP_TOT",
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
               )
             ),
      IN003 = case_when(
        .data$IN003 == 0 ~ NA,
        TRUE ~ .data$IN003
        )
    )

  numerico2 = c("IN002","IN031","IN101","IN049","IN019","IN023","IN024",
                "IN055","IN056","IN057","IN075","IN076","IN084","IN046",
                "IN015","IN009","IN013","IN029","IN058","IN004","IN003",
                "POP_URB","POP_TOT","IN006","IN016","AG022")
  numerico = c("IN002","IN031","IN101","IN049","IN019","IN023","IN024",
               "IN055","IN056","IN057","IN075","IN076","IN084","IN046",
               "IN015","IN009","IN013","IN029","IN058","IN004","IN003","POP_URB",
               "POP_TOT","AG013","IN006","IN016","IN047","AG022")

  input1 = mice::mice(df[,numerico],      m = 5, method = "cart", printFlag = FALSE, seed = 1) |> complete() |> as_tibble() |> suppressWarnings()
  input2 = mice::mice(input1[,numerico2], m = 5, method = "cart", printFlag = FALSE, seed = 1) |> complete() |> as_tibble() |> suppressWarnings()

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
        ),
      prestador2 = factor(prestador2)
    )

  municipios_duplicados =
    system.file("extdata", "duplicatas.ods", package = "snis") |>
    readODS::read_ods() |>
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
    as_tibble() |>
    relocate(c("prestador2", "tarifa", "micromedida", "urbanização"), .after = "prestador")

  df
}
