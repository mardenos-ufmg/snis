# fazer sentido dentro do grupo
# falta normalizar
#' Análise Fatorial para dados do SNIS
#'
#' Função geradora dos scores e loadings da análise fatorial com dados do SINS
#'
#' @section Grupos:
#'
#' O argumento grupos deve ser um data.frame contendo as colunas.
#'
#'
#' @param df data.frame com dados para análise fatorial
#' @param grupos data.frame com esquema de grupos para a análise fatorial, pode ser passada string 'eee' ou 'su'
#' @param normalizar lógico, `TRUE` (padrão) se os scores devem ser padronizados, `FALSE` se não.
#' @param rotacao string, tipo de rotação a ser usada no argumento `rotate` da função `psych::fa()`, o padrão é 'oblimin'
#'
#' @returns objeto do tipo `fa-snis`, uma lista contendo os scores e loadings de cada grupo
#'
#' @export
#'
#' @importFrom purrr pluck
#' @importFrom readODS read_ods
#' @importFrom psych fa factor.scores
fa = function(df, grupos, normalizar = T, rotacao = 'oblimin') {
  if (identical(grupos, "eee") | identical(grupos, "su")) {
    grupos =
      system.file("extdata", "grupos.ods", package = "snis") |>
      readODS::read_ods(sheet = toupper(grupos))
  }

  grupos =
    grupos |>
    `colnames<-`(c("variavel", "grupo")) |>
    mutate(
      grupo = tolower(grupo)
    )
  FA = list()

  for (grupo_nome in unique(grupos$grupo)) {

    variaveis =
      grupos |>
      filter(.data$grupo == grupo_nome) |>
      purrr::pluck("variavel")

    dados =
      df |>
      select(all_of(variaveis)) |>
      scale() |>
      as_tibble()

    if (!bartlett(dados)) {
      warning(
        cat("Não passou no teste de Bartlett o grupo ", grupo_nome, "\n")
      )
    }

    autovalores =
      dados |>
      cor(use = "na.or.complete") |>
      eigen() |>
      purrr::pluck("values")

    quant_fatores = sum(autovalores > 1 - 1e-5)
    loadings = psych::fa(dados, nfactors = quant_fatores, fm = "minres", rotate = rotacao)
    scores   = psych::factor.scores(dados, loadings, method = "regression")

    loadings$df =
      loadings$loadings |>
      unclass() |>
      as_tibble() |>
      mutate(
        variável = variaveis
      ) |>
      relocate(variável)

    scores$df =
      scores$scores |>
      as_tibble() |>
      mutate() |>
      mutate(
        !!grupo_nome := rowSums(across(everything()), na.rm = TRUE),
        `código do município` = df$`código do município`,
        across(
          .cols = !!grupo_nome,
          .fns = ~ pnorm(., mean = mean(.), sd = sd(.))
        )
      ) |>
      relocate(c(`código do município`, !!grupo_nome))


    FA[[grupo_nome]] =
      list(
        scores   = scores,
        loadings = loadings
      )
  }

  FA$geral$scores =
    lapply(FA, function(x) x$scores$df[,2] |> purrr::pluck(1)) |>
    as_tibble() |>
    mutate(
      `médio` = round(rowMeans(across(everything()), na.rm = TRUE), 3),
      `código do município` = df$`código do município`
      ) |>
    relocate(c(`código do município`, `médio`))

  class(FA) = "fa-snis"

  FA
}
