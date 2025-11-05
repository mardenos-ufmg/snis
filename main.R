df_features <- readxl::read_excel("data2/DADOS_SCORE_21_09_refatoracao.xlsx", sheet = "Planilha2")
df_features_dh <- read_excel("data2/DADOS_SCORE_21_09_Pablo.xlsx", sheet = "Planilha2")
df <- readxl::read_excel("data2/dados tratados missing 5.xlsx") #%>% janitor::clean_names()


df = process_data(2016)
FA_EEE = fa(df, grupos = readODS::read_ods("inst/extdata/grupos.ods", sheet = "EEE"))
FA_SU  = fa(df, grupos = readODS::read_ods("inst/extdata/grupos.ods", sheet = "SU"))
df_ = update_df_fa(df, FA_EEE, FA_SU)

boxplot(df_$`score médio eee` ~ quartile(df_$IN004))
boxplot(df_$`score médio eee` ~ df_$`natureza jurídica`)

plot_map(df_, "score médio eee")
plot_map(df_, "score médio eee", T)

plot_loading(FA_EEE)
plot_loading(FA_SU)




dados = list()
for (ano in 2000:2022) {
  cat("\nProcessando ano", ano, "... ")
  df = tryCatch(
    {
      x = process_data(ano)
      FA_EEE = fa(x, grupos = readODS::read_ods("inst/extdata/grupos.ods", sheet = "EEE"))
      FA_SU  = fa(x, grupos = readODS::read_ods("inst/extdata/grupos.ods", sheet = "SU"))
      update_df_fa(x, FA_EEE, FA_SU)
    },
    error = function(e) {
      cat("falhou:", conditionMessage(e))
      return(NULL)
    }
  )
  if (is.null(df)) next

  FA_EEE =
  FA_SU  =

  dados[[as.character(ano)]]$df         = df
  dados[[as.character(ano)]]$fa$eee     = fa(df, grupos = readODS::read_ods("inst/extdata/grupos.ods", sheet = "EEE"))
  dados[[as.character(ano)]]$fa$su      = fa(df, grupos = readODS::read_ods("inst/extdata/grupos.ods", sheet = "SU"))
  dados[[as.character(ano)]]$grupos$eee = readODS::read_ods("inst/extdata/grupos.ods", sheet = "EEE")
  dados[[as.character(ano)]]$grupos$su  = readODS::read_ods("inst/extdata/grupos.ods", sheet = "SU")
}
saveRDS(dados, "inst/extdata/dados.rds")
usethis::use_data(dados)

dados = readRDS("data/dados.rds")
for (ano in 2010:2022) {
  df = dados[[as.character(ano)]]$df
  df = read(ano)
  col_na = colnames(df)[sapply(df, function(x) all(is.na(x)))]
  if (length(col_na)) cat("\n", ano, "\t", col_na)
}


# Processando ano 2011 ...
# Processando ano 2012 ... falhou: missing value where TRUE/FALSE needed
# Processando ano 2013 ...
# Processando ano 2014 ...
# Processando ano 2015 ...
# Processando ano 2016 ...
# Processando ano 2017 ... falhou: missing value where TRUE/FALSE needed
# Processando ano 2018 ...
# Processando ano 2019 ...
# Processando ano 2020 ...
# Processando ano 2021 ...

codigos =
  file("data/Desagregado-2021.csv", encoding = "UTF-16LE") |>
  readLines(1) |>
  strsplit(";") |>
  purrr::pluck(1) |>
  {\(.) gsub("\"", "", .)}() |>
  {\(.) gsub("\\\\", "", .)}() |>
  tibble::tibble() |>
  `colnames<-`("texto") |>
  tidyr::separate(texto, into = c("código", "descrição"), sep = " - ", extra = "merge", fill = "left") |>
  {\(.)
    {.[1:10,1] = tolower(purrr::pluck(.[1:10,2], 1)); .}
    }() |>
  rbind(
    data.frame(
      código    = c("código da região intermediária", "região intermediária", "código da região imediata", "região imediata"),
      descrição = c("Região Geográfica Intermediária", "Nome Região Geográfica Intermediária", "Região Geográfica Imediata", "Nome Região Geográfica Imediata")
    )
  ) |>
  rbind(
    data.frame(
      código    = c("prestador2", "tarifa", "micromedida", "urbanização"),
      descrição = c("prestador2", "tarifa", "micromedida", "urbanização")
    )
  ) |>
  {\(.){
    x = read(2021)
    mutate(., tipo = sapply( código, \(cod) class(x[[cod]])[1] ))
  }}()

codigos |>
  filter( código %in% colnames(dados[["2021"]]$df) )

