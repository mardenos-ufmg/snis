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


ibge =
  system.file("extdata", "RELATORIO_DTB_BRASIL_2024_MUNICIPIOS.ods", package = "snis") |>
  readODS::read_ods(skip = 6) |>
  filter(Nome_UF == "Minas Gerais") |>
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
  )

df_ = read(2021)
df = process_data(2021)

setdiff(ibge$`Código do Município`, df$`código do município`)
setdiff(ibge$`Código do Município`, df_$`código do município`)
