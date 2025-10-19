df_features <- readxl::read_excel("data2/DADOS_SCORE_21_09_refatoracao.xlsx", sheet = "Planilha2")
df_features_dh <- read_excel("data2/DADOS_SCORE_21_09_Pablo.xlsx", sheet = "Planilha2")
df <- readxl::read_excel("data2/dados tratados missing 5.xlsx") #%>% janitor::clean_names()


df = process_data(2019)
FA_EEE = fa(df)
FA_SU  = fa(df, features = readODS::read_ods("data/features.ods", sheet = "SU"))
df_ = update_df_fa(df, FA_EEE, FA_SU)

boxplot(df_$`score médio eee` ~ quartile(df_$IN004))
boxplot(df_$`score médio eee` ~ df_$`natureza jurídica`)

plot_map(df_, "score médio eee")
plot_map(df_, "score médio eee", T)

plot_loading(FA_EEE)
plot_loading(FA_SU)


# set.seed(123)
# input1 = mice::mice(df[,numerico],  m = 5, method = "cart", printFlag = FALSE) |> complete() |> as_tibble() |> suppressWarnings()
# 
# set.seed(123)
# input2 = mice::mice(df[,numerico2],  m = 5, method = "cart", printFlag = FALSE) |> complete() |> as_tibble() |> suppressWarnings()
