df_features <- readxl::read_excel("data2/DADOS_SCORE_21_09_refatoracao.xlsx", sheet = "Planilha2")
df_features_dh <- read_excel("data2/DADOS_SCORE_21_09_Pablo.xlsx", sheet = "Planilha2")
df <- readxl::read_excel("data2/dados tratados missing 5.xlsx") #%>% janitor::clean_names()

df <- df %>%
  rename(    IN002 = in002, IN031 = in031, IN101 = in101, IN049 = in049, IN019 = in019, IN023 = in023, IN024 = in024, IN055 = in055, IN056 = in056, IN057 = in057, IN075 = in075, IN076 = in076, IN084 = in084, IN046 = in046, IN015 = in015, IN009 = in009, IN013 = in013, IN029 = in029, IN058 = in058, IN004 = in004, IN003 = in003,
             POP_URB = pop_urb, POP_TOT = pop_tot, AG013 = ag013, IN006 = in006, IN016 = in016, IN047 = in047, AG022 = ag022, ES006 = es006, ES005 = es005,
             
             Tarifa = tarifa, qtde_n_micromedida = qtde_n_micromedida,
             
             `Grau urbanização` = grau_urbanizacao, `Código do Prestador` = codigo_do_prestador, Prestador = prestador, `Natureza jurídica` = natureza_juridica, Nome_Mesorregião = nome_mesorregiao, Abrangência = abrangencia, Nome_Município = nome_municipio, `Tipo de serviço` = tipo_de_servico, Prestador2 = prestador2
  )



# set.seed(123)
# input1 = mice::mice(df[,numerico],  m = 5, method = "cart", printFlag = FALSE) |> complete() |> as_tibble() |> suppressWarnings()
# 
# set.seed(123)
# input2 = mice::mice(df[,numerico2],  m = 5, method = "cart", printFlag = FALSE) |> complete() |> as_tibble() |> suppressWarnings()
# 
