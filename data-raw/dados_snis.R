## code to prepare `dados_snis` dataset goes here

devtools::load_all()

dados_snis = list()
for (ano in 2000:2022) {
  df = tryCatch(
    {
      x = processar_dados(ano)
      FA_EEE = fa(x, grupos = "eee")
      FA_SU  = fa(x, grupos = "su")
      update_df_fa(x, FA_EEE, FA_SU)
    },
    error = function(e) {
      cat("\n", ano, "falhou:", conditionMessage(e))
      return(NULL)
    }
  )
  if (is.null(df)) next

  dados_snis[[as.character(ano)]]$df         = df
  dados_snis[[as.character(ano)]]$fa$eee     = fa(df, grupos = "eee")
  dados_snis[[as.character(ano)]]$fa$su      = fa(df, grupos = "su")
  dados_snis[[as.character(ano)]]$grupos$eee = readODS::read_ods("inst/extdata/grupos.ods", sheet = "EEE")
  dados_snis[[as.character(ano)]]$grupos$su  = readODS::read_ods("inst/extdata/grupos.ods", sheet = "SU")
}
saveRDS(dados_snis, "inst/extdata/dados_snis.rds")

# 2000 falhou: missing value where TRUE/FALSE needed
# 2001 falhou: missing value where TRUE/FALSE needed
# 2002 falhou: missing value where TRUE/FALSE needed
# 2007 falhou: missing value where TRUE/FALSE needed
# 2017 falhou: missing value where TRUE/FALSE needed
# 2022 falhou: missing value where TRUE/FALSE needed

for (ano in 2000:2022) {
  cols = c(
    "POP_URB","POP_TOT",
    "IN002","IN031","IN101","IN049","IN019","IN023","IN024",
    "IN055","IN056","IN057","IN075","IN076","IN084","IN046",
    "IN015","IN009","IN013","IN029","IN058","IN004","IN003",
    "AG013","AG022","IN006","IN016","IN047","ES006","ES005"
  )

  df = dados_snis[[as.character(ano)]]$df
  df = read(ano)
  col_na = colnames(df)[sapply(df, function(x) all(is.na(x)))]
  col_na = intersect(cols, col_na)
  if (length(col_na)) cat("\n", ano, "\t", col_na)
}

# 2000 	 IN101 IN055 IN056 IN057 IN084 IN058
# 2001 	 IN101 IN057 IN084 IN058
# 2002 	 IN101 IN057 IN084 IN058
# 2007 	 IN101 IN084
# 2022 	 POP_URB IN023 IN024 IN047


usethis::use_data(dados_snis, overwrite = TRUE)
