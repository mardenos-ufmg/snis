set.seed(123)
input1 = mice::mice(df[,numerico],  m = 5, method = "cart", printFlag = FALSE) |> complete() |> as_tibble() |> suppressWarnings()

set.seed(123)
input2 = mice::mice(df[,numerico2],  m = 5, method = "cart", printFlag = FALSE) |> complete() |> as_tibble() |> suppressWarnings()