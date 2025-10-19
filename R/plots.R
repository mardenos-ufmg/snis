plot_map = function(df, var, quart = F) {
  df =
    df[c("código do município", var)] |>
    dplyr::mutate(`código do município` = as.numeric(`código do município`))
  
  legenda = colnames(df)[2]
  colnames(df)[2] = "var"
  
  if (quart) {
    df[[2]] = df[[2]] |> quartile()
    legenda = paste0(legenda, "\n(quartis)")
  }
  
  mapa =
    geobr::read_municipality(code_muni = "MG", year = 2020) |> 
    dplyr::left_join(df, by = c("code_muni" = "código do município"))
  
  ggplot(mapa) +
    geom_sf(aes(fill = .data[["var"]]), color = NA) +
    {if (quart) {
      scale_fill_viridis_d(option = "plasma", na.value = "grey90")
    } else {
      scale_fill_viridis_c(option = "plasma", na.value = "grey90")
    }
    } +
    labs(title = "Mapa",
         fill = legenda) +
    theme_void()
}


plot_loading = function(FA) {
  plot_list = list()
  grupos = colnames(FA$geral$scores)[-(1:2)]
  
  for (grupo in grupos) {
    df_long =
      FA[[grupo]]$loadings$df |>
      pivot_longer(
        cols = -variável,
        names_to = "fator",
        values_to = "carga"
      )
    
    plot_list[[grupo]] =
      ggplot(df_long, aes(x = fator, y = variável, fill = carga)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                           midpoint = 0,
                           limit = c(min(df_long$carga, na.rm = TRUE), 
                                     max(df_long$carga, na.rm = TRUE))) +
      geom_text(aes(label = round(carga, 2)), color = "black", size = 3) +
      labs(title = paste("Grupo:", grupo)) +
      theme_minimal() +
      scale_x_discrete(
        labels = seq_along(unique(df_long$fator))
      ) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())
    
  }
  
  gridExtra::grid.arrange(grobs = plot_list, ncol = length(grupos))
}
