plot_score = function(FA, var = "Score Médio", quant = F) {
  mapa =
    geobr::read_municipality(code_muni = "MG", year = 2020) |> 
    dplyr::left_join(
      dplyr::mutate(FA$Geral$Scores, `Código do Município` = as.numeric(`Código do Município`)),
      by = c("code_muni" = "Código do Município")
      )
  
  ggplot(mapa) +
    geom_sf(aes(fill = .data[[var]]), color = NA) +
    scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
    labs(title = "Mapa de Scores",
         fill = var) +
    theme_void()
}


plot_loading = function(FA) {
  plot_list = list()
  grupos = colnames(FA$Geral$Scores)[-(1:2)]
  
  for (grupo in grupos) {
    df_long =
      FA[[grupo]]$loadings$df |>
      pivot_longer(
        cols = -Variável,
        names_to = "Fator",
        values_to = "Carga"
      )
    
    plot_list[[grupo]] =
      ggplot(df_long, aes(x = Fator, y = Variável, fill = Carga)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                           midpoint = 0,
                           limit = c(min(df_long$Carga, na.rm = TRUE), 
                                     max(df_long$Carga, na.rm = TRUE))) +
      geom_text(aes(label = round(Carga, 2)), color = "black", size = 3) +
      labs(title = paste("Grupo:", grupo)) +
      theme_minimal() +
      scale_x_discrete(
        labels = seq_along(unique(df_long$Fator))
      ) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())
    
  }
  
  gridExtra::grid.arrange(grobs = plot_list, ncol = length(grupos))
}
