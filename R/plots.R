plot_score = function(year) {
  
}

plot_loading = function(year) {
  plot_list = list()
  
  
  for (grupo_nome in unique(features$`Grupo DH`)) {
    p =
      ggplot(STR_long, aes(x = Fator, y = Variavel, fill = Carga)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                           midpoint = 0,
                           limit = c(min(STR_long$Carga, na.rm = TRUE), 
                                     max(STR_long$Carga, na.rm = TRUE))) +
      geom_text(aes(label = round(Carga, 2)), color = "black", size = 3) +
      labs(title = paste("Grupo:", grupo_nome)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))
    
    plot_list[[grupo_nome]] = p
  }
  
  gridExtra::grid.arrange(grobs = plot_list, ncol = 3)
}
