# ================
# Função Fatores 
# ===============
fatores <- function(df, df_features, delta = 0.05, rotacao = 'oblimin', verbose = FALSE) {
  
  grupos_unicos <- unique(df_features$Grupo)
  dfs <- list()
  plots_list <- list()
  
  for (grupo_nome in grupos_unicos) {
    
    gr_var <- df_features %>% 
      filter(Grupo == grupo_nome) %>% 
      pull(Variavel)
    
    dados <- df %>% select(all_of(gr_var))
    
    # Teste de Bartlett
    if (!Bartlett(dados)) {
      cat(paste0('❌ Não passou no teste de Bartlett para o grupo: ', grupo_nome, '\n'))
      next 
    }
    
    # Quantidade de fatores
    n_fatores <- quant_fatores(dados, delta)
    
    # Análise fatorial  ############################# NÃO ENTENDI ou não é válido sempre, ver ml
    FA_eficiencia <- tryCatch({
      fa(dados, nfactors = n_fatores, rotate = rotacao, fm = "minres")
    }, error = function(e) {
      warning(paste0("Erro ao rodar fa() com fm='minres' no grupo ", grupo_nome, 
                     ". Tentando fm='ml'."))
      fa(dados, nfactors = n_fatores, rotate = rotacao, fm = "ml")
    })
    
    # Matriz de estrutura
    STR <- as.data.frame(FA_eficiencia$loadings)  
    n_linhas_load <- nrow(FA_eficiencia$loadings)
    STR = STR[1:n_linhas_load,]
    STR_long <- as.data.frame(STR) %>%
      tibble::rownames_to_column(var = "Variavel") %>% 
      pivot_longer(cols = -Variavel, names_to = "Fator", values_to = "Carga") |>
      mutate(
        Carga = as.numeric(Carga)
      )
    
    if (verbose) {
      p <- ggplot(STR_long, aes(x = Fator, y = Variavel, fill = Carga)) +
        geom_tile(color = "white") +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                             midpoint = 0,
                             limit = c(min(STR_long$Carga, na.rm = TRUE), 
                                       max(STR_long$Carga, na.rm = TRUE))) +
        geom_text(aes(label = round(Carga, 2)), color = "black", size = 3) +
        labs(title = paste("Grupo:", grupo_nome),
             subtitle = paste("Rotação:", rotacao)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))
      
      plots_list[[grupo_nome]] <- p
    }
    
    # Corrige sentido
    STR_corrigido <- sentido(STR, df_features, rotacao, verbose)
    
    # Scores fatoriais
    X_scale <- scale(dados)
    
    fs <- tryCatch({
      psych::factor.scores(as.data.frame(X_scale), FA_eficiencia, method = "regression")
    }, error = function(e) {
      warning(paste0("Erro ao calcular factor.scores para grupo ", grupo_nome, 
                     ". Tentando método 'tenBerge'."))
      psych::factor.scores(as.data.frame(X_scale), FA_eficiencia, method = "tenBerge")
    })
    
    novo_efetividade_matrix <- fs$scores
    novo_efetividade <- as.data.frame(novo_efetividade_matrix)
    
    # Score total por grupo
    novo_efetividade <- novo_efetividade %>%
      mutate(!!grupo_nome := rowSums(across(everything()), na.rm = TRUE))
    
    dfs[[grupo_nome]] <- novo_efetividade[[grupo_nome]]
  }
  
  if (verbose) {
    gridExtra::grid.arrange(grobs = plots_list, ncol = 3)
  }
  
  scores_finais <- as.data.frame(dfs)
  cat(paste0("✅ Análise fatorial concluída com sucesso para ", length(dfs), " grupos.\n"))
  
  return( scores_finais )
}
