# Histograma de uma variável numérica

Cria um histograma mostrando a distribuição dos valores de uma variável
numérica.

## Usage

``` r
plot_hist_score(df, group_col, titulo = NULL)
```

## Arguments

- df:

  Data frame contendo os dados.

- titulo:

  (opcional) Título do gráfico. Se `NULL`, é gerado automaticamente.

- var_col:

  String com o nome da coluna numérica a ser analisada, como `"IN023"`.

## Value

Um objeto `ggplot` com o histograma.
