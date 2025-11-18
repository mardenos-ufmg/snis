# Histograma com curva de densidade

Cria um histograma com a curva de densidade sobreposta, permitindo
visualizar simultaneamente a frequência e a distribuição suave.

## Usage

``` r
plot_hist(df, var, titulo = NULL)
```

## Arguments

- df:

  Data frame contendo os dados.

- var:

  String com o nome da coluna numérica a ser analisada, como `"IN023"`.

- titulo:

  (opcional) Título do gráfico. Se `NULL`, é gerado automaticamente.

## Value

Um objeto `ggplot` com o histograma e a curva de densidade.
