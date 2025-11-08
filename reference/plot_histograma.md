# Histograma de uma variável numérica

Cria um histograma mostrando a distribuição dos valores de uma variável
numérica.

## Usage

``` r
plot_histograma(df, var, titulo = NULL)
```

## Arguments

- df:

  Data frame contendo os dados.

- var:

  String com o nome da coluna numérica a ser analisada, como `"IN023"`.

- titulo:

  (opcional) Título do gráfico. Se `NULL`, é gerado automaticamente.

## Value

Um objeto `ggplot` com o histograma.
