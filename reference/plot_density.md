# Densidade de uma variável numérica

Cria um gráfico de densidade mostrando a distribuição suave dos valores
de uma variável numérica, como uma "onda".

## Usage

``` r
plot_density(df, var, titulo = NULL)
```

## Arguments

- df:

  Data frame contendo os dados.

- var:

  String com o nome da coluna numérica a ser analisada, como `"IN023"`.

- titulo:

  (opcional) Título do gráfico. Se `NULL`, é gerado automaticamente.

## Value

Um objeto `ggplot` com o gráfico de densidade.
