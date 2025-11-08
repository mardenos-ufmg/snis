# Gráfico de barras com a Mediana por Grupo

Essa função cria um barplot horizontal mostrando a mediana de uma
variável numérica (`score_col`) por grupos (`group_col`), adicionando os
valores das medianas sobre as barras.

## Usage

``` r
plot_median_barplot(
  df,
  score_cols,
  group_cols,
  titulo = NULL,
  cor_paleta = "plasma"
)
```

## Arguments

- df:

  Data frame contendo os dados.

- score_cols:

  Vetor de strings com os nomes das colunas que contêm os scores, como
  `score médio su`, `score médio eee`.

- group_cols:

  Vetor de strings com os nomes das colunas de agrupamento, como
  `região intermediária`.

- titulo:

  (opcional) Título do gráfico. Se `NULL`, é gerado automaticamente

- cor_paleta:

  (opcional) Paleta de cores viridis a ser usada. Opções: `"viridis"`,
  `"plasma"`, `"magma"`, `"cividis"`, `"inferno"`. Padrão: `"plasma"`.

## Value

Um objeto `ggplot` contendo o barplot formatado.
