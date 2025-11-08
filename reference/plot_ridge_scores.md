# Gráfico tipo "ondas" (ridge) para múltiplos scores

Cria um gráfico de densidade estilo "ridge plot", mostrando a
distribuição de diferentes scores (`score_cols`) por grupos
(`group_col`).

## Usage

``` r
plot_ridge_scores(
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
  `região intermediária` e `natureza jurídica`.

- titulo:

  (opcional) Título do gráfico. Se `NULL`, é gerado automaticamente

- cor_paleta:

  (opcional) Paleta de cores viridis a ser usada. Opções: `"viridis"`,
  `"plasma"`, `"magma"`, `"cividis"`, `"inferno"`. Padrão: `"plasma"`.

## Value

Um objeto `ggplot` com o ridge plot.
