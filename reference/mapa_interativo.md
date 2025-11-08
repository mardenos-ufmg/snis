# Mapa interativo de Scores por Região

Gera um mapa interativo mostrando a distribuição de um score por região,
colorindo os municípios de acordo com quartis do score.

## Usage

``` r
mapa_interativo(df, score_col, titulo = NULL, quart = FALSE)
```

## Arguments

- df:

  Data frame contendo os dados.

- score_col:

  String com o nome da coluna que contém o score, como
  `"score médio su"`.

- titulo:

  (opcional) Título do mapa. Se `NULL`, é gerado automaticamente com
  base no nome da coluna de score.

- quart:

  (opcional) Lógico. Se `TRUE`, colore os municípios por **quartis** do
  score; se `FALSE` (padrão), usa os valores contínuos.

## Value

Um objeto `tmap` interativo com os municípios coloridos por quartis do
score.
