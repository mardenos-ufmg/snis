# Mapa interativo de Scores por Região

Gera um mapa interativo mostrando a distribuição de um score por região,
colorindo os municípios de acordo com quartis do score.

## Usage

``` r
mapa_interativo(df, var, quart = FALSE, titulo = NULL)
```

## Arguments

- df:

  Data frame contendo os dados.

- var:

  String com o nome da coluna que contém o score, como
  `"score médio su"`.

- quart:

  (opcional) Lógico. Se `TRUE`, colore os municípios por **quartis** do
  score; se `FALSE` (padrão), usa os valores contínuos.

- titulo:

  (opcional) Título do mapa. Se `NULL`, é gerado automaticamente com
  base no nome da coluna de score.

## Value

Um objeto `tmap` interativo com os municípios coloridos por quartis do
score.
