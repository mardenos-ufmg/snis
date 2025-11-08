# Tabela de Medianas por Grupo

Calcula a mediana de uma ou mais colunas de score por uma ou mais
colunas de agrupamento.

## Usage

``` r
table_median(df, score_cols, group_cols)
```

## Arguments

- df:

  Data frame contendo os dados.

- score_cols:

  Vetor de strings com os nomes das colunas que contêm os scores, como
  `score médio su`e `score médio eee`.

- group_cols:

  Vetor de strings com os nomes das colunas de agrupamento, como
  `região intermediária`.

## Value

Data frame com duas colunas:

- `group_col`: grupos;

- `Score_Mediana`: mediana do score em cada grupo.
