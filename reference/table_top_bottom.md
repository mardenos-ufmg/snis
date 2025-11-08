# Tabela dos Melhores e Piores municípios

Exibe uma tabela cos os Melhores e Piores Municípios com base no Score
Médio das categorias.

## Usage

``` r
table_top_bottom(df, top_n, bottom_n)
```

## Arguments

- df:

  Data frame contendo os dados.

- top_n:

  Número de municípios com maiores *scores médios* a serem listados.

- bottom_n:

  Número de municípios com menores *scores médios* a serem listados.

## Value

Uma lista com dois elementos:

- `Melhores`: data frame com os municípios com maiores *scores médios*;

- `Piores`: data frame com os municípios com menores *scores médios*.
