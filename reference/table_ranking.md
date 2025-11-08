# Tabela dos Rankings

Gera uma Tabela com o Ranking dos Municípios com base no Score Médio das
categorias desejadas.

## Usage

``` r
table_ranking(df, top_n = 10)
```

## Arguments

- df:

  Data frame contendo os dados.

- top_n:

  (opcional) Número de municípios a serem exibidos no ranking final. Se
  não for especificado, retorna 10 municípios.

## Value

Um data frame contendo as colunas:

- `município`: nome do município;

- `Ranking`: posição no ranking baseado no Score Médio;

- `Rank_Medio`: ranking médio de todas as dimensões;

- `Diferenca`: diferença entre o Ranking e o Rank_Medio;

- `Sustentabilidade`, `Universalidade`, `Score_Medio`: valores médios
  dos scores;
