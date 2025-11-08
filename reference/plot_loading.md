# Heatmap das Cargas Fatoriais por Grupo

Essa função gera um conjunto de heatmaps mostrando as cargas fatoriais
(`loadings`) de cada variável em cada fator para diferentes grupos em
uma análise fatorial. Cada grupo recebe um plot separado, e todos são
combinados em um único grid.

## Usage

``` r
plot_loading(FA)
```

## Arguments

- FA:

  Objeto de análise fatorial criado previamente, que deve conter:

  - `FA$geral$scores`: tabela com scores gerais (usada para identificar
    os grupos);

  - `FA[[grupo]]$loadings$df`: data frame com as cargas fatoriais de
    cada variável por fator.

## Value

Um gráfico `ggplot` combinado (grid) de todos os grupos, mostrando:

- Variáveis no eixo y;

- Fatores no eixo x;

- Cargas fatoriais codificadas por cores (azul = negativa, vermelho =
  positiva, branco = zero);

- Valores numéricos das cargas sobre os tiles.

## Details

- Cada grupo é representado separadamente, com seu próprio título;

- As cores são escaladas individualmente para cada grupo, respeitando os
  valores mínimos e máximos das cargas;

- Os plots são organizados em um grid com número de colunas igual ao
  número de grupos.
