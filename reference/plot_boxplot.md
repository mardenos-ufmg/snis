# Boxplots Scores por Grupos

Essa função gera um boxplot que mostra a distribuição de uma variável de
*score* em diferentes grupos (como prestadores, regiões ou naturezas
jurídicas).

## Usage

``` r
plot_boxplot(df, var, group, titulo = NULL, paleta = "plasma")
```

## Arguments

- df:

  Data frame contendo os dados.

- var:

  Vetor de strings com os nomes das colunas que contêm os scores, como
  `score médio su`, `score médio eee`.

- group:

  Vetor de strings com os nomes das colunas de agrupamento, como
  `natureza jurídica`.

- titulo:

  (opcional) Título do gráfico. Se `NULL`, é gerado automaticamente

- paleta:

  (opcional) Paleta de cores viridis a ser usada. Opções: `"viridis"`,
  `"plasma"`, `"magma"`, `"cividis"`, `"inferno"`. Padrão: `"plasma"`.

## Value

Um objeto `ggplot` contendo o boxplot formatado.
