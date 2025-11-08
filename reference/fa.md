# Análise Fatorial para dados do SNIS

Função geradora dos scores e loadings da análise fatorial com dados do
SINS

## Usage

``` r
fa(df, grupos, normalizar = T, rotacao = "oblimin")
```

## Arguments

- df:

  data.frame com dados para análise fatorial

- grupos:

  data.frame com esquema de grupos para a análise fatorial, pode ser
  passada string 'eee' ou 'su'

- normalizar:

  lógico, `TRUE` (padrão) se os scores devem ser padronizados, `FALSE`
  se não.

- rotacao:

  string, tipo de rotação a ser usada no argumento `rotate` da função
  [`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html), o padrão é
  'oblimin'

## Value

objeto do tipo `fa-snis`, uma lista contendo os scores e loadings de
cada grupo

## Grupos

O argumento grupos deve ser um data.frame contendo as colunas.
