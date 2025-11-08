# Teste de Barlett

O objetivo dessa função é verificar se os dados são adequados para uma
Análise Fatorial, isto é, se há correlação entre as variáveis.

## Usage

``` r
bartlett(df, alpha = 0.05)
```

## Arguments

- df:

  data.frame com dados

- alpha:

  número entre zero e um; nível de significancia aceito; o padrão é
  0.05.

## Value

`TRUE` se há dependência entre os dados ou `FALSE` se não há (logo não
faz sentido uma Análise Fatorial), é possível retornar `NA` se não há
linhas completas.

## Tipo de correlação

Para computar a matriz de covarância escolhemos o tipo `na.or.complete`,
que considera apenas as linha completas (sem `NA`) de `df`. Se não
houver linhas completas, o valor retornado será um `NA`. Originalmente
usava-se o `pairwise.complete.obs`, mas isso pode retornar matrizes de
correlçao com autoravalores negativos, o que não é adequado. Esses
métodos de computação da matriz de covariância estão disponíveis no
argumento `use` da função
[`stats::cor`](https://rdrr.io/r/stats/cor.html).
