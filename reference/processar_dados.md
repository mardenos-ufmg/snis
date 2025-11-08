# Processar dados para análise fatorial

Rotina de processamento (e criação) de variáveis para a aplicação de
análise fatorial

## Usage

``` r
processar_dados(ano)
```

## Arguments

- ano:

  inteiro entre 2000 e 2022

## Value

tibble

## Inputação

A inputação foi feita pelo pacote `mice` em duas etapas. Detectamos que
as seguintes variáveis têm forte correlação entre si: IN024 com IN047;
AG022 com AG013. Como essa forte multicolinearidade piora a qualidade
das previsões feitas pelo `mice`, decidimos excluindo IN047 e AG013 para
a primeira fase de imputação (em que IN024 e AG022 são imputadas). Em
seguida, usamos os valores imputados para prever IN047 e AG013.

O pacote `mice` usa alguma aleatoriedade para suas previsões, por isso
decidimos fixar a semente das previsões para que os mesmos resultados
sejam retornados em cada ano.

Os valores de algumas variáveis foram imputadas por simples lógica, como
as que se seguem:

- `IN003`: ;

## Variáveis criadas

Algumas variáveis foram criadas com as seguintes fórmulas:

- `tarifa`: ;

- `micromedida`: ;

- `urbanização`: ;

- `prestador2`: ;
