# Ler CSV do SNIS

Ler CSV do SNIS

## Usage

``` r
read(ano)
```

## Arguments

- ano:

  inteiro entre 2000 e 2022

## Value

tibble

## Origem dos dados

Os dados foram retirados de
`https://app4.cidades.gov.br/serieHistorica/` com as seguintes opções:

- Informações e indicadores desagregados;

- Ano de referẽncia: `ano`;

- Região: `Sudeste`;

- Estado: `Minas Gerais`;

- Demais filtros: `Marcar todos`.

## Estrutura do CSV

Por meio da função
[`readr::guess_encoding()`](https://readr.tidyverse.org/reference/encoding.html)
podemos ver que a codificação dos CSVs é `UTF-16LE`.

As colunas são separadas por `;` e possuem um `;` extra ao final de cada
linha, o que pode produzir uma coluna extra a depender da forma como
esse arquivo é lido. Isso foi evitado aqui por meio da exclusão do
último caractere de cada durante a leitra do CSV.

A última linha - que originalmente continha valores totais das variáveis
da amostra - foi excluída.

Para os números, o separador decimal original era a vírgula e o de
centenas, o ponto.

## Estilo

Optamos por manter acentos e diacríticos (como o til) no nome das
colunas, mas o nome de todas as colunas foi passada para caracteres
minúsculos. Originalmente, a descrição de cada variável estava no
pŕoprio nome da coluna, após um travessão `-`, mas aqui optamos por
excluir tudo o que vinha depois disso. A descrição das variáveis está
disponível na vinheta
[`dados`](https://mardenos-ufmg.github.io/snis/articles/dados.html).

## IBGE

Originalmente, o `código do município` no CSV não continha o último
dígito de verificação. Optamos por colocar esse último dígito, conforme
padrão do IBGE. Ainda de acordo com o IBGE, as colunas referentes à
`região intermediária` e à `região imediata` foram adicionadas.
