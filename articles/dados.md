# dados

``` r
library(snis)
#> Loading required package: ggplot2
#> Loading required package: shiny
#> Loading required package: tidyverse
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.6
#> ✔ forcats   1.0.1     ✔ stringr   1.6.0
#> ✔ lubridate 1.9.4     ✔ tibble    3.3.0
#> ✔ purrr     1.2.0     ✔ tidyr     1.3.1
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
#> Loading required package: tmap
#> Warning: replacing previous import 'shiny::dataTableOutput' by
#> 'DT::dataTableOutput' when loading 'snis'
#> ℹ tmap modes "plot" - "view"
#> ℹ toggle with `tmap::ttm()`
#> Modo interativo do tmap ativado: tmap_mode('view')
library(knitr)

codigos =
  system.file("extdata", "Desagregado-2021.csv", package = "snis") |>
  file(encoding = "UTF-16LE") |>
  readLines(1) |>
  strsplit(";") |>
  purrr::pluck(1) |>
  {\(.) gsub("\"", "", .)}() |>
  {\(.) gsub("\\\\", "", .)}() |>
  tibble::tibble() |>
  `colnames<-`("texto") |>
  tidyr::separate(texto, into = c("código", "descrição"), sep = " - ", extra = "merge", fill = "left") |>
  {\(.)
    {.[1:10,1] = tolower(purrr::pluck(.[1:10,2], 1)); .}
    }() |>
  rbind(
    data.frame(
      código    = c("código da região intermediária", "região intermediária", "código da região imediata", "região imediata"),
      descrição = c("Região Geográfica Intermediária", "Nome Região Geográfica Intermediária", "Região Geográfica Imediata", "Nome Região Geográfica Imediata")
    )
  ) |>
  rbind(
    data.frame(
      código    = c("prestador2", "tarifa", "micromedida", "urbanização"),
      descrição = c("prestador2", "tarifa", "micromedida", "urbanização")
    )
  ) |>
  {\(.){
    x = read(2021)
    mutate(., tipo = sapply( código, \(cod) class(x[[cod]])[1] ))
  }}()

codigos2 =
  codigos |>
  filter( código %in% colnames(dados_snis[["2021"]]$df) )
```

``` r
kable(codigos2, caption = "Tabela com dados filtrados")
```

| código                         | descrição                                                                           | tipo      |
|:-------------------------------|:------------------------------------------------------------------------------------|:----------|
| código do município            | Código do Município                                                                 | numeric   |
| município                      | Município                                                                           | character |
| ano de referência              | Ano de Referência                                                                   | integer   |
| código do prestador            | Código do Prestador                                                                 | integer   |
| prestador                      | Prestador                                                                           | factor    |
| abrangência                    | Abrangência                                                                         | factor    |
| tipo de serviço                | Tipo de serviço                                                                     | factor    |
| natureza jurídica              | Natureza jurídica                                                                   | factor    |
| POP_TOT                        | População total do município do ano de referência (Fonte: IBGE):                    | numeric   |
| POP_URB                        | População urbana do município do ano de referência (Fonte: IBGE):                   | numeric   |
| AG013                          | Quantidade de economias residenciais ativas de água                                 | numeric   |
| AG022                          | Quantidade de economias residenciais ativas de água micromedidas                    | numeric   |
| ES005                          | Volume de esgotos coletado                                                          | numeric   |
| ES006                          | Volume de esgotos tratado                                                           | numeric   |
| IN002                          | Índice de produtividade: economias ativas por pessoal próprio                       | numeric   |
| IN003                          | Despesa total com os serviços por m3 faturado                                       | numeric   |
| IN004                          | Tarifa média praticada                                                              | numeric   |
| IN006                          | Tarifa média de esgoto                                                              | numeric   |
| IN009                          | Índice de hidrometração                                                             | numeric   |
| IN013                          | Índice de perdas faturamento                                                        | numeric   |
| IN015                          | Índice de coleta de esgoto                                                          | numeric   |
| IN016                          | Índice de tratamento de esgoto                                                      | numeric   |
| IN019                          | Índice de produtividade: economias ativas por pessoal total (equivalente)           | numeric   |
| IN023                          | Índice de atendimento urbano de água                                                | numeric   |
| IN024                          | Índice de atendimento urbano de esgoto referido aos municípios atendidos com água   | numeric   |
| IN029                          | Índice de evasão de receitas                                                        | numeric   |
| IN031                          | Margem da despesa com pessoal próprio                                               | numeric   |
| IN046                          | Índice de esgoto tratado referido à água consumida                                  | numeric   |
| IN047                          | Índice de atendimento urbano de esgoto referido aos municípios atendidos com esgoto | numeric   |
| IN049                          | Índice de perdas na distribuição                                                    | numeric   |
| IN055                          | Índice de atendimento total de água                                                 | numeric   |
| IN056                          | Índice de atendimento total de esgoto referido aos municípios atendidos com água    | numeric   |
| IN057                          | Índice de fluoretação de água                                                       | numeric   |
| IN058                          | Índice de consumo de energia elétrica em sistemas de abastecimento de água          | numeric   |
| IN075                          | Incidência das análises de cloro residual fora do padrão                            | numeric   |
| IN076                          | Incidência das análises de turbidez fora do padrão                                  | numeric   |
| IN084                          | Incidência das análises de coliformes totais fora do padrão                         | numeric   |
| IN101                          | Índice de suficiência de caixa                                                      | numeric   |
| código da região intermediária | Região Geográfica Intermediária                                                     | numeric   |
| região intermediária           | Nome Região Geográfica Intermediária                                                | factor    |
| código da região imediata      | Região Geográfica Imediata                                                          | numeric   |
| região imediata                | Nome Região Geográfica Imediata                                                     | factor    |
| prestador2                     | prestador2                                                                          | NULL      |
| tarifa                         | tarifa                                                                              | NULL      |
| micromedida                    | micromedida                                                                         | NULL      |
| urbanização                    | urbanização                                                                         | NULL      |

Tabela com dados filtrados

``` r
kable(codigos, caption = "Tabela com dados filtrados")
```

| código                         | descrição                                                                                                  | tipo      |
|:-------------------------------|:-----------------------------------------------------------------------------------------------------------|:----------|
| código do município            | Código do Município                                                                                        | numeric   |
| município                      | Município                                                                                                  | character |
| estado                         | Estado                                                                                                     | character |
| ano de referência              | Ano de Referência                                                                                          | integer   |
| código do prestador            | Código do Prestador                                                                                        | integer   |
| prestador                      | Prestador                                                                                                  | factor    |
| sigla do prestador             | Sigla do Prestador                                                                                         | factor    |
| abrangência                    | Abrangência                                                                                                | factor    |
| tipo de serviço                | Tipo de serviço                                                                                            | factor    |
| natureza jurídica              | Natureza jurídica                                                                                          | factor    |
| GE001                          | Quantidade de municípios atendidos com abastecimento de água com delegação em vigor                        | integer   |
| GE002                          | Quantidade de municípios atendidos com abastecimento de água com delegação vencida                         | integer   |
| GE003                          | Quantidade de municípios atendidos com abastecimento de água sem delegação                                 | integer   |
| GE008                          | Quantidade de Sedes municipais atendidas com abastecimento de água                                         | integer   |
| GE009                          | Quantidade de Sedes municipais atendidas com esgotamento sanitário                                         | integer   |
| GE010                          | Quantidade de Localidades (excluídas as sedes) atendidas com abastecimento de água                         | integer   |
| GE011                          | Quantidade de Localidades (excluídas as sedes) atendidas com esgotamento sanitário                         | integer   |
| GE014                          | Quantidade de municípios atendidos com esgotamento sanitário com delegação em vigor                        | integer   |
| GE015                          | Quantidade de municípios atendidos com esgotamento sanitário com delegação vencida                         | integer   |
| GE016                          | Quantidade de municípios atendidos com esgotamento sanitário sem delegação                                 | integer   |
| GE017                          | Ano de vencimento da delegação de abastecimento de água                                                    | numeric   |
| GE018                          | Ano de vencimento da delegação de esgotamento sanitário                                                    | numeric   |
| GE019                          | Onde atende com abastecimento de água                                                                      | character |
| GE020                          | Onde atende com esgotamento sanitário                                                                      | character |
| GE030                          | Quantidade de municípios não atendidos com esgotamento sanitário e sem delegação para prestar esse serviço | integer   |
| POP_TOT                        | População total do município do ano de referência (Fonte: IBGE):                                           | numeric   |
| POP_URB                        | População urbana do município do ano de referência (Fonte: IBGE):                                          | numeric   |
| AG001                          | População total atendida com abastecimento de água                                                         | numeric   |
| AG001A                         | População total atendida com abastecimento de água no ano anterior ao de referência.                       | numeric   |
| AG002                          | Quantidade de ligações ativas de água                                                                      | numeric   |
| AG002A                         | Quantidade de ligações ativas de água no ano anterior ao de referência.                                    | numeric   |
| AG003                          | Quantidade de economias ativas de água                                                                     | numeric   |
| AG003A                         | Quantidade de economias ativas de água no ano anterior ao de referência.                                   | numeric   |
| AG004                          | Quantidade de ligações ativas de água micromedidas                                                         | numeric   |
| AG004A                         | Quantidade de ligações ativas de água micromedidas no ano anterior ao de referência.                       | numeric   |
| AG005                          | Extensão da rede de água                                                                                   | numeric   |
| AG005A                         | Extensão da rede de água no ano anterior ao de referência.                                                 | numeric   |
| AG006                          | Volume de água produzido                                                                                   | numeric   |
| AG007                          | Volume de água tratada em ETAs                                                                             | numeric   |
| AG008                          | Volume de água micromedido                                                                                 | numeric   |
| AG010                          | Volume de água consumido                                                                                   | numeric   |
| AG011                          | Volume de água faturado                                                                                    | numeric   |
| AG012                          | Volume de água macromedido                                                                                 | numeric   |
| AG013                          | Quantidade de economias residenciais ativas de água                                                        | numeric   |
| AG013A                         | Quantidade de economias residenciais ativas de água no ano anterior ao de referência.                      | numeric   |
| AG014                          | Quantidade de economias ativas de água micromedidas                                                        | numeric   |
| AG014A                         | Quantidade de economias ativas de água micromedidas no ano anterior ao de referência.                      | numeric   |
| AG015                          | Volume de água tratada por simples desinfecção                                                             | numeric   |
| AG017                          | Volume de água bruta exportado                                                                             | numeric   |
| AG018                          | Volume de água tratada importado                                                                           | numeric   |
| AG019                          | Volume de água tratada exportado                                                                           | numeric   |
| AG020                          | Volume micromedido nas economias residenciais ativas de água                                               | numeric   |
| AG021                          | Quantidade de ligações totais de água                                                                      | numeric   |
| AG021A                         | Quantidade de ligações totais de água no ano anterior ao de referência.                                    | numeric   |
| AG022                          | Quantidade de economias residenciais ativas de água micromedidas                                           | numeric   |
| AG022A                         | Quantidade de economias residenciais ativas de água micromedidas no ano anterior ao de referência.         | numeric   |
| AG024                          | Volume de serviço                                                                                          | numeric   |
| AG025A                         | População rural atendida com abastecimento de água no ano anterior ao de referência.                       | logical   |
| AG026                          | População urbana atendida com abastecimento de água                                                        | numeric   |
| AG026A                         | População urbana atendida com abastecimento de água no ano anterior ao de referência.                      | numeric   |
| AG027                          | Volume de água fluoretada                                                                                  | numeric   |
| AG028                          | Consumo total de energia elétrica nos sistemas de água                                                     | numeric   |
| ES001                          | População total atendida com esgotamento sanitário                                                         | numeric   |
| ES001A                         | População total atendida com esgotamento sanitário no ano anterior ao de referência.                       | numeric   |
| ES002                          | Quantidade de ligações ativas de esgotos                                                                   | numeric   |
| ES002A                         | Quantidade de ligações ativas de esgoto no ano anterior ao de referência.                                  | numeric   |
| ES003                          | Quantidade de economias ativas de esgotos                                                                  | numeric   |
| ES003A                         | Quantidade de economias ativas de esgoto no ano anterior ao de referência.                                 | numeric   |
| ES004                          | Extensão da rede de esgotos                                                                                | numeric   |
| ES004A                         | Extensão da rede de esgoto no ano anterior ao de referência.                                               | numeric   |
| ES005                          | Volume de esgotos coletado                                                                                 | numeric   |
| ES006                          | Volume de esgotos tratado                                                                                  | numeric   |
| ES007                          | Volume de esgotos faturado                                                                                 | numeric   |
| ES008                          | Quantidade de economias residenciais ativas de esgotos                                                     | numeric   |
| ES008A                         | Quantidade de economias residenciais ativas de esgoto no ano anterior ao de referência.                    | numeric   |
| ES009                          | Quantidade de ligações totais de esgotos                                                                   | numeric   |
| ES009A                         | Quantidade de ligações totais de esgoto no ano anterior ao de referência.                                  | numeric   |
| ES012                          | Volume de esgoto bruto exportado                                                                           | numeric   |
| ES013                          | Volume de esgotos bruto importado                                                                          | numeric   |
| ES014                          | Volume de esgoto importado tratado nas instalações do importador                                           | numeric   |
| ES015                          | Volume de esgoto bruto exportado tratado nas instalações do importador                                     | numeric   |
| ES025A                         | População rural atendida com esgotamento sanitário no ano anterior ao de referência.                       | logical   |
| ES026                          | População urbana atendida com esgotamento sanitário                                                        | numeric   |
| ES026A                         | População urbana atendida com esgotamento sanitário no ano anterior ao de referência.                      | numeric   |
| ES028                          | Consumo total de energia elétrica nos sistemas de esgotos                                                  | numeric   |
| FN001                          | Receita operacional direta total                                                                           | numeric   |
| FN002                          | Receita operacional direta de água                                                                         | numeric   |
| FN003                          | Receita operacional direta de esgoto                                                                       | numeric   |
| FN004                          | Receita operacional indireta                                                                               | numeric   |
| FN005                          | Receita operacional total (direta + indireta)                                                              | numeric   |
| FN006                          | Arrecadação total                                                                                          | numeric   |
| FN007                          | Receita operacional direta de água exportada (bruta ou tratada)                                            | numeric   |
| FN008                          | Créditos de contas a receber                                                                               | numeric   |
| FN008A                         | Crédito de contas a receber no ano anterior ao de referência.                                              | numeric   |
| FN010                          | Despesa com pessoal próprio                                                                                | numeric   |
| FN011                          | Despesa com produtos químicos                                                                              | numeric   |
| FN013                          | Despesa com energia elétrica                                                                               | numeric   |
| FN014                          | Despesa com serviços de terceiros                                                                          | numeric   |
| FN015                          | Despesas de Exploração (DEX), sendo FN015 = FN010 + FN011 + FN013 + FN014 + FN020 + FN039 + FN021 + FN027  | numeric   |
| FN016                          | Despesas com juros e encargos do serviço da dívida                                                         | numeric   |
| FN017                          | Despesas totais com os serviços (DTS), sendo FN017 = FN015 + FN016 + FN019 + FN022 + FN028                 | numeric   |
| FN018                          | Despesas capitalizáveis realizadas pelo prestador de serviços                                              | numeric   |
| FN019                          | Despesas com depreciação, amortização do ativo diferido e provisão para devedores duvidosos                | numeric   |
| FN020                          | Despesa com água importada (bruta ou tratada)                                                              | numeric   |
| FN021                          | Despesas fiscais ou tributárias computadas na DEX                                                          | numeric   |
| FN022                          | Despesas fiscais ou tributárias não computadas na DEX                                                      | numeric   |
| FN023                          | Investimento realizado em abastecimento de água pelo prestador de serviços                                 | numeric   |
| FN024                          | Investimento realizado em esgotamento sanitário pelo prestador de serviços                                 | numeric   |
| FN025                          | Outros investimentos realizados pelo prestador de serviços                                                 | numeric   |
| FN026                          | Quantidade total de empregados próprios                                                                    | numeric   |
| FN026A                         | Quantidade total de empregados próprios no ano anterior ao de referência.                                  | numeric   |
| FN027                          | Outras despesas de exploração                                                                              | numeric   |
| FN028                          | Outras despesas com os serviços                                                                            | numeric   |
| FN030                          | Investimento com recursos próprios realizado pelo prestador de serviços.                                   | numeric   |
| FN031                          | Investimento com recursos onerosos realizado pelo prestador de serviços.                                   | numeric   |
| FN032                          | Investimento com recursos não onerosos realizado pelo prestador de serviços.                               | numeric   |
| FN033                          | Investimentos totais realizados pelo prestador de serviços                                                 | numeric   |
| FN034                          | Despesas com amortizações do serviço da dívida                                                             | numeric   |
| FN035                          | Despesas com juros e encargos do serviço da dívida, exceto variações monetária e cambial                   | numeric   |
| FN036                          | Despesa com variações monetárias e cambiais das dívidas                                                    | numeric   |
| FN037                          | Despesas totais com o serviço da dívida                                                                    | numeric   |
| FN038                          | Receita operacional direta - esgoto bruto importado                                                        | numeric   |
| FN039                          | Despesa com esgoto exportado                                                                               | numeric   |
| FN041                          | Despesas capitalizáveis realizadas pelo(s) município(s)                                                    | numeric   |
| FN042                          | Investimento realizado em abastecimento de água pelo(s) município(s)                                       | numeric   |
| FN043                          | Investimento realizado em esgotamento sanitário pelo(s) município(s)                                       | numeric   |
| FN044                          | Outros investimentos realizados pelo(s) município(s)                                                       | numeric   |
| FN045                          | Investimento com recursos próprios realizado pelo(s) município(s)                                          | numeric   |
| FN046                          | Investimento com recursos onerosos realizado pelo(s) município(s)                                          | numeric   |
| FN047                          | Investimento com recursos não onerosos realizado pelo(s) município(s)                                      | numeric   |
| FN048                          | Investimentos totais realizados pelo(s) município(s)                                                       | numeric   |
| FN051                          | Despesas capitalizáveis realizadas pelo estado                                                             | numeric   |
| FN052                          | Investimento realizado em abastecimento de água pelo estado                                                | numeric   |
| FN053                          | Investimento realizado em esgotamento sanitário pelo estado                                                | numeric   |
| FN054                          | Outros investimentos realizados pelo estado                                                                | numeric   |
| FN055                          | Investimento com recursos próprios realizado pelo estado                                                   | numeric   |
| FN056                          | Investimento com recursos onerosos realizado pelo estado                                                   | numeric   |
| FN057                          | Investimento com recursos não onerosos realizado pelo estado                                               | numeric   |
| FN058                          | Investimentos totais realizados pelo estado                                                                | numeric   |
| QD001                          | Tipo de atendimento da portaria sobre qualidade da água                                                    | character |
| QD002                          | Quantidades de paralisações no sistema de distribuição de água                                             | integer   |
| QD003                          | Duração das paralisações                                                                                   | numeric   |
| QD004                          | Quantidade de economias ativas atingidas por paralisações                                                  | numeric   |
| QD006                          | Quantidade de amostras para cloro residual (analisadas)                                                    | numeric   |
| QD007                          | Quantidade de amostras para cloro residual com resultados fora do padrão                                   | numeric   |
| QD008                          | Quantidade de amostras para turbidez (analisadas)                                                          | numeric   |
| QD009                          | Quantidade de amostras para turbidez fora do padrão                                                        | numeric   |
| QD011                          | Quantidades de extravasamentos de esgotos registrados                                                      | numeric   |
| QD012                          | Duração dos extravasamentos registrados                                                                    | numeric   |
| QD015                          | Quantidade de economias ativas atingidas por interrupções sistemáticas                                     | numeric   |
| QD019                          | Quantidade mínima de amostras para turbidez (obrigatórias)                                                 | numeric   |
| QD020                          | Quantidade mínima de amostras para cloro residual (obrigatórias)                                           | numeric   |
| QD021                          | Quantidade de interrupções sistemáticas                                                                    | numeric   |
| QD022                          | Duração das interrupções sistemáticas                                                                      | numeric   |
| QD023                          | Quantidade de reclamações ou solicitações de serviços                                                      | numeric   |
| QD024                          | Quantidade de serviços executados                                                                          | numeric   |
| QD025                          | Tempo total de execução dos serviços                                                                       | numeric   |
| QD026                          | Quantidade de amostras para coliformes totais (analisadas)                                                 | numeric   |
| QD027                          | Quantidade de amostras para coliformes totais com resultados fora do padrão                                | integer   |
| QD028                          | Quantidade mínima de amostras para coliformes totais (obrigatórias)                                        | numeric   |
| IN001                          | Densidade de economias de água por ligação                                                                 | numeric   |
| IN002                          | Índice de produtividade: economias ativas por pessoal próprio                                              | numeric   |
| IN003                          | Despesa total com os serviços por m3 faturado                                                              | numeric   |
| IN004                          | Tarifa média praticada                                                                                     | numeric   |
| IN005                          | Tarifa média de água                                                                                       | numeric   |
| IN006                          | Tarifa média de esgoto                                                                                     | numeric   |
| IN007                          | Incidência da desp. de pessoal e de serv. de terc. nas despesas totais com os serviços                     | numeric   |
| IN008                          | Despesa média anual por empregado                                                                          | numeric   |
| IN009                          | Índice de hidrometração                                                                                    | numeric   |
| IN010                          | Índice de micromedição relativo ao volume disponibilizado                                                  | numeric   |
| IN011                          | Índice de macromedição                                                                                     | numeric   |
| IN012                          | Indicador de desempenho financeiro                                                                         | numeric   |
| IN013                          | Índice de perdas faturamento                                                                               | numeric   |
| IN014                          | Consumo micromedido por economia                                                                           | numeric   |
| IN015                          | Índice de coleta de esgoto                                                                                 | numeric   |
| IN016                          | Índice de tratamento de esgoto                                                                             | numeric   |
| IN017                          | Consumo de água faturado por economia                                                                      | numeric   |
| IN018                          | Quantidade equivalente de pessoal total                                                                    | numeric   |
| IN019                          | Índice de produtividade: economias ativas por pessoal total (equivalente)                                  | numeric   |
| IN020                          | Extensão da rede de água por ligação                                                                       | numeric   |
| IN021                          | Extensão da rede de esgoto por ligação                                                                     | numeric   |
| IN022                          | Consumo médio percapita de água                                                                            | numeric   |
| IN023                          | Índice de atendimento urbano de água                                                                       | numeric   |
| IN024                          | Índice de atendimento urbano de esgoto referido aos municípios atendidos com água                          | numeric   |
| IN025                          | Volume de água disponibilizado por economia                                                                | numeric   |
| IN026                          | Despesa de exploração por m3 faturado                                                                      | numeric   |
| IN027                          | Despesa de exploração por economia                                                                         | numeric   |
| IN028                          | Índice de faturamento de água                                                                              | numeric   |
| IN029                          | Índice de evasão de receitas                                                                               | numeric   |
| IN030                          | Margem da despesa de exploração                                                                            | numeric   |
| IN031                          | Margem da despesa com pessoal próprio                                                                      | numeric   |
| IN032                          | Margem da despesa com pessoal total (equivalente)                                                          | numeric   |
| IN033                          | Margem do serviço da divida                                                                                | numeric   |
| IN034                          | Margem das outras despesas de exploração                                                                   | numeric   |
| IN035                          | Participação da despesa com pessoal próprio nas despesas de exploração                                     | numeric   |
| IN036                          | Participação da despesa com pessoal total (equivalente) nas despesas de exploração                         | numeric   |
| IN037                          | Participação da despesa com energia elétrica nas despesas de exploração                                    | numeric   |
| IN038                          | Participação da despesa com produtos químicos nas despesas de exploração (DEX)                             | numeric   |
| IN039                          | Participação das outras despesas nas despesas de exploração                                                | numeric   |
| IN040                          | Participação da receita operacional direta de água na receita operacional total                            | numeric   |
| IN041                          | Participação da receita operacional direta de esgoto na receita operacional total                          | numeric   |
| IN042                          | Participação da receita operacional indireta na receita operacional total                                  | numeric   |
| IN043                          | Participação das economias residenciais de água no total das economias de água                             | numeric   |
| IN044                          | Índice de micromedição relativo ao consumo                                                                 | numeric   |
| IN045                          | Índice de produtividade: empregados próprios por 1000 ligações de água                                     | numeric   |
| IN046                          | Índice de esgoto tratado referido à água consumida                                                         | numeric   |
| IN047                          | Índice de atendimento urbano de esgoto referido aos municípios atendidos com esgoto                        | numeric   |
| IN048                          | Índice de produtividade: empregados próprios por 1000 ligações de água + esgoto                            | numeric   |
| IN049                          | Índice de perdas na distribuição                                                                           | numeric   |
| IN050                          | Índice bruto de perdas lineares                                                                            | numeric   |
| IN051                          | Índice de perdas por ligação                                                                               | numeric   |
| IN052                          | Índice de consumo de água                                                                                  | numeric   |
| IN053                          | Consumo médio de água por economia                                                                         | numeric   |
| IN054                          | Dias de faturamento comprometidos com contas a receber                                                     | numeric   |
| IN055                          | Índice de atendimento total de água                                                                        | numeric   |
| IN056                          | Índice de atendimento total de esgoto referido aos municípios atendidos com água                           | numeric   |
| IN057                          | Índice de fluoretação de água                                                                              | numeric   |
| IN058                          | Índice de consumo de energia elétrica em sistemas de abastecimento de água                                 | numeric   |
| IN059                          | Índice de consumo de energia elétrica em sistemas de esgotamento sanitário                                 | numeric   |
| IN060                          | Índice de despesas por consumo de energia elétrica nos sistemas de água e esgotos                          | numeric   |
| IN071                          | Economias atingidas por paralisações                                                                       | numeric   |
| IN072                          | Duração média das paralisações                                                                             | numeric   |
| IN073                          | Economias atingidas por intermitências                                                                     | numeric   |
| IN074                          | Duração média das intermitências                                                                           | numeric   |
| IN075                          | Incidência das análises de cloro residual fora do padrão                                                   | numeric   |
| IN076                          | Incidência das análises de turbidez fora do padrão                                                         | numeric   |
| IN077                          | Duração média dos reparos de extravasamentos de esgotos                                                    | numeric   |
| IN079                          | Índice de conformidade da quantidade de amostras - cloro residual                                          | numeric   |
| IN080                          | Índice de conformidade da quantidade de amostras - turbidez                                                | numeric   |
| IN082                          | Extravasamentos de esgotos por extensão de rede                                                            | numeric   |
| IN083                          | Duração média dos serviços executados                                                                      | numeric   |
| IN084                          | Incidência das análises de coliformes totais fora do padrão                                                | numeric   |
| IN085                          | Índice de conformidade da quantidade de amostras - coliformes totais                                       | numeric   |
| IN101                          | Índice de suficiência de caixa                                                                             | numeric   |
| IN102                          | Índice de produtividade de pessoal total (equivalente)                                                     | numeric   |
| código da região intermediária | Região Geográfica Intermediária                                                                            | numeric   |
| região intermediária           | Nome Região Geográfica Intermediária                                                                       | factor    |
| código da região imediata      | Região Geográfica Imediata                                                                                 | numeric   |
| região imediata                | Nome Região Geográfica Imediata                                                                            | factor    |
| prestador2                     | prestador2                                                                                                 | NULL      |
| tarifa                         | tarifa                                                                                                     | NULL      |
| micromedida                    | micromedida                                                                                                | NULL      |
| urbanização                    | urbanização                                                                                                | NULL      |

Tabela com dados filtrados
