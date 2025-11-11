# SNIS

Esse pacote tem por objetivo formalizar e disponibilizar funções para a leitura e análise de dados do SNIS
(Sistema Nacional de Informações sobre Saneamento).
As funções estão documentadas e há algumas vinhetas que ajudam a entender o funcionamento do pacote.
Isso está disponível no [site do pacote](https://mardenos-ufmg.github.io/snis/).

Para utilizar o pacote, siga alguma das opções abaixo.
Em ambos os casos é interessante instalar o pacote `pak` em seu ambiente R via `install.packages("pak")` e usar o [RStudio](https://posit.co/download/rstudio-desktop/).
Sempre que houver uma alteração no pacote será necessário repetir o procedimento para usufruir das atualizações.

## Via `pak`

Instale o `pak` em seu ambiente R e, em seguida, instale o `snis` via `pak`.

``` r
if (!{"pak" %in% rownames(installed.packages())}) install.packages("pak")
pak::pak("mardenos-ufmg/snis")
```

## Via `devtools`

Primeiro é necessário baixar a pasta do nosso repositório no GitHub.
O jeito mais fácil é acessando o [nosso repositório no GitHub](https://github.com/mardenos-ufmg/snis) e clicando no botão verde escrito `code` e em seguida `Download Zip`.
Em seguida faça a descompressão (unzip) da pasta e instale o `devtools` via R no seu PC.

``` r
install.packages("devtools")
```

ou, preferencialmente

``` r
if (!{"pak" %in% rownames(installed.packages())}) install.packages("pak")
if (!{"devtools" %in% rownames(installed.packages())}) pak::pak("devtools")

```

Em seguida abra o repositório `snis` descompactado em seu PC e clique em `snis.Rproj`.
Isso iniciará uma sessão do RStudio com o projeto já aberto.
Finalmente, rode no console do RStudio o comando `devtools::load_all()` e você poderá usar todas as funções do nosso pacote!
