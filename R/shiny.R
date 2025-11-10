#' ShinyApp
#'
#' Função geradora da aplicação em Shiny do pacote SNIS
#'
#' @export
#'
#' @importFrom DT dataTableOutput datatable
#' @importFrom tmap tmapOutput renderTmap
#' @import shiny
shiny = function() {
  header_col = function(title, color, height, width = 12) {
    column(width,
           div(
             style = paste0(
               "background-color:", color, ";",
               "color: #3e3e3e;
             font-size: 35px;
             height: calc(",height,"vh - 10px);",
               "line-height: calc(", height,"vh - 10px);",
               "text-align: center;
             margin: 5px 2.5px;
             border-radius: 10px;
             "),
             title
           )
    )
  }

  ###########################
  #####  Painel Tabela  #####
  ###########################
  PainelTabela = tabPanel(
    title = "Tabela",

    fluidRow(header_col("Dados", "#87C2CC", 12)),
    fluidRow(
      DT::dataTableOutput("tabela-tabela")
    ),

    fluidRow(
      column(7,
        fluidRow(header_col("Mapa", "#a8f2fe", 8)),
        tmap::tmapOutput("tabela-mapa")
      ),
      column(5,
        fluidRow(header_col("Resumo", "#a8f2fe", 8)),
        plotOutput("tabela-hist"),
        tableOutput("tabela-smry")
      )
    )
  )

  #######################
  #####  Painel FA  #####
  #######################
  PainelFA = tabPanel(
    title = "Análise Fatorial",

    fluidRow(header_col("Scores", "#a8f2fe", 8)),

    fluidRow(
      div(
        style = "display: inline-block; width: 200px; margin-right: 20px;",
        selectInput("fa-scores-var", label = "Variável", choices = "")
      ),
      div(
        style = "display: inline-block; margin-top: 25px;",
        checkboxInput("fa-scores-quart", "Quartis", value = TRUE)
      )
    ),

    fluidRow(
      column(8,
        tmap::tmapOutput("fa-scores-mapa")
      ),
      column(4,
        fluidRow(tableOutput("fa-scores-summary")),
        fluidRow(verbatimTextOutput("fa-scores-summary2"))

      )
    ),

    fluidRow(header_col("Loadings", "#a8f2fe", 8)),

    fluidRow(
      column(7,
        fluidRow(header_col("EEE", "#a8f2fe", 8)),
        plotOutput("fa-loadings-eee")
      ),
      column(5,
        fluidRow(header_col("SU", "#a8f2fe", 8)),
        plotOutput("fa-loadings-su")
      )
    )
  )

  ########################
  #####  Inferência  #####
  ########################
  PainelInferencia = tabPanel("Inferência", h3("Inferência"))


  #############################
  #####  Painel Comparar  #####
  #############################
  PainelComparar = tabPanel("Comparar", h3("Painel Comparar (sem seletor de ano)"))



  ####################
  #####  Server  #####
  ####################
  shiny_server = function(input, output, session) {

    ###  Reactive  ###
    ano   = reactive({ input$"geral-ano" })
    #var   = reactive({ input$"fa-scores-var" })
    #quart = reactive({ input$"fa-scores-quart" })
    df    = reactive({ dados_snis[[ano()]]$df })
    geo_df = reactive({
      df() %>%
        select(
          name = all_of("município"),
          Score = all_of(input$"fa-scores-var"),
          Grupo = all_of("região intermediária")
        ) %>%
        mutate(Score = round(Score, 4)) %>%
        distinct(name, .keep_all = TRUE) %>%
        left_join(x = mapa_MG, by = "name")
    })

    ###  PainelFA  ###
    output$"fa-scores-mapa" = tmap::renderTmap({
      req(df())
      mapa_interativo(df(), var = input$"fa-scores-var", quart = input$"fa-scores-quart") |> suppressMessages()
      # plot_map(df(), var = input$"fa-scores-var", quart = input$"fa-scores-quart") |>
      #   plotly::ggplotly()
    })

    observeEvent(df(), {
      updateSelectInput(session, "fa-scores-var",
        #choices = colnames(df())
        choices = c("score médio eee", "score efetividade", "score eficácia", "score eficiência", "score médio su", "score sustentabilidade", "score universalidade")
      )
    })

    output$"fa-scores-summary" = renderTable({
      req(df())
      df()[[ input$"fa-scores-var" ]] |>
        summary() |>
        as.matrix() |>
        t() |>
        as.data.frame()
    })

    output$"fa-loadings-eee" = renderPlot({
      req(ano())
      #FA_EEE = fa(df(), features = readODS::read_ods("data/features.ods", sheet = "EEE"))
      plot_loading(dados_snis[[ano()]]$fa$eee)
    })

    output$"fa-loadings-su" = renderPlot({
      req(ano())
      #FA_SU = fa(df(), features = readODS::read_ods("data/features.ods", sheet = "SU"))
      plot_loading(dados_snis[[ano()]]$fa$su)
    })

    ###  PainelTabela  ###
    output$"tabela-tabela" = DT::renderDataTable({
      DT::datatable(df(),
                    selection = list(mode = "single", target = "column"),
                    rownames = FALSE, filter = 'none',
                    extensions = c('FixedColumns'),
                    options = list(
                      dom = 'Bfrtip',
                      paging = TRUE, searching = TRUE, info = FALSE,
                      sort = TRUE, scrollX = TRUE, fixedColumns = list(leftColumns = 3)
                    )
                  )
    })

    coluna_selecionada = reactive({
      req(input$"tabela-tabela_columns_selected")
      colnames(df())[input$"tabela-tabela_columns_selected" + 1]
    })

    output$"tabela-hist" = renderPlot({
      x = df()[[coluna_selecionada()]]

      if (is.numeric(x)) {
        hist(x,
             main = paste("Histograma de", coluna_selecionada()),
             xlab = coluna_selecionada(),
             col = "skyblue", border = "white")
      } else {
        barplot(table(x),
                main = paste("Frequência de", coluna_selecionada()),
                col = "orange", border = "white")
      }
    })

    output$"tabela-smry" = renderTable({
      df()[[coluna_selecionada()]] |>
        summary() |>
        as.matrix() |>
        t() |>
        as.data.frame()
    })

    output$"tabela-mapa" = tmap::renderTmap({
      mapa_interativo(df(), coluna_selecionada(), quart = T) |> suppressMessages()
    })

    observeEvent(input$"fa-scores-mapa_shape_click", {
      click = input$"fa-scores-mapa_shape_click"

      click_municipio =
        geo_df() |>
        slice(as.integer(substring(click$id,2))) |>
        as_tibble() |>
        pull("id") |>
        as.integer()

      output$"fa-scores-summary2" = renderPrint({
        df() |>
          filter(`código do município` == click_municipio) |>
          select(all_of(c(
            "município", "código do município", "natureza jurídica", "tipo de serviço", "abrangência", "código do prestador", "prestador"
            )))
      })

    })


  }



  ####################
  #####  shinyApp ####
  ####################
  ui = navbarPage(
    title = "SNIS app",

    header = tagList(
      tags$script(HTML("
        $(document).on('shown.bs.tab', 'a[data-bs-toggle=\"tab\"]', function (e) {
          var tabName = $(e.target).text().trim();
          if (tabName === 'Comparar') {
            $('#geral-ano-div').hide();
          } else {
            $('#geral-ano-div').show();
          }
        });
      ")),

      div(
        id = "geral-ano-div",
        style = "padding: 10px;",
        selectInput("geral-ano", label = "Ano", choices = as.character(2010:2021))
      )
    ),

    PainelTabela,
    PainelFA,
    PainelInferencia,
    PainelComparar
  )

  shinyApp(ui = ui, server = shiny_server)
}
