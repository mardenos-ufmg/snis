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

  dfs = readRDS("data/dfs.rds")

  ##########################
  #####  Panel Tabela  #####
  ##########################
  PanelTabela = tabPanel(
    title = "Tabela",

    fluidRow(header_col("Dados", "#87C2CC", 12)),
    fluidRow(
      tableOutput("tabela-tabela")
    ),

    fluidRow(
      column(7,
        fluidRow(header_col("Mapa", "#a8f2fe", 8)),
        plotOutput("tabela-mapa")
      ),
      column(5,
        fluidRow(header_col("Resumo", "#a8f2fe", 8)),
        plotOutput("tabela-resumo")
      )
    )
  )

  ######################
  #####  Panel FA  #####
  ######################
  PanelFA = tabPanel(
    title = "Plots",

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
        plotly::plotlyOutput("fa-scores-mapa")
      ),
      column(4,
        tableOutput("fa-scores-summary")
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

  ####################
  #####  Server  #####
  ####################
  shiny_server = function(input, output, session) {

    ano   = reactive({ input$"geral-ano" })
    var   = reactive({ input$"fa-scores-var" })
    quart = reactive({ input$"fa-scores-quart" })
    df    = reactive({ dfs[[ano()]] })

    output$"fa-scores-mapa" = plotly::renderPlotly({
      req(df(), var())
      plot_map(df(), var = var(), quart = quart()) |>
        plotly::ggplotly()
    })

    observeEvent(df(), {
      updateSelectInput(session, "fa-scores-var",
        #choices = colnames(df())
        choices = c("score médio eee", "score efetividade", "score eficácia", "score eficiência", "score médio su", "score sustentabilidade", "score universalidade")
      )
    })

    output$"fa-scores-summary" = renderTable({
      req(df(), var())
      smry = summary(df()[[var()]])
      smry |>
        as.matrix() |>
        t() |>
        as.data.frame()
    })

    output$"fa-loadings-eee" = renderPlot({
      req(df())
      FA_EEE = fa(df(), features = readODS::read_ods("data/features.ods", sheet = "EEE"))
      plot_loading(FA_EEE)
    })

    output$"fa-loadings-su" = renderPlot({
      req(df())
      FA_SU = fa(df(), features = readODS::read_ods("data/features.ods", sheet = "SU"))
      plot_loading(FA_SU)
    })
  }


  ####################
  #####  shinyApp ####
  ####################
  shinyApp(
    ui = navbarPage(
      title = "SNIS app",
      header = tagList(
        div(
          style = "padding: 10px;",
          selectInput("geral-ano", label = "Ano", choices = as.character(2010:2021))
        )
      ),
      PanelTabela,
      PanelFA
    ),
    server = shiny_server
  )
}
