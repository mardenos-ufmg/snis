shiny = function() {
  header_col = function(title, color, height, width) {
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

  #########################
  #####  Panel Plots  #####
  #########################

  PanelPlots = tabPanel(
    title = "Plots",

    fluidRow(header_col("Plots", "#87C2CC", 12, 12)),
    fluidRow(
      selectInput("plots-ano", label = "Ano", choices = as.character(2010:2021)),
    ),

    fluidRow(header_col("Scores", "#a8f2fe", 8, 12)),

    fluidRow(
      selectInput("plots-scores-var", label = "Variável", choices = ""),
      checkboxInput("plots-scores-quart", "Quaris", value = FALSE),
      column(8,
        plotly::plotlyOutput("plots-scores-mapa"),
      ),
      column(4,
        tableOutput("plots-scores-summary")
      )
    ),

    fluidRow(header_col("Loadings", "#a8f2fe", 8, 12)),

    fluidRow(
      column(7,
        fluidRow(header_col("EEE", "#a8f2fe", 8, 12)),
        plotOutput("plots-loadings-eee")
      ),
      column(5,
        fluidRow(header_col("SU", "#a8f2fe", 8, 12)),
        plotOutput("plots-loadings-su")
      )
    )

  )

  ####################
  #####  Server  #####
  ####################
  shiny_server = function(input, output, session) {

    ano   = reactive({ input$"plots-ano" })
    var   = reactive({ input$"plots-scores-var" })
    quart = reactive({ input$"plots-scores-quart" })
    df    = reactive({ dfs[[ano()]] })

    output$"plots-scores-mapa" = plotly::renderPlotly({
      req(df(), var(), quart())
      plot_map(df(), var = var(), quart = quart()) |>
        plotly::ggplotly()
    })

    observeEvent(df(), {
      updateSelectInput(session, "plots-scores-var",
        choices = colnames(df())
      )
    })

    output$"plots-scores-summary" = renderTable({
      req(df(), var())
      smry = summary(df()[[var()]])
      smry |>
        as.matrix() |>
        t() |>
        as.data.frame()
    })

    output$"plots-loadings-eee" = renderPlot({
      req(df())
      FA_EEE = fa(df(), features = readODS::read_ods("data/features.ods", sheet = "EEE"))
      plot_loading(FA_EEE)
    })

    output$"plots-loadings-su" = renderPlot({
      req(df())
      FA_SU = fa(df(), features = readODS::read_ods("data/features.ods", sheet = "SU"))
      plot_loading(FA_SU)
    })
  }

  shinyApp(
    ui = navbarPage(title = "SNIS app", PanelPlots),
    server = shiny_server
  )

}
