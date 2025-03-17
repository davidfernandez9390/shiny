# Instal·lació i càrrega de paquets necessaris
list.of.packages <- c("shiny", "ggplot2", "plotly", "readxl", "writexl")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# Obtenim la llista de models disponibles
model_dirs <- list.dirs("models", recursive = FALSE, full.names = FALSE)

# UI
ui <- fluidPage(
  titlePanel("Cost-effectiveness simulation"),
  sidebarLayout(
    sidebarPanel(
      style = "height: 90vh; overflow-y: auto;",
      
      h4("Model selection:"),
      selectInput("selected_model", "Choose model:", choices = model_dirs),
      
      h4("Strategy selection:"),
      uiOutput("strategies_ui"),
      div(style = "display: flex; gap: 4px;",
          actionButton("select_all", "Select All"),
          actionButton("deselect_all", "Deselect All")
      ),
      
      h4("Parameter values:"),
      uiOutput("parameters_tabs"),
      
      actionButton("reset", "Reset to Default")    
    ),
    
    mainPanel(
      actionButton("run", "Run simulation"),
      br(), br(),
      h4("Table of results:"),
      br(),
      tableOutput("resultsTable"), 
      downloadButton("downloadData", "Download"),
      h4("Results Plot:"),
      plotlyOutput("resultsPlot")
    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  # Canvi de model: carreguem shiny_interface del model seleccionat
  observeEvent(input$selected_model, {
    source(file.path("models", input$selected_model, "shiny_interface.R"), local = TRUE)
    
    # Actualitzem la UI de les estratègies
    output$strategies_ui <- renderUI({
      checkboxGroupInput("strategies", "Select strategies:",
                         choices = get.strategies(),
                         selected = get.strategies())
    })
    
    # Actualitzem la UI dels paràmetres en pestanyes si tenen `stratum`
    output$parameters_tabs <- renderUI({
      parameters <- get.parameters()
      # se separen els paràmetres en funció del stratum (si no en té se'n fa un anomenat Constant Parameters)
      stratified_params <- split(parameters, sapply(parameters, function(p) p$stratum %||% "Constant Parameters")) 
      #es recorren els estrats i es fa una pestanya per cadascuin
      tab_panels <- lapply(names(stratified_params), function(stratum) {
        tabPanel(
          title = stratum,
          # es fa un numeric input per a cada paràmetre segons el seu estrat
          lapply(stratified_params[[stratum]], function(p) {
            numericInput(p$name, p$name, value = p$base.value, min = 0, 
                         max = ifelse(startsWith(p$name, "p."), 1, p$base.value * 4)) #la condició pel max.value més sintetitzada
          })
        )
      })
      # si només hi ha una pestanya, no cal fer el tabsetPanel, si n'hi ha en fa un amb les pestanyes "tabPanel" que calgui 
      if (length(tab_panels) == 1) {
        tab_panels[[1]]
      } else {
        do.call(tabsetPanel, c(id = "parameter_tabs", tab_panels))
      }
    })
  })
  
  # Botons per seleccionar/deseleccionar totes les estratègies
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "strategies", selected = get.strategies())
  })
  
  observeEvent(input$deselect_all, {
    updateCheckboxGroupInput(session, "strategies", selected = character(0))
  })
  
  # Reset dels paràmetres
  observeEvent(input$reset, {
    lapply(get.parameters(), function(p) {
      updateNumericInput(session, p$name, value = p$base.value)
    })
  })
  
  # Execució de la simulació
  results <- eventReactive(input$run, {
    params <- lapply(get.parameters(), function(p) input[[p$name]])  
    names(params) <- sapply(get.parameters(), function(p) p$name)
    run.simulation(input$strategies, params)
  })
  
  # Mostrem la taula de resultats
  output$resultsTable <- renderTable({
    taula_resultats <- results()$summary
    ce_analysis <- CEAModel::analyzeCE(taula_resultats, plot = TRUE)
    ce_analysis$summary
  })
  
  # Downloadable CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("results_table", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(results()$summary, file, row.names = FALSE)
    }
  )
  
  # Gràfic interactiu
  output$resultsPlot <- renderPlotly({
    taula_resultats <- results()$summary
    ce_analysis <- CEAModel::analyzeCE(taula_resultats, plot = TRUE)
    ggplotly(ce_analysis$plot)
  })
}

shinyApp(ui = ui, server = server)  

