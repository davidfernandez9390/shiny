# Instal·lació dels paquets necessaris:
list.of.packages <- c("shiny", "ggplot2", "plotly")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Càrrega dels paquets necessaris:
lapply(list.of.packages, require, character.only = TRUE)

# Importació del codi del shiny_interface:
source("shiny_interface.R")

# BLOC DEL UI:
ui <- fluidPage(
  titlePanel("Cost-effectiveness simulation"),
  sidebarLayout(
    sidebarPanel(
      style = "height: 90vh; overflow-y: auto;",
      
      h4("Strategy selection:"),
      
      # Checkbox per seleccionar les diferents estratègies del get.strategies() des de la source:
      checkboxGroupInput("strategies", "Select strategies:",
                         choices = get.strategies(),
                         selected = get.strategies()),  # Totes seleccionades per defecte
      
      div(style = "display: flex; gap: 4px;",
          actionButton("select_all", "Select All"),      # Botó per seleccionar-les totes
          actionButton("deselect_all", "Deselect All")),  # Botó per desseleccionar-les totes
      
      h4("Parameter values:"),
      
      lapply(get.parameters(), function(p) {
        probabilitats <- grepl("^p\\.", p$name)  # Detecta paràmetres que comencen amb 'p.'
        
        if (probabilitats) {
          max.value <- 1  # Per probabilitats, el màxim ha de ser 1
        } else if (!is.null(p$max.value)) {
          max.value <- p$max.value  # Si té un max.value ja definit en el model, l'assignem
        } else {
          max.value <- p$base.value * 4  # Si no té max.value, assignem base.value * 4
        }
        
        numericInput(p$name, p$name, value = p$base.value, min = 0, max = max.value)
      }),
      
      # Botó per fer reset dels paràmetres:
      actionButton("reset", "Reset to Default")    
    ),
    
    mainPanel(
      actionButton("run", "Run simulation"),
      br(),
      h4("Table of results:"),
      tableOutput("resultsTable"), # Col·locació de la taula
      downloadButton("downloadData", "Download"),
      br(), br(),
      h4("Results Plot:"),
      plotlyOutput("resultsPlot")  # Col·locació del gràfic
    )
  )
)

# BLOC DEL SERVER:
server <- function(input, output, session) {
  # Execució del botó select_all, se seleccionen totes les estratègies:
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "strategies", selected = get.strategies())
  })
  
  # Execució del botó deselect_all, es desseleccionen totes les estratègies:
  observeEvent(input$deselect_all, {
    updateCheckboxGroupInput(session, "strategies", selected = character(0))  # Cap seleccionada
  })
  
  # Execució del Botó de Reset dels paràmetres
  observeEvent(input$reset, {
    lapply(get.parameters(), function(p) {
      updateNumericInput(session, p$name, value = p$base.value)  # Canviem updateSliderInput() per updateNumericInput()
    })
  })
  
  # Resultats quan es prem el botó Run:
  results <- eventReactive(input$run, {
    params <- lapply(get.parameters(), function(p) input[[p$name]])  
    names(params) <- sapply(get.parameters(), function(p) p$name)
    run.simulation(input$strategies, params)  # Passa una llista d'estratègies
  })
  
  # Anàlisis amb la funció analyzeCE de CEAModel dins un reactive per ser usat en altres moments:
  ce_analysis <- reactive({
    taula_resultats <- results()$summary
    CEAModel::analyzeCE(taula_resultats, plot = TRUE)
  }) 
  
  # Taula de resultats a partir del anàlisis de CEAModel:
  output$resultsTable <- renderTable({
    ce_analysis()$summary
  })
  
  # Downloadable CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("results_table", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(ce_analysis()$summary, file, row.names = FALSE)
    }
  )
  
  # Gràfic interactiu
  output$resultsPlot <- renderPlotly({
    ggplotly(ce_analysis()$plot)
  })

}

shinyApp(ui = ui, server = server)

