# Instal·lació i càrrega de paquets necessaris
list.of.packages <- c("shiny", "ggplot2", "plotly", "readxl", "writexl", "shinycssloaders")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# Llista de models amb el nom i l'enllaç del repositori GITHUB retornats per la funció:
get.models.repo <- function(){
  return(list(
    list(name='(Selecciona un model)', url=NULL),  # Opció per defecte sense URL
    list(name='anus', url='https://github.com/davidfernandez9390/anus.git'),
    list(name='model1', url='https://github.com/davidfernandez9390/model1.git'),
    list(name='model2', url='https://github.com/davidfernandez9390/model2.git')
  ))
}


#bloc del UI
ui <- fluidPage(
  titlePanel("Cost-effectiveness simulation"),
  
  #Estil de la shiny amb barra lateral
  sidebarLayout(
    #Definició de la barra lateral:
    sidebarPanel(
      h4("Model selection:"),
      selectInput("selected_model", "Choose model:", 
                  choices = sapply(get.models.repo(), function(x) x$name)),
      
      # Botó per clonar (si cal) i carregar el model des de GITHUB:
      actionButton("load_model", "Load a model"),  
      
      # Estratègies - uiOutput amb un spinner
      h4("Strategy selection:"),
      shinycssloaders::withSpinner(
        uiOutput("strategies_ui"),
        type=4, size=0.3,
        caption = div(strong("loading model"))),
    
      div(style = "display: flex; gap: 4px;",
          actionButton("select_all", "Select All"),
          actionButton("deselect_all", "Deselect All")
      ),
      
      #Paràmetres - uiOutput amb un spinner 
      h4("Parameter values:"),
      shinycssloaders::withSpinner(
        uiOutput("parameters_tabs"),
        type=4, size=0.3,
        caption = div(strong("loading model"))),
      
      actionButton("reset", "Reset to Default")    
    ),
    
    #mainPanel primer amb PLOT i després amb TABLE
    mainPanel(
      actionButton("run", "Run simulation"),
      br(), br(),
      h4("Results Plot:"),
      plotlyOutput("resultsPlot"),
      br(),
      h4("Table of results:"),
      tableOutput("resultsTable"), 
      downloadButton("downloadData", "Download"),
      br(),

    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  #Selecció del model:
  observeEvent(input$load_model, {  
    selected_model_info <- get.models.repo()[[which(sapply(get.models.repo(), function(x) x$name) == input$selected_model)]]
    
    # missatge d'error i es para la selecció si no hi ha URL no és vàlid (potser canviar això si es vol reutilitzar per seleccionar també carpetes)
    if (is.null(selected_model_info$url)) {
      showNotification("Choose a valid model", type = "error")
      return()
    }
    
    model_path <- file.path("models_cloned", input$selected_model)
    
    # si no hi ha carpetes dels models, missatge de clonar i clonar amb git2r::clone()
    if (!dir.exists(model_path)) {
      showNotification(paste("cloning repository:", input$selected_model), type = "message")
      git2r::clone(selected_model_info$url, local_path = model_path)
      #si ja hi ha carpetes, no clonar res i mostrar el warning de que ja és present
    } else {
      showNotification("model already existing in PC", type = "warning")
    }
    
    # Carregar shiny_interface.R del model seleccionat buscant a la caerpeta indicada
    model_dir <- file.path(model_path)
    # Resetjear les funcions antigues per evitar problemes al carregar nou model
    rm(list = c("run.simulation", "get.strategies", "get.parameters"), envir = .GlobalEnv)
    
    #Importar el shiny_interface.R
    withr::with_dir(model_dir, {
      source("shiny_interface.R", local = FALSE)
    })
    
    # Acctualitzar la UI de les estratègies
    output$strategies_ui <- renderUI({
      checkboxGroupInput("strategies", "Select strategies:",
                         choices = get.strategies(),
                         selected = get.strategies())
    })
    
    # Actualitzar la UI dels paràmetres
    output$parameters_tabs <- renderUI({
      parameters <- get.parameters()
      stratified_params <- split(parameters, sapply(parameters, function(p) p$stratum %||% "Constant Parameters"))
      
      tab_panels <- lapply(names(stratified_params), function(stratum) {
        tabPanel(
          title = stratum,
          lapply(stratified_params[[stratum]], function(p) {
            numericInput(p$name, p$name, value = p$base.value, min = 0, 
                         max = ifelse(startsWith(p$name, "p."), 1, p$base.value * 4))
          })
        )
      })
      
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
  
  # Execució de la simulació amb barra de progrés
  results <- eventReactive(input$run, {
    # Definir el progrés
    withProgress(message = 'Running simulation...', {
      params <- lapply(get.parameters(), function(p) input[[p$name]])  
      names(params) <- sapply(get.parameters(), function(p) p$name)
      result <- run.simulation(input$strategies, params)
      result
    })
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
