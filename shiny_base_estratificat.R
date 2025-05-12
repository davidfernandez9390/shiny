# Instal·lació i càrrega de paquets necessaris
list.of.packages <- c("shiny", "ggplot2", "plotly", "readxl", "writexl", "shinycssloaders")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)  # Instal·la els paquets que no estiguin instal·lats
lapply(list.of.packages, require, character.only = TRUE)  # Carrega tots els paquets

# Funció que retorna una llista de models amb nom i enllaç al repositori GITHUB
get.models.repo <- function(){
  return(list(
    list(name='(Selecciona un model)', url=NULL),  # Opció per defecte
    list(name='anus', url='https://github.com/davidfernandez9390/anus.git'),
    list(name='model1', url='https://github.com/davidfernandez9390/model1.git'),
    list(name='model2', url='https://github.com/davidfernandez9390/model2.git')
  ))
}

# BLOC DEL UI
ui <- fluidPage(
  titlePanel("Cost-effectiveness simulation"),  
  
  #Estil de la shiny amb barra lateral
  sidebarLayout(
    #Definició de la barra lateral:
    sidebarPanel(
      h4("Model selection:"),  # Secció per seleccionar un model
      selectInput("selected_model", "Choose model:",
                  choices = sapply(get.models.repo(), function(x) x$name)),
      
      actionButton("load_model", "Load a model"),  # Botó per carregar/clonar el model des de GitHub
      
      h4("Strategy selection:"),  # Secció per escollir estratègies
      shinycssloaders::withSpinner(
        uiOutput("strategies_ui"),  # Espai per mostrar les estratègies disponibles
        type=4, size=0.3,
        caption = div(strong("loading model"))),
      
      # Botons per seleccionar/deseleccionar totes les estratègies
      div(style = "display: flex; gap: 4px;",
          actionButton("select_all", "Select All"),
          actionButton("deselect_all", "Deselect All")
      ),
      
      h4("Parameter values:"),  # Paràmetres del model
      shinycssloaders::withSpinner(
        uiOutput("parameters_tabs"),  # Inputs pels valors dels paràmetres
        type=4, size=0.3,
        caption = div(strong("loading model"))),
      
      actionButton("reset", "Reset to Default")  # Botó per reiniciar els valors per defecte
    ),
    
    mainPanel(
      actionButton("run", "Run simulation"),  # Executar simulació
      br(), br(),
      h4("Results Plot:"),
      plotlyOutput("resultsPlot"),  # Resultats en forma de gràfic
      br(),
      h4("Table of results:"),
      tableOutput("resultsTable"),  # Taula amb els resultats
      downloadButton("downloadData", "Download")  # Botó per descarregar la taula
    )
  )
)

# BLOC DEL SERVER:
server <- function(input, output, session) {
  
  # Quan es clica "Load a model"
  observeEvent(input$load_model, {
    # Agafa el model seleccionat en el selectInput i també el seu URL: 
    selected_model_info <- get.models.repo()[[which(sapply(get.models.repo(), function(x) x$name) == input$selected_model)]] #filtrant el nom que coincideix amb el seleccionat
    
    # Si no hi ha URL vàlida, mostra error
    if (is.null(selected_model_info$url)) {
      showNotification("Choose a valid model", type = "error")
      return()
    }
    
    # Defineix on es guardarà el model al PC
    model_path <- file.path("models_cloned", input$selected_model)
    
    # Si no existeix ja el model, es clona des de GitHub
    if (!dir.exists(model_path)) {
      showNotification(paste("cloning repository:", input$selected_model), type = "message")
      git2r::clone(selected_model_info$url, local_path = model_path)
    } else {
      showNotification("model already existing in PC", type = "warning")
    }
    
    # Elimina les funcions anteriors carregades a l'entorn (potser millor fer-ho com en el DSA però aquí de moment no donava errors)
    model_dir <- file.path(model_path)
    rm(list = c("run.simulation", "get.strategies", "get.parameters"), envir = .GlobalEnv)
    
    # Executa el script del model carregat que defineix les funcions
    withr::with_dir(model_dir, {
      source("shiny_interface.R", local = FALSE)
    })
    
    # Actualitza les estratègies del model seleccionat
    output$strategies_ui <- renderUI({
      checkboxGroupInput("strategies", "Select strategies:",
                         choices = get.strategies(),
                         selected = get.strategies())
    })
    
    # Actualitza els inputs dels paràmetres agrupats per estrats per pestanyes
    output$parameters_tabs <- renderUI({
      parameters <- get.parameters() 
      #s'agrupen els paràmetres segons el seu estrat i, si no en tenen, es genera un de genèric "constant parameters" per agrupar-los en una pestanya.
      stratified_params <- split(parameters, sapply(parameters, function(p) p$stratum %||% "Constant Parameters"))
      
      #es genera una pestanya per cada estrat
      tab_panels <- lapply(names(stratified_params), function(stratum) {
        tabPanel(
          title = stratum,  # Nom de l'estrat com a títol de pestanya
          lapply(stratified_params[[stratum]], function(p) {
            input_id <- if (!is.null(p$stratum)) { #es fa el id únic per cada estrat afegint-lo com a sufix, així no tenen el mateix nom
              paste0(p$name, "_", gsub("\\s+", "_", p$stratum))
            } else {
              p$name
            }
            
            # Crea un numericInput amb id únic per paràmetre/estrat
            numericInput(inputId = input_id,
                         label = p$name,
                         value = p$base.value,
                         min = 0,
                         max = ifelse(startsWith(p$name, "p."), 1, p$base.value * 4)) #!! al final potser és innecessari, millor no fer max.
          })
        )
      })
      
      if (length(tab_panels) == 1) {
        tab_panels[[1]]  # Si només hi ha un estrat, no fem tabsetPanel
      } else {
        do.call(tabsetPanel, c(id = "parameter_tabs", tab_panels))  # Si n’hi ha més, usem pestanyes
      }
    })
  })
  
  # Botó "Select All": selecciona totes les estratègies
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "strategies", selected = get.strategies())
  })
  
  # Botó "Deselect All": desselecciona totes les estratègies
  observeEvent(input$deselect_all, {
    updateCheckboxGroupInput(session, "strategies", selected = character(0))
  })
  
  # Botó per fer reset i tornar als valors base. 
  observeEvent(input$reset, {
    lapply(get.parameters(), function(p) {
      input_id <- if (!is.null(p$stratum)) {
        paste0(p$name, "_", gsub("\\s+", "_", p$stratum))
      } else {
        p$name
      }
      updateNumericInput(session, input_id, value = p$base.value) #actualitzar el valor de nou amb el base
    })
  })
  
  # Execució de la simulació quan es clica "Run simulation"
  results <- eventReactive(input$run, {  # Reactiu que s'executa quan es prem el botó "Run simulation"
    
    progress <- shiny::Progress$new()  # Crea una nova barra de progrés per mostrar l'estat de la simulació
    on.exit(progress$close())  # Quan acabi aquesta funció, es tanca automàticament la barra de progrés
    
    progress$set(message = "Running simulation...", value = 0)  # Mostra missatge inicial i valor 0% a la barra
    
    parameters <- get.parameters()  # Obté la llista de paràmetres del model carregat
    params <- list()  # Inicialitza una llista buida per guardar els valors dels paràmetres més tard
    
    # Recorre cada paràmetre i extreu el valor actual que ha posat l'usuari
    for (p in parameters) {
      input_id <- if (!is.null(p$stratum)) {  # Si el paràmetre és estratificat... (fer això d'afegir el sufix del estrat al id com una funció per no repetir!!!)
        paste0(p$name, "_", gsub("\\s+", "_", p$stratum))  # ...crea l'ID combinant el nom i l’estrat (substituint espais per guions baixos)
      } else {
        p$name  # Si no és estratificat, l'ID és simplement el nom
      }
      
      value <- input[[input_id]]  # Per un determinat id del input es guarda el seu valor introduït manualment en la variable 'value'
      
      if (!is.null(p$stratum)) {  # Si el paràmetre és estratificat (té stratum)...
        if (is.null(params[[p$name]])) {  # Si encara no existeix cap entrada per aquest nom de paràmetre, la crea com a llista
          params[[p$name]] <- list() #quan hi ha estrat llavors dintre de la llista params es genera una llista pel p$name (p$name = list()) on es ficaran els valors dels estrats
        }
        params[[p$name]][[p$stratum]] <- value  # Dins de la llista del p$name guarda el valor a la clau del estrat corresponent (si hi ha estrat)
      } else {
        params[[p$name]] <- value  # Si no és estratificat, desa el valor directament (no com dins d'una altra llista sino com a valor de la clau p$name)
      }
    }
    
    # Inicialitza una llista per guardar els resultats de cada estratègia
    results_list <- list()
    
    for (i in seq_along(input$strategies)) {  # Itera sobre el numero d'estratègies seleccionades per l'usuari (amb seq_along no cal que faci lenght)
      strategy <- input$strategies[i]  # Obté el nom de l’estratègia
      result <- run.simulation(strategy, params)  # Executa la simulació amb aquesta estratègia i els paràmetres
      
      results_list[[i]] <- result$summary  # Guarda el resum dels resultats en la llista
      
      # Actualitza la barra de progrés segons el progrés de l'iteració
      progress$inc(1 / length(input$strategies), 
                   detail = paste("Processing:", strategy, round(i / length(input$strategies) * 100), "% done"))
    }
    
    return(do.call(rbind, results_list))  # Combina tots els resultats en una sola taula
  })
  
  
  # Mostra la taula de resultats
  output$resultsTable <- renderTable({
    taula_resultats <- results()
    ce_analysis <- CEAModel::analyzeCE(taula_resultats, plot = TRUE)
    ce_analysis$summary
  })
  
  # Descarregar els resultats de la taula en format CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("results_table", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(results()$summary, file, row.names = FALSE)
    }
  )
  
  # Mostra el gràfic dels resultats
  output$resultsPlot <- renderPlotly({
    taula_resultats <- results()
    ce_analysis <- CEAModel::analyzeCE(taula_resultats, plot = TRUE)
    ggplotly(ce_analysis$plot)  # Converteix el ggplot a plotly per tenir-lo interactiu
  })
}


shinyApp(ui = ui, server = server)
