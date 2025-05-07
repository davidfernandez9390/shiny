# Instal·lació i càrrega de paquets necessaris
list.of.packages <- c("shiny", "ggplot2", "plotly", "readxl", "writexl", "shinycssloaders", "git2r", "withr", "shinyTree")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

source('plot.tornado.R')

# Llista de models
get.models.repo <- function(){
  return(list(
    list(name='(Selecciona un model)', url=NULL),
    list(name='anus', url='https://github.com/davidfernandez9390/anus.git'),
    list(name='model1', url='https://github.com/davidfernandez9390/model1.git'),
    list(name='model2', url='https://github.com/davidfernandez9390/model2.git')
  ))
}

# UI
ui <- fluidPage(
  titlePanel("Cost-effectiveness simulation"),
  sidebarLayout(
    sidebarPanel(
      h4("Model selection:"),
      selectInput("selected_model", "Choose model:", 
                  choices = sapply(get.models.repo(), function(x) x$name)),
      actionButton("load_model", "Load a model"),
      
      h4("Strategy selection:"),
      uiOutput("ref_strategies_ui"),
      uiOutput("alt_strategies_ui"),
      
      h4("Variation (%)", style = "color: #191970; font-weight: bold; font-family: Segoe UI;"),
      sliderInput("variation_percent", label=NULL, step=10, min = 0, max = 100, value = 20),
      
      h4("WTP:", style = "color: #191970; font-weight: bold; font-family: Segoe UI;"),
      numericInput("wtp", label=NULL, value = 22000),
      
      h4("Parameter selection:"),
      #estructura en tree per seleccionar els paràmetres
      shinyTree::shinyTree("parameter_tree", checkbox = TRUE, theme = "proton", themeIcons = FALSE)
    ),
    
    mainPanel(
      actionButton("run", "Run simulation"),
      br(), br(),
      h4("Results Plot:"),
      plotlyOutput("resultsPlot"),
      #botons per seleccionar ICER o NHB
      radioButtons("icer", "Choose the measure:",
                   choices = c("ICER", "NHB"),
                   selected = "ICER",
                   inline = TRUE),
      br(),
      h4("Table of results:"),
      tableOutput("resultsTable"), 
      downloadButton("downloadData", "Download")
    )
  )
)

#server
server <- function(input, output, session) {
  # Valors reactius: un tipus de llista que emmagatzemarà les funcions del model quan es carregui:
  model_funs <- reactiveValues(
    get.parameters = NULL,
    get.strategies = NULL,
    run.simulation = NULL
  )
  
  #selecció del model i càrrega de paràmetres i estratègies
  observeEvent(input$load_model, {  
    selected_model_info <- get.models.repo()[[which(sapply(get.models.repo(), function(x) x$name) == input$selected_model)]]
    if (is.null(selected_model_info$url)) {
      showNotification("Choose a valid model", type = "error")
      return()
    }
    
    #selecció del path corresponent i si no existeix clonat desde GIT:
    model_path <- file.path("models_cloned", input$selected_model)
    if (!dir.exists(model_path)) {
      showNotification(paste("cloning repository:", input$selected_model), type = "message") #missatge de clonació
      git2r::clone(selected_model_info$url, local_path = model_path) #clonació desde GIT si toca
    } else {
      showNotification("model already existing in PC", type = "warning") #missatge que ja hi ha el directori
    }
    
    # Creació d'un entorn aïllat (temporal perquè cada vegada que es carrega un model es genera de nou):
    temp_env <- new.env()
    withr::with_dir(model_path, {
      source("shiny_interface.R", local = temp_env)
    })
    
    # assignar les funcions de l'entorn al Reactive Values per poder-los emprar en la shiny:
    model_funs$get.parameters <- temp_env$get.parameters
    model_funs$get.strategies <- temp_env$get.strategies
    model_funs$run.simulation <- temp_env$run.simulation
    
    #càrrega d'estratègia de referència (per defecte la primera)
    output$ref_strategies_ui <- renderUI({
      req(model_funs$get.strategies)
      selectInput("ref_strategy", "Reference strategy:",
                  choices = model_funs$get.strategies(),
                  selected = model_funs$get.strategies()[1])
    })
    
    #càrrega d'estatreagia alternativa (per defecte la segona)
    output$alt_strategies_ui <- renderUI({
      req(model_funs$get.strategies)
      selectInput("alt_strategy", "Alternative strategy:",
                  choices = model_funs$get.strategies(),
                  selected = model_funs$get.strategies()[2])
    })
    
    # Selecció dels paràmetres i Creació del shinyTree amb agrupació per classe (només visual)
    req(model_funs$get.parameters)
    parameters <- model_funs$get.parameters() #parameters conté la llista de llistes del get.parameters()
    class_grouped_params <- split(parameters, sapply(parameters, function(p) p$class %||% "General Parameters")) #class_grouped_params: llista on cada element és una class associada a la llista de params del grup.
    
    # Es generen els noms de les branques de l'arbre
    tree_list <- lapply(class_grouped_params, function(param_group) { #param_group es l'iteració per grups de la llista class_grouped_params
      unique_names <- unique(sapply(param_group, function(p) p$name)) #i amb sapply p$name s'agafen tots els noms dels paràmetres i amb unique s'eliminen les duplicacions dels estratificats
      setNames(as.list(rep(NA, length(unique_names))), unique_names) # es genera un vector amb NAs de la mida del noms unics dels parametres. Després, amb as.list es genera a llista i amb setNames s'associen com a claus els noms dels params als NA i cada llista correspon a una class. 
    })
    #mostrar el shinytree
    output$parameter_tree <- renderTree({ tree_list })
  })
  
  # funcio reactive per generar una llista de tots els noms de paràemtres amb els seus valors (fins i tot si hi ha estratS)
  all_param_values <- reactive({
    req(model_funs$get.parameters)
    parameters <- model_funs$get.parameters() #estic repetint, corregir posant-ho tot en un reactive o algo...
    param_list <- list() #per emmagatzemar eks paràmetres i els valors base
    for (p in parameters) {
      param_name <- p$name #agafem el name del paràmetre
      #si té estrat llavors s'enganxa l'estrat al final del nom del paràmetre
      if (!is.null(p$stratum)) {
        param_name <- paste0(p$name, "[", p$stratum, "]")
      }
      param_list[[param_name]] <- p$base.value #per cada nom se li associa el seu base.value
    }
    return(param_list) #tindrà tots els names dels params (inclosos els estratificats) amb els seus valors.
  })
  
  # CÀLCUL DE RESULTATS PELS PARÀMETRES:
  results <- eventReactive(input$run, {
    req(input$ref_strategy, input$alt_strategy, input$parameter_tree, model_funs$run.simulation)
    
    base_pars <- all_param_values() #variable que agafa la llista del reactive anterior (tots els paràmetres i els seus valors)
    
    # Vector amb els noms dels paràmetres seleccionats a l'arbre
    selected_tree <- get_selected(input$parameter_tree, format = "names") #s'obtenen en format llista els noms del que se selecciona amb l'arbre (get_selected funció de shinyTree)
    selected_param_names <- unlist(selected_tree) #convertir la llista anterior en vector simple dels noms 
    
    #càlcul % variació i associació de les estrategies del input a variables més senzilles:
    variation <- input$variation_percent / 100 
    ref <- input$ref_strategy
    alt <- input$alt_strategy
    
    #avís per si se seleccionen dues estrategies iguals.
    if (ref == alt) {
      showNotification("Reference strategy cannot be the same as the alternative strategy.", type = "error")
      return(NULL)
    }
    
    all_params <- model_funs$get.parameters() #!!!estic repetint de nou això, = que parameters, fer un reactive i fer-ho un sol cop per a tots.
    
    # llista aniudada per AGRUPAR tots els paràmetres del model pel nom: si un nom té diversos estrats hi haurà per cadascun la llista amb el param, base.value, estrat...
    grouped_params <- split(all_params, sapply(all_params, function(p) p$name))  # Agrupa els paràmetres pel seu nom (ignora estrats)
    
    results_list <- list()  # Inicialitza la llista per guardar els resultats
    
    for (pname in selected_param_names) {  # Itera per cada nom del vector de noms dels paràmetres seleccionats en l'arbre 
      param_group <- grouped_params[[pname]]  # param_group extreu el grup de paràmetres del nom iterat (1 si no té estrat, diversos si té estrat)
      if (is.null(param_group)) next  # Si no existeix el grup, passem al següent
      
      #Simulació pel % per sota
      pars_minus <- base_pars  # Copia dels valors base per modificar-los amb el -%
      
      for (p in param_group) {  # Itera per cada paràmetre (o grup de paràmetres si hi ha estrats)
        full_name <- pname  # Nom base
        if (!is.null(p$stratum)) {  # Si té estrat, s'afegeix al nom per poder-los diferenciar quan es passen al run.simulation (!!!POSSIBLE PROBLEMA perque el run.simulation pot no acceptar els noms)
          full_name <- paste0(pname, "[", p$stratum, "]")
        }
        pars_minus[[full_name]] <- p$base.value * (1 - variation)  # Aplica la variació (-%) al valor base
      }
      
      res_minus <- model_funs$run.simulation(c(ref, alt), pars_minus)$summary  # Executa simulació amb valors modificats
      res_minus$param <- pname  # Guarda el nom del paràmetre modificat
      res_minus$param.value <- round(param_group[[1]]$base.value * (1 - variation), 4)  #param_group[[1]] per mostrar el valor del primer de la llista a la taula
      
      #comprovacio#
      #print("pars_minus")
      #print(pars_minus)
      #print("param_group")
      #print(param_group)
      #####
      
      
      #Simulació pel % per dalt (igual que -%)
      pars_plus <- base_pars
      for (p in param_group) {
        full_name <- pname
        if (!is.null(p$stratum)) {
          full_name <- paste0(pname, "[", p$stratum, "]")
        }
        pars_plus[[full_name]] <- p$base.value * (1 + variation)
      }
      res_plus <- model_funs$run.simulation(c(ref, alt), pars_plus)$summary
      res_plus$param <- pname
      res_plus$param.value <- round(param_group[[1]]$base.value * (1 + variation), 4)
      
      results_list <- c(results_list, list(res_minus, res_plus))
    }
    
    do.call(rbind, results_list)
  })
  
  
  #es genera un dataframe selecionant només les files de estrategia alternativa i afegint columnes:
  df_alt <- reactive({
    taula_resultats <- results()
    req(taula_resultats)
    
    alt_rows <- seq(2, nrow(taula_resultats), by = 2)
    df_alt <- taula_resultats[alt_rows, ]
    ref_rows <- alt_rows - 1
    df_ref <- taula_resultats[ref_rows, ]
    
    # Càlculs de IC, IE, ICER, NHB
    df_alt$IC <- df_alt$C - df_ref$C
    df_alt$IE <- df_alt$E - df_ref$E 
    df_alt$ICER <- df_alt$IC / df_alt$IE
    df_alt$NHB <- (df_alt$IE - (df_alt$IC / input$wtp))
    
    df_alt
  })
  
  # Taula de resultats
  output$resultsTable <- renderTable({
    validate(
      need(input$ref_strategy != input$alt_strategy, 
           "Reference strategy cannot be the same as the alternative strategy."),
      need(!is.null(results()), "No results to display.")
    )
    df <- df_alt()
    
    # Formatejar números amb 4 decimals
    df[] <- lapply(df, function(col) {
      if (is.numeric(col)) sprintf("%.4f", col) else col
    })
    
    df
  })
  
  # Download dels resultatss
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("results_table_", input$selected_model, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(results(), file, row.names = FALSE)
    }
  )
  
  # Gràfic de resultats
  output$resultsPlot <- renderPlotly({
    validate(
      need(input$ref_strategy != input$alt_strategy, 
           "Reference strategy cannot be the same as the alternative strategy."),
      need(!is.null(results()), "No results to display.")
    )
    results_data <- df_alt()
    tornado_plot <- plot.tornado(results_data, WTP = input$wtp, use.nhb = (input$icer == "NHB"))
    ggplotly(tornado_plot)
  })
}

# Execució de l'aplicació
shinyApp(ui = ui, server = server)