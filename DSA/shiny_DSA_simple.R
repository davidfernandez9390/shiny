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
  titlePanel("DSA Cost-effectiveness simulation"),
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
    
    # Selecció dels paràmetres
    req(model_funs$get.parameters)
    parameters <- model_funs$get.parameters()
    stratified_params <- split(parameters, sapply(parameters, function(p) p$stratum %||% "General Parameters"))
    
    # Es generen els noms de les branques de l'arbre
    tree_list <- lapply(names(stratified_params), function(stratum) {
      params <- stratified_params[[stratum]]
      setNames(as.list(rep(NA, length(params))), sapply(params, function(p) p$name))
    })
    names(tree_list) <- names(stratified_params)
    
    #es mostra l'arbre en l'output
    output$parameter_tree <- renderTree({
      tree_list
    })
  })
  
  #Reactive (per ser usable fora) per seleccoinar els valors dels paràmetres del get.parameters
  selected_base_values <- reactive({
    req(model_funs$get.parameters)
    all_params <- model_funs$get.parameters()
    param_list <- list()
    for (p in all_params) {
      param_list[[p$name]] <- p$base.value
    }
    return(param_list)
  })
  
  #resultats del botó RUN:
  results <- eventReactive(input$run, {
    req(input$ref_strategy, input$alt_strategy, input$parameter_tree, model_funs$run.simulation)
    #nou nom per la llista de valors base dels paràmetres del reactive anterior
    base_pars <- selected_base_values()
    #selecció (i conversió a vector simple) dels noms dels paràmetres marcats en l'arbre
    selected_tree <- get_selected(input$parameter_tree, format = "names")
    selected_param_names <- unlist(selected_tree)
    
    variation <- input$variation_percent / 100 
    ref <- input$ref_strategy
    alt <- input$alt_strategy
    
    if (ref == alt) {
      showNotification("Reference strategy cannot be the same as the alternative strategy.", type = "error")
      return(NULL)
    }
    
    results_list <- list() #on s'emmagatzema el que fa el bucle for
    
    #es recorre el vector de noms (selected_param_names)
    for (pname in selected_param_names) {
      if (!pname %in% names(base_pars)) next #si el nom de selected_param_names està entre els noms dels params del get.parameters continua
      
      #valor base del nom del paràmetre
      base_value <- base_pars[[pname]]
      
      #Simulació pel % per sota
      pars_minus <- base_pars
      pars_minus[[pname]] <- base_value * (1 - variation) #canvi dels valors pel càlcul del % inferior
      res_minus <- model_funs$run.simulation(c(ref, alt), pars_minus)$summary #df amb el summary del run.simulation amb els nous valors
      res_minus$param <- pname 
      res_minus$param.value <- base_value * (1 - variation) #columna amb el valor després del % de variació
      
      #Simulació pel % per sobre
      pars_plus <- base_pars
      pars_plus[[pname]] <- base_value * (1 + variation)
      res_plus <- model_funs$run.simulation(c(ref, alt), pars_plus)$summary
      res_plus$param <- pname
      res_plus$param.value <- base_value * (1 + variation)
      
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