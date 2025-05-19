# Instal·lació i càrrega de paquets necessaris
list.of.packages <- c("shiny", "ggplot2", "plotly", "readxl", "writexl", "shinycssloaders", "git2r", "withr", "shinyTree")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

source('params.psa.R')
source('plots.psa.R')

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
      
      h4("Number of Simulations:", style = "color: #191970; font-weight: bold; font-family: Segoe UI;"),
      numericInput("n", label=NULL, value = 1000),
      
      h4("Parameter selection:"),
      #estructura en tree per seleccionar els paràmetres
      shinyTree::shinyTree("parameter_tree", checkbox = TRUE, theme = "proton", themeIcons = FALSE)
    ),
    
    mainPanel(
      actionButton("run", "Run PSA"),
      br(), br(),
      h4("Scatter Plot:"),
      plotlyOutput("scatterPlot"),
      br(),
      h4("Acceptability Plot:"),
      plotlyOutput("acceptPlot"),
      br(),
      h4("TABLE:"),
      br(),
      h4("Table of results:"),
      tableOutput("resultsTable"),
      downloadButton("downloadData", "Download")
    )
  )
)



#server
server <- function(input, output, session) {
  # Valors reactius: un tipus de llista que emmagatzemarà les funcions del model quan es carregui de forma reactiva:
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
    
    model_funs$get.parameters <- temp_env$get.parameters
    model_funs$get.strategies <- temp_env$get.strategies
    model_funs$run.simulation <- temp_env$run.simulation
    
    # Obtenir les estratègies de la funció del model
    returned_strategies <- model_funs$get.strategies()
    
    # Validar i adaptar el format de les estratègies retornades
    if (is.list(returned_strategies) && all(sapply(returned_strategies, is.character))) {
      # Si és una llista de vectors de caràcters (grups d'estratègies), aplanar-la
      model_funs$get.strategies <- function() {
        unlist(returned_strategies, use.names = FALSE)
      }
    } else if (is.character(returned_strategies) && length(returned_strategies) >= 1) {
      # Si ja és un vector de caràcters, mantenir-lo tal com està
      model_funs$get.strategies <- function() {
        returned_strategies
      }
    } else {
      # Cas invàlid: ni vector ni llista de vectors
      showNotification(
        "The function get.strategies() from the model is not valid. It must return either a character vector or a list of character vectors if strategies are grouped.",
        type = "error"
      )
      model_funs$get.strategies <- function() character(0)
    }
    
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
  
  # reactive per generar una llista de tots els noms de paràemtres, si hi ha paràmetre la clau d'un paràmetre te com a valor una llista amb valor per cada estrat.
  # aquesta estructura és idèntica a la que s'ha de passar al run.simulation()
  
  all_param_values <- reactive({
    req(model_funs$get.parameters)  
    parameters <- model_funs$get.parameters()  # Obtenim tots els paràmetres del model
    param_list <- list()  # Inicialitzem la llista on guardarem els valors base
    
    for (p in parameters) {  # Iterem sobre cada "entrada" del get.parameters()
      pname <- p$name  # Agafem el nom del paràmetre
      
      if (!is.null(p$stratum)) {  # Si el paràmetre està estratificat...
        if (is.null(param_list[[pname]])) param_list[[pname]] <- list()  # Creem subllista si no existeix
        param_list[[pname]][[p$stratum]] <- p$base.value  # Assignem valor base a l’estrat concret
      } else {
        param_list[[pname]] <- p$base.value  # Si no té estrats, assignem valor base directament
      }
    }
    
    return(param_list)  # Retornem la llista final amb valors base
  })
  
  
  # CÀLCUL DE RESULTATS PELS PARÀMETRES:
  results <- eventReactive(input$run, {
    print("##### RUN Starting simulation...")  # DEBUG
    #req(input$ref_strategy, input$alt_strategy, input$parameter_tree, model_funs$run.simulation)
    
    if (input$ref_strategy == input$alt_strategy) {
      showNotification("Reference strategy cannot be the same as the alternative strategy.", type = "error")
      return(NULL)
    }
    
    # Agafem tots els valors base des del reactive anterior.
    base_pars <- all_param_values()
    print("##### Base parameters carregats:")# DEBUG
    print(str(base_pars))  # DEBUG
    # Noms seleccionats de l'arbre (format pla)
    selected_tree <- get_selected(input$parameter_tree, format = "names") #retorna els valors seleccionats però en l'estructura complexa de l'arbre
    selected_param_names <- unlist(selected_tree) #amb unlist s'aplana l'estructura de l'arbre per obtenir els noms en vector de strings de noms
    print("##### Selected parameters del shinyTree:") #DEBUG
    print(selected_param_names)  # DEBUGç
  
    
    variation <- input$variation_percent / 100
    ref <- input$ref_strategy
    alt <- input$alt_strategy
    n_sim <- input$n #numero de simulacions definit en el numericInput
    
    # Bloc per agrupar els paràmetres pel seu nom
    all_params <- model_funs$get.parameters() #el get.parameters() sencer del shiny_interface.R
    grouped_params <- split(all_params, sapply(all_params, function(p) p$name)) #s'agrupen els paràmetres pel nom i a cada nom se li assigna una llista amb llistes per cada estrat
    
    results_list <- list()  # Per emmagatzemar els resultats
    
    # fer tants cicles com numero de simulacions
    for (i in seq_len(n_sim)) {
      print(paste("##### Iteracions:", i))  # DEBUG
      sim_pars <- base_pars #retorna tots els paràmetre del reactiu all_param_values()
      
      for (pname in selected_param_names) {
        param_group <- grouped_params[[pname]]
        if (is.null(param_group)) next
        
        for (p in param_group) {
          distribution <- p$distribution %||% "normal" # si un paràmetre no té element 'distribution' fer-li per defecte {distribution = 'normal'}
          fit_pars <- fit.parameter(distribution, p$base.value, variation)
          sampled_val <- sample.parameter(distribution, fit_pars, n = 1) #funció sample.parameter() del params.psa.R (n=1 per fer-ho un a un en cada cicle del bucle de les n simulacions)
          print(paste("Sampled", pname, if (!is.null(p$stratum)) paste0("[", p$stratum, "]"), ":", sampled_val))  # DEBUG PER MOSTRAR ELS VALORS DELS PARÀMETRES MOSTREJATS
          
          if (!is.null(p$stratum)) {
            if (is.null(sim_pars[[pname]])) sim_pars[[pname]] <- list()
            sim_pars[[pname]][[p$stratum]] <- sampled_val
          } else {
            sim_pars[[pname]] <- sampled_val
          }
        }
      }
      
      sim_result <- model_funs$run.simulation(c(ref, alt), sim_pars)$summary
      print("##### run.simulation result:") #DEBUG
      print(sim_result)#DEBUG
      sim_result$iteration <- i
      results_list[[i]] <- sim_result
    }
    
    results_df <- do.call(rbind, results_list)
    return(results_df)
  })
  
  
  #es genera un dataframe selecionant només les files de estrategia alternativa i afegint columnes (igual que el DSA):
  df_alt <- reactive({
    taula_resultats <- results()
    req(taula_resultats)
    
    alt_rows <- seq(2, nrow(taula_resultats), by = 2) #saltar de dos en dos per mostrar només les files de l'estratègia alternativa
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
  
  # Plot de dispersió:
  output$scatterPlot <- renderPlotly({
    req(df_alt())  # Ens assegurem que hi hagi resultats
    gg <- plot.scatter(df_alt(), wtp = input$wtp)
    ggplotly(gg)
  })
  
  # PLot d'acceptabilitat:
  output$acceptPlot <- renderPlotly({
    req(results())
    gg <- plot.acceptability(df_alt())
    ggplotly(gg)
  })
  
  
  # Taula de resultats (afegir columnes dels paràmetres seleccionats per mostrejar)
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
}

shinyApp(ui = ui, server = server)