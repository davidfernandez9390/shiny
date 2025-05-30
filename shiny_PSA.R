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


#_______________________________________________________________________________________________________________________________
#___________________________________________________________ UI ________________________________________________________________


ui <- fluidPage(
  titlePanel("Probabilistic Sensitivity Analysis"),
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
      h4("Scatter Plot:", style = "color: #191970; font-weight: bold; font-family: Segoe UI;"),
      plotlyOutput("scatterPlot"),
      br(),
      h4("Acceptability Plot:", style = "color: #191970; font-weight: bold; font-family: Segoe UI;"),
      plotlyOutput("acceptPlot"),
      br(),
      br(),
      h4("Table of results:", style = "color: #191970; font-weight: bold; font-family: Segoe UI;"),
      br(),
      downloadButton("downloadData", "Download"),
      br(),
      br(),
      tableOutput("resultsTable"), #COMENTAR AQUESTA LÍNIA PER NO MOSTRAR LA TAULA DE RESULTATS
      br(),
      br()
      
    )
  )
)

#_______________________________________________________________________________________________________________________________
#___________________________________________________________ SERVER ____________________________________________________________


server <- function(input, output, session) {
  # Valors reactius: un tipus de llista que emmagatzemarà les funcions del model quan es carregui de forma reactiva:
  model_funs <- reactiveValues(
    get.parameters = NULL, #ara estan buits a l'espera que quan es carrega un model en un entorn s'omplin amb les funcions del model
    get.strategies = NULL,
    run.simulation = NULL
  )
  
  # LOAD MODEL -> selecció del model i càrrega de paràmetres i estratègies
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
    
    model_funs$get.parameters <- temp_env$get.parameters #la funció de l'entorn la passem a l'objecte reactiveValues() model_funs
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
    
    if (input$ref_strategy == input$alt_strategy) {
      showNotification("Reference strategy cannot be the same as the alternative strategy.", type = "error")
      return(NULL)
    }
    
    base_pars <- all_param_values()
    print("##### Base parameters carregats:")
    print(str(base_pars))
    
    selected_tree <- get_selected(input$parameter_tree, format = "names")
    selected_param_names <- unlist(selected_tree)
    print("##### Selected parameters del shinyTree:")
    print(selected_param_names)
    
    variation <- input$variation_percent / 100
    ref <- input$ref_strategy
    alt <- input$alt_strategy
    n_sim <- input$n
    
    all_params <- model_funs$get.parameters()
    grouped_params <- split(all_params, sapply(all_params, function(p) p$name))
    
    results_list <- list()
    param_values_matrix <- list()  # aquí guardarem els valors mostrejats
    
    for (i in seq_len(n_sim)) {
      print(paste("##### Iteració:", i))
      sim_pars <- base_pars
      
      for (pname in selected_param_names) {
        param_group <- grouped_params[[pname]]
        if (is.null(param_group)) next
        
        for (p in param_group) {
          distribution <- p$distribution %||% "normal"
          fit_pars <- fit.parameter.psa(distribution, p$base.value, p$base.value / 5)
          sampled_val <- sample.parameter.psa(distribution, fit_pars, n = 1)
          
          if (!is.null(p$stratum)) {
            if (is.null(sim_pars[[pname]])) sim_pars[[pname]] <- list()
            sim_pars[[pname]][[p$stratum]] <- sampled_val
          } else {
            sim_pars[[pname]] <- sampled_val
          }
        }
      }
      
      sim_result <- model_funs$run.simulation(c(ref, alt), sim_pars)$summary
      sim_result$iteration <- i #columna per veure en quina iteració s'esta fent cada cosa 
      results_list[[i]] <- sim_result #per cada iteració s'emmagatzema en format llista cada taula summary del C i E
      
      # ---------------------
      # Guardem els valors mostrejats d’aquesta iteració
      sampled_params_iter <- list(iteration = i) # es genera una llista buida que té un primer element referent a la iteració del cicle
      
      #dintre dels paràmetres seleccionats:
      for (pname in selected_param_names) {
        param_group <- grouped_params[[pname]]
        if (is.null(param_group)) next
        
        for (p in param_group) { #s'usa el sim_pars que és la llista ja feta dels paràmetres mostrejats amb sample.parameter.psa()
          sampled_val <- if (!is.null(p$stratum)) sim_pars[[pname]][[p$stratum]] else sim_pars[[pname]] #extreure el valor mostrejat en aquest cicle
          sampled_name <- if (!is.null(p$stratum)) paste0(pname, "[", p$stratum, "]") else pname #per tenir els noms dels paràmetres estratificats
          sampled_params_iter[[sampled_name]] <- sampled_val #assignar el valor mostrejat a la llista afegint el nom anterior
        }
      }
      
      param_values_matrix[[i]] <- sampled_params_iter #ficar la llista sampled_params_iter a la posicio que toca de la llista de llistes param_values_matrix
      # ---------------------
    }
    
    results_df <- do.call(rbind, results_list) #results_df com en la versió anterior amb iteration, C, E, strategy
    params_df <- do.call(rbind, lapply(param_values_matrix, as.data.frame)) # es genera un data frame amb els valors dels paràmetres iterats amb una primera columna iteration
    results_df <- merge(results_df, params_df, by = "iteration") # s'ajunten les dues taules usant la columna iteration com a nexe d'unió entre els dataframes amb merge()
    
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
    #formats
    df$IC <- sprintf("%.4f", df$IC )
    df$IE <- sprintf("%.4f", df$IE)
    df$NHB <- sprintf("%.2f", df$NHB)
    df$ICER <- sprintf("%.0f", df$ICER)
    df$iteration <- sprintf("%.0f", df$iteration)
    
    # Formatejar números amb 4 decimals
    df[] <- lapply(df, function(col) {
      if (is.numeric(col)) sprintf("%.4f", col) else col
    })
    
   
    #fer servir relocate de dyplr per poder deixar un ordre concret de les primeres columnes 
    df <- df %>%
      relocate(iteration, C, E, IC, IE, ICER, NHB)
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