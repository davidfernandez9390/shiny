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

      #checkbox per seleccionar les diferents estrategies del get.strategies() des de la source:
      checkboxGroupInput("strategies", "Select strategies:",
                         choices = get.strategies(),
                         selected = get.strategies()),  # Totes seleccionades per defecte

      actionButton("select_all", "Select All"),      # Botó per seleccionar-les totes
      actionButton("deselect_all", "Deselect All"),  # Botó per desseleccionar-les totes

      lapply(get.parameters(), function(p) {
        probabilitats <- c("p.healthy.cancer", "p.healthy.death", "p.cancer.death",
                           "p.screening.effective", "p.treatment.effective", "discount")

        if (p$name %in% probabilitats) {
          max.value <- 1  # Per probabilitats, el màxim ha de ser 1
        } else if (!is.null(p$max.value)) {
          max.value <- p$max.value  # Si té un max.value ja definit en el model, l'assignem
        } else {
          max.value <- p$base.value * 4  # Si no té max.value, assignem base.value * 4
        }

        sliderInput(p$name, p$name, min = 0, max = max.value, value = p$base.value)
      }),

      actionButton("reset", "Reset to Default")     #Botó per fer reset dels paràmetres:
    ),

    mainPanel(
      tableOutput("resultsTable"),
      plotlyOutput("resultsPlot")
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

  # Execució del Botó de Reset dels paràmatres
  observeEvent(input$reset, {
    lapply(get.parameters(), function(p) {
      updateSliderInput(session, p$name, value = p$base.value)
    })
  })

  # Resultats en temps real amb reactive:
  results <- reactive({
    params <- lapply(get.parameters(), function(p) input[[p$name]])
    names(params) <- sapply(get.parameters(), function(p) p$name)
    run.simulation(input$strategies, params)  #executa la simulació amb estratègies i paràmetres
  })

  # Taula de resultats:
  output$resultsTable <- renderTable({
    # summary dels resultats generats en la simulació a temps real (bloc anterior)
    taula_resultats <- results()$summary
    # Anàlisis cost-efectivitat del CEAModel passant-li la taula de resultats anterior
    ce_analysis <- CEAModel::analyzeCE(taula_resultats, plot = TRUE)
    # Display del summary de la taula extesa (amb ICER):
    ce_analysis$summary
  })

  # Gràfic interactiu
  output$resultsPlot <- renderPlotly({
    taula_resultats <- results()$summary
    # Executa l'anàlisi de cost-efectivitat
    ce_analysis <- CEAModel::analyzeCE(taula_resultats, plot = TRUE)
    # Retorna el gràfic interactiu
    ggplotly(ce_analysis$plot)
  })
}

shinyApp(ui = ui, server = server)
