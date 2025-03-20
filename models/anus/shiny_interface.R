options(java.parameters = "-Xss256m")

setwd(model.wd)
library(here)

source('markov.R')
source('load_models.R')

get.strategies <- function() {
  return(names(strategies$hiv_msm))
}

get.parameters <- function() {
  indep.pars <- getIndependentParameters(paste0(here(), '/params/context.xlsx'))
  indep.pars <- indep.pars[!indep.pars %in% c('periodicity_times_in_year')]
  is.age.specific <- sapply(indep.pars, function(p) {
    par.values <- sapply(strat.ctx, function(ctx) ctx[[p]])
    return(!all(par.values==par.values[1]))
  })
  
  par.list <- list()
  for(p in indep.pars) {
    if (!is.age.specific[p]) {
      par.list <- append(par.list, list(
        list(
          name=p,
          base.value=strat.ctx[[1]][[p]]
        )
      ))
    } else {
      for(stratum in names(strat.ctx)) {
        par.list <- append(par.list, list(
          list(
            name=p,
            base.value=strat.ctx[[1]][[p]],
            stratum=stratum
          )
        ))
      }
    }
  }
  par.list <- append(par.list, list(
    list(
      name='discount',
      base.value=0.03
    ),
    list(
      name='starting.age',
      base.value=40
    ),
    list(
      name='max.age',
      base.value=80
    )
  ))
  return(par.list)
}

run.simulation <- function(strategies, pars) {
  # discount <- pars[[which(sapply(pars, function(p) p$name=='discount'))]]$base.value
  # starting.age <- pars[[which(sapply(pars, function(p) p$name=='starting.age'))]]$base.value
  # max.age <- pars[[which(sapply(pars, function(p) p$name=='max.age'))]]$base.value
  for(p in names(pars)) {
    if (p %in% names(strat.ctx[[1]])) {
      # if (is.null(p$stratum)) {
        strat.ctx <- lapply(strat.ctx, function(ctx) {
          ctx[[p]] <- pars[p]
          return(ctx)
        })
      # } else {
      #   strat.ctx[[p$stratum]][[p$name]] <- p$base.value
      # }
    }
  }
  
  strat.ctx <- refresh.context(c(), full.strat.ctx, excel.strata.df, context.setup)
  
  sim.strategies <- trees[names(trees) %in% strategies]
  initial.state <- sapply(markov$nodes,
                          function(n) if (n$name=='hiv_positive') 1 else 0)
  results <- simulate('hiv_msm',
                      sim.strategies,
                      markov,
                      strat.ctx,
                      initial.state,
                      start.age=pars$starting.age,
                      max.age=pars$max.age,
                      discount.rate=pars$discount)
  return(results)
}


