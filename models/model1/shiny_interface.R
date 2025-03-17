library(ggplot2)

source('model.R')

get.strategies <- function() {
  return(c('no_intervention', 'screening', 'treatment'))
}

get.parameters <- function() {
  return(list(
    list(
      name='p.healthy.cancer',
      base.value=0.01
    ),
    list(
      name='p.healthy.death',
      base.value=0.001
    ),
    list(
      name='p.cancer.death',
      base.value=0.3
    ),
    list(
      name='p.screening.effective',
      base.value=0.5
    ),
    list(
      name='p.treatment.effective',
      base.value=0.005
    ),
    list(
      name='cost.screening',
      base.value=100
    ),
    list(
      name='cost.cancer.treatment',
      base.value=10000,
      max.value=50000
    ),
    list(
      name='utility.cancer',
      base.value=0.6
    ),
    list(
      name='simulated.years',
      base.value=20
    ),
    list(
      name='discount',
      base.value=0.0
    )
  ))
}

run.simulation <- function(strategies, pars) {
  results <- simulate(strategies,
                     p.healthy.cancer=pars[['p.healthy.cancer']],
                     p.healthy.death=pars[['p.healthy.death']],
                     p.cancer.death=pars[['p.cancer.death']],
                     p.screening.effective=pars[['p.screening.effective']],
                     p.treatment.effective=pars[['p.treatment.effective']],
                     cost.screening=pars[['cost.screening']],
                     cost.cancer.treatment=pars[['cost.cancer.treatment']],
                     utility.cancer=pars[['utility.cancer']],
                     simulated.years=pars[['simulated.years']],
                     discount=pars[['discount']])
  return(results)
}




# ### TEST
#
# strategies <- get.strategies()
# param.info <- get.parameters()
# param.values <- sapply(param.info, function(p) p$base.value)
# names(param.values) <- sapply(param.info, function(p) p$name)
#
# results <- run.simulation(strategies, param.values)
# print(results$summary)
#
# print(
#   ggplot(results$summary, aes(x=C, y=E, color=strategy)) +
#     geom_point(size=3) +
#     coord_cartesian(xlim=c(0, max(results$summary$C)), ylim=c(0, 20)) +
#     theme_minimal()
# )

