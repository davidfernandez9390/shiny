library(doParallel)
library(pbapply)

source('load_models.R')
source('markov.R')
# source('utils.R')

WTP <- 20000
DISCOUNT.RATE <- .03
EXPERIMENTS <- list(
  c(type='conventional', target=conventional_hpv16la, reference=conventional),
  c(type='ascus_diff', target=ascus_lsil_diff_hpv16la, reference=conventional),
  c(type='conventional_t', target=conventional_hpv16la_t, reference=conventional_t),
  c(type='ascus_diff_t', target=ascus_lsil_diff_hpv16la_t, reference=conventional_t)
)

do.parameter.sweep <- function(strat.ctx, 
                               excel.strata.df,
                               parameters, 
                               values, 
                               markov, 
                               cohort,  
                               reference,
                               target,
                               n.cores=7) {
  
  initial.state <- sapply(markov$nodes,
                          function(n) if (n$name=='hiv_positive') 1 else 0)
  
  cl <- makeCluster(n.cores)
  registerDoParallel(cl)
  clusterExport(cl=cl,varlist=ls(pos=1),
                envir=environment())
  clusterEvalQ(cl=cl,{
    library(data.table)
    library(stringr)
    library(ggplot2)
    library(plotly)
  })
  
  sweep.df <- pbapply(cl=cl, MARGIN=1, X=values, FUN=function(val) {
  # sweep <- apply(values, 1, function(val) {
    strat.ctx.sweep <- lapply(strat.ctx, function(ctx) {
      ctx[parameters] <- val
      ctx
    })
    strat.ctx.sweep <- refresh.context(parameters, strat.ctx.sweep, excel.strata.df, context.setup)
    results <- simulate(cohort,
                        list(hiv_msm=target),
                        markov,
                        strat.ctx.sweep,
                        initial.state,
                        start.age=DEFAULT.START.AGE,
                        max.age=DEFAULT.MAX.AGE)$summary
    data.frame(sensitivity=val[1], specificity=1-val[2], C=results$C, E=results$E)
  }) %>% bind_rows()
  
  stopCluster(cl)
  
  print(plt)
  return(sweep.df)
}


sens <- seq(0,1,.05)
spec_c <- seq(0,1,.05)
values <- expand.grid(sens, spec_c)

sweep.df <- data.frame()
for(experiment in EXPERIMENTS) {
  current.sweep.df <- do.parameter.sweep(strat.ctx=strat.ctx,
                     excel.strata.df=excel.strata.df,
                     parameters=c('p_hpv16la_p___hsil', 'p_hpv16la_p___no_hsil'),
                     cohort='hiv_msm',
                     reference=experiment$reference,
                     target=experiment$target,
                     values=values,
                     markov=markov,
                     n.cores=1)
  current.sweep.df$type <- experiment$type
  sweep.df <- rbind(sweep.df, current.sweep.df)
}

ref.results <- simulate('hiv_msm',
                    list(hiv_msm=conventional),
                    markov,
                    strat.ctx,
                    initial.state,
                    start.age=DEFAULT.START.AGE,
                    max.age=DEFAULT.MAX.AGE)$summary
ref.results$type <- 'reference'

ref.results.t <- simulate('hiv_msm',
                        list(hiv_msm=conventional_t),
                        markov,
                        strat.ctx,
                        initial.state,
                        start.age=DEFAULT.START.AGE,
                        max.age=DEFAULT.MAX.AGE)$summary
ref.results.t$type <- 'reference_t'

sweep2.df <- sweep.df[(sweep.df$sensitivity == 0 & sweep.df$specificity == 0) |
                        (sweep.df$sensitivity == 0 & sweep.df$specificity == 1) |
                        (sweep.df$sensitivity == 1 & sweep.df$specificity == 0) |
                        (sweep.df$sensitivity == 1 & sweep.df$specificity == 1),]
sweep2.df <- sweep2.df[c(1,3,4,2,5,7,8,6,9,11,12,10,13,15,16,14),]

# plt <- ggplot(sweep.df, aes(x=sensitivity, y=specificity, fill=ICER)) + 
#   geom_tile() +
#   scale_fill_gradient2(midpoint=30000)
# print(plt)

plt.df <- rbind(sweep2.df[,c('C', 'E', 'type')])
plt <- ggplot(plt.df, aes(x=C, y=E)) + 
  geom_polygon(aes(fill=type), alpha=.3) +
  geom_point(data=ref.results, aes(x=C,y=E,color=type), size=3) +
  geom_point(data=ref.results.t, aes(x=C,y=E,color=type), size=3) + 
  scale_fill_discrete(name='Strategy type', labels=c('ASCUS diff', 'ASCUS diff [treatment]', 'Cyto + test', 'Cyto + test [treatment]')) +
  scale_color_manual(name='Reference strategies', labels=c('Cyto', 'Cyto [treatment]'), values=c('black', '#00dd00')) +
  ggtitle('Areas for all combinations of sensitivity/specificity (test cost = 25€)')
plt

