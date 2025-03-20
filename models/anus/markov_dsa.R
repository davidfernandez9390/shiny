library(ggplot2)
library(plotly)
library(xlsx)
library(htmlwidgets)
library(doParallel)
library(pbapply)
library(CEAModel)
library(scales)

source('markov.R')
source('excel_params.R')

N.POINTS.DEFAULT <- 5
WTP.THRESHOLDS <- c(5000, 10000, 20000, 50000)
COLOR.PALETTE <- c('#2a9d8f', '#e9c46a', '#f4a261', '#e76f51', '#ff0000')
DEFAULT.DISCOUNT <- .05
PRESERVE.TORNADO.ORDER.NHB.ICER <- TRUE

dsa.n <- function(pars,
                  strat.ctx,
                  excel.file,
                  population,
                  strategy,
                  markov,
                  discount.rate=DEFAULT.DISCOUNT,
                  initial.state=NULL,
                  range.estimate.func=NULL,
                  context.setup.func=NULL,
                  n.param.points=N.POINTS.DEFAULT,
                  n.cores=NULL,
                  cluster=NULL,
                  sample.by.stratum=FALSE) {
  if (!all(pars %in% names(strat.ctx[[1]])))
    stop(paste0("Some parameters don't exist in the context: ",
                paste0('"', pars[!pars %in% names(strat.ctx[[1]])], '"', collapse = ', ')))
  pars <- pars[order(pars)]
  if (is.null(range.estimate.func)) {
    range.estimate.func <- function(par, val) {
      if (any(startsWith(par, c('p_', '.p_', '.sensitivity_', '.specificity_', '.survival_', '.u_')))) {
        return(c(val*.25, min(1, val*1.75)))
      } else if (any(startsWith(par, c('c_', '.c_',  'ly_', '.ly_', '.hr_', '.age_', 'n_')))) {
        return(c(val*.25, val*1.75))
      }
    }
  }

  cat('****************************************\n')
  cat(paste0('Starting DSA for ', strategy, '\n'))

  results <- data.frame()
  simulation.strategies <- strategies[[population]]
  strategy.names <- sapply(strategies[[population]], function(s) s$name)
  simulation.strategies <- simulation.strategies[strategy.names %in% c(reference, strategy)]
  if (is.null(initial.state)) {
    initial.state <- ifelse(seq_along(markov$nodes)==1, 1, 0)
    names(initial.state) <- sapply(markov$nodes, function(n) n$name)
  }
  if (is.null(n.cores)) n.cores <- detectCores()
  cat(paste0('* ', n.cores, ' core(s)\n'))
  cat(paste0('* ', n.param.points, ' points per parameter\n'))
  cat(paste0('* ', discount.rate, ' discount rate\n'))
  cat(paste0('* Sample by stratum: ', sample.by.stratum, '\n'))
  cat(paste0('* Range estimate function:\n'))
  print(range.estimate.func)
  cat('* Parameters included:\n')
  cat(paste0('- ', paste0(pars, collapse = ', '), '\n'))
  cat('\n')
  cat('Simulating base case...\n')

  if (!is.null(context.setup.func))
    strat.ctx <- context.setup.func(strat.ctx)
  base.output <- simulate(population,
                          simulation.strategies,
                          markov,
                          strat.ctx,
                          initial.state,
                          start.age=DEFAULT.START.AGE[[population]],
                          max.age=DEFAULT.MAX.AGE[[population]],
                          discount.rate=discount.rate)
  base.result <- base.output$summary
  base.ref <- base.result[base.result$strategy == reference,]
  base.strat <- base.result[base.result$strategy == strategy,]
  IC <- base.strat$C - base.ref$C
  IE <- base.strat$E - base.ref$E
  cat(paste0('Base ICER=', formatC(IC/IE, big.mark = ',', digits = 0, format = 'd'), ' €/QALY\n'))
  named.strata.ctx <- lapply(names(strat.ctx),
                             function(stratum) {
                               names(strat.ctx[[stratum]]) <- paste0(stratum,
                                                                     '_',
                                                                     names(strat.ctx[[stratum]]));
                               # Only returning the first value for each parameter
                               return(lapply(strat.ctx[[stratum]], function(p)p[1]))
                             })
  named.strata.ctx <- Reduce(function(x,y) merge(x, y, all=TRUE), named.strata.ctx)
  iter.result.base <- cbind(data.frame(
    iter=-1,
    strategy=strategy,
    C=base.strat$C,
    IC=IC,
    E=base.strat$E,
    IE=IE,
    ICER=IC/IE,
    n_cancers=mean(base.output$info[[strategy]]$additional.info$n_cancers),
    n_new_deaths_cancer=mean(base.output$info[[strategy]]$additional.info$n_new_deaths_cancer),
    n_new_detected_false_hsils=mean(base.output$info[[strategy]]$additional.info$n_new_detected_false_hsils),
    n_new_detected_true_hsils=mean(base.output$info[[strategy]]$additional.info$n_new_detected_true_hsils),
    n_new_undetected_hsils=mean(base.output$info[[strategy]]$additional.info$n_new_undetected_hsils),
    n_new_surgeries_no_cancer=mean(base.output$info[[strategy]]$additional.info$n_new_surgeries_no_cancer),
    n_new_semestral_followup_hsils=mean(base.output$info[[strategy]]$additional.info$n_new_semestral_followup_hsils),
    n_new_semestral_followup_no_hsils=mean(base.output$info[[strategy]]$additional.info$n_new_semestral_followup_no_hsils),
    n_cyto=mean(base.output$info[[strategy]]$additional.info$n_cyto),
    n_hpv=mean(base.output$info[[strategy]]$additional.info$n_hpv),
    n_hra_no_hsil=mean(base.output$info[[strategy]]$additional.info$n_hra_no_hsil),
    n_hra_hsil=mean(base.output$info[[strategy]]$additional.info$n_hra_hsil),
    n_treatment_no_hsil=mean(base.output$info[[strategy]]$additional.info$n_treatment_no_hsil),
    n_treatment_hsil=mean(base.output$info[[strategy]]$additional.info$n_treatment_hsil)
  ), named.strata.ctx)
  results <- rbind(results, iter.result.base)

  # excel.strata.df <- list()
  # strata <- names(xlsx::getSheets(xlsx::loadWorkbook(excel.file)))
  # cat('Reading parameters from excel data...\n')
  # for(stratum in strata) {
  #   excel.strata.df[[stratum]] <- read.xlsx(excel.file, sheetName = stratum, keepFormulas = T)[,c(1,2)]
  # }

  cat('******************\n')
  cat(paste0('Simulating DSA for ', paste0(pars, collapse=', '), '\n'))
  p.results <- .dsa.n(pars,
                      strat.ctx,
                      excel.strata.df,
                      population,
                      strategy,
                      markov,
                      discount.rate,
                      initial.state,
                      range.estimate.func=range.estimate.func,
                      context.setup.func=context.setup.func,
                      n.param.points=n.param.points,
                      cluster=cluster,
                      sample.by.stratum=sample.by.stratum,
                      keep.all.strategies=keep.all.strategies)

  results <- rbind(results, p.results)

  results$CE_THRESHOLD <- Inf
  for(wtp in rev(WTP.THRESHOLDS)) {
    results$CE_THRESHOLD <- ifelse(results$IE >= results$IC / wtp, wtp, results$CE_THRESHOLD)
  }
  results$CE_THRESHOLD <- factor(results$CE_THRESHOLD, levels=c(WTP.THRESHOLDS, Inf))

  cat('DSA Done\n')
  cat('****************************************\n')

  plt <- NULL
  plt2 <- NULL
  if (length(pars) == 1) {
    param.display.names <- lapply(full.strat.metadata[[1]], function(r) r$display_name)
    ret <- plot.tornado(results, population, reference=reference, param.display.names = param.display.names, WTP = 25000)
    plt <- ret[[1]]
    plt2 <- ret[[2]]
    param.order <- NULL

    if (PRESERVE.TORNADO.ORDER.NHB.ICER) {
      param.order <- ret[[3]]
    }

    ret <- plot.tornado.nhb(results, population, reference=reference, param.order=param.order, param.display.names = param.display.names, WTP = 25000)
    plt.nhb <- ret[[1]]
    plt2.nhb <- ret[[2]]
    plts <- list(plt, plt2, plt.nhb, plt2.nhb)
  } else if (length(pars) == 2) {
    plt <- plot.heatmap(pars, results, population, reference=reference)
    plts <- list(plt)
  } else {
    plt <- plot.scatter(results, population, reference=reference)
    plts <- list(plt)
  }

  if (length(pars) == 1) plot.curves <- plot.curves(results, pars)
  else plot.curves <- NULL

  return(list(
    summary=results,
    plots=plts,
    plot.curves=plot.curves
  ))
}

dsa.1 <- function(pars,
                strat.ctx,
                excel.file,
                population,
                strategy,
                markov,
                reference,
                discount.rate=DEFAULT.DISCOUNT,
                initial.state=NULL,
                range.estimate.func=NULL,
                context.setup.func=NULL,
                n.param.points=N.POINTS.DEFAULT,
                n.cores=1,
                cluster=NULL,
                sample.by.stratum=FALSE,
                keep.all.strategies=FALSE) {
  if (!all(pars %in% names(strat.ctx[[1]])))
    stop(paste0("Some parameters don't exist in the context: ",
                paste0('"', pars[!pars %in% names(strat.ctx[[1]])], '"', collapse = ', ')))

  # Forcing even number of points
  if (n.param.points %% 2 == 1)
    n.param.points <- n.param.points + 1

  pars <- pars[order(pars)]
  if (is.null(range.estimate.func)) {
    range.estimate.func <- function(par, val) {
      if (any(startsWith(par, c('p_', '.p_', '.sensitivity_', '.specificity_', '.survival_', '.u_')))) {
        return(c(val*.25, min(1, val*1.75)))
      } else if (any(startsWith(par, c('c_', '.c_',  'ly_', '.ly_', '.hr_', '.age_', 'n_')))) {
        return(c(val*.25, val*1.75))
      }
    }
  }

  results <- data.frame()

  simulation.strategies <- strategies[[population]]
  strategy.names <- sapply(strategies[[population]], function(s) s$name)
  simulation.strategies <- simulation.strategies[strategy.names %in% c(reference, strategy)]
  if (is.null(initial.state)) {
    initial.state <- ifelse(seq_along(markov$nodes)==1, 1, 0)
    names(initial.state) <- sapply(markov$nodes, function(n) n$name)
  }
  if (is.null(n.cores)) n.cores <- detectCores()
  cat('****************************************\n')
  cat(paste0('Starting DSA for ', strategy, '\n'))
  cat(paste0('* ', n.cores, ' core(s)\n'))
  cat(paste0('* ', n.param.points, ' points per parameter\n'))
  cat(paste0('* ', discount.rate, ' discount rate\n'))
  cat(paste0('* Sample by stratum: ', sample.by.stratum, '\n'))
  cat(paste0('* Range estimate function:\n'))
  print(range.estimate.func)
  cat('* Parameters included:\n')
  cat(paste0('- ', paste0(pars, collapse = ', '), '\n'))
  cat('\n')
  cat('Simulating base case...\n')

  if (!is.null(context.setup.func))
    strat.ctx <- context.setup.func(strat.ctx)

  base.output <- simulate(population,
                          simulation.strategies,
                          markov,
                          strat.ctx,
                          initial.state,
                          start.age=DEFAULT.START.AGE[[population]],
                          max.age=DEFAULT.MAX.AGE[[population]],
                          discount.rate=discount.rate)
  base.result <- base.output$summary

  base.ref <- base.result[startsWith(base.result$strategy, reference),]
  base.strat <- base.result[startsWith(base.result$strategy, strategy),]
  IC <- base.strat$C - base.ref$C
  IE <- base.strat$E - base.ref$E
  cat(paste0('Base ICER=', formatC(IC/IE, big.mark = ',', digits = 0, format = 'd'), ' €/QALY\n'))
  named.strata.ctx <- lapply(names(strat.ctx),
                             function(stratum) {
                               names(strat.ctx[[stratum]]) <- paste0(stratum,
                                                                         '_',
                                                                         names(strat.ctx[[stratum]]));
                               # Only returning the first value for each parameter
                               return(lapply(strat.ctx[[stratum]], function(p)p[1]))
                             })
  named.strata.ctx <- Reduce(function(x,y) merge(x, y, all=TRUE), named.strata.ctx)
  if (keep.all.strategies) {
    for(stg in base.result$strategy) {
      IC <- base.result[base.result$strategy == stg,]$C - base.ref$C
      IE <- base.result[base.result$strategy == stg,]$E - base.ref$E
      iter.result.base <- cbind(data.frame(
        iter=-1,
        strategy=stg,
        C=base.result[base.result$strategy == stg,]$C,
        IC=IC,
        E=base.result[base.result$strategy == stg,]$E,
        IE=IE,
        ICER=IC/IE,
        n_cancers=mean(base.output$info[[stg]]$additional.info$n_cancers),
        n_new_deaths_cancer=mean(base.output$info[[stg]]$additional.info$n_new_deaths_cancer),
        n_new_detected_false_hsils=mean(base.output$info[[stg]]$additional.info$n_new_detected_false_hsils),
        n_new_detected_true_hsils=mean(base.output$info[[stg]]$additional.info$n_new_detected_true_hsils),
        n_new_undetected_hsils=mean(base.output$info[[stg]]$additional.info$n_new_undetected_hsils),
        n_new_surgeries_no_cancer=mean(base.output$info[[stg]]$additional.info$n_new_surgeries_no_cancer),
        n_new_semestral_followup_hsils=mean(base.output$info[[stg]]$additional.info$n_new_semestral_followup_hsils),
        n_new_semestral_followup_no_hsils=mean(base.output$info[[stg]]$additional.info$n_new_semestral_followup_no_hsils),
        n_cyto=mean(base.output$info[[stg]]$additional.info$n_cyto),
        n_hpv=mean(base.output$info[[stg]]$additional.info$n_hpv),
        n_hra_no_hsil=mean(base.output$info[[stg]]$additional.info$n_hra_no_hsil),
        n_hra_hsil=mean(base.output$info[[stg]]$additional.info$n_hra_hsil),
        n_treatment_no_hsil=mean(base.output$info[[stg]]$additional.info$n_treatment_no_hsil),
        n_treatment_hsil=mean(base.output$info[[stg]]$additional.info$n_treatment_hsil)
      ), named.strata.ctx)
      iter.result.base$param <- NA
      results <- rbind(results, iter.result.base)
    }
  } else {
    iter.result.base <- cbind(data.frame(
      iter=-1,
      strategy=strategy,
      C=base.strat$C,
      IC=IC,
      E=base.strat$E,
      IE=IE,
      ICER=IC/IE,
      n_cancers=mean(base.output$info[[strategy]]$additional.info$n_cancers),
      n_new_deaths_cancer=mean(base.output$info[[strategy]]$additional.info$n_new_deaths_cancer),
      n_new_detected_false_hsils=mean(base.output$info[[strategy]]$additional.info$n_new_detected_false_hsils),
      n_new_detected_true_hsils=mean(base.output$info[[strategy]]$additional.info$n_new_detected_true_hsils),
      n_new_undetected_hsils=mean(base.output$info[[strategy]]$additional.info$n_new_undetected_hsils),
      n_new_surgeries_no_cancer=mean(base.output$info[[strategy]]$additional.info$n_new_surgeries_no_cancer),
      n_new_semestral_followup_hsils=mean(base.output$info[[strategy]]$additional.info$n_new_semestral_followup_hsils),
      n_new_semestral_followup_no_hsils=mean(base.output$info[[strategy]]$additional.info$n_new_semestral_followup_no_hsils),
      n_cyto=mean(base.output$info[[strategy]]$additional.info$n_cyto),
      n_hpv=mean(base.output$info[[strategy]]$additional.info$n_hpv),
      n_hra_no_hsil=mean(base.output$info[[strategy]]$additional.info$n_hra_no_hsil),
      n_hra_hsil=mean(base.output$info[[strategy]]$additional.info$n_hra_hsil),
      n_treatment_no_hsil=mean(base.output$info[[strategy]]$additional.info$n_treatment_no_hsil),
      n_treatment_hsil=mean(base.output$info[[strategy]]$additional.info$n_treatment_hsil)
    ), named.strata.ctx)
    iter.result.base$param <- NA
    results <- rbind(results, iter.result.base)
  }

  # excel.strata.df <- list()
  # strata <- names(xlsx::getSheets(xlsx::loadWorkbook(excel.file)))
  # cat('Reading parameters from excel data...\n')
  # for(stratum in strata) {
  #   excel.strata.df[[stratum]] <- read.xlsx(excel.file, sheetName = stratum, keepFormulas = T)[,c(1,2)]
  # }

  for(p in pars) {
    cat('******************\n')
    cat(paste0('Simulating DSA for ', p, '\n'))
    excel.strata.df.copy <- lapply(excel.strata.df, function(df) {
      return(data.frame(df))
    })

    p.results <- .dsa.n(p,
                       strat.ctx,
                       excel.strata.df.copy,
                       population,
                       strategy,
                       markov,
                       reference=reference,
                       discount.rate=discount.rate,
                       initial.state=initial.state,
                       range.estimate.func=range.estimate.func,
                       context.setup.func=context.setup.func,
                       n.param.points=n.param.points,
                       cluster=cluster,
                       sample.by.stratum=sample.by.stratum,
                       keep.all.strategies=keep.all.strategies)
    p.results$param <- p
    results <- rbind(results, p.results)
    p.results <- NULL
  }
  cat('DSA Done\n')
  cat('****************************************\n')
  # return(list(
  #   summary=results,
  #   plot=plot.tornado(results[results$strategy==strategy,], population, reference=reference),
  #   plot.curves=plot.curves(results[results$strategy==strategy,], pars)
  #   ))

  param.display.names <- lapply(full.strat.metadata[[1]], function(r) r$display_name)

  ret <- plot.tornado(results[results$strategy==strategy,], population, reference=reference, param.display.names=param.display.names, WTP = 25000)
  plt <- ret[[1]]
  plt2 <- ret[[2]]
  param.order <- NULL

  if (PRESERVE.TORNADO.ORDER.NHB.ICER) {
    param.order <- ret[[3]]
  }

  ret <- plot.tornado.nhb(results[results$strategy==strategy,], population, reference=reference, param.order=param.order, param.display.names=param.display.names, WTP = 25000)
  plt.nhb <- ret[[1]]
  plt2.nhb <- ret[[2]]
  plts <- list(plt, plt2, plt.nhb, plt2.nhb)

  plot.curves <- plot.curves(results[results$strategy==strategy,], pars)

  return(list(
    summary=results,
    plots=plts,
    plot.curves=plot.curves
  ))
}

.dsa.n <- function(pars,
                strat.ctx,
                excel.strata.df,
                population,
                strategy,
                markov,
                reference=NULL,
                discount.rate=DEFAULT.DISCOUNT,
                initial.state=NULL,
                range.estimate.func=NULL,
                context.setup.func=NULL,
                n.param.points=4,
                cluster=NULL,
                sample.by.stratum=FALSE,
                keep.all.strategies=FALSE) {
  cl <- cluster
  if (is.null(cluster)) pboptions(type='none')
  else pboptions(type='timer')

  if (is.null(range.estimate.func)) {
    range.estimate.func <- function(par, val) {
      if (any(startsWith(par, c('p_', '.p_', '.sensitivity_', '.specificity_', '.survival_', '.u_')))) {
        return(c(val*.25, min(1, val*1.75)))
      } else if (any(startsWith(par, c('c_', '.c_',  'ly_', '.ly_', '.hr_', '.age_', 'n_')))) {
        return(c(val*.25, val*1.75))
      }
    }
  }
  if (is.null(reference)) {
    reference <- POPULATION.REFERENCES[[population]]
  }

  simulation.strategies <- strategies[[population]]
  strategy.names <- sapply(strategies[[population]], function(s) s$name)
  simulation.strategies <- simulation.strategies[strategy.names %in% c(reference, strategy)]
  cat('Fitting distributions for parameters...\n')
  strat.dist.params <- .fit.dsa.params(strat.ctx, range.estimate.func)
  cat(paste0('Simulating ', ifelse(length(pars)==1, n.param.points, paste0(n.param.points, '^', length(pars))), ' DSA points for parameter(s) ', paste0(pars, collapse=', '), '...\n'))
  param.grid <- do.call('expand.grid', lapply(pars, function(x)seq(0,n.param.points-1)))

  results <- pblapply(cl=cl, X=seq(nrow(param.grid)), FUN=function(i) {
    intervals <- as.numeric(param.grid[i,])
    dsa.strat.ctx <- sample.dsa.params(pars,
                                        intervals,
                                        n.param.points,
                                        strat.dist.params,
                                        strat.ctx,
                                        excel.strata.df = excel.strata.df,
                                        sample.by.stratum=sample.by.stratum)
    if (!is.null(context.setup.func))
      dsa.strat.ctx <- context.setup.func(dsa.strat.ctx)

    if (is.null(initial.state)) {
      initial.state <- ifelse(seq_along(markov$nodes)==1, 1, 0)
      names(initial.state) <- sapply(markov$nodes, function(n) n$name)
    }

    dsa.output <- simulate(population,
                           simulation.strategies,
                           markov,
                           dsa.strat.ctx,
                           initial.state,
                           start.age=DEFAULT.START.AGE[[population]],
                           max.age=DEFAULT.MAX.AGE[[population]],
                           discount.rate=discount.rate)
    dsa.result <- dsa.output$summary
    if (keep.all.strategies) {
      iter.result <- data.frame()
      for(stg in dsa.result$strategy) {
        dsa.ref <- dsa.result[startsWith(dsa.result$strategy, reference),]
        dsa.strat <- dsa.result[startsWith(dsa.result$strategy, stg),]
        IC <- dsa.strat$C - dsa.ref$C
        IE <- dsa.strat$E - dsa.ref$E
        named.strata.ctx <- lapply(names(dsa.strat.ctx),
                                   function(stratum) {
                                     names(dsa.strat.ctx[[stratum]]) <- paste0(stratum,
                                                                               '_',
                                                                               names(dsa.strat.ctx[[stratum]]));
                                     dsa.strat.ctx[[stratum]]
                                   })
        named.strata.ctx <- lapply(named.strata.ctx, function(x) lapply(x, function(y)y[1]))  # FIX: Why is this necessary?
        named.strata.ctx <- Reduce(function(x,y) merge(x, y, all=TRUE), named.strata.ctx)
        iter.result <- rbind(iter.result,
          cbind(data.frame(
            iter=i,
            strategy=stg,
            C=dsa.strat$C,
            IC=IC,
            E=dsa.strat$E,
            IE=IE,
            ICER=IC/IE,
            n_cancers=mean(dsa.output$info[[strategy]]$additional.info$n_cancers),
            n_new_deaths_cancer=mean(dsa.output$info[[strategy]]$additional.info$n_new_deaths_cancer),
            n_new_detected_false_hsils=mean(dsa.output$info[[strategy]]$additional.info$n_new_detected_false_hsils),
            n_new_detected_true_hsils=mean(dsa.output$info[[strategy]]$additional.info$n_new_detected_true_hsils),
            n_new_undetected_hsils=mean(dsa.output$info[[strategy]]$additional.info$n_new_undetected_hsils),
            n_new_surgeries_no_cancer=mean(dsa.output$info[[strategy]]$additional.info$n_new_surgeries_no_cancer),
            n_new_semestral_followup_hsils=mean(dsa.output$info[[strategy]]$additional.info$n_new_semestral_followup_hsils),
            n_new_semestral_followup_no_hsils=mean(dsa.output$info[[strategy]]$additional.info$n_new_semestral_followup_no_hsils),
            n_cyto=mean(dsa.output$info[[strategy]]$additional.info$n_cyto),
            n_hpv=mean(dsa.output$info[[strategy]]$additional.info$n_hpv),
            n_hra_no_hsil=mean(dsa.output$info[[strategy]]$additional.info$n_hra_no_hsil),
            n_hra_hsil=mean(dsa.output$info[[strategy]]$additional.info$n_hra_hsil),
            n_treatment_no_hsil=mean(dsa.output$info[[strategy]]$additional.info$n_treatment_no_hsil),
            n_treatment_hsil=mean(dsa.output$info[[strategy]]$additional.info$n_treatment_hsil)
          ), named.strata.ctx)
        )
      }
    } else {
      dsa.ref <- dsa.result[startsWith(dsa.result$strategy, reference),]
      dsa.strat <- dsa.result[startsWith(dsa.result$strategy, strategy),]
      IC <- dsa.strat$C - dsa.ref$C
      IE <- dsa.strat$E - dsa.ref$E
      named.strata.ctx <- lapply(names(dsa.strat.ctx),
                                 function(stratum) {
                                   names(dsa.strat.ctx[[stratum]]) <- paste0(stratum,
                                                                             '_',
                                                                             names(dsa.strat.ctx[[stratum]]));
                                   dsa.strat.ctx[[stratum]]
                                 })
      named.strata.ctx <- lapply(named.strata.ctx, function(x) lapply(x, function(y)y[1]))  # FIX: Why is this necessary?
      named.strata.ctx <- Reduce(function(x,y) merge(x, y, all=TRUE), named.strata.ctx)
      iter.result <- cbind(data.frame(
        iter=i,
        strategy=strategy,
        C=dsa.strat$C,
        IC=IC,
        E=dsa.strat$E,
        IE=IE,
        ICER=IC/IE,
        n_cancers=mean(dsa.output$info[[strategy]]$additional.info$n_cancers),
        n_new_deaths_cancer=mean(dsa.output$info[[strategy]]$additional.info$n_new_deaths_cancer),
        n_new_detected_false_hsils=mean(dsa.output$info[[strategy]]$additional.info$n_new_detected_false_hsils),
        n_new_detected_true_hsils=mean(dsa.output$info[[strategy]]$additional.info$n_new_detected_true_hsils),
        n_new_undetected_hsils=mean(dsa.output$info[[strategy]]$additional.info$n_new_undetected_hsils),
        n_new_surgeries_no_cancer=mean(dsa.output$info[[strategy]]$additional.info$n_new_surgeries_no_cancer),
        n_new_semestral_followup_hsils=mean(dsa.output$info[[strategy]]$additional.info$n_new_semestral_followup_hsils),
        n_new_semestral_followup_no_hsils=mean(dsa.output$info[[strategy]]$additional.info$n_new_semestral_followup_no_hsils),
        n_cyto=mean(dsa.output$info[[strategy]]$additional.info$n_cyto),
        n_hpv=mean(dsa.output$info[[strategy]]$additional.info$n_hpv),
        n_hra_no_hsil=mean(dsa.output$info[[strategy]]$additional.info$n_hra_no_hsil),
        n_hra_hsil=mean(dsa.output$info[[strategy]]$additional.info$n_hra_hsil),
        n_treatment_no_hsil=mean(dsa.output$info[[strategy]]$additional.info$n_treatment_no_hsil),
        n_treatment_hsil=mean(dsa.output$info[[strategy]]$additional.info$n_treatment_hsil)
      ), named.strata.ctx)
    }
    return(iter.result)
  })
  cat('Simulation done, generating results...\n')
  if (is.null(cluster) && !is.null(cl)) stopCluster(cl)  # Stopping the cluster only if it was created here
  results <- do.call(rbind, results)

  cat('All done\n')
  cat('******************\n')
  cat('\n')
  return(results)
}

.fit.dsa.params <- function(strat.ctx, range.estimate.func) {
  strat.dist.params <- lapply(names(strat.ctx), function(strat.name) {
    ctx <- strat.ctx[[strat.name]]
    ctx.params <- lapply(names(ctx), function(p) {
      return(range.estimate.func(p, ctx[[p]][1], strat.name))
    })
    names(ctx.params) <- names(ctx)
    return(ctx.params)
  })
  names(strat.dist.params) <- names(strat.ctx)
  return(strat.dist.params)
}

sample.dsa.params <- function(pars, intervals, n.points, strat.dist.params, strat.ctx, excel.strata.df, sample.by.stratum=FALSE) {
  dsa.strat.ctx <- strat.ctx
  for(i in seq_along(pars)) {
    par <- pars[i]
    interval <- intervals[i]
    dsa.strat.ctx <- lapply(names(dsa.strat.ctx), function(stratum) {
      ctx <- dsa.strat.ctx[[stratum]]
      min.val <- strat.dist.params[[stratum]][[par]][1]
      max.val <- strat.dist.params[[stratum]][[par]][2]
      base.val <- strat.ctx[[stratum]][[par]]
      # Equal split of lower and upper intervals (centered on the base value)
      # E.g. (.5 - 1) centered on .9 split in 5 -> (.5, .7, .9, .95, 1)
      if (interval == (n.points-1)/2) {
        sampled.par <- base.val
      } else if (interval < (n.points-1)/2) {
        sampled.par <- min.val + 2*interval/(n.points-1) * (base.val - min.val)
      } else {
        sampled.par <- base.val + 2*(interval-(n.points-1)/2)/(n.points-1) * (max.val - base.val)
      }
      ctx[par][1] <- sampled.par
      return(ctx)
    })
    names(dsa.strat.ctx) <- names(strat.ctx)
  }
  dsa.strat.ctx <- refresh.context(pars, dsa.strat.ctx, excel.strata.df)
  return(dsa.strat.ctx)
}


plot.tornado <- function(results,
                         population,
                         reference,
                         WTP=c(22000, 25000),
                         param.order=NULL,
                         param.display.names=NULL,
                         show.points=FALSE,
                         truncate.icers=NULL,
                         bar.color='blue',
                         min.icer.range=NULL,
                         plot=NULL) {
  if (is.character(results)) {
    # If character, assume it is a file path with the results data
    results <- read.csv(results)
  }
  base.params <- results[is.na(results$param),][1,]
  base.icer <- base.params$ICER
  results <- results[!is.na(results$param),]

  plot.df <- data.frame()
  scatter.df <- data.frame()

  for(p in unique(results$param)) {
    par.names <- names(results)[endsWith(names(results), p)]
    sub.df <- results[results$param==p,]
    par.range <- range(sub.df[[par.names[1]]])
    if (diff(par.range) != 0) {
      icer.par.range <- sapply(par.range, function(v) sub.df[sub.df[par.names[1]] == v, 'ICER'])
    } else {
      # If range width = 0, the ICER is the same
      icer.par.range <- rep(sub.df$ICER[1], 2)
    }
    if (!is.null(param.display.names)) {
      p.display <- param.display.names[[p]]
      if (is.null(p.display)) {
        p.display <- p
      }
    } else {
      p.display <- p
    }

    if ((abs(diff(icer.par.range)) < 1e-12) ||
        (base.icer >= icer.par.range[1] && base.icer <= icer.par.range[2]) ||
        (base.icer <= icer.par.range[1] && base.icer >= icer.par.range[2])) {
      if (diff(icer.par.range) > 0) {
        label <- paste0(p.display, '\n[', formatC(par.range[1], format='fg', digits=3), ' - ', formatC(par.range[2], format='fg', digits=3), ']')
      } else {
        label <- paste0(p.display, '\n[', formatC(par.range[2], format='fg', digits=3), ' - ', formatC(par.range[1], format='fg', digits=3), ']')
      }
      plot.df <- rbind(plot.df,
                       data.frame(
                         strategy=results$strategy[1],
                         param=p,
                         label=label,
                         min.v=min(icer.par.range),
                         max.v=max(icer.par.range),
                         width=abs(diff(icer.par.range))
                       ))
    } else {
      if (is.null(truncate.icers)) truncate.icers <- c(-1e6, 1e6)
      lim.val.1 <- ifelse(par.range[1] == 1, '1.0', formatC(par.range[1], format='fg', digits=3))
      lim.val.2 <- ifelse(par.range[2] == 1, '1.0', formatC(par.range[2], format='fg', digits=3))
      if (diff(icer.par.range) < 0) {
        label <- paste0(p.display, '\n[', lim.val.1, ' - ', lim.val.2, ']')
      } else {
        label <- paste0(p.display, '\n[', lim.val.2, ' - ', lim.val.1, ']')
      }
      if (truncate.icers[1] < min(icer.par.range))
        plot.df <- rbind(plot.df,
                         data.frame(
                           strategy=results$strategy[1],
                           param=p,
                           label=label,
                           min.v=truncate.icers[1],
                           max.v=min(icer.par.range),
                           width=1e10-abs(diff(icer.par.range))
                         ))
      if (truncate.icers[2] > max(icer.par.range))
        plot.df <- rbind(plot.df,
                         data.frame(
                           strategy=results$strategy[1],
                           param=p,
                           label=label,
                           min.v=max(icer.par.range),
                           max.v=truncate.icers[2],
                           width=1e10-abs(diff(icer.par.range))
                         ))
    }
    scatter.df <- rbind(scatter.df,
                        data.frame(
                          strategy=results$strategy[1],
                          param=p,
                          icer=sub.df$ICER
                        ))
  }

  if (!is.null(param.order)) {
    plot.df <- plot.df[match(param.order, plot.df$param),]
  } else {
    plot.df <- plot.df[order(plot.df$width),]
    if (!is.null(min.icer.range))
      plot.df <- plot.df[plot.df$width > min.icer.range,]
    param.order <- plot.df$param[!duplicated(plot.df$param)]
  }
  ordered.pars <- plot.df[!duplicated(plot.df$param), 'param']
  plot.df$pos <- sapply(plot.df$param, function(p) match(p, ordered.pars))
  scatter.df$pos <- sapply(scatter.df$param, function(p)plot.df[plot.df$param==p,]$pos[1])

  breaks <- seq(max(plot.df$pos))
  labels <- sapply(breaks, function(b) plot.df[plot.df$pos==b,]$label[1])
  if (is.null(plot)) {
    plt <- ggplot(plot.df) +
      geom_segment(size=6, color=bar.color, aes(x=min.v, xend=max.v, y=pos, yend=pos)) +
      annotate('segment', x=base.icer,xend=base.icer,y=0,yend=length(unique(plot.df$param))+.44,
                          color='orange',
                          linetype='dashed') +
      annotate('segment', x=base.icer,xend=base.icer,y=length(unique(plot.df$param))+.44,yend=length(unique(plot.df$param))+.45,
                          arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed",),
                          color='orange') +
      geom_vline(xintercept=0, color='black') +
      # geom_vline(xintercept=WTP[1], color='red', linetype=2) +
      xlab('ICER (€/QALY)') +
      ylab('') +
      # scale_x_continuous(
      #   breaks=seq(-5000,40000,5000),
      #   labels=formatC(seq(-5000,40000,5000), format='d', big.mark=',')) +
      scale_y_continuous(
        breaks=breaks,
        labels=labels,
        limits = c(0, length(unique(plot.df$param))+1),
        oob=squish_infinite) +
      ggtitle(paste0('Tornado diagram [', results$strategy[1], ' vs ', reference, '] (Base ICER = ', formatC(base.icer, digits = 0, format = 'd'), ' €/QALY)')) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank(), plot.title = element_text(size=10))
  } else {
    plt <- plot +
      geom_segment(data=plot.df, size=3, color=bar.color, aes(x=min.v, xend=max.v, y=pos, yend=pos))
  }

  if (length(WTP) == 1) {
    plt <- plt +
      geom_vline(xintercept=WTP[1], color='red', linetype=2)
  } else if (length(WTP) == 2) {
    plt <- plt +
      # geom_vline(xintercept=WTP[2], color='red', linetype=2) +
      annotate("rect", xmin = WTP[1], xmax = WTP[2], ymin = 0, ymax = length(unique(plot.df$param)), alpha = .3) +
      annotate('text', label='COST-EFFECTIVENESS THRESHOLD', x=(WTP[2]+WTP[1])/2, y=length(unique(plot.df$param))/2, angle=90, size=2)
  }

  plt.w.points <- plt + geom_point(data=scatter.df, mapping=aes(x=icer, y=pos), color='yellow')

  return(list(plt, plt.w.points, param.order))
}

plot.tornado.nhb <- function(results,
                         population,
                         reference,
                         WTP=22000,
                         param.order=NULL,
                         param.display.names=NULL,
                         show.points=FALSE,
                         truncate.icers=NULL,
                         bar.color='blue',
                         plot=NULL) {
  if (is.character(results)) {
    # If character, assume it is a file path with the results data
    results <- read.csv(results)
  }
  results$NHB <- results$IE - results$IC/WTP

  base.params <- results[is.na(results$param),][1,]
  base.nhb <- base.params$NHB
  results <- results[!is.na(results$param),]

  plot.df <- data.frame()
  scatter.df <- data.frame()
  for(p in unique(results$param)) {
    par.names <- names(results)[endsWith(names(results), p)]
    sub.df <- results[results$param==p,]
    par.range <- range(sub.df[[par.names[1]]])
    if (diff(par.range) != 0) {
      nhb.par.range <- sapply(par.range, function(v) sub.df[sub.df[par.names[1]] == v, 'NHB'])
    } else {
      # If range width = 0, the NHB is the same
      nhb.par.range <- rep(sub.df$NHB[1], 2)
    }
    if (!is.null(param.display.names)) {
      p.display <- param.display.names[[p]]
      if (is.null(p.display)) {
        p.display <- p
      }
    } else {
      p.display <- p
    }
    if (diff(nhb.par.range) > 0) {
      label <- paste0(p.display, '\n[', formatC(par.range[1], format='fg', digits=3), ' - ', formatC(par.range[2], format='fg', digits=3), ']')
    } else {
      label <- paste0(p.display, '\n[', formatC(par.range[2], format='fg', digits=3), ' - ', formatC(par.range[1], format='fg', digits=3), ']')
    }
    plot.df <- rbind(plot.df,
                     data.frame(
                       strategy=results$strategy[1],
                       param=p,
                       label=label,
                       min.v=min(nhb.par.range),
                       max.v=max(nhb.par.range),
                       width=abs(diff(nhb.par.range))
                     ))
    scatter.df <- rbind(scatter.df,
                        data.frame(
                          strategy=results$strategy[1],
                          param=p,
                          nhb=sub.df$NHB
                        ))
  }
  if (!is.null(param.order)) {
    plot.df <- plot.df[match(param.order, plot.df$param),]
  } else {
    plot.df <- plot.df[order(plot.df$width),]
  }

  ordered.pars <- plot.df[!duplicated(plot.df$param), 'param']
  plot.df$pos <- sapply(plot.df$param, function(p) match(p, ordered.pars))
  scatter.df$pos <- sapply(scatter.df$param, function(p)plot.df[plot.df$param==p,]$pos[1])

  breaks <- seq(max(plot.df$pos))
  labels <- sapply(breaks, function(b) plot.df[plot.df$pos==b,]$label[1])
  if (is.null(plot)) {
    plt <- ggplot(plot.df) +
      geom_segment(size=6, color=bar.color, aes(x=min.v, xend=max.v, y=pos, yend=pos)) +
      annotate('segment', x=base.nhb,xend=base.nhb,y=0,yend=length(unique(plot.df$param))+.44,
               color='orange',
               linetype='dashed') +
      annotate('segment', x=base.nhb,xend=base.nhb,y=length(unique(plot.df$param))+.44,yend=length(unique(plot.df$param))+.45,
               arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed",),
               color='orange') +
      geom_vline(xintercept=0, color='black', linetype='dashed') +
      # geom_vline(xintercept=WTP[1], color='red', linetype=2) +
      xlab('NHB (QALY)') +
      ylab('') +
      # scale_x_continuous(
      #   breaks=seq(-5000,40000,5000),
      #   labels=formatC(seq(-5000,40000,5000), format='d', big.mark=',')) +
      scale_y_continuous(
        breaks=breaks,
        labels=labels,
        limits = c(0, length(unique(plot.df$param))+1),
        oob=squish_infinite) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      ggtitle(paste0('Tornado diagram [', results$strategy[1], ' vs ', reference, ']'))
  } else {
    plt <- plot +
      geom_segment(data=plot.df, size=3, color=bar.color, aes(x=min.v, xend=max.v, y=pos, yend=pos))
  }

  # if (length(WTP) == 2) {
  #   plt <- plt +
  #     # geom_vline(xintercept=WTP[2], color='red', linetype=2) +
  #     annotate("rect", xmin = WTP[1], xmax = WTP[2], ymin = 0, ymax = length(unique(plot.df$param)), alpha = .3) +
  #     annotate('text', label='COST-EFFECTIVENESS THRESHOLD', x=(WTP[2]+WTP[1])/2, y=length(unique(plot.df$param))/2, angle=90, size=2)
  # }

  plt.w.points <- plt + geom_point(data=scatter.df, mapping=aes(x=nhb, y=pos), color='yellow')

  return(list(plt, plt.w.points))
}

plot.curves <- function(results, pars, icer.limits=c(-1e5,1e5), WTP=c(20000, 25000), base.value=NULL) {
  params <- pars
  names(params) <- params
  plot.curves <- lapply(params, function(p) {
      res <- results
      res <- res[res$param==p,]
      res <- res[-1,]

      res$par.val <- res[,paste0('y25_29_', p)]
      res$color <- ifelse(res$IE < 0 & res$IC > 0, 'red', 'black')
      res$color <- ifelse(res$IE > 0 & res$IC < 0, 'green', res$color)
      plt <- ggplot(res, aes(x=par.val, y=ICER)) + geom_line() +
            geom_point(aes(color=color)) +
            geom_hline(yintercept=WTP[1], color='red', linetype=2) +
            coord_cartesian(ylim=icer.limits) +
            scale_color_manual(values=c('black', 'red', 'green'),
                               breaks=c('black', 'red', 'green')) +
            theme(legend.position='none') +
            geom_hline(yintercept = 0, color='black') +
            ggtitle(p)

      if (length(WTP) == 2) {
        plt <- plt +
          geom_hline(yintercept=WTP[2], color='red', linetype=2) +
          annotate("rect", ymin = WTP[1], ymax = WTP[2], xmin = min(res$par.val), xmax = max(res$par.val), alpha = .3)
      }

      if (!is.null(base.value)) {
        plt <- plt +
          geom_vline(xintercept = base.value, color='grey', linetype=2)
      }
      return(plt)
  })
  return(plot.curves)
}

plot.heatmap <- function(pars, results, population, reference, WTP=20000) {
  if (length(pars) != 2) stop('Heatmaps can only be done for pairs of parameters')

  if (is.character(results)) {
    # If character, assume it is a file path with the results data
    results <- read.csv(results)
  }

  base.icer <- results[1,'ICER']
  selected.params.indices <- rep(F, ncol(results))
  for(par in pars) {
    selected.params.indices <- selected.params.indices | endsWith(names(results), par)
  }
  hm.params <- names(results)[selected.params.indices]
  hm.params <- hm.params[seq(length(pars))]
  df <- results[-1, c('ICER', 'IE', hm.params)]
  names(df) <- c('ICER', 'IE', pars)

  df[df$ICER < 0, 'ICER'] <- NA

  plt <- ggplot(df, aes_string(x=pars[1], y=pars[2])) +
    geom_tile(aes(fill=ICER)) +
    scale_fill_gradient2(low='green',
                         mid='white',
                         high='red',
                         na.value = 'black', # TODO: Differentiate between IE > 0 and IE < 0
                         midpoint=base.icer) +
    ggtitle(paste0('Heatmap [', results$strategy[1], ' vs ', reference, '] (Base ICER = ', formatC(base.icer, digits = 0, format = 'd'), ' €/QALY)'))

  return(plt)
}

plot.scatter <- function(results, population, reference) {
  if (is.character(results)) {
    # If character, assume it is a file path with the results data
    results <- read.csv(results)
    results$CE_THRESHOLD <- factor(results$CE_THRESHOLD, levels=c(WTP.THRESHOLDS, Inf))
  }
  ce.labels <- c(paste0('<= ', formatC(WTP.THRESHOLDS, big.mark = ',', format='d')),
                 paste0('> ', formatC(WTP.THRESHOLDS[length(WTP.THRESHOLDS)], big.mark = ',', format='d')))

  lines.df <- data.frame(wtp=factor(WTP.THRESHOLDS, levels=levels(results$CE_THRESHOLD)))

  plt <- ggplot(results, aes(x=IC, y=IE, color=CE_THRESHOLD)) +
    geom_abline(data=lines.df, mapping=aes(slope=1/as.numeric(as.character(wtp)), intercept=0, color=wtp)) +
    geom_point() +
    xlim(c(min(results$IC, 0), max(results$IC, 0))) +
    ylim(c(min(results$IE, 0), max(results$IE, 0))) +
    scale_color_manual(name='ICER (€/QALY)',
                       breaks = levels(results$CE_THRESHOLD),
                       labels=ce.labels,
                       values=COLOR.PALETTE) +
    ggtitle(paste0(results$strategy[1], ' vs ', reference))

  return(plt)
}
