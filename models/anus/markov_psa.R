library(ggplot2)
library(plotly)
library(xlsx)
library(htmlwidgets)
library(doParallel)
library(pbapply)
library(CEAModel)
library(scales)

source('markov.R')
source('distributions.R')
source('excel_params.R')

PSA.SEED <- 1234
N.ITERS.DEFAULT <- 1000
DISCOUNT.RATE.DEFAULT <- .05
WTP.THRESHOLDS <- c(5000, 10000, 20000, 50000)
COLOR.PALETTE <- c('#2a9d8f', '#e9c46a', '#f4a261', '#e76f51', '#ff0000')

results <- data.frame()
psa.1 <- function(pars,
                  strat.ctx,
                  excel.file,
                  population,
                  strategy,
                  markov,
                  discount.rate=DISCOUNT.RATE.DEFAULT,
                  sd.estimate.func=function(p,mu)mu,
                  context.setup.func=NULL,
                  n.iters=N.ITERS.DEFAULT,
                  n.cores=NULL,
                  sample.by.stratum=FALSE,
                  seed=0) {
  if (!all(pars %in% names(strat.ctx[[1]])))
    stop(paste0("Some parameters don't exist in the context: ", 
                paste0('"', pars[!pars %in% names(strat.ctx[[1]])], '"', collapse = ', ')))
  
  if (is.null(n.cores)) n.cores <- detectCores()
  if (is.null(cluster) && n.cores > 1) {
    out.file <- ifelse(Sys.getenv('SLURM_JOB_ID') == '', 'workers.out', paste0('workers-', Sys.getenv('SLURM_JOB_ID'), '.out'))
    cl <- makeCluster(n.cores, outfile=out.file)
    on.exit({
      stopCluster(cl)
    }, add=TRUE)
    registerDoParallel(cl)
    
    varlist <- ls(pos=1)
    # varlist <- varlist[!varlist %in% c('results', 'trees')]
    # varlist <- varlist[!startsWith(varlist, 'conventional')]
    # varlist <- varlist[!startsWith(varlist, 'ascus_lsil_diff')]
    # varlist <- varlist[!startsWith(varlist, 'arnme6e7')]
    
    clusterExport(cl=cl,varlist=varlist,
                  envir=environment())
    clusterEvalQ(cl=cl,{
      library(data.table)
      library(stringr)
      library(ggplot2)
      library(plotly)
      library(pbapply)
    })
  } else {
    cl <- cluster
  }
  
  results <- list()
  for(p in pars) {
    results[[p]] <- psa.n(p,
                          strat.ctx,
                          excel.file,
                          population,
                          strategy,
                          markov,
                          reference=reference,
                          discount.rate=discount.rate,
                          sd.estimate.func=sd.estimate.func,
                          context.setup.func=context.setup.func,
                          n.iters=n.iters,
                          n.cores=n.cores,
                          cluster=cl,
                          sample.by.stratum=sample.by.stratum,
                          seed=seed)
    results[[p]]$plot.scatter <- results[[p]]$plot.scatter + 
      ggtitle(paste0(strategy, ' vs ', reference, ' (', p, ')'))
    results[[p]]$plot.scatter.j <- results[[p]]$plot.scatter.j + 
      ggtitle(paste0(strategy, ' vs ', reference, ' (', p, ')'))
    results[[p]]$plot.acceptability <- results[[p]]$plot.acceptability + 
      ggtitle(paste0(strategy, ' vs ', reference, ' (', p, ')'))
  }
  return(results)
}


psa.n <- function(pars,
                  strat.ctx,
                  excel.file,
                  population,
                  strategy,
                  reference,
                  markov,
                  discount.rate=DISCOUNT.RATE.DEFAULT,
                  sd.estimate.func=NULL,
                  context.setup.func=NULL,
                  n.iters=N.ITERS.DEFAULT,
                  n.cores=NULL,
                  cluster=NULL,
                  sample.by.stratum=FALSE,
                  seed=0,
                  jitter.x=.05,
                  jitter.y=.05) {
  if (!all(pars %in% names(strat.ctx[[1]])))
    stop(paste0("Some parameters don't exist in the context: ", 
                paste0('"', pars[!pars %in% names(strat.ctx[[1]])], '"', collapse = ', ')))
  pars <- pars[order(pars)]
  if (is.null(sd.estimate.func)) {
    sd.estimate.func <- function(p.name, mu) mu
  }
  
  if (is.null(n.cores)) n.cores <- detectCores()
  if (is.null(cluster) && n.cores > 1) {
    out.file <- ifelse(Sys.getenv('SLURM_JOB_ID') == '', 'workers.out', paste0('workers-', Sys.getenv('SLURM_JOB_ID'), '.out'))
    cl <- makeCluster(n.cores, outfile=out.file)
    on.exit({
      stopCluster(cl)
    }, add=TRUE)
    registerDoParallel(cl)
    
    varlist <- ls(pos=1)
    # varlist <- varlist[!varlist %in% c('results', 'trees')]
    # varlist <- varlist[!startsWith(varlist, 'conventional')]
    # varlist <- varlist[!startsWith(varlist, 'ascus_lsil_diff')]
    # varlist <- varlist[!startsWith(varlist, 'arnme6e7')]
    
    clusterExport(cl=cl,varlist=varlist,
                  envir=environment())
    clusterEvalQ(cl=cl,{
      library(data.table)
      library(stringr)
      library(ggplot2)
      library(plotly)
      library(pbapply)
    })
  } else {
    cl <- cluster
    if (is.null(cluster)) pboptions(type='none')
    else pboptions(type='timer')
  }
  
  cat('****************************************\n')
  cat(paste0('Starting PSA for ', strategy))
  if (length(pars) == 1) cat(paste0(' [', pars[1], ']'))
  cat('\n')
  cat(paste0('* ', n.cores, ' core(s)\n'))
  cat(paste0('* ', n.iters, ' iterations\n'))
  cat(paste0('* Sample by stratum: ', sample.by.stratum, '\n'))
  cat(paste0('* SD estimate function:\n'))
  print(sd.estimate.func)
  cat('* Parameters included:\n')
  cat(paste0('- ', paste0(pars, collapse = ', '), '\n'))
  cat('\n')
  # excel.strata.df <- list()
  # strata <- names(xlsx::getSheets(xlsx::loadWorkbook(excel.file)))
  # cat('Reading parameters from excel data...\n')
  # for(stratum in strata) {
  #   excel.strata.df[[stratum]] <- read.xlsx(excel.file, sheetName = stratum, keepFormulas = T)[,c(1,2)]
  # }
  
  simulation.strategies <- strategies[[population]]
  strategy.names <- sapply(strategies[[population]], function(s) s$name)
  simulation.strategies <- simulation.strategies[strategy.names %in% c(reference, strategy)]
  cat('Fitting distributions for parameters...\n')
  strat.dist.params <- .fit.psa.params(pars, strat.ctx, sd.estimate.func)
  cat(paste0('Simulating ', n.iters, ' PSA iterations...\n'))
  
  df.results <- pblapply(cl=cl, X=seq(n.iters), FUN=function(j) {
    psa.result <- NULL
    count <- 0
    MAX.SAMPLING.ATTEMPTS <- 20
    attempt <- 1
    while (is.null(psa.result) && attempt <= MAX.SAMPLING.ATTEMPTS) {
      attempt <- attempt + 1
      psa.strat.ctx <- NULL
      while (is.null(psa.strat.ctx)) {
        message('SEED: ', seed + j + 100000*count)
        set.seed(seed + j + 100000*count)
        psa.strat.ctx <- sample.psa.params(pars,
                                           strat.dist.params,
                                           strat.ctx,
                                           excel.strata.df = excel.strata.df,
                                           sample.by.stratum=sample.by.stratum,
                                           context.setup.func=context.setup.func)
        count <- count + 1
      }
      initial.state <- ifelse(seq_along(markov$nodes)==1, 1, 0)
      names(initial.state) <- sapply(markov$nodes, function(n) n$name)
      psa.result <- 
        tryCatch({
         simulate(population,
           simulation.strategies,
           markov,
           psa.strat.ctx,
           initial.state,
           n.cores=1,
           discount.rate=discount.rate)$summary
        }
        ,
         error=function(e) {
           message(e$message)
           NULL
         })
      if (is.null(psa.result)) {
        message('Simulation failed while sampling ', paste0(pars, collapse=', '), '. Resampling...')
      }
    }
    # If MAX.ATTEMPTS attempts in a row fail, give up
    if (is.null(psa.result)) {
      stop('Simulation failed ', MAX.SAMPLING.ATTEMPTS, ' times in a row while sampling ', paste0(pars, collapse=', '), ', aborting PSA.')
    }

      psa.ref <- psa.result[startsWith(psa.result$strategy, reference),]  # We ignore strategy suffixes
      psa.strat <- psa.result[startsWith(psa.result$strategy, strategy),]
      IC <- psa.strat$C - psa.ref$C
      IE <- psa.strat$E - psa.ref$E
      named.strata.ctx <- lapply(names(psa.strat.ctx),
                                function(stratum) {
                                  names(psa.strat.ctx[[stratum]]) <- paste0(stratum,
                                                                            '_',
                                                                            names(psa.strat.ctx[[stratum]]));
                                  psa.strat.ctx[[stratum]]
                                  })
      
      named.strata.ctx <- lapply(named.strata.ctx, function(x) lapply(x, function(y)y[1]))  # FIX: Why is this necessary?
      named.strata.ctx <- Reduce(function(x,y) merge(x, y, all=TRUE), named.strata.ctx)
      
      iter.result <- cbind(data.frame(
                          iter=j,
                          strategy=strategy,
                          IC=IC,
                          IE=IE,
                          ICER=IC/IE
                        ), named.strata.ctx)
      return(iter.result)
  })
  cat('Simulation done, generating results...\n')
  
  df.results <- do.call(rbind, df.results)

  df.results$CE_THRESHOLD <- Inf
  for(wtp in rev(WTP.THRESHOLDS)) {
    df.results$CE_THRESHOLD <- ifelse(df.results$IE >= df.results$IC / wtp, wtp, df.results$CE_THRESHOLD)
  }
  df.results$CE_THRESHOLD <- factor(df.results$CE_THRESHOLD, levels=c(WTP.THRESHOLDS, Inf))
   
  cat('All done\n')
  cat('****************************************\n')
  cat('\n')
  
  results <- generate.psa.summary(df.results, strategy, population, reference, strat.ctx, pars, jitter.x=jitter.x, jitter.y=jitter.y)
  
  return(results)
}

generate.psa.summary <- function(df.results, strategy, population, reference, strat.ctx, pars, jitter.x=.05, jitter.y=.05) {
  # Display the parameter value as color if there is only one parameter in the PSA
  # and this parameter is not stratified (same value for all strata -> variance = 0).
  if (length(pars) == 1 && var(t(df.results[1,names(df.results)[endsWith(names(df.results), pars)]])) == 0) {
    color.var <- names(df.results)[endsWith(names(df.results), pars)][[1]]
  } else {
    color.var <- NULL
  }
  
  plot.scatter <- .plot.psa(df.results, strategy, population, reference, strat.ctx, pars, color.var = color.var)
  plot.scatter.j <- .plot.psa(df.results, strategy, population, reference, strat.ctx, pars, color.var = color.var, jitter.x=jitter.x, jitter.y=jitter.y)
  plot.acceptability <- .plot.acceptability(df.results, strategy, population, reference, strat.ctx, pars)
  return(list(
    summary=df.results,
    plot.scatter=plot.scatter,
    plot.scatter.j=plot.scatter.j,
    plot.acceptability=plot.acceptability
  ))
}

.fit.psa.params <- function(pars, strat.ctx, sd.estimate.func) {
  strat.dist.params <- lapply(strat.ctx, function(ctx) {
    ctx.params <- lapply(pars, function(p) {
      return(fit.parameter(p, ctx[[p]][1], sd.estimate.func(p, ctx[[p]][1])))
    })
    names(ctx.params) <- pars
    return(ctx.params)
  })
  return(strat.dist.params)
}

sample.psa.params <- function(pars, strat.dist.params, strat.ctx, excel.strata.df, sample.by.stratum=FALSE, context.setup.func=NULL) {
  psa.strat.ctx <- strat.ctx
  # Using a permutation to avoid bias when sampling multiple parameters with restrictions
  permuted.pars <- sample(pars)
  for(p in permuted.pars) {
    par.values <- sapply(psa.strat.ctx, function(ctx) ctx[[p]][1])
    is.constant.par <- all(par.values==par.values[1])
    if (is.constant.par & !sample.by.stratum) {
      sampled.par <- sample.parameter(p, strat.dist.params[[1]][[p]])
      psa.strat.ctx <- lapply(names(psa.strat.ctx), function(stratum) {
        ctx <- psa.strat.ctx[[stratum]]
        ctx[p][1] <- sampled.par # Same value for all strata
        return(ctx)
      })
    } else {
      psa.strat.ctx <- lapply(names(psa.strat.ctx), function(stratum) {
        ctx <- psa.strat.ctx[[stratum]]
        sample.ok <- FALSE
        while (!sample.ok) {
          ctx[p][1] <- sample.parameter(p, strat.dist.params[[stratum]][[p]]) # Different sample for each strata
          strat.ctx.tmp <- psa.strat.ctx
          strat.ctx.tmp[[stratum]] <- ctx
          strat.ctx.tmp <- refresh.context(p, strat.ctx.tmp, excel.strata.df, context.setup)
          sample.ok <- !is.null(strat.ctx.tmp)
        }
        return(ctx)
      })
    }
    names(psa.strat.ctx) <- names(strat.ctx)
  }
  
  psa.strat.ctx <- refresh.context(pars, psa.strat.ctx, excel.strata.df, context.setup.func)
  return(psa.strat.ctx)
}

.plot.psa <- function(results, strategy, population, reference, strat.ctx, params, color.var=NULL, jitter.x=0, jitter.y=0) {
  if (is.character(results)) {
    # If character, assume it is a file path with the results data
    results <- read.csv(results)
  }
  
  results.display <- results
  # results.display$IC <- results.display$IC + rnorm(nrow(results.display), sd=abs(mean(results.display$IC))*jitter.x)
  # results.display$IE <- results.display$IE + rnorm(nrow(results.display), sd=abs(mean(results.display$IE))*jitter.y)
  mu.c <- results.display$IC
  mu.e <- results.display$IE
  range.c <- abs(jitter.x * results.display$IC)
  range.e <- abs(jitter.y * results.display$IE)
  results.display$IC <- runif(nrow(results.display), mu.c - range.c, mu.c + range.c)
  results.display$IE <- runif(nrow(results.display), mu.e - range.e, mu.e + range.e)
  
  # TODO: Refactor: Use 22k and 25k WTP
  
  results.display[, 'CE_THRESHOLD'] <- Inf
  # results[results$IC < 25000 * results$IE, 'CE_THRESHOLD'] <- 25000
  results.display[results.display$IC < 22000 * results.display$IE, 'CE_THRESHOLD'] <- 22000
  WTP.THRESHOLDS <- 22000
  COLOR.PALETTE <- c('#2bbe6d', '#f44f59')
  ###
  results.display$CE_THRESHOLD <- factor(results.display$CE_THRESHOLD, levels=c(WTP.THRESHOLDS, Inf))
  
  if (is.null(color.var)) color.var <- 'CE_THRESHOLD'
  lines.df <- data.frame(wtp=factor(WTP.THRESHOLDS, levels=levels(results.display$CE_THRESHOLD)))
  
  plt <- ggplot(results.display, aes(x=IC, y=IE, color=get(color.var))) +
    ggtitle(paste0(strategy, ' vs ', reference))
  
  if (color.var=='CE_THRESHOLD' || length(color.var) > 1) {
    ce.labels <- c(paste0('<= ', formatC(WTP.THRESHOLDS, big.mark = ',', format='d')), 
                   paste0('> ', formatC(WTP.THRESHOLDS[length(WTP.THRESHOLDS)], big.mark = ',', format='d')))
    
    plt <- plt + 
      geom_point() +
      scale_color_manual(name='ICER (€/QALY)',
                         breaks = levels(results.display$CE_THRESHOLD),
                         labels=ce.labels,
                         values=COLOR.PALETTE) +
      geom_abline(data=lines.df, mapping=aes(slope=1/as.numeric(as.character(wtp)), intercept=0), color='black')
  } else {
    midpoint <- rescale(c(strat.ctx[[1]][[params[1]]], results.display[,color.var]))[1]
    plt <- plt + 
      geom_point(size=1) +
      scale_color_gradientn(name=color.var,
                                       colors=c('red', '#777777', 'green'),
                                       values=c(0, midpoint, 1)
                                       ) +
                 geom_abline(data=lines.df, mapping=aes(slope=1/as.numeric(as.character(wtp)), intercept=0))
  }
  
  plt <- plt +
    xlim(c(min(results.display$IC, 0), max(results.display$IC, 0))) +
    ylim(c(min(results.display$IE, 0), max(results.display$IE, 0)))
  
  return(plt)
}

.plot.acceptability <- function(results, strategy, population, reference, strat.ctx, pars) {
  if (is.character(results)) {
    # If character, assume it is a file path with the results data
    results <- read.csv(results)
    results$CE_THRESHOLD <- factor(results$CE_THRESHOLD, levels=c(WTP.THRESHOLDS, Inf))
  }
  wtp.range <- seq(0,WTP.THRESHOLDS[length(WTP.THRESHOLDS)], 1000)
  line.data <- sapply(wtp.range, function(wtp) {
    return(mean(results$IC <= results$IE * wtp) * 100)
  })
  
  line.df <- data.frame(wtp=c(wtp.range, wtp.range),
                        p.ce=c(line.data, 100-line.data),
                        strategy=c(rep(strategy, length(wtp.range)), 
                                   rep(reference, length(wtp.range))))
  
  plt <- ggplot(line.df, aes(x=wtp, y=p.ce, color=strategy)) + 
    geom_line() + 
    ylim(c(0,100)) +
    xlab('WTP (€/QALY)') +
    ylab('% Cost-effective simulations') +
    ggtitle(paste0(strategy, ' vs ', reference)) +
    scale_color_manual(name='Strategy',
                       values=c('#002fff', '#ff9d00')) 
  return(plt)
}

plot.ci.sweep <- function(pars,
                         strat.ctx,
                         excel.file,
                         population,
                         strategy,
                         markov,
                         sweep.parameter,
                         sweep.values,
                         wtp=20000,
                         sd.estimate.func=NULL,
                         context.setup.func=NULL,
                         n.iters=N.ITERS.DEFAULT,
                         n.cores=NULL,
                         sample.by.stratum=FALSE,
                         seed=0) {
  sweep.df <- lapply(sweep.values, function(val) {
    # Assuming sweep parameter has same value for all strata
    psa.strat.ctx <- lapply(strat.ctx, function(ctx) {
      ctx[[sweep.parameter]] <- val
      ctx
    })
    psa.df <- psa.n(pars,
                    psa.strat.ctx,
                    excel.file,
                    population,
                    strategy,
                    markov,
                    sd.estimate.func,
                    context.setup.func,
                    n.iters,
                    n.cores,
                    sample.by.stratum,
                    seed)
    icers <- psa.df$summary$ICER
    m <- mean(icers)
    sd.error <- qnorm(.975)*sd(icers)/sqrt(length(icers))
    l <- m - sd.error
    h <- m + sd.error
    return(data.frame(val=val, mean=m, low=l, high=h))
  })
  sweep.df <- bind_rows(sweep.df)
  
  p <- ggplot(sweep.df, aes(x=val, y=mean)) +
    geom_hline(yintercept = wtp, color='red') +
    geom_line() +
    geom_point() +
    geom_ribbon(aes(ymin=low, ymax=high), linetype=2, alpha=.1) +
    xlab(sweep.parameter) +
    ylab('ICER') +
    ggtitle(paste0(sweep.parameter, ' sweep on ', strategy))
  return(list(
    plot=p,
    data=sweep.df
  ))
}

# p <- plot.ci.sweep(pars, strat.ctx,
#               'bleeding', 'tree_bleeding_molecular',
#               markov,
#               '.c_molecular_test',
#               seq(0,1000, 50),
#               wtp=20000,
#               excel.file = 'params/context_by_age_adjusted_ly_dogc.xlsx',
#               sd.estimate.func=sd.estimate.func,
#               context.setup.func=context.setup,
#               # n.cores = 1,
#               n.iters=1000,
#               seed=1)
# print(p)


store.results.psa <- function(results, psa.type, population, strategy.name, filename) {
  time.preffix <- format(Sys.time(), "%Y_%m_%d__%H_%M_")
  output.dir <- paste0(getwd(), '/output/psa_', psa.type, '/', population)
  suppressWarnings(dir.create(output.dir, recursive=TRUE))
  write.csv(results$summary, paste0(output.dir, '/', filename, '.csv'), row.names = F)
}

build.plots <- function(psa.type, population, strategy, reference, strat.ctx, sim.options, sd.estimate.name, suffix, pars=NULL, param.set.name=NULL) {
  slides <- read_pptx('Plantilla_HPVinformationCentre.pptx')
  
  filename.preffix <- paste0(strategy, '__sd_', sd.estimate.name, '__par_')
  files.dir <- paste0(getwd(), '/output/psa_', psa.type, '/', population)
  
  files <- list.files(files.dir)
  if (!is.null(param.set.name)) {
    files <- files[startsWith(files, filename.preffix) &
                     endsWith(files, paste0('__par_', param.set.name, '.csv'))   ]
  }
  if (!is.null(pars)) {
    endsWithMulti <- Vectorize(function(x) any(endsWith(x, paste0(pars, '.csv'))), SIMPLIFY = TRUE)
    files <- files[startsWith(files, filename.preffix) &
                     endsWithMulti(files) ]
  }
  time.preffix <- format(Sys.time(), "%Y_%m_%d__%H_%M_")
  
  range.union <- function(r1, r2) {
    return(c(min(r1[1], r2[1]), max(r1[2], r2[2])))
  }
  
  ic.range <- c(0, 0)
  ie.range <- c(0, 0)
  
  for(f in files) {
    df <- read.csv(paste0(files.dir, '/', f))
    ic.range <- range.union(range(df$IC), ic.range)
    ie.range <- range.union(range(df$IE), ie.range)
  }
  
  for(f in files) {
    # if (psa.type=='univariate') {
    #   # If null, 
    #   slides <- read_pptx('Plantilla_HPVinformationCentre.pptx')
    # }
    filename <- paste0(files.dir, '/', f)
    par <- str_match(filename, '^.*_par_(.*?).csv')[2]
    df <- read.csv(filename)
    filename.stripped <- substr(filename, 1, nchar(filename)-4)
    
    plt <- .plot.psa(filename, strategy, population, reference, strat.ctx, par) +
      coord_cartesian(xlim=ic.range, ylim=ie.range) +
      theme_minimal()
    ggsave(paste0(filename.stripped, '.pdf'), 
           plt,
           width = 300,
           height = 200,
           units = 'mm',
           device = cairo_pdf
    )
    
    plt.j <- .plot.psa(filename, strategy, population, reference, strat.ctx, par, jitter.x=JITTER.X, jitter.y=JITTER.Y) +
      coord_cartesian(xlim=ic.range, ylim=ie.range) +
      theme_minimal()
    ggsave(paste0(filename.stripped, '_jitter.pdf'), 
           plt.j,
           width = 300,
           height = 200,
           units = 'mm',
           device = cairo_pdf
    )
    
    plt.acc <- .plot.acceptability(filename, strategy, population, reference, strat.ctx, par) +
      theme_minimal()
    ggsave(paste0(filename.stripped, '_acceptability.pdf'), 
           plt.acc,
           width = 300,
           height = 200,
           units = 'mm',
           device = cairo_pdf
    )
    
    ### Commented out since they take too much space, uncomment if needed
    # htmlwidgets::saveWidget(ggplotly(plt),
    #                         paste0(filename.stripped, '.html'),
    #                         title=strategy,
    #                         libdir=paste0(getwd(), paste0('/output/psa_', psa.type, '/', population, '/lib')))
    # htmlwidgets::saveWidget(ggplotly(plt.j),
    #                         paste0(filename.stripped, '_jitter.html'),
    #                         title=paste0(strategy, ' [jitter]'),
    #                         libdir=paste0(getwd(), paste0('/output/psa_', psa.type, '/', population, '/lib')))
    # htmlwidgets::saveWidget(ggplotly(plt.acc),
    #                         paste0(filename.stripped, '_acceptability.html'),
    #                         title=strategy,
    #                         libdir=paste0(getwd(), paste0('/output/psa_', psa.type, '/', population, '/lib')))
    
    plots <- list(plot.scatter=plt,
                  plot.scatter.j=plt.j,
                  plot.acceptability=plt.acc)
    slides <- add.results.to.slide(slides, df, plots, par, sim.options, sd.estimate.name)
  }
  slides %>% print(paste0('output/psa_', psa.type, '/', sim.options$population, '/', strategy, '__sd_', sd.estimate.name, '__par_', suffix, '.pptx'))
}

add.results.to.slide <- function(slides, summary, plots, par, sim.options, sd.estimate.name) {
  perc.ce <- mean(summary$IE > summary$IC / 25000)
  # par.name <- unname(ifelse(is.na(param.names[par]), par, param.names[par]))
  
suppressMessages({  
  plot.scatter <- plots$plot.scatter +
    ggtitle(paste0('PSA (', par, ') SD=', sd.estimate.name)) +
    scale_x_continuous(labels=scales::number_format(big.mark=',')) +
    xlab(paste0('Incremental cost\n\nWTP = 25,000 €/QALY\n(', 
                formatC(perc.ce*100, format='f', digits=1), 
                '% cost-effective)')) +
    ylab('Incremental QALY') +
    theme_classic() +
    theme(legend.position = 'none',
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))
  
  plot.scatter.j <- plots$plot.scatter.j +
    ggtitle(paste0('PSA (', par, ' [jitter]) SD=', sd.estimate.name)) +
    scale_x_continuous(labels=scales::number_format(big.mark=',')) +
    xlab(paste0('Incremental cost\n\nWTP = 25,000 €/QALY\n(', 
                formatC(perc.ce*100, format='f', digits=1), 
                '% cost-effective)')) +
    ylab('Incremental QALY') +
    theme_classic() +
    theme(legend.position = 'none',
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))
  
  plot.acceptability <- plots$plot.acceptability +
    ggtitle('') +
    scale_x_continuous(labels=scales::number_format(big.mark=',')) +
    scale_color_manual(name='Strategy',
                       breaks=c(sim.options$reference, sim.options$strategy),
                       labels=c(sim.options$reference.name, sim.options$strategy.name),
                       values=c('#002fff', '#ff9d00')) +
    xlab('WTP (€/QALY)\n\n\n') +
    ylab('Probability of cost-effective simulations (%)') +
    theme_classic() +
    theme(legend.position = c(.8,.9),
          panel.grid.major = element_line(color='#f2f2f2', linetype='dashed'),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))
})  
  
  slides <- slides %>%
    add_slide(layout='Titulo y objetos', master='Plantilla ICO') %>%
    ph_with(rvg::dml(ggobj=plot.scatter), ph_location_type('body', id=1))
  
  slides <- slides %>%
    add_slide(layout='Titulo y objetos', master='Plantilla ICO') %>%
    ph_with(rvg::dml(ggobj=plot.scatter.j), ph_location_type('body', id=1))
  
  slides <- slides %>%
    add_slide(layout='Titulo y objetos', master='Plantilla ICO') %>%
    ph_with(rvg::dml(ggobj=plot.acceptability), ph_location_type('body', id=1))
  
  return(slides)
}