# library(Cairo)

setwd('~/Documents/models_ce/anus')

source('load_models.R')
source('markov_dsa.R')

N.PARAM.POINTS.TORNADO <- 4
DISCOUNT.RATE <- .03
N.CORES <- 8

DEFAULT.START.AGE <- list(hiv_msm=40)
DEFAULT.MAX.AGE <- list(hiv_msm=80)

EXCLUDED.PARAMS <- names(strat.ctx$y25_29[strat.ctx$y25_29 %in% c(0,1)])
EXCLUDED.PARAMS <- c(EXCLUDED.PARAMS,
                     'periodicity_months',
                     'periodicity_times_in_year'
                     )

pars <- independent.pars
pars <- pars[!pars %in% EXCLUDED.PARAMS]

dsa.pars <- list(
  all=pars
  # ,
  # p_01=c('p_hra_hsil___cyto_hsil__no_hsil')
  # ,
  # p_02=c('p_hra_invasive_cancer___cyto_hsil__no_hsil')
  # ,
  # irc=c('c_surgery','c_cyto','p_death_other_annual','p_cyto_hsil___no_hsil','c_irc',
  #       'u_cancer','survival_5year','u_hiv_p','p_hra_hsil___cyto_hsil__hsil','p_no_hsil___hsil_irc',
  #       'c_surgery_delayed','p_hsil_regression_annual','c_arn_kit','u_cancer_delayed','p_cyto_b___no_hsil',
  #       'p_arnmhr_p___no_hsil','c_hra_treatment','p_undetected_hsil_treatment_whole_followup',
  #       'p_arnmhr_p___hsil','p_cyto_b___hsil','u_hsil___irc','p_cyto_hsil___hsil')
  # ,
  # costs=pars[startsWith(pars, '.c_') |
  #            startsWith(pars, 'c_')]
  # ,
  # utilities=pars[startsWith(pars, '.u_') |
  #                startsWith(pars, 'u_')]
  # ,
  # probs=pars[startsWith(pars, '.p_') |
  #              startsWith(pars, 'p_') |
  #              startsWith(pars, '.rate_') |
  #              startsWith(pars, '.sensitivity_') |
  #              startsWith(pars, '.specificity_') |
  #              startsWith(pars, '.survival_') |
  #              startsWith(pars, '.hr_')]
  # ,
  # ranged=pars[sapply(pars,
  #                    function(p)
  #                      any(full.strat.ctx$y25_29[[p]][2:3] != c(-1, -1)))]
  # ,
  # not_ranged=pars[!sapply(pars,
  #                         function(p)
  #                           any(full.strat.ctx$y25_29[[p]][2:3] != c(-1, -1)))]
)

GET.RANGE.FUNC <- function(range) {
  function(par.name, val, strat.name) {
    if (any(startsWith(par.name, c('p_', '.p_', '.rate', '.sensitivity_', '.specificity_', '.survival_', 'survival_', '.u_', 'u')))) {
      return(c(val*max(0, 1-range), min(1, val*(1+range))))
    } else if (any(startsWith(par.name, c('c_', '.c_',  'ly_', '.ly_', '.hr_', '.age_', 'n_')))) {
      return(c(val*max(0, 1-range), val*(1+range)))
    }
  }
}

RANGE.ESTIMATE.FUNCTIONS <- list(
  # standard=function(par.name, val, strat.name) {
  #   # If the original excel dataframe exists, check if there is a valid range:
  #   # a numeric value (not excel formula) different to (-1, -1).
  #   # Otherwise fall back to estimates.
  #   if (exists('excel.strata.df.full')) {
  #     df <- excel.strata.df.full[[strat.name]]
  #     lower <- as.character(df[df$variable == par.name,'lower'])
  #     if (length(lower) == 0) {
  #       # If ranges are not found in current stratum, look in the first
  #       df <- excel.strata.df.full[[1]]
  #       lower <- as.character(df[df$variable == par.name,'lower'])
  #     }
  #     if (length(lower) > 0 && !is.na(suppressWarnings(as.numeric(lower)))) {
  #       upper <- as.character(df[df$variable == par.name,'upper'])
  #       if (lower != '-1' || upper != '-1')
  #         return(c(as.numeric(lower), as.numeric(upper)))
  #     }
  #   }
  #   if (any(startsWith(par.name, c('p_', '.p_', '.rate', '.sensitivity_', '.specificity_', '.survival_', '.u_', 'u')))) {
  #     # These parameters are considered less trustworthy than the rest
  #     if (par.name %in% c('.pipelle_success', '.sensitivity_hysteroscopy_bleeding', '.sensitivity_molecular', '.sensitivity_pipelle')) {
  #       return(c(val*.5, min(1, val*1.5)))
  #     } else {
  #       return(c(val*.75, min(1, val*1.25)))
  #     }
  #   } else if (any(startsWith(par.name, c('c_', '.c_',  'ly_', '.ly_', '.hr_', '.age_')))) {
  #     return(c(val*.75, val*1.25))
  #   }
  # }
  # ,
  # not_ranged_5=GET.RANGE.FUNC(.05)
  # ,
  range_10=GET.RANGE.FUNC(.1)
  ,
  # range_20=GET.RANGE.FUNC(.2)
  # ,
  # range_25=GET.RANGE.FUNC(.25)
  # ,
  range_50=GET.RANGE.FUNC(.5)
  # ,
  # range_100=GET.RANGE.FUNC(1)
)

SIMULATION.OPTIONS <- list(
  # tca=list(
  #   population='hiv_msm',
  #   reference='conventional_t_tca',
  #   strategy='arnme6e7_hpvhr_t_tca',
  #   display.name='ARN HPV-HR (TCA)'
  # ),
  irc=list(
    population='hiv_msm',
    reference='conventional_t_irc',
    strategy='arnme6e7_hpvhr_t_irc',
    display.name='ARN HPV-HR (IRC)'
  )
  # ,
  # arnhpvhr=list(
  #   population='hiv_msm',
  #   reference='arnme6e7_hpvhr_t_irc',
  #   strategy='arnme6e7_hpvhr_t_tca',
  #   display.name='ARN HPV-HR (TCA)'
  # )
  # ,
  # treatment=list(
  #   population='hiv_msm',
  #   reference='conventional_t_tca',
  #   strategy='ascus_lsil_diff_arnme6e7_16_t_tca',
  #   display.name='ASCUS/LSIL diff - ARNME6E7 16 (TCA)'
  # )
)

store.results.dsa <- function(results, dsa.type, population, display.name, filename) {
  time.preffix <- format(Sys.time(), "%Y_%m_%d__%H_%M_")
  output.dir <- paste0(getwd(), '/output/dsa_', dsa.type, '/', population)
  suppressWarnings(dir.create(output.dir, recursive=TRUE))
  write.csv(results$summary, paste0(output.dir, '/', filename, '.csv'), row.names = F)
  suffixes <- c('icer', 'icer_test', 'nhb', 'nhb_test')
  for(i in seq_along(results$plots)) {
    grDevices::cairo_pdf(paste0(output.dir, '/', filename, '_', suffixes[i], '.pdf'),
                         width = 9,
                         height= 15)
    print(results$plots[[i]])
    dev.off()
    # ggsave(paste0(output.dir, '/', filename, '_', i, '.pdf'),
    #        plot = results$plots[[i]],
    #        width = 300,
    #        height = 200,
    #        units = 'mm',
    #        device = 'pdf'
    #        # device = 'cairo_pdf'
    # )
  }
  # ggsave(paste0(output.dir, '/', filename, '_2.pdf'),
  #        plot = results$plot2,
  #        width = 300,
  #        height = 200,
  #        units = 'mm',
  #        device = 'pdf'#cairo_pdf
  # )
  if (dsa.type == 'univariate') {
    for(par in names(results$plot.curves)) {
      grDevices::cairo_pdf(paste0(output.dir, '/', filename, '__', par, '.pdf'))
      print(results$plot.curves[[par]])
      dev.off()
      # ggsave(paste0(output.dir, '/', filename, '__', par, '.pdf'),
      #        plot = results$plot.curves[[par]],
      #        width = 300,
      #        height = 200,
      #        units = 'mm',
      #        device = 'pdf'
      #        # device = 'cairo_pdf'
      # )
    }
  }
  if (dsa.type != 'bivariate') {
    for(i in seq_along(results$plots)) {
      htmlwidgets::saveWidget(ggplotly(results$plots[[i]]),
                              paste0(output.dir, '/', filename, '_', i, '.html'),
                              title=display.name,
                              libdir=paste0(output.dir, '/lib'))
    }
  }

}

######################## Univariate DSA

args <- commandArgs(trailingOnly=TRUE)
if (length(args) == 0) {
  n.cores <- N.CORES
} else {
  n.cores <- as.numeric(args[1])
}
# cluster <- makeCluster(n.cores, outfile='')
# on.exit({
#   stopCluster(cluster)
# }, add=TRUE)
#registerDoParallel(cluster)
cluster <- NULL
cat('********** STARTING UNIVARIATE SENSITIVITY ANALYSIS **********\n')
start <- Sys.time()

for(param.set.name in names(dsa.pars)) {
  param.set <- dsa.pars[[param.set.name]]
  for(option.name in names(SIMULATION.OPTIONS)) {
    options <- SIMULATION.OPTIONS[[option.name]]
    for(range.estimate.name in names(RANGE.ESTIMATE.FUNCTIONS)) {
      range.estimate.func <- RANGE.ESTIMATE.FUNCTIONS[[range.estimate.name]]
      results <- dsa.1(param.set, strat.ctx,
                       options$population, options$strategy,
                       markov,
                       reference=options$reference,
                       excel.file = 'params/context.xlsx',
                       n.param.points = N.PARAM.POINTS.TORNADO,
                       n.cores = n.cores,
                       range.estimate=range.estimate.func,
                       context.setup.func=context.setup,
                       discount.rate = DISCOUNT.RATE)
      filename <- paste0(option.name, '__', range.estimate.name, '_params_', param.set.name)
      store.results.dsa(results, 'univariate', options$population, options$display.name, filename)
    }
  }
}

end <- Sys.time()
cat('********** UNIVARIATE SENSITIVITY ANALYSIS DONE **********\n')
print(end - start)
# stopCluster(cluster)

######################## Bivariate DSA

N.PARAM.POINTS.HEATMAP <- 50

PARAMETER.PAIRS <- list(
  # asymptomatic=list(
  #   c('.sensitivity_pipelle', '.specificity_pipelle'),
  #   c('.sensitivity_molecular', '.specificity_molecular'),
  #   c('.sensitivity_hysteroscopy', '.specificity_hysteroscopy'),
  #   c('.sensitivity_tvu_4mm', '.specificity_tvu_4mm'),
  #   c('.c_molecular_test', '.sensitivity_molecular'),
  #   c('.c_molecular_test', '.specificity_molecular'),
  #   c('.c_molecular_test', '.p_cancer__asymptomatic')
  # )
  # ,
  bleeding=list(
  #   c('.sensitivity_pipelle', '.specificity_pipelle'),
  #   c('.sensitivity_molecular', '.specificity_molecular'),
  #   c('.sensitivity_hysteroscopy___bleeding', '.specificity_hysteroscopy___bleeding'),
  #   c('.sensitivity_tvu_4mm', '.specificity_tvu_4mm'),
  #   c('.c_molecular_test', '.sensitivity_molecular'),
  #   c('.c_molecular_test', '.specificity_molecular'),
  #   c('.c_molecular_test', '.p_cancer__bleeding'),
  #   c('p_survive', 'p_cancer___survive')
    c('.p_bleeding', 'u_hysterectomy')
  )
  # ,
  # lynch=list(
  #   c('.sensitivity_pipelle', '.specificity_pipelle'),
  #   c('.sensitivity_molecular', '.specificity_molecular'),
  #   c('.sensitivity_hysteroscopy', '.specificity_hysteroscopy'),
  #   c('.sensitivity_tvu_10mm', '.specificity_tvu_10mm'),
  #   c('.c_molecular_test', '.sensitivity_molecular'),
  #   c('.c_molecular_test', '.specificity_molecular'),
  #   c('.c_molecular_test', '.p_cancer__lynch')
  # )
)

# for(option.name in names(SIMULATION.OPTIONS)) {
#   options <- SIMULATION.OPTIONS[[option.name]]
#   for(parameter.pair in PARAMETER.PAIRS[[options$population]]) {
#     for(range.estimate.name in names(RANGE.ESTIMATE.FUNCTIONS)) {
#       range.estimate.func <- RANGE.ESTIMATE.FUNCTIONS[[range.estimate.name]]
#       results <- dsa.n(parameter.pair, strat.ctx,
#                        options$population, options$strategy,
#                        markov,
#                        excel.file = 'params/context.xlsx',
#                        n.param.points = N.PARAM.POINTS.HEATMAP,
#                        # n.cores = 1,
#                        range.estimate=range.estimate.func,
#                        context.setup.func=context.setup,
#                        discount.rate = DISCOUNT.RATE)
#       filename <- paste0(options$strategy.name, '__range_', range.estimate.name, '__params_', paste0(parameter.pair, collapse='_'))
#       store.results.dsa(results, 'bivariate', options$population, options$display.name, filename)
#     }
#   }
# }



######################## Multivariate DSA

N.PARAM.POINTS.SCATTER <- 2

# PARAMETER.SETS <- list(
#   asymptomatic=list(
#     c('.sensitivity_pipelle', '.specificity_pipelle', '.sensitivity_molecular'),
#   )
#   ,
#   bleeding=list(
#   )
#   ,
#   lynch=list(
#   )
# )

# for(option.name in names(SIMULATION.OPTIONS)) {
#   options <- SIMULATION.OPTIONS[[option.name]]
#   for(parameter.pair in PARAMETER.SETS[[options$population]]) {
#     for(range.estimate.name in names(RANGE.ESTIMATE.FUNCTIONS)) {
#       range.estimate.func <- RANGE.ESTIMATE.FUNCTIONS[[range.estimate.name]]
#       results <- dsa.n(parameter.pair, strat.ctx,
#                        options$population, options$strategy,
#                        markov,
#                        excel.file = 'params/context.xlsx',
#                        n.param.points = N.PARAM.POINTS.SCATTER,
#                        # n.cores = 1,
#                        range.estimate=range.estimate.func,
#                        context.setup.func=context.setup,
#                        discount.rate = DISCOUNT.RATE))
#       filename <- paste0(options$strategy.name, '__range_', range.estimate.name, '__params_', paste0(parameter.pair, collapse='_'))
#       store.results.dsa(results, 'multivariate', options$population, options$display.name, filename)
#     }
#   }
# }
