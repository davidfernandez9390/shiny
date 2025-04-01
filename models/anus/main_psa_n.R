library(officer)
library(rvg)
library(magrittr)

setwd('~/Documents/models_ce/anus')

source('load_models.R')
source('markov_psa.R')

PSA.SEED <- 1234
N.ITERS <- 1000
N.CORES <- 8
DISCOUNT.RATE <- .03
DEBUG <- F  # 1 core if TRUE

JITTER.X <- .05
JITTER.Y <- .05

EXCLUDED.PARAMS <- names(strat.ctx$y25_29[strat.ctx$y25_29 %in% c(0,1)])
EXCLUDED.PARAMS <- c(EXCLUDED.PARAMS, 
                     'periodicity_months'
)

pars <- independent.pars
pars <- pars[!pars %in% EXCLUDED.PARAMS]

args <- commandArgs(trailingOnly=TRUE)
if (length(args) == 0) {
  n.cores <- N.CORES
} else {
  n.cores <- as.numeric(args[1])
}

GET.SD.FUNC <- function(divisor) {
  function(p.name, value) value/divisor
}

SD.ESTIMATE.FUNCTIONS <- list(
  # sd_10=GET.SD.FUNC(10)
  # ,
  sd_5=GET.SD.FUNC(5)
  # ,
  # sd_2=GET.SD.FUNC(2)
  # ,
  # sd_1=GET.SD.FUNC(1)
  # ,
  # medium=function(par.name, value) {
  #   # These parameters are considered less trustworthy than the rest
  #   if (par.name %in% c('.pipelle_success',
  #                  '.sensitivity_hysteroscopy_bleeding',
  #                  '.sensitivity_molecular',
  #                  '.sensitivity_pipelle')) {
  #     return(value/10)
  #   } else {
  #     return(value/20)
  #   }
  # }
)

SIMULATION.OPTIONS <- list(
  followup=list(
    population='hiv_msm',
    reference='conventional',
    strategy='ascus_lsil_diff_arnme6e7_16',
    reference.name='Conventional',
    strategy.name='ASCUS/LSIL diff - ARNME6E7 16'
  )
  # ,
  # treatment=list(
  #   population='hiv_msm',
  #   reference='conventional_t_tca',
  #   strategy='ascus_lsil_diff_arnme6e7_16_t_tca',
  #   reference.name='Conventional (TCA)',
  #   strategy.name='ASCUS/LSIL diff - ARNME6E7 16 (TCA)'
  # )
)

psa.pars <- list(
  # all=pars
  # ,
  costs=pars[startsWith(pars, 'c_')]
  # ,
  # utilities=pars[startsWith(pars, 'u_')]
  # ,
  # probs=pars[startsWith(pars, 'p_') |
  #              startsWith(pars, 'survival_')]
)

param.names <- c(
  .specificity_molecular='Molecular test specificity',
  .sensitivity_molecular='Molecular test sensitivity',
  .specificity_molecular_cyto='Specificity of molecular test (cytology)',
  .sensitivity_molecular_cyto='Sensitivity of molecular test (cytology)',
  .specificity_molecular_pipelle='Specificity of molecular test (pipelle)',
  .sensitivity_molecular_pipelle='Sensitivity of molecular test (pipelle)',
  u_hysterectomy='Non-cancer hysterectomy utility',
  .p_cancer___bleeding='Prevalence of EC in women with PMB',
  .sensitivity_tvu='Sensitivity of TVU',
  .sensitivity_pipelle___bleeding='Sensitivity of pipelle',
  .c_molecular_test='Cost of molecular test',
  u_cancer='EC utility',
  u_bleeding='PMB utility',
  .specificity_pipelle='Specificity of pipelle',
  .p_cancer___asymptomatic='Prevalence of EC',
  .specificity_tvu='Specificity of TVU',
  # .survival_5year___stage1='5-year survival (stage 1)',
  .survival_5year='5-year EC survival',
  .p_pipelle_success='Pipelle success probability',
  .p_cancer_stage_1='Percentage of EC cases at stage 1',
  .specificity_hysteroscopy___bleeding='Specificity of hysteroscopy',
  .sensitivity_hysteroscopy___bleeding='Sensitivity of hysteroscopy',
  .p_bmi_high='Proportion of obese women (BMI > 30)',
  .c_tvu='Cost of TVU',
  .c_hysterectomy___stage_1='Cost of hysterectomy, EC stage I (€)',
  .c_hysterectomy___stage_2_4='Cost of hysterectomy, EC stage II-IV (€)',
  c_phone_visit='Cost of phone visit',
  .c_pipelle='Cost of pipelle',
  .p_bleeding__cancer='Prevalence of PMB in women with EC',
  .c_hysteroscopy='Cost of hysteroscopy',
  .c_visit='Cost of visit',
  .c_treatment='Cost of treatment (€)',
  u_cancer_s1='EC utility (stage I)',
  u_cancer_s2='EC utility (stage II)',
  u_cancer_s3='EC utility (stage III)',
  u_cancer_s4='EC utility (stage IV)',
  u_undetected_cancer='Undetected EC utility',
  .survival_5year_s1='5-year survival (stage I)',
  .survival_5year_s2='5-year survival (stage II)',
  .survival_5year_s3='5-year survival (stage III)',
  .survival_5year_s4='5-year survival (stage IV)',
  .p_bleeding='Probability of bleeding persistence',
  .p_pipelle_tissue_success='Probability of pipelle success (tissue)',
  .p_pipelle_insertion_success='Probability of pipelle success (insertion)',
  .p_pipelle_insertion_success___parous='Probability of pipelle success (insertion)',
  .rate_recurrence_s1='Recurrence rate (stage I)',
  .rate_recurrence_s2='Recurrence rate (stage II)',
  .rate_recurrence_s3='Recurrence rate (stage III)',
  .rate_recurrence_s4='Recurrence rate (stage IV)',
  .p_progression_cancer_s1_2='Annual cancer progression (stage I-II)',
  .p_progression_cancer_s2_3='Annual cancer progression (stage II-III)',
  .p_progression_cancer_s3_4='Annual cancer progression (stage III-IV)',
  .p_death_other='Probability of death from other causes',
  .c_cytology='Cost of cytology',
  .c_first_visit='Cost of first visit',
  .rate_prevalence='Prevalence rate of EC (global)',
  p_death_other='Probability of death from other causes',
  .p_bleeding___cancer='Prevalence of PMB in women with EC',
  .hr_bmi='HR BMI',
  .p_cancer___bleeding_pooled='Probability of EC in PMB',
  .p_cancer___postmenopausal='Probability of EC in postmenopausal women',
  Discount='Discount'
)




#### MULTIVARIATE PSAS

RESUME.PAR <- ''

trees.all <- trees

if (DEBUG) {
  n.cores <- 1
  cl <- NULL
} else {
  out.file <- ifelse(Sys.getenv('SLURM_JOB_ID') == '', 'workers.out', paste0('workers-', Sys.getenv('SLURM_JOB_ID'), '.out'))
  cl <- makeCluster(n.cores, outfile=out.file)
  on.exit({
    stopCluster(cl)
  }, add=TRUE)
  registerDoParallel(cl)
  
  varlist <- ls(pos=1)
  # varlist <- varlist[!varlist %in% c('results', 'trees')]
  
  clusterExport(cl=cl,varlist=varlist[(varlist %in% c('conventional', 'ascus_lsil_diff_arnme6e7_16')) | (!startsWith(varlist, 'conventional_') & !startsWith(varlist, 'ascus_lsil_diff') & !startsWith(varlist, 'arnme6e7') & !varlist %in% c('trees', 'strategies'))],
                envir=environment())
  clusterEvalQ(cl=cl,{
    library(data.table)
    library(stringr)
    library(ggplot2)
    library(plotly)
    library(pbapply)
  })
}

for(param.set.name in names(psa.pars)) {
  param.set <- psa.pars[[param.set.name]]
  if (RESUME.PAR != '')
    param.set <- param.set[match(RESUME.PAR, param.set):length(param.set)]
  
  for(option.name in names(SIMULATION.OPTIONS)) {
    trees <- trees.all
    options <- SIMULATION.OPTIONS[[option.name]]
    output.dir <- paste0(getwd(), '/output/psa_multivariate/', options$population)
    
    if (!is.null(cl)) {
      # We prune the 'tree' list to avoid copying it to each process
      trees <- trees[names(trees) %in% c(options$strategy, options$reference) | startsWith(names(trees), 'semestral_')]
      clusterExport(cl=cl,varlist='trees',
                    envir=environment())
    }
    
    for(sd.estimate.name in names(SD.ESTIMATE.FUNCTIONS)) {
      sd.estimate.func <- SD.ESTIMATE.FUNCTIONS[[sd.estimate.name]]
      csv.data <- data.frame()
      filename <- paste0(options$strategy, '__sd_', sd.estimate.name, '__par_', param.set.name)
      
      if (!is.null(cl))
        clusterExport(cl=cl,varlist=c('param.set', 'options', 'sd.estimate.func'),
                      envir=environment())
      
      results <- psa.n(param.set, 
                       strat.ctx,
                       options$population, 
                       options$strategy,
                       options$reference,
                       markov,
                       excel.file = 'params/context.xlsx',
                       sd.estimate.func=sd.estimate.func,
                       context.setup.func=context.setup,
                       n.cores = n.cores,
                       cluster = cl,
                       n.iters=N.ITERS,
                       seed=PSA.SEED,
                       discount.rate=DISCOUNT.RATE,
                       jitter.x=JITTER.X,
                       jitter.y=JITTER.Y)
      
      # Remove constant parameters
      # for(col in names(results$summary)) {n
      #   if (length(unique(results$summary[[col]])) == 1)
      #     results$summary[[col]] <- NULL
      # }
      # Remove all strata but one for non-stratified parameters
      # par.cols <- names(results$summary)
      # for(p in param.set) {
      #   p.cols <- par.cols[endsWith(par.cols, paste0('_',p))]
      #   p.df <- results$summary[p.cols]
      #   p.is.stratified <- !all(sapply(1:ncol(p.df), function(c) all(p.df[1]==p.df[c])))
      #   if (!p.is.stratified)
      #     results$summary[p.cols[2:length(p.cols)]] <- NULL
      # }
# browser()
      # filename <- paste0(output.dir, '/', filename, '.csv')
      # write.csv(results$summary, filename)
      # results <- NULL
      # 
      # df.results <- data.frame()
      # for(f in list.files(output.dir, pattern = paste0(filename, '\\d*.csv'))) {
      #   df.results <- rbind(df.results, read.csv(paste0(output.dir, '/', f)))
      #   unlink(paste0(output.dir, '/', f))
      # }
      # results <- generate.psa.summary(df.results, options$strategy, options$population, options$reference, strat.ctx, param.set, jitter.x=JITTER.X, jitter.y=JITTER.Y)
    
      filename <- paste0(options$strategy, '__sd_', sd.estimate.name, '__par_', param.set.name)
      store.results.psa(results, 'multivariate', options$population, options$strategy.name, filename)
      
      build.plots('multivariate', options$population, options$strategy, options$reference, strat.ctx, options, sd.estimate.name, suffix=param.set.name, param.set.name=param.set.name)
    }
  }
}


