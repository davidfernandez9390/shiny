library(dplyr)

source('load_models.R')
source('markov.R')

VARIATION <- 1

# strategy <- 'arnme6e7_hpvhr_t_irc'
# reference <- 'conventional_t_irc'
# strategy <- 'arnme6e7_hpvhr_t_tca'
# reference <- 'conventional_t_tca'
strategy <- 'arnme6e7_hpvhr_t_tca'
reference <- 'arnme6e7_hpvhr_t_irc'

initial.state <- sapply(markov$nodes,
                        function(n) if (n$name=='hiv_positive') 1 else 0)

sim.strats <- strategies$hiv_msm
sim.strats <- sim.strats[names(sim.strats) %in% c(strategy, reference)]

calculate.output <- function(par.val, par.name) {
  strat.ctx.i <- lapply(strat.ctx, function(ctx) {
    ctx[par.name] <- par.val
    ctx
  })
  strat.ctx.i <- refresh.context(par, strat.ctx.i, excel.strata.df)

  x <- simulate('hiv_msm',
                sim.strats,
                markov,
                strat.ctx.i,
                initial.state,
                discount.rate = 0.03)$summary
  return(x)
}

calculate.nhb <- function(x) {
  st <- x[startsWith(x$strategy,strategy),]
  ref <- x[startsWith(x$strategy,reference),]
  nhb <- (st$E-ref$E) - (st$C-ref$C)/25000
  return(nhb)
}

calculate.icer <- function(x) {
  st <- x[startsWith(x$strategy,strategy),]
  ref <- x[startsWith(x$strategy,reference),]
  icer <- (st$C-ref$C)/(st$E-ref$E)
  return(icer)
}

icer.diff <- function(par.val, par.name) {
  x <- calculate.output(par.val, par.name)

  nhb.diff <- (calculate.nhb(x))^2
  # icer.diff <- ((st$C-ref$C)/(st$E-ref$E) - 25000)^2
  # cat(par.val, nhb.diff, icer.diff, '\n')
  print(nhb.diff)
  return(nhb.diff)
}

par.list <- pars[startsWith(pars, 'u_')]
# par.list <- c('u_hiv_p',
#               'p_cyto_b___hsil',
#               'p_arnmhr_p___hsil',
#               'p_undetected_hsil_treatment_whole_followup',
#               'u_cancer_delayed',
#               'c_hra_treatment',
#               'p_arnmhr_p___no_hsil',
#               'p_cyto_b___no_hsil',
#               'c_arn_kit',
#               'p_no_hsil___hsil_tca',
#               'survival_5year',
#               'p_hsil_regression_annual',
#               'c_surgery_delayed',
#               'p_hra_hsil___cyto_hsil__hsil')

# par.list <- c('u_hsil___irc', 'p_cyto_b___hsil', 'p_undetected_hsil_treatment_whole_followup', 'p_arnmhr_p___hsil')
# par.list <- c(
#   'c_cyto',
#   'c_arn_kit',
#   'c_hra_treatment',
#   'c_irc',
#   'c_surgery',
#   'c_surgery_delayed',
#   'p_cyto_b___hsil',
#   'p_cyto_b___no_hsil',
#   'p_cyto_hsil___no_hsil',
#   'p_death_other_annual',
#   'p_arnmhr_p___hsil',
#   'p_arnmhr_p___no_hsil',
#   'p_hra_hsil___cyto_hsil__hsil',
#   'p_hsil_regression_annual',
#   'p_undetected_hsil_treatment_whole_followup',
#   'survival_5year',
#   'p_no_hsil___hsil_irc',
#   'u_hiv_p',
#   'u_hsil___irc',
#   'u_cancer',
#   'u_cancer_delayed'
# )

# IRC
# par.list <- c('c_arn_kit','c_hra_treatment','c_surgery_delayed','p_arnmhr_p___hsil',
#               'p_arnmhr_p___no_hsil','p_cyto_b___hsil','p_cyto_b___no_hsil',
#               'p_hsil_regression_annual','p_no_hsil___hsil_irc',
#               'p_undetected_hsil_treatment_whole_followup','u_cancer_delayed','u_hsil___irc')

# TCA
# par.list <- c('c_arn_kit','c_hra_treatment','c_surgery_delayed','p_arnmhr_p___hsil',
#               'p_arnmhr_p___no_hsil','p_cyto_b___hsil','p_cyto_b___no_hsil',
#               'p_hsil_regression_annual','p_no_hsil___hsil_tca',
#               'p_undetected_hsil_treatment_whole_followup','u_cancer_delayed','u_hsil___tca')

# ARN-HPV-HR
# par.list <- c('u_hsil___irc', 'u_hsil___tca', 'u_hiv_p___irc', 'u_hiv_p___tca',
#               'p_no_hsil___hsil_irc', 'p_no_hsil___hsil_tca')


df <- data.frame()

for(par in par.list) {
  cat('Finding', par, 'value with NHB~0...\n')
  initial.guess <- strat.ctx$y40_44[[par]]

  suppressWarnings(sink())
  sink('/dev/null')
  lower.limit <- initial.guess*(1-VARIATION)
  upper.limit <- ifelse(!any(startsWith(par, c('p', '.p', 'u'))),
                         initial.guess*(1+VARIATION),
                         min(initial.guess*(1+VARIATION), 1))

  res <- optimize(icer.diff,
                  lower=lower.limit,
                  upper=upper.limit,
                  par.name=par,
                  tol=1e-6)

  optim.output <- calculate.output(res$minimum, par)
  sink()
  df <- rbind(df, data.frame(par=par,
                             sq.nhb.diff=res$objective,
                             nhb=calculate.nhb(optim.output),
                             icer=calculate.icer(optim.output),
                             base.value=initial.guess,
                             value=res$minimum))
  print(df)
}

print(df)
# View(df)
df <- df[,c('par', 'icer', 'base.value', 'value')]
write.xlsx(df, 'output/results/critical_values.xlsx')
