library(CEAModel)
library(ggplot2)
library(reshape2)
library(plotly)
library(dplyr)
library(pbapply)
library(doParallel)
library(doFuture)

EPSILON <- 1e-7  # Maximum allowed error due to precision errors
EXCEL.START.AGE <- 25  # Start age for the first sheet of the excel context file
# Do linear interpolation of contiguous strata when current simulation period falls between two of them
INTERPOLATE.STRATA <- TRUE

DISCOUNT.RATE <- .03

DEFAULT.START.AGE <- list(
  hiv_msm=40
)
DEFAULT.MAX.AGE <- list(
  hiv_msm=80
)

if (INTERPOLATE.STRATA) {
  get.context.stratum <- function(strat.context, year, period) {
    current.stratum <- ((year-EXCEL.START.AGE) %/% 5) + 1
    prev.stratum <- ((year-period-EXCEL.START.AGE) %/% 5) + 1
    if (current.stratum == prev.stratum) {
      ctx <- strat.context[[min(prev.stratum, length(strat.context))]]
    } else {
      current.w <- year %% 5
      prev.w <- period - year %% 5
      current.ctx <- strat.context[[min(current.stratum, length(strat.context))]]
      prev.ctx <- strat.context[[min(prev.stratum, length(strat.context))]]
      ctx <- lapply(names(current.ctx), function(n) {
        return((current.w * current.ctx[[n]] + prev.w * prev.ctx[[n]]) / period)
      })
      names(ctx) <- names(current.ctx)
    }
    return(ctx)
  }

  get.matrix.stratum <- function(strat.matrices, year, period) {
    current.stratum <- ((year-EXCEL.START.AGE) %/% 5) + 1
    prev.stratum <- ((year-period-EXCEL.START.AGE) %/% 5) + 1
    if (current.stratum == prev.stratum) {
      mtx <- strat.matrices[[min(prev.stratum, length(strat.matrices))]]
    } else {
      current.w <- year %% 5
      prev.w <- period - year %% 5
      current.mtx <- strat.matrices[[min(current.stratum, length(strat.matrices))]]
      prev.mtx <- strat.matrices[[min(prev.stratum, length(strat.matrices))]]
      mtx <- lapply(names(current.mtx), function(n) {
        return((current.w * current.mtx[[n]] + prev.w * prev.mtx[[n]]) / period)
      })
      names(mtx) <- names(current.mtx)
    }
    return(mtx)
  }
} else {
  get.context.stratum <- function(strat.context, year, period) {
    year <- floor(year-period+EPSILON)  #  TODO: Numerical instability, find more robust alternative
    ctx <- strat.context[[min(((year-EXCEL.START.AGE) %/% 5) + 1, length(strat.context))]]
    return(ctx)
  }

  get.matrix.stratum <- function(strat.matrices, year, period) {
    year <- floor(year-period+EPSILON)  #  TODO: Numerical instability, find more robust alternative
    mtx <- strat.matrices[[min(((year-EXCEL.START.AGE) %/% 5) + 1, length(strat.matrices))]]
    return(mtx)
  }
}

# get.context.stratum <- function(strat.context, year) {
#   ctx <- strat.context[[min(((year-25) %/% 5) + 1, length(strat.context))]]
#   return(ctx)
# }
#
# get.matrix.stratum <- function(strat.matrices, year) {
#   mtx <- strat.matrices[[min(((year-25) %/% 5) + 1, length(strat.matrices))]]
#   return(mtx)
# }

setup.markov <- function(trees, strat.ctx) {
  costs <- list()
  utilities <- list()
  tpMatrices <- list()
  extended.strat.ctx <- list()
  for(stratum in names(strat.ctx)) {
    . <- function(x) {
      if (is.na(x) || length(x) == 0) {
        return(0)
      }
      else return(x)
    }

    costs[[stratum]] <- numeric(0)
    utilities[[stratum]] <- numeric(0)

    context.full <- strat.ctx[[stratum]]
    context <- lapply(context.full, function(e) e[1]) # Base values

    # Parameter handling between types of HSIL treatment (IRC/TCA)
    if (endsWith(trees$hiv_msm$name, '_t_tca')) {
      context$c_treatment <- context$c_tca
      context$c_followup_treatment <- context$c_followup_tca
      context$u_hiv_p___treatment <- context$u_hiv_p___tca
      context$u_hsil___treatment <- context$u_hsil___tca
      context$hr_cancer_treatment <- context$hr_cancer_tca
      context$p_hsil_regression___semestral_followup_hsil_treatment <- context$p_hsil_regression___semestral_followup_hsil_tca
    } else if (endsWith(trees$hiv_msm$name, '_t_irc')) {
      context$c_treatment <- context$c_irc
      context$c_followup_treatment <- context$c_followup_irc
      context$u_hiv_p___treatment <- context$u_hiv_p___irc
      context$u_hsil___treatment <- context$u_hsil___irc
      context$hr_cancer_treatment <- context$hr_cancer_irc
      context$p_hsil_regression___semestral_followup_hsil_treatment <- context$p_hsil_regression___semestral_followup_hsil_irc
    } else {
      # Unused parameters when no treatment is simulated, but they need to be defined
      context$u_hiv_p___treatment <- context$u_hiv_p
      context$u_hsil___treatment <- context$u_hsil
      context$hr_cancer_treatment <- 1
      context$p_hsil_regression___semestral_followup_hsil_treatment <- 0
    }

    # Assign markov probabilities according to tree outcomes
    # Outcomes for non-HSIL
    outcomes <- trees$hiv_msm$summarize(context, prevalence=0)
    row.names(outcomes) <- outcomes$name

    context$p_semestral_followup___no_hsil <- .(sum(outcomes[c('semestral_followup_no_hsil'), 'prob']))
    context$p_semestral_followup_treatment___no_hsil <- .(sum(outcomes[c('semestral_followup_no_hsil_treatment'), 'prob']))
    context$p_surgery_no_cancer <- .(sum(outcomes[c('surgery_no_cancer'), 'prob']))

    # Outcomes for HSIL
    outcomes.undetected <- trees$hiv_msm$summarize(context, prevalence=1)
    row.names(outcomes.undetected) <- outcomes.undetected$name

    context$p_semestral_followup___hsil <- .(sum(outcomes.undetected[c('semestral_followup_hsil', 'semestral_followup_hsil___hra'), 'prob']))
    context$p_semestral_followup_treatment___hsil <- .(sum(outcomes.undetected[c('semestral_followup_hsil_treatment'), 'prob']))
    # context$p_cancer___undetected_hsil <- .(sum(outcomes.undetected[c('surgery_cancer'), 'prob']))
    context$p_surgery_no_cancer___undetected_hsil <- .(sum(outcomes.undetected[c('surgery_no_cancer'), 'prob']))


    # Count HSIL for incidence purposes only if diagnosed by HRA (true positives)
    # Assuming undetected HSILs for no intervention strategy
    if (trees$hiv_msm$name == 'no_intervention') {
      context$detection_new_hsil <- context$p_undetected_hsil
    } else {
      context$detection_new_hsil <- .(sum(outcomes.undetected[c('semestral_followup_hsil_treatment', 'semestral_followup_hsil___hra'), 'prob'], na.rm=TRUE))
    }
    # context$detection_new_hsil <- 1 - (1 - context$detection_new_hsil)^2

    # Outcomes for semestral followup (no HSIL)
    outcomes.sem <- trees$semestral_followup$summarize(context, prevalence=0)
    row.names(outcomes.sem) <- outcomes.sem$name

    context$p_hsil_detected___semestral_followup_no_hsil <- .(sum(outcomes.sem[c('semestral_followup_hsil'), 'prob']))
    context$p_hsil_detected___semestral_followup_no_hsil_treatment <- .(sum(outcomes.sem[c('semestral_followup_hsil_treatment'), 'prob']))
    context$p_surgery_no_cancer___semestral_followup_no_hsil <- .(sum(outcomes.sem[c('surgery_no_cancer'), 'prob']))
    # Assuming IRC does not affect cancer probabilities
    context$p_surgery_no_cancer___semestral_followup_treatment_no_hsil <- .(sum(outcomes.sem[c('surgery_no_cancer'), 'prob']))

    # Outcomes for semestral followup (HSIL)
    outcomes.sem.hsil <- trees$semestral_followup$summarize(context, prevalence=1)
    row.names(outcomes.sem.hsil) <- outcomes.sem.hsil$name

    context$p_hsil_detected___semestral_followup_hsil <- .(sum(outcomes.sem.hsil[c('semestral_followup_hsil'), 'prob']))
    context$p_hsil_detected___semestral_followup_hsil_treatment <- .(sum(outcomes.sem.hsil[c('semestral_followup_hsil_treatment'), 'prob']))
    context$p_cancer___semestral_followup_hsil <- .(sum(outcomes.sem.hsil[c('surgery_cancer'), 'prob']))

    context$p_cancer___semestral_followup_treatment_hsil <- .(sum(outcomes.sem.hsil[c('surgery_cancer'), 'prob'])) / context$hr_cancer_treatment
    context$p_surgery_no_cancer___semestral_followup_hsil <- .(sum(outcomes.sem.hsil[c('surgery_no_cancer'), 'prob']))
    context$p_surgery_no_cancer___semestral_followup_treatment_hsil <- .(sum(outcomes.sem.hsil[c('surgery_no_cancer'), 'prob']))

    # Replace parameters that change with treatment
    if (grepl('_t_.{3}$', trees$hiv_msm$name, perl=TRUE)) {
      context$p_hsil___semestral_followup_no_hsil <- context$p_hsil___semestral_followup_no_hsil_treatment
      context$p_hsil___semestral_followup_no_hsil_treatment <- context$p_hsil___semestral_followup_no_hsil_treatment
      context$p_undetected_hsil <- context$p_undetected_hsil_treatment
    }

    cost.hiv_msm <- weighted.mean(outcomes$cost, outcomes$prob)
    cost.undetected.hsil <- weighted.mean(outcomes.undetected$cost, outcomes.undetected$prob)
    cost.sem.followup <- weighted.mean(outcomes.sem$cost, outcomes.sem$prob)
    cost.sem.followup.hsil <- weighted.mean(outcomes.sem.hsil$cost, outcomes.sem.hsil$prob)

    extended.strat.ctx[[stratum]] <- context
    tpMatrices[[stratum]] <- markov$evaluateTpMatrix(context)
  # print(tpMatrices[[stratum]])
  # print(stratum)

    costs[[stratum]]['hiv_positive'] <- cost.hiv_msm
    costs[[stratum]]['undetected_hsil'] <- cost.undetected.hsil
    costs[[stratum]]['cancer_delayed'] <- context$c_surgery_delayed

    # Check if name finishes in "_t_xxx"
    if (grepl('_t_.{3}$', trees$hiv_msm$name, perl=TRUE)) {
      costs[[stratum]]['semestral_followup1_treatment_no_hsil'] <- cost.sem.followup
      costs[[stratum]]['semestral_followup2_treatment_no_hsil'] <- cost.sem.followup
      costs[[stratum]]['semestral_followup1_treatment_hsil'] <- cost.sem.followup.hsil
      costs[[stratum]]['semestral_followup2_treatment_hsil'] <- cost.sem.followup.hsil
    }
    else {
      costs[[stratum]]['semestral_followup1_no_hsil'] <- cost.sem.followup
      costs[[stratum]]['semestral_followup2_no_hsil'] <- cost.sem.followup
      costs[[stratum]]['semestral_followup1_hsil'] <- cost.sem.followup.hsil
      costs[[stratum]]['semestral_followup2_hsil'] <- cost.sem.followup.hsil
    }

    for(matrix.name in names(tpMatrices)) {
      missing.names <- mapply(function(e){if (is.na(as.numeric(e)) && e != '#') e else NA}, tpMatrices[[matrix.name]][[stratum]])
      missing.names <- missing.names[!is.na(missing.names)]
      missing.names <- missing.names[!duplicated(missing.names)]
      if (length(missing.names) > 0)
        stop(paste0('Variables are missing in matrix: ', paste0(missing.names, collapse=', ')))
    }
  }

  return(list(tpMatrices=tpMatrices,
              strat.ctx=extended.strat.ctx,
              costs=costs,
              utilities=utilities))
}

calculate.iteration.measures <- function(trees, additional.info, year, iter, current.state, tpMatrix, cost, eff, ctx) {
  strat <- trees[[1]]$name
  cs.df <- as.data.frame(current.state)

  n_healthy <- sum(current.state[1,c('hiv_positive', 'hiv_positive_annual_followup1', 'hiv_positive_annual_followup2',
                                   'semestral_followup1_no_hsil', 'semestral_followup2_no_hsil', 'semestral_followup1_treatment_no_hsil',
                                   'semestral_followup2_treatment_no_hsil', 'surgery_no_cancer')])
  n_cancers <- sum(current.state[1,'cancer'])
  n_hsils <- sum(current.state[1,c('semestral_followup1_hsil', 'semestral_followup2_hsil',
                                   'semestral_followup1_treatment_hsil', 'semestral_followup2_treatment_hsil',
                                   'undetected_hsil', 'undetected_hsil_annual_followup1', 'undetected_hsil_annual_followup2')])

  cancer.states <- c('cancer', 'cancer_delayed')
  n_new_cancers <- sum(cs.df[!names(cs.df) %in% 'cancer'] * tpMatrix$strategy[!names(cs.df) %in% 'cancer', 'cancer'])
  n_new_cancers_delayed <- sum(cs.df[!names(cs.df) %in% 'cancer_delayed'] * tpMatrix$other[!names(cs.df) %in% 'cancer_delayed', 'cancer_delayed'])
  incidence_cancer <- (n_new_cancers + n_new_cancers_delayed) / sum(cs.df[!names(cs.df) %in% c('death_cancer', 'death_other')])

  n_new_deaths_cancer <- sum(cs.df[!names(cs.df) %in% 'death_cancer'] * tpMatrix$other[!names(cs.df) %in% 'death_cancer', 'death_cancer'])

  n_new_detected_false_hsils <- cs.df[['hiv_positive']] *
                                  sum(tpMatrix$strategy['hiv_positive',
                                                        c('semestral_followup1_no_hsil', 'semestral_followup1_treatment_no_hsil')])
  n_new_detected_true_hsils <- cs.df[['undetected_hsil']] *
                                  sum(tpMatrix$strategy['undetected_hsil',
                                                        c('semestral_followup1_hsil', 'semestral_followup1_treatment_hsil')])
  n_new_undetected_hsils <- sum(cs.df[c('hiv_positive', 'hiv_positive_annual_followup1', 'hiv_positive_annual_followup2')] *
                                  tpMatrix$other[c('hiv_positive', 'hiv_positive_annual_followup1', 'hiv_positive_annual_followup2'),
                                                 'undetected_hsil'])

  # if (strat == 'no_intervention')
  #   n_new_undetected_hsils <- n_new_undetected_hsils * 5
  n_new_semestral_followup_hsils <- cs.df[['undetected_hsil']] * tpMatrix$strategy['undetected_hsil', 'semestral_followup1_hsil'] +
    cs.df[['undetected_hsil']] * tpMatrix$strategy['undetected_hsil', 'semestral_followup1_treatment_hsil']
  n_new_semestral_followup_no_hsils <- cs.df[['hiv_positive']] * tpMatrix$strategy['hiv_positive', 'semestral_followup1_no_hsil'] +
    cs.df[['hiv_positive']] * tpMatrix$strategy['hiv_positive', 'semestral_followup1_treatment_no_hsil']
  n_new_surgeries_no_cancer <- sum(cs.df[!names(cs.df) %in% 'surgery_no_cancer'] * tpMatrix$strategy[!names(cs.df) %in% 'surgery_no_cancer', 'surgery_no_cancer'])

  # Including undetected, otherwise no-intervention strategy would be zero
  # incidence_hsil <- (n_new_detected_false_hsils + n_new_detected_true_hsils + n_new_undetected_hsils) / sum(cs.df[!names(cs.df) %in% c('death_cancer', 'death_other', 'cancer', 'cancer_delayed', 'survive', 'surgery_no_cancer')])

  # if (trees$hiv_msm$name == 'no_intervention') {
  #   incidence_hsil <- ctx$detection_new_hsil
  # } else {
  #   incidence_hsil <- cs.df[['undetected_hsil']] * ctx$detection_new_hsil
  # }
  # browser()
  incidence_hsil <- ctx$detection_new_hsil

  if (strat == 'no_intervention') {
    n_cyto <- 0
  } else {
    n_cyto <- sum(cs.df[c('hiv_positive',
                           'undetected_hsil',
                           'semestral_followup1_no_hsil',
                           'semestral_followup2_no_hsil',
                           'semestral_followup1_hsil',
                           'semestral_followup2_hsil',
                           'semestral_followup1_treatment_no_hsil',
                           'semestral_followup2_treatment_no_hsil',
                           'semestral_followup1_treatment_hsil',
                           'semestral_followup2_treatment_hsil')])
  }

  n_hpv <- 0
  if (startsWith(strat, 'conventional_hpv') || startsWith(strat, 'arnme6e7')) {
    n_hpv <- sum(cs.df[c('hiv_positive', 'undetected_hsil')])
  } else if (startsWith(strat, 'ascus_lsil_diff')) {
    n_hpv <- cs.df[['hiv_positive']] * ctx$p_cyto_ascus_or_lsil___no_hsil + cs.df[['undetected_hsil']] * ctx$p_cyto_ascus_or_lsil___hsil
  }

  test.name <- list(
    arnme6e7_hpv16='arnm16',
    arnme6e7_hpv161845='arnm161845',
    arnme6e7_hpvhr='arnmhr',
    ascus_lsil_diff_arnme6e7_hpv16='arnm16',
    ascus_lsil_diff_arnme6e7_hpv161845='arnm161845',
    ascus_lsil_diff_arnme6e7_hpvhr='arnmhr',
    ascus_lsil_diff_hpv1618la='hpv1618la',
    ascus_lsil_diff_hpv16la='hpv16la',
    ascus_lsil_diff_hpvhrhc='hpvhrhc',
    ascus_lsil_diff_hpvhrla='hpvhrla',
    conventional_hpv1618la='hpv1618la',
    conventional_hpv16la='hpv16la',
    conventional_hpvhrhc='hpvhrhc',
    conventional_hpvhrla='hpvhrla'
  )
  test.name.t1 <- test.name
  names(test.name.t1) <- paste0(names(test.name.t1), '_t_tca')
  test.name <- append(test.name, test.name.t1)
  test.name.t2 <- test.name
  names(test.name.t2) <- paste0(names(test.name.t2), '_t_irc')
  test.name <- append(test.name, test.name.t2)

  if (strat %in% c('conventional', 'conventional_t_tca', 'conventional_t_irc')) {
    n_hra_no_hsil <- cs.df[[c('hiv_positive')]] * (1 - ctx$p_cyto_b___no_hsil)
    n_hra_hsil <- cs.df[['undetected_hsil']] * (1 - ctx$p_cyto_b___hsil)
  } else if (startsWith(strat, 'conventional_hpv') || startsWith(strat, 'arnme6e7')) {
    p.hpv.p_hsil <- ctx[[paste0('p_', test.name[[strat]], '_p___hsil')]]
    p.hpv.p_no.hsil <- ctx[[paste0('p_', test.name[[strat]], '_p___no_hsil')]]

    n_hra_no_hsil <- cs.df[[c('hiv_positive')]] * (p.hpv.p_no.hsil + (1 - p.hpv.p_no.hsil) * (1 - ctx$p_cyto_b___no_hsil)) +
      sum(cs.df[c('semestral_followup1_no_hsil',
                  'semestral_followup2_no_hsil',
                  'semestral_followup1_treatment_no_hsil',
                  'semestral_followup2_treatment_no_hsil')])
    n_hra_hsil <- cs.df[['undetected_hsil']] * (p.hpv.p_hsil + (1 - p.hpv.p_hsil) * (1 - ctx$p_cyto_b___hsil)) +
      sum(cs.df[c('semestral_followup1_hsil',
                  'semestral_followup2_hsil',
                  'semestral_followup1_treatment_hsil',
                  'semestral_followup2_treatment_hsil')])
  } else if (startsWith(strat, 'ascus_lsil_diff')) {
    p.hpv.p_hsil <- paste0('p_', test.name[[strat]], '_p___hsil')
    p.hpv.p_no.hsil <- paste0('p_', test.name[[strat]], '_p___no_hsil')
    p.cyto.hsil_hsil <- (1 - ctx$p_cyto_b___hsil - ctx$p_cyto_ascus_or_lsil___hsil)
    p.cyto.hsil_no.hsil <- (1 - ctx$p_cyto_b___no_hsil - ctx$p_cyto_ascus_or_lsil___no_hsil)

    n_hra_no_hsil <- cs.df[[c('hiv_positive')]] * (p.cyto.hsil_no.hsil + ctx$p_cyto_ascus_or_lsil___no_hsil * ctx[[p.hpv.p_no.hsil]]) +
      sum(cs.df[c('semestral_followup1_no_hsil',
                  'semestral_followup2_no_hsil',
                  'semestral_followup1_treatment_no_hsil',
                  'semestral_followup2_treatment_no_hsil')])
    n_hra_hsil <- cs.df[['undetected_hsil']] * (p.cyto.hsil_hsil + ctx$p_cyto_ascus_or_lsil___hsil * ctx[[p.hpv.p_hsil]]) +
      sum(cs.df[c('semestral_followup1_hsil',
                  'semestral_followup2_hsil',
                  'semestral_followup1_treatment_hsil',
                  'semestral_followup2_treatment_hsil')])
  } else if (strat == 'no_intervention') {
    n_hra_no_hsil <- 0
    n_hra_hsil <- 0
  }

  n_treatment_no_hsil <- 0
  n_treatment_hsil <- 0
  if (grepl('_t_.{3}$', strat, perl=TRUE)) {
    n_treatment_no_hsil <- n_hra_no_hsil * (
      ctx$p_cyto_hsil___no_hsil * (1 - ctx$p_hra_invasive_cancer___cyto_hsil__no_hsil) * ctx$p_hra_hsil___cyto_hsil__no_hsil +
      (1 - ctx$p_cyto_b___no_hsil - ctx$p_cyto_hsil___no_hsil) * (1 - ctx$p_hra_invasive_cancer___cyto_no_hsil__no_hsil) * ctx$p_hra_hsil___cyto_no_hsil__no_hsil
       )
    n_treatment_hsil <- n_hra_hsil * (
      ctx$p_cyto_hsil___hsil * (1 - ctx$p_hra_invasive_cancer___cyto_hsil__hsil) * ctx$p_hra_hsil___cyto_hsil__hsil +
        (1 - ctx$p_cyto_b___hsil - ctx$p_cyto_hsil___hsil) * (1 - ctx$p_hra_invasive_cancer___cyto_no_hsil__hsil) * ctx$p_hra_hsil___cyto_no_hsil__hsil
    )
  }

  additional.info <- rbind(additional.info,
                           list(
                             year=year,
                             iter=iter,
                             cost=cost,
                             eff=eff,
                             n_healthy=n_healthy,
                             n_hsils=n_hsils,
                             n_cancers=n_cancers,
                             incidence_cancer=incidence_cancer,
                             incidence_hsil=incidence_hsil,
                             n_new_cancers=n_new_cancers,
                             n_new_cancers_delayed=n_new_cancers_delayed,
                             n_new_deaths_cancer=n_new_deaths_cancer,
                             n_new_detected_false_hsils=n_new_detected_false_hsils,
                             n_new_detected_true_hsils=n_new_detected_true_hsils,
                             n_new_undetected_hsils=n_new_undetected_hsils,
                             n_new_surgeries_no_cancer=n_new_surgeries_no_cancer,
                             n_new_semestral_followup_hsils=n_new_semestral_followup_hsils,
                             n_new_semestral_followup_no_hsils=n_new_semestral_followup_no_hsils,
                             n_cyto=n_cyto,
                             n_hpv=n_hpv,
                             n_hra_no_hsil=n_hra_no_hsil,
                             n_hra_hsil=n_hra_hsil,
                             n_treatment_no_hsil=n_treatment_no_hsil,
                             n_treatment_hsil=n_treatment_hsil))
  return(additional.info)
}

simulate.markov <- function(trees,
                            markov,
                            initial.state,
                            strat.ctx,
                            start.age=DEFAULT.START.AGE['hiv_msm'],
                            max.age=DEFAULT.MAX.AGE['hiv_msm'],
                            iters.per.year=2,
                            discount.rate=DISCOUNT.RATE) {
  cat(paste0(' Simulating ', trees$hiv_msm$name, '...\n'))
  additional.info <- data.frame(year=start.age,
                                iter=1,
                                cost=0,
                                eff=0,
                                n_healthy=1,
                                n_hsils=0,
                                n_cancers=0,
                                incidence_cancer=0,
                                incidence_hsil=0,
                                n_new_cancers=0,
                                n_new_cancers_delayed=0,
                                n_new_deaths_cancer=0,
                                n_new_detected_false_hsils=0,
                                n_new_detected_true_hsils=0,
                                n_new_undetected_hsils=0,
                                n_new_surgeries_no_cancer=0,
                                n_new_semestral_followup_hsils=0,
                                n_new_semestral_followup_no_hsils=0,
                                n_cyto=0,
                                n_hpv=0,
                                n_hra_no_hsil=0,
                                n_hra_hsil=0,
                                n_treatment_no_hsil=0,
                                n_treatment_hsil=0)

  period <- 1/iters.per.year

  setup.result <- setup.markov(trees, strat.ctx)
  tpMatrices <- setup.result$tpMatrices
  strat.ctx <- setup.result$strat.ctx
  costs <- setup.result$costs
  # utilities <- setup.result$utilities

  # ASSUMPTION: all node info is equal for all strata
  ctx <- get.context.stratum(strat.ctx, start.age+period, period)
  ctx <- lapply(ctx, function(e) e[1]) # Base values

  evaluated.markov <- markov$evaluate(ctx)

  node.costs <- sapply(names(strat.ctx), function(stratum) sapply(evaluated.markov$nodes, function(n)n$info$cost), simplify=FALSE, USE.NAMES=TRUE)
  costs <- lapply(names(strat.ctx), function(stratum) {
    node.costs[[stratum]][names(costs[[stratum]])] <- costs[[stratum]]
    node.costs[[stratum]]
  })
  names(costs) <- names(strat.ctx)
  # costs['lynch'] <- lynch.cost
  # costs['postmenopausal_asymptomatic'] <- asymptomatic.cost
  # costs['postmenopausal_bleeding'] <- bleeding.cost
  utilities <- sapply(names(strat.ctx), function(stratum) sapply(evaluated.markov$nodes, function(n)n$info$outcome), simplify=FALSE, USE.NAMES=TRUE)

  current.state <- t(initial.state)
  overall.cost <- 0
  overall.eff <- 0
  states <- data.frame()
  # for(year in seq(start.age, max.age)) {
  #   for(iter in seq(iters.per.year)) {

  n.periods <- ceiling((max.age - start.age) * iters.per.year)
  if (((max.age - start.age) * iters.per.year) %% 1 != 0) {
    warning('Simulation time is not multiple of monthly periodicity ',
            12/iters.per.year,
            ', rounding to highest multiple: ',
            (max.age - start.age), ' -> ', formatC(n.periods/iters.per.year, digits = 2, format='f'))
  }

  for(iter in seq(n.periods)) {
    real.year <- iter / iters.per.year + start.age
    year <- floor(real.year)
    states <- rbind(states, current.state)
    tpMatrix <- get.matrix.stratum(tpMatrices, real.year, period)
    ctx <- get.context.stratum(strat.ctx, real.year, period)
    # tpMatrix <- get.matrix.stratum(tpMatrices, year)
    # ctx <- get.context.stratum(strat.ctx, year)

    # year.costs <- get.context.stratum(costs, year)
    # year.utilities <- get.context.stratum(utilities, year)
    iter.costs <- unlist(get.context.stratum(costs, real.year, period))
    iter.utilities <- unlist(get.context.stratum(utilities, real.year, period))
    # Discount used to be ...^(year-start.age), adapted to variable simulation steps
    current.cost <- sum(current.state * iter.costs) * (1-discount.rate)^(iter/iters.per.year)
    overall.cost <- overall.cost + current.cost
    current.eff <- sum(current.state * iter.utilities) * (1-discount.rate)^(iter/iters.per.year)
    overall.eff <- overall.eff + current.eff

    additional.info <- calculate.iteration.measures(trees, additional.info, year, iter, current.state, tpMatrix, current.cost, current.eff, ctx)

    next.state <- current.state %*% tpMatrix$strategy %*% tpMatrix$other

    if (any(next.state < -EPSILON)) {
      # print(trees$hiv_msm$name)
      # print(tpMatrix$strategy)
      stop('States with negative populations, probabilities might have errors.')
    }
    else if (any(next.state < 0)) {
      # ASSUMPTION: Small rounding errors, renormalize
      # TODO: Reconsider if other alternative might be better
      next.state[next.state < 0] <- 0
      next.state <- next.state / sum(next.state)
    }
    current.state <- next.state
  }

states$all_hiv_positive <- apply(states, 1, function(r) sum(r[c('hiv_positive', 'hiv_positive_annual_followup1', 'hiv_positive_annual_followup2',
                                                            'semestral_followup1_no_hsil', 'semestral_followup2_no_hsil',
                                                            'semestral_followup1_treatment_no_hsil', 'semestral_followup2_treatment_no_hsil')]))
states$all_hsil <- apply(states, 1, function(r) sum(r[c('undetected_hsil', 'undetected_hsil_annual_followup1', 'undetected_hsil_annual_followup2',
                                                        'semestral_followup1_hsil', 'semestral_followup2_hsil',
                                                        'semestral_followup1_treatment_hsil', 'semestral_followup2_treatment_hsil')]))

  states.death <- apply(states[startsWith(names(states), 'death_')], 1, sum)
  states.prevs <- as.data.frame(apply(states, 2, function(col) col/(1-states.death)))
  states.prevs <- states.prevs[!startsWith(names(states.prevs), 'death_')]

  states$age <- (as.numeric(row.names(states))-1)*(max.age-start.age)/n.periods + start.age
  states.prevs$age <- (as.numeric(row.names(states.prevs))-1)*(max.age-start.age)/n.periods + start.age

  melted.states <- reshape2::melt(states, id.vars='age')
  melted.states.prevs <- reshape2::melt(states.prevs, id.vars='age')

  strategy.name <- paste0(sapply(trees, function(t) t$name), collapse = '-')
  p <- ggplot(melted.states, aes(x=age, y=value, color=variable)) +
    geom_line() +
    ylim(0,1) +
    xlab('Age') +
    ylab('Cohort (%)') +
    ggtitle(paste0('% in states: ', strategy.name))
  p <- ggplotly(p)

  p.prevs <- ggplot(melted.states.prevs, aes(x=age, y=value, color=variable)) +
    geom_line() +
    ylim(0,1) +
    xlab('Age') +
    ylab('Cohort (%)') +
    ggtitle(paste0('Prevalences: ', strategy.name))
  p.prevs <- ggplotly(p.prevs)

  results.df <- data.frame(strategy=strategy.name,
                           C=overall.cost,
                           E=overall.eff / iters.per.year,
                           stringsAsFactors = FALSE)
  return(list(
    plot=p,
    plot.prevs=p.prevs,
    states=states,
    additional.info=additional.info,
    summary=results.df
  )
  )
}

simulate <- function(type,
                     strategies,
                     markov,
                     strat.ctx,
                     initial.state,
                     start.age=DEFAULT.START.AGE[['hiv_msm']],
                     max.age=DEFAULT.MAX.AGE[['hiv_msm']],
                     discount.rate=DISCOUNT.RATE,
                     n.cores=1) {
  # results.df <- data.frame()
  markov.outputs <- list()
  # for(tree in strategies) {
  # registerDoFuture()
  # plan(multisession)
  if (n.cores==1) {
    cl <- NULL
    pboptions(type='none')
  } else {
    cl <- makeCluster(n.cores)
    pboptions(type='timer')
    on.exit({
      # plan(old_plan)
      stopCluster(cl)
    }, add=TRUE)
    export.names <- ls(1)
    export.names <- export.names[!(startsWith(export.names, 'conventional') |
                                     startsWith(export.names, 'ascus') |
                                     startsWith(export.names, 'arnm'))]
    clusterExport(cl,
                  c(ls(1)))
    clusterEvalQ(cl, {
      library(ggplot2)
      library(plotly)
    })
    # pboptions(type='txt', style=3)
  }
  results <- pblapply(strategies, cl=cl, FUN=function(tree) {
    used.trees <- list()
    used.trees[[type]] <- tree

    # Check if name finishes in "_t_xxx"
    if (endsWith(tree$name, '_t_tca')) {
      used.trees$semestral_followup <- trees[['semestral_followup_t_tca']]
    } else if (endsWith(tree$name, '_t_irc')) {
      used.trees$semestral_followup <- trees[['semestral_followup_t_irc']]
    }
    else {
      used.trees$semestral_followup <- trees[['semestral_followup']]
    }

    periodicity.months <- tree$root$info$periodicity_months
    if (is.null(periodicity.months)) {
      iters.per.year <- 2
      strat.ctx.period <- strat.ctx
    } else {
      iters.per.year <- 12 %/% periodicity.months
      strat.ctx.period <- lapply(strat.ctx, function(ctx) {
        ctx$periodicity_months <- periodicity.months
        ctx
      })
      strat.ctx.period <- refresh.context('periodicity_months', strat.ctx.period, excel.strata.df)
    }

    result <- simulate.markov(used.trees, markov, initial.state, strat.ctx,
                              start.age=start.age, max.age=max.age,
                              iters.per.year=iters.per.year,
                              discount.rate=discount.rate)
    # markov.outputs[[tree$name]] <- result
    # additional.info[[tree$name]] <- result$additional.info
    return(result)
    # results.df <- rbind(results.df, result$summary)
  })

  results.df <- do.call(rbind, lapply(results, function(r) r$summary))
  # results <- do.call(rbind, lapply(results, function(r) r$summary))
  r <- CEAModel::analyzeCE(results.df, cost.label='€', eff.label='QALY', plot=TRUE)
  r$info <- results
  return(r)
}
