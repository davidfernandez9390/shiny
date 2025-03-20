library(ggplot2)
library(plyr)
library(dplyr)

source('load_models.R')
source('markov.R')

cost.label <- '€'
eff.label <- 'QALY'
nudge.x.icer <- .01
nudge.y.icer <- .005


# arn.cost.values <- c(6, 25)
# discount.values <- c(0, .03)
# delayed.cancer.cost.values <- c(0, 100, 3189)
arn.cost.values <- c(6)
discount.values <- c(0.03)
delayed.cancer.cost.values <- c(3189)

markov.results <- list()

for(discount in discount.values) {
  for(arn.cost in arn.cost.values) {
    for(delayed.cancer.cost in delayed.cancer.cost.values) {
    strat.ctx.i <- lapply(strat.ctx, function(ctx) {
      ctx[c('c_arnm16','c_arnm161845','c_arnmhr')] <- arn.cost
      ctx['c_surgery_delayed'] <- delayed.cancer.cost
      ctx
    })

    initial.state <- sapply(markov$nodes,
                            function(n) if (n$name=='hiv_positive') 1 else 0)
    markov.result <- simulate('hiv_msm',
                              strategies$hiv_msm,
                              markov,
                              strat.ctx.i,
                              initial.state,
                              discount.rate=discount)

    # Remove redundant suffix for strategy names
    markov.result$summary$strategy <- gsub('(.*?)-(.*)', '\\1', markov.result$summary$strategy)





    x.label <- ifelse(is.null(cost.label), "Cost", paste0("Cost [€]"))
    y.label <- ifelse(is.null(eff.label), "Effectiveness",
                      paste0("Effectiveness [QALY]"))
    ce.label <- ifelse(is.null(cost.label) && is.null(eff.label),
                       "", paste0(" ", cost.label, "/", eff.label))
    fullSummary <- markov.result$summary

    # fullSummary <- fullSummary[grepl('la', fullSummary$strategy, perl=TRUE) | grepl('diff_hpv16', fullSummary$strategy, perl=TRUE),]
    # fullSummary <- fullSummary[grepl('arnm', fullSummary$strategy, perl=TRUE) |
    #                              grepl('^conventional-', fullSummary$strategy, perl=TRUE) |
    #                              grepl('^conventional_t-', fullSummary$strategy, perl=TRUE),]
    # fullSummary <- fullSummary[grepl('_t$', fullSummary$strategy, perl=TRUE),]
    fullSummary <- analyzeCE(fullSummary, plot=TRUE)$summary

    plot.df <- fullSummary
    undominated.df <- fullSummary[fullSummary$domination ==
                                    "undominated", ]
    if (nrow(undominated.df) > 0) {
      undominated.df$label <- paste0("ICER=", formatC(round(undominated.df$ICER,
                                                            digits = 2), big.mark = ",", format = "d"), ce.label)
      undominated.df[1, "label"] <- ""
    } else {
      undominated.df$label <- numeric(0)
    }
    y.range <- diff(range(plot.df$E))
    x.range <- diff(range(plot.df$C))
    plt <- ggplot2::ggplot(plot.df, ggplot2::aes(x = C,
                                                 y = E)) + ggplot2::geom_line(data = undominated.df) +
      ggplot2::geom_text(data = undominated.df, mapping = ggplot2::aes(label = label),
                         nudge_x = -nudge.x.icer * x.range, nudge_y = nudge.y.icer *
                           y.range)

    strat.names <- c(
      'no_intervention',
      'conventional',
      'conventional_hpv16la',
      'conventional_hpv1618la',
      'conventional_hpvhrla',
      'conventional_hpvhrhc',
      'arnme6e7_hpv16',
      'arnme6e7_hpv161845',
      'arnme6e7_hpvhr',
      'ascus_lsil_diff_hpv16la',
      'ascus_lsil_diff_hpv1618la',
      'ascus_lsil_diff_hpvhrla',
      'ascus_lsil_diff_hpvhrhc',
      'ascus_lsil_diff_arnme6e7_hpv16',
      'ascus_lsil_diff_arnme6e7_hpv161845',
      'ascus_lsil_diff_arnme6e7_hpvhr',
      'conventional_t_irc',
      'conventional_hpv16la_t_irc',
      'conventional_hpv1618la_t_irc',
      'conventional_hpvhrla_t_irc',
      'conventional_hpvhrhc_t_irc',
      'arnme6e7_hpv16_t_irc',
      'arnme6e7_hpv161845_t_irc',
      'arnme6e7_hpvhr_t_irc',
      'ascus_lsil_diff_hpv16la_t_irc',
      'ascus_lsil_diff_hpv1618la_t_irc',
      'ascus_lsil_diff_hpvhrla_t_irc',
      'ascus_lsil_diff_hpvhrhc_t_irc',
      'ascus_lsil_diff_arnme6e7_hpv16_t_irc',
      'ascus_lsil_diff_arnme6e7_hpv161845_t_irc',
      'ascus_lsil_diff_arnme6e7_hpvhr_t_irc',
      'conventional_t_tca',
      'conventional_hpv16la_t_tca',
      'conventional_hpv1618la_t_tca',
      'conventional_hpvhrla_t_tca',
      'conventional_hpvhrhc_t_tca',
      'arnme6e7_hpv16_t_tca',
      'arnme6e7_hpv161845_t_tca',
      'arnme6e7_hpvhr_t_tca',
      'ascus_lsil_diff_hpv16la_t_tca',
      'ascus_lsil_diff_hpv1618la_t_tca',
      'ascus_lsil_diff_hpvhrla_t_tca',
      'ascus_lsil_diff_hpvhrhc_t_tca',
      'ascus_lsil_diff_arnme6e7_hpv16_t_tca',
      'ascus_lsil_diff_arnme6e7_hpv161845_t_tca',
      'ascus_lsil_diff_arnme6e7_hpvhr_t_tca'
    )

    strat.labels <- c(
      'No intervention',
      'Conventional',
      'HPV-16 LA',
      'HPV-16/18 LA',
      'HPV-HR LA',
      'HPV HC',
      'ARNmE6/E7 (HPV-16)',
      'ARNmE6/E7 (HPV-16/18/45)',
      'ARNmE6/E7 (HPV-HR)',
      'ASCUS/LSIL diff (HPV-16 LA)',
      'ASCUS/LSIL diff (HPV-16/18 LA)',
      'ASCUS/LSIL diff (HPV-HR LA)',
      'ASCUS/LSIL diff (HPV HC)',
      'ASCUS/LSIL diff (ARNmE6/E7 HPV-16)',
      'ASCUS/LSIL diff (ARNmE6/E7 HPV-16/18/45)',
      'ASCUS/LSIL diff (ARNmE6/E7 HPV-HR)',
      'Conventional [T=IRC]',
      'HPV-16 LA [T=IRC]',
      'HPV-16/18 LA [T=IRC]',
      'HPV-HR LA [T=IRC]',
      'HPV HC [T=IRC]',
      'ARNmE6/E7 (HPV-16) [T=IRC]',
      'ARNmE6/E7 (HPV-16/18/45) [T=IRC]',
      'ARNmE6/E7 (HPV-HR) [T=IRC]',
      'ASCUS/LSIL diff (HPV-16 LA) [T=IRC]',
      'ASCUS/LSIL diff (HPV-16/18 LA) [T=IRC]',
      'ASCUS/LSIL diff (HPV-HR LA) [T=IRC]',
      'ASCUS/LSIL diff (HPV HC) [T=IRC]',
      'ASCUS/LSIL diff (ARNmE6/E7 HPV-16) [T=IRC]',
      'ASCUS/LSIL diff (ARNmE6/E7 HPV-16/18/45) [T=IRC]',
      'ASCUS/LSIL diff (ARNmE6/E7 HPV-HR) [T=IRC]',
      'Conventional [T=TCA]',
      'HPV-16 LA [T=TCA]',
      'HPV-16/18 LA [T=TCA]',
      'HPV-HR LA [T=TCA]',
      'HPV HC [T=TCA]',
      'ARNmE6/E7 (HPV-16) [T=TCA]',
      'ARNmE6/E7 (HPV-16/18/45) [T=TCA]',
      'ARNmE6/E7 (HPV-HR) [T=TCA]',
      'ASCUS/LSIL diff (HPV-16 LA) [T=TCA]',
      'ASCUS/LSIL diff (HPV-16/18 LA) [T=TCA]',
      'ASCUS/LSIL diff (HPV-HR LA) [T=TCA]',
      'ASCUS/LSIL diff (HPV HC) [T=TCA]',
      'ASCUS/LSIL diff (ARNmE6/E7 HPV-16) [T=TCA]',
      'ASCUS/LSIL diff (ARNmE6/E7 HPV-16/18/45) [T=TCA]',
      'ASCUS/LSIL diff (ARNmE6/E7 HPV-HR) [T=TCA]'
    )

    names(strat.labels) <- strat.names

    # strat.shapes <- c(22, 25, 25, 24, 24, 23, 23, 23, 23, 23, 23)
    strat.shapes <- c(15, rep(22, 5), rep(23, 3), rep(24, 7), rep(22, 5), rep(23, 3), rep(24, 7), rep(22, 5), rep(23, 3), rep(24, 7))
    names(strat.shapes) <- strat.names

    # strat.fill <- c('black', '#00ccff', '#0000ff', '#00ff00', '#007700', '#6f0e18', '#961b1e', '#b91f26', '#ee3f3f', '#f48e8a', '#f9c7c2')
    strat.fill <- c('black', rep(c(
      '#ffffcc',
      '#ffeda0',
      '#fed976',
      '#feb24c',
      '#fd8d3c',
      '#abe098', # ARN
      '#57c84d',
      '#6ef62c',
      '#eff3ff', # ASCUSS diff
      '#c6dbef',
      '#9ecae1',
      '#6baed6',
      '#4292c6',
      '#2171b5',
      '#084594'
      ), 3
    ))
    names(strat.fill) <- strat.names

    strat.colors <- c('black', rep('black', 15), rep('red', 15), rep('blue', 15))
    names(strat.colors) <- strat.names

    strat.indices <- strat.names %in% fullSummary$strategy
    strat.names <- strat.names[strat.indices]
    strat.colors <- strat.colors[strat.indices]
    strat.fill <- strat.fill[strat.indices]
    strat.shapes <- strat.shapes[strat.indices]

    fullSummary$strategy <- factor(fullSummary$strategy, levels=strat.names, ordered=TRUE)

    plt <- plt +
      geom_point(data=fullSummary, size=5, aes(fill=strategy, color=strategy, shape=strategy), stroke=1) +
      scale_shape_manual(values=strat.shapes,
                         labels=strat.labels,
                         name='Strategy') +
      scale_color_manual(values=strat.colors,
                         labels=strat.labels,
                         name='Strategy') +
      scale_fill_manual(values=strat.fill,
                        labels=strat.labels,
                        name='Strategy') +
      theme(legend.position = 'bottom')
      # coord_cartesian(ylim=c(16.94, 16.96))
    # print(plt)
    # ggplotly(plt)
    # ggsave(paste0('output/results/eff_disc_', discount, '_c_arn_', arn.cost, '_c_cancer_delayed_', delayed.cancer.cost, '.png'),
    #        plt,
    #        width=5000,
    #        height=3000,
    #        units = 'px')
    }
  }
}

# display.df <- fullSummary
# display.df$strategy <- mapvalues(display.df$strategy, strat.names, strat.labels)
#
# results.df <- do.call(rbind, lapply(markov.result$info, function(strat) {
#   sapply(names(strat$additional.info), function(x) {
#     ind <- which(names(strat$additional.info) == x)
#     val <- strat$additional.info[[x]]
#     if (ind %in% c(1,2)) {
#       -1
#     } else if (ind == 3) {
#       sum(val)
#     } else if (ind == 4) {
#       sum(val)/2
#     } else if (ind %in% c(5:8)) {
#       mean(val)*1e5
#     } else if (ind %in% c(9:17)) {
#       mean(val)*2*1e5
#     } else if (ind %in% c(18:23)) {
#       mean(val)*2
#     }
#   })
# }))
# results.df <- as.data.frame(results.df)[-c(1,2)]
# # names(results.df) <- names(markov.result$info$conventional$additional.info)[-c(1,2)]
# results.df$year <- NULL
# results.df$iter <- NULL
# # View(results.df)
#
# # strat.base.names <- c("arnme6e7_hpv16",
# #                       "arnme6e7_hpv16",
# #                       "arnme6e7_hpv16",
# #                       "arnme6e7_hpv161845",
# #                       "arnme6e7_hpv161845",
# #                       "arnme6e7_hpv161845",
# #                       "arnme6e7_hpvhr",
# #                       "arnme6e7_hpvhr",
# #                       "arnme6e7_hpvhr",
# #                       "ascus_lsil_diff_arnme6e7_16",
# #                       "ascus_lsil_diff_arnme6e7_16",
# #                       "ascus_lsil_diff_arnme6e7_16",
# #                       "ascus_lsil_diff_arnme6e7_161845",
# #                       "ascus_lsil_diff_arnme6e7_161845",
# #                       "ascus_lsil_diff_arnme6e7_161845",
# #                       "ascus_lsil_diff_arnme6e7_hr",
# #                       "ascus_lsil_diff_arnme6e7_hr",
# #                       "ascus_lsil_diff_arnme6e7_hr",
# #                       "ascus_lsil_diff_hpv1618la",
# #                       "ascus_lsil_diff_hpv1618la",
# #                       "ascus_lsil_diff_hpv1618la",
# #                       "ascus_lsil_diff_hpv16la",
# #                       "ascus_lsil_diff_hpv16la",
# #                       "ascus_lsil_diff_hpv16la",
# #                       "ascus_lsil_diff_hpvhrhc",
# #                       "ascus_lsil_diff_hpvhrhc",
# #                       "ascus_lsil_diff_hpvhrhc",
# #                       "ascus_lsil_diff_hpvhrla",
# #                       "ascus_lsil_diff_hpvhrla",
# #                       "ascus_lsil_diff_hpvhrla",
# #                       "conventional",
# #                       "conventional_hpv1618la",
# #                       "conventional_hpv1618la",
# #                       "conventional_hpv1618la",
# #                       "conventional_hpv16la",
# #                       "conventional_hpv16la",
# #                       "conventional_hpv16la",
# #                       "conventional_hpvhrhc",
# #                       "conventional_hpvhrhc",
# #                       "conventional_hpvhrhc",
# #                       "conventional_hpvhrla",
# #                       "conventional_hpvhrla",
# #                       "conventional_hpvhrla",
# #                       "conventional",
# #                       "conventional",
# #                       "no_intervention")
#
# tab1 <- results.df[,c('n_cyto', 'n_hpv')]
#
# tab1$n_cyto100k <- tab1$n_cyto * 1e5
# tab1$n_hpv100k <- tab1$n_hpv * 1e5
#
# tab2 <- results.df[,c('n_new_detected_false_hsils', 'n_new_detected_true_hsils', 'n_new_undetected_hsils')]
# tab2$n_total_hsils <- tab2$n_new_detected_true_hsils + tab2$n_new_undetected_hsils
#
# tab3 <- results.df[, c('n_hra_no_hsil', 'n_hra_hsil')] * 1e5
# tab3$n_hra_total <- tab3$n_hra_no_hsil + tab3$n_hra_hsil
# tab3$n_hra_total_person <- tab3$n_hra_total / 1e5
#
# tab4 <- results.df[, c('n_new_semestral_followup_no_hsils', 'n_new_semestral_followup_hsils')]
#
# tab5 <- results.df[, c('n_treatment_no_hsil', 'n_treatment_hsil', 'n_new_surgeries_no_cancer')]
#
# tab6 <- results.df[, c('n_new_cancers', 'n_new_cancers_delayed', 'n_new_deaths_cancer')]
# tab6$n_new_cancers_total <- tab6$n_new_cancers + tab6$n_new_cancers_delayed
