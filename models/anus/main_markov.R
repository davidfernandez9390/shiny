library(dplyr)
#setwd('~/Documents/models_ce/anus')
source('load_models.R')
source('markov.R')

cat('Running markov model...\n')
markov.outputs <- list()

initial.state <- sapply(markov$nodes,
                        function(n) if (n$name=='hiv_positive') 1 else 0)
markov.result <- simulate('hiv_msm',
                           strategies$hiv_msm,
                          # strategies$hiv_msm['no_intervention'],
                          # strategies$hiv_msm[c('conventional_t_irc', 'arnme6e7_hpvhr_t_irc')],
                          # strategies$hiv_msm[c('arnme6e7_hpvhr_t_tca', 'arnme6e7_hpvhr_t_irc')],
                          # strategies$hiv_msm[c('arnme6e7_hpvhr_t_irc', 'arnme6e7_hpvhr_t_tca')],
                           markov,
                           strat.ctx,
                           initial.state,
                           discount.rate=.03)

# Remove redundant suffix for strategy names
# markov.result$summary$strategy <- gsub('(.*?)-(.*)', '\\1', markov.result$summary$strategy)

print(markov.result$plot)
print(markov.result$summary)
plot(markov.result$info$no_intervention$additional.info$incidence_hsil, type='l')
print(ggplotly(markov.result$plot + theme(legend.position = 'none')))

# x <- markov.result$info$conventional$additional.info
# xt <- markov.result$info$conventional_t$additional.info
# df <- data.frame(iter=x$iter,
#                  strategy='conventional',
#                  ac_incidence=x$ac_incidence,
#                  ac_mortality=x$ac_mortality)
# df <- rbind(df, data.frame(iter=xt$iter,
#                            strategy='conventional_t',
#                            ac_incidence=xt$ac_incidence,
#                            ac_mortality=xt$ac_mortality))
# df <- melt(df, id.vars = c('iter', 'strategy'))
# plt <- ggplot(df, aes(x=iter, y=value, color=strategy, linetype=variable)) +
#   geom_line()
#
# print(plt)

cat('Done.\n')


### Parameter combination set

arn.cost.values <- c(6, 25)
discount.values <- c(0, .03)
delayed.cancer.cost.values <- c(0, 100, 3189)

markov.results <- list()

for(discount in discount.values) {
  for(arn.cost in arn.cost.values) {
    for(delayed.cancer.cost in delayed.cancer.cost.values) {
      strat.ctx.i <- lapply(strat.ctx, function(ctx) {
        ctx[c('c_arn_kit')] <- arn.cost
        ctx['c_surgery_delayed'] <- delayed.cancer.cost
        ctx
      })

      strat.ctx.i <- refresh.context(c('c_arn_kit', 'c_surgery_delayed'), strat.ctx.i, excel.strata.df)

      cat('*** Simulating set with discount=', discount, ', ARN cost=', arn.cost, ' and delayed cancer cost=', delayed.cancer.cost, '\n', sep = '')
      x <- simulate('hiv_msm',
                          strategies$hiv_msm,
                          markov,
                          strat.ctx.i,
                          initial.state,
                          discount.rate = discount)

      markov.results[[paste0('disc_', discount, '_arn_', arn.cost, '_delayed_cancer_', delayed.cancer.cost)]] <- x
    }
  }
}

summary.df <- lapply(names(markov.results[[1]]$info), function(strat) {
  info <- markov.results[[1]]$info[[strat]]$additional.info
  info.sum <- lapply(info, sum)
  info.sum$strategy <- strat
  info.sum$year <- NULL
  info.sum$iter <- NULL
  info.sum$eff <- NULL
  info.sum$cost <- NULL

  for(set.name in names(markov.results)) {
    info <- markov.results[[set.name]]$info[[strat]]$additional.info

    info.sum.i <- lapply(info, sum)

    info.sum[paste0('qalys.disc.', str_split(set.name,'_')[[1]][2])] <- info.sum.i$eff / 2
    info.sum[paste0('cost.', set.name)] <- info.sum.i$cost
  }

  # info.sum$n_healthy <- mean(info$n_healthy)
  info.sum$n_hsils <- mean(info$n_hsils)
  info.sum$n_cancers <- mean(info$n_cancers)

  info.sum$incidence_hsil <- 2*mean(info$incidence_hsil)
  info.sum$incidence_cancer <- 2*mean(info$incidence_cancer)

  info.sum
}) %>% bind_rows()

summary.df <- summary.df[,c(21:35, 2:20)]
summary.df[,c(16:34)] <- summary.df[,c(16:34)] * 100000

write.xlsx(summary.df, 'output/results/summary_anus_long.xlsx')
# summary.df <- read.xlsx('output/results/summary_anus_new_v4_long.xlsx', sheetIndex=1)

summary.df$strat.treatment <- 'followup'
summary.df[endsWith(summary.df$strategy, '_t_tca'), 'strat.treatment'] <- 'TCA'
summary.df[endsWith(summary.df$strategy, '_t_irc'), 'strat.treatment'] <- 'IRC'
summary.df$strat.basename <- gsub('_t_tca|_t_irc|conventional_|ascus_lsil_diff_', '', summary.df$strategy, perl=TRUE)
summary.df[summary.df$strategy %in% c('conventional', 'conventional_t_irc', 'conventional_t_tca'), 'strat.basename'] <- 'conventional'
summary.df[summary.df$strategy %in% c('no_intervention'), 'strat.basename'] <- 'no_intervention'
summary.df$ascus.diff <- ifelse(startsWith(summary.df$strategy, 'ascus_lsil_diff'),
                                'ascus.diff',
                                'no.ascus.diff')
summary.df[summary.df$strat.basename=='conventional','ascus.diff'] <- 'conventional'
summary.df[summary.df$strat.basename=='no_intervention','ascus.diff'] <- 'no_intervention'
summary.df$strategy <- NULL

summary.df <- summary.df[c(31, 44, 45, 38:40, 35:37, 32:34, 41:43, 1:3, 4:6, 7:9, 25:27, 22:24, 19:21, 28:30, 10:12, 13:15, 16:18, 46),]
#
# summary.df <- summary.df[summary.df$strategy != 'no_intervention',]
#
var.names <- names(summary.df)[1:33]

strat.names <- c('conventional', 'hpvhrhc', 'hpv16la', 'hpv1618la', 'hpvhrla', 'arnme6e7_hpv16', 'arnme6e7_hpv161845', 'arnme6e7_hpvhr', 'no_intervention')
treatment.names <- c('followup', 'IRC', 'TCA')

new.summary.df <- data.frame(strategy=c('conventional',
                                        rep(strat.names[2:8], 2),
                                        'no_intervention'),
                             ascus.diff=c('-',
                                          rep('no.ascus.diff', 7),
                                          rep('ascus.diff', 7),
                                          'no_intervention'))
for(var.name in var.names) {
  for(treatment in treatment.names) {
    var.summary.df <- summary.df[summary.df$strat.treatment==treatment,]
    var.summary.df.0 <- var.summary.df[var.summary.df$ascus.diff=='conventional',]
    var.summary.df.1 <- var.summary.df[var.summary.df$ascus.diff=='no.ascus.diff',] %>% arrange(factor(strat.basename, strat.names))
    var.summary.df.2 <- var.summary.df[var.summary.df$ascus.diff=='ascus.diff',] %>% arrange(factor(strat.basename, strat.names))
    var.summary.df.3 <- var.summary.df[var.summary.df$ascus.diff=='no_intervention',]
    if (nrow(var.summary.df.3) == 0) var.summary.df.3 <- NA
    else var.summary.df.3 <- var.summary.df.3[[var.name]]

    new.summary.df[paste0(var.name, '_', treatment)] <- c(var.summary.df.0[[var.name]],
                 var.summary.df.1[[var.name]],
                 var.summary.df.2[[var.name]],
                 var.summary.df.3
                )

  }
}

# View(new.summary.df)
write.xlsx(new.summary.df, 'output/results/summary_anus_wide.xlsx')
