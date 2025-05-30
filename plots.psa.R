plot.scatter <- function(results, wtp=25000, jitter=0) {
  results.display <- results
  
  results.display$IC <- results.display$IC + rnorm(nrow(results.display), sd=abs(mean(results.display$IC))*jitter)
  results.display$IE <- results.display$IE + rnorm(nrow(results.display), sd=abs(mean(results.display$IE))*jitter)
  
  results.display[, 'CE_THRESHOLD'] <- Inf
  
  results.display[results.display$IC < wtp * results.display$IE, 'CE_THRESHOLD'] <- wtp
  results.display[results.display$IE < 0, 'CE_THRESHOLD'] <- Inf
  
  WTP.THRESHOLDS <- wtp
  COLOR.PALETTE <- c('#2bbe6d', '#f44f59')
  
  results.display$CE_THRESHOLD <- factor(results.display$CE_THRESHOLD, levels=c(WTP.THRESHOLDS, Inf))
  
  lines.df <- data.frame(wtp=factor(WTP.THRESHOLDS, levels=levels(results.display$CE_THRESHOLD)))
  
  plt <- ggplot(results.display, aes(x=IC, y=IE, color=CE_THRESHOLD))
  
  ce.labels <- c('Cost-effective', 'Not cost-effective')
  
  xlims <- c(min(results.display$IC, 0), max(results.display$IC, 0))
  ylims <- c(min(results.display$IE, 0), max(results.display$IE, 0))
  lines.df <- data.frame(x=c(xlims[1], 0, xlims[2]),
                         y=c(0, 0, xlims[2]/wtp))
  
  plt <- plt + 
    geom_point() +
    scale_color_manual(name=paste0('Cost-effective simulations (WTP=', WTP.THRESHOLDS, ')'),
                       breaks = levels(results.display$CE_THRESHOLD),
                       labels=ce.labels,
                       values=COLOR.PALETTE) +
    geom_line(data=lines.df, mapping=aes(x=x,y=y), color='black', linewidth=1)
  
  plt <- plt +
    coord_cartesian(xlim=xlims, ylim=ylims) +
    xlab('Incremental cost [€]') +
    ylab('Incremental effectiveness [QALY]') +
    theme_minimal() +
    theme(legend.position='bottom')
  
  return(plt)
}

plot.acceptability <- function(results) {
  max.wtp <- 100000
  wtp.range <- seq(0,max.wtp, 1000)
  line.data <- sapply(wtp.range, function(wtp) {
    return(mean(results$IC <= results$IE * wtp & results$IE > 0) * 100)
  })
  
  line.df <- data.frame(wtp=c(wtp.range, wtp.range),
                        p.ce=c(line.data, 100-line.data),
                        strategy=c(rep('Alternative strategy', length(wtp.range)), 
                                   rep('Reference strategy', length(wtp.range))))
  
  plt <- ggplot(line.df, aes(x=wtp, y=p.ce, color=strategy)) + 
    geom_line(size=1) + 
    ylim(c(0,100)) +
    xlab('WTP (€/QALY)') +
    ylab('% Cost-effective simulations') +
    scale_color_manual(name='Strategy',
                       values=c('#002fff', '#ff9d00')) +
    theme_minimal() +
    theme(legend.position = 'bottom')
  return(plt)
}
