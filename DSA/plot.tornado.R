# DEFINICIÓ DEL PLOT.TORNADO
plot.tornado <- function(results,
                         WTP=22000,
                         use.nhb=TRUE,
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
  results$ICER <- results$IC / results$IE
  
  base.params <- results[is.na(results$param),][1,]
  if (use.nhb) {
    base <- base.params$NHB
    base.label <- 'NHB'
  } else {
    base <- base.params$ICER
    base.label <- 'ICER'
  }
  results <- results[!is.na(results$param),]
  
  plot.df <- data.frame()
  scatter.df <- data.frame()
  for(p in unique(results$param)) {
    sub.df <- results[results$param==p,]
    par.range <- range(sub.df$param.value)
    if (diff(par.range) != 0) {
      measure.par.range <- sapply(par.range, function(v) sub.df[sub.df$param.value == v, base.label])
    } else {
      # If range width = 0, the NHB is the same
      measure.par.range <- rep(sub.df[1, base.label], 2)
    }
    if (!is.null(param.display.names)) {
      p.display <- param.display.names[[p]]
      if (is.null(p.display)) {
        p.display <- p
      }
    } else {
      p.display <- p
    }
    if (diff(measure.par.range) > 0) {
      label <- paste0(p.display, '\n[', formatC(par.range[1], format='fg', digits=3), ' - ', formatC(par.range[2], format='fg', digits=3), ']')
    } else {
      label <- paste0(p.display, '\n[', formatC(par.range[2], format='fg', digits=3), ' - ', formatC(par.range[1], format='fg', digits=3), ']')
    }
    plot.df <- rbind(plot.df,
                     data.frame(
                       # strategy=results$strategy[1],
                       param=p,
                       label=label,
                       min.v=min(measure.par.range),
                       max.v=max(measure.par.range),
                       width=abs(diff(measure.par.range))
                     ))
    scatter.df <- rbind(scatter.df,
                        data.frame(
                          # strategy=results$strategy[1],
                          param=p,
                          measure=sub.df[[base.label]]
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
    if (use.nhb) {
      x.label <- 'NHB (QALY)'
    } else {
      x.label <- 'ICER (€/QALY)'
    }
    if (!use.nhb) {
      format.labels <- function(x) {
        formatC(x, format='d', big.mark = ',')
      }
    } else {
      format.labels <- function(x) {
        x
      }
    }
    plt <- ggplot(plot.df) +
      geom_segment(size=6, color=bar.color, aes(x=min.v, xend=max.v, y=pos, yend=pos)) +
      annotate('segment', x=base,xend=base,y=0,yend=length(unique(plot.df$param))+.44,
               color='orange',
               linetype='dashed') +
      annotate('segment', x=base,xend=base,y=length(unique(plot.df$param))+.44,yend=length(unique(plot.df$param))+.45,
               arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed",),
               color='orange') +
      geom_vline(xintercept=0, color='black', linetype='dashed') +
      xlab(x.label) +
      ylab('') +
      scale_x_continuous(labels=format.labels) +
      scale_y_continuous(
        breaks=breaks,
        labels=labels,
        limits = c(0, length(unique(plot.df$param))+1),
        oob=scales::squish_infinite) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank())
    if (!use.nhb) {
      plt <- plt +
        geom_vline(xintercept=WTP[1], color='red', linetype=2)
    }
  } else {
    plt <- plot +
      geom_segment(data=plot.df, size=3, color=bar.color, aes(x=min.v, xend=max.v, y=pos, yend=pos))
  }
  
  return(plt)
}