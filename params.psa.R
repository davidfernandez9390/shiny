fit.parameter <- function(dist, mean, sd) {
  if (dist == 'beta') return(fit.beta(mean, sd))
  else if (dist == 'normal') return(fit.normal(mean,sd))
  else if (dist == 'lognormal') return(fit.lognormal(mean,sd))
  else if (dist == 'gamma') return(fit.gamma(mean,sd))
  else if (dist == 'exponential') return(fit.exponential(mean,sd))
  else stop(paste0('Distribution "', dist, '" not supported'))
}

fit.exponential <- function(mean, sd) {
  # sd is equal to the mean, parameter ignored
  return(list(lambda=1/mean))
}

fit.beta <- function(mean, sd) {
  if (sd^2 > mean*(1-mean)) {
    # warning('Deviation too high for beta distribution, setting to (arbitrarily) [mean*(1-mean)/2]')
    sd <- sqrt(mean*(1-mean))
  }
  alpha_p_beta <- mean*(1-mean)/sd^2 - 1
  alpha <- mean*alpha_p_beta
  beta <- alpha_p_beta - alpha
  return(list(alpha=alpha, beta=beta))
}

fit.gamma <- function(mean, sd) {
  alpha <- mean^2/(sd^2)
  beta <- sd^2/mean
  return(list(alpha=alpha, beta=beta))
}

fit.lognormal <- function(mean, sd) {
  norm.mean <- 2*log(mean) - .5*log(sd^2+mean^2)
  norm.var <- -2*log(mean)+log(sd^2+mean^2)
  return(list(mean=norm.mean, sd=sqrt(norm.var)))
}

fit.normal <- function(mean, sd) {
  return(list(mean=mean, sd=sd))
}


sample.parameter <- function(dist, dist.params, n=1) {
  if (dist == 'beta') return(rbeta(n, shape1=dist.params$alpha, shape2=dist.params$beta))
  else if (dist == 'normal') return(rnorm(n, dist.params$mean, dist.params$sd))
  else if (dist == 'lognormal') return(exp(rnorm(n, dist.params$mean, dist.params$sd)))
  else if (dist == 'gamma') return(rgamma(n, shape=dist.params$alpha, scale=dist.params$beta))
  else if (dist == 'exponential') return(rexp(n, rate=dist.params$lambda))
  else stop(paste0('Distribution "', dist, '" not recognized'))
}