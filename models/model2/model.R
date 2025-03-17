simulate <- function(strategies,
                     p.healthy.cancer,
                     p.healthy.death,
                     p.cancer.death,
                     p.screening.effective,
                     p.treatment.effective,
                     cost.screening,
                     cost.cancer.treatment,
                     utility.cancer,
                     simulated.years,
                     discount) {

  df <- data.frame()
  cohort.states <- list()
  for(strategy in strategies) {
    state.costs <- c(0, 0, 0)
    state.utilities <- c(1, utility.cancer, 0)

    if (strategy == 'untreated') {
      state.costs[1] <- 0
      state.costs[2] <- 0
      p.cancer <- p.healthy.cancer
      p.cancer.healthy <- 0
    } else if (strategy == 'screening') {
      state.costs[1] <- cost.screening
      state.costs[2] <- 0
      p.cancer <- p.healthy.cancer * p.screening.effective
      p.cancer.healthy <- p.treatment.effective
    } else if (strategy == 'treatment') {
      state.costs[1] <- 0
      state.costs[2] <- cost.cancer.treatment
      p.cancer <- p.healthy.cancer
      p.cancer.healthy <- p.treatment.effective
    }

    tp.matrix <- matrix(c(1-p.cancer-p.healthy.death, p.cancer, p.healthy.death,
                          p.cancer.healthy, 1-p.cancer.healthy-p.cancer.death, p.cancer.death,
                          0, 0, 1),
                        nrow=3, byrow = TRUE)
  Sys.sleep(3)

    costs <- c()
    utilities <- c()

    cohort <- list()
    cohort[[1]] <- c(1,0,0)
    for(i in seq(2,simulated.years+1)) {
      costs <- c(costs, state.costs %*% cohort[[i-1]] * (1-discount)^(i-1))
      utilities <- c(utilities, state.utilities %*% cohort[[i-1]] * (1-discount)^(i-1))
      cohort[[i]]  <- as.numeric(cohort[[i-1]] %*% tp.matrix)
    }
    cohort.states[[strategy]] <- cohort

    df <- rbind(df, data.frame(strategy=strategy,
                      C=mean(costs),
                      E=sum(utilities)))
  }
  return(list(
    summary=df,
    cohort.info=cohort.states
  ))
}
