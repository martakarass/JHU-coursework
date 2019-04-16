
#' @author 
#' Marta Karas <marta.karass@gmail.com>
#' 
#' @description 
#' Script to reproduce results contained in Table 2 in the publication
#' William F Rosenberger, Feifang Huc (2004), "Maximizing power and minimizing 
#' treatment failures in clinical trials", Clinical Trials 2004; 1: 141 -1

rm(list = ls())

library(dplyr)
library(ggplot2)

project.dir   <- "/Users/martakaras/Dropbox/JHU-coursework/PH-140-850-Adaptive-Clinical-Trials/final-project/"
param.df.path <- file.path(project.dir, "results/param_df.csv")
out.df.path   <- file.path(project.dir, "results/out_df.csv")

## Whether or not rerun and replace simulation results
## sample size n 
get.nn.replace <- FALSE
## power, expected # of failures 
sim.replace <- FALSE

## Simulation repetition number  
get.nn.rep.N <- 10000
sim.rep.N <- 10000



## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## SIMULATE RESULT: sample size n 

## Function to get sample size that yields expected power
## in two-sided test of equal proportions 
get.nn <- function(p_a, p_b, alpha = 0.05, power = 0.9){
  n.tmp <- 20
  while (TRUE){
    out <- replicate(get.nn.rep.N, {
      x1 <- rbinom(1, n.tmp/2, p_a)
      x2 <- rbinom(1, n.tmp/2, p_b)
      res <- prop.test(
        x = c(x1, x2), n = c(n.tmp/2, n.tmp/2), 
        correct = FALSE, alternative = "two.sided")
      res$p.value < alpha
    })
    if (mean(out, na.rm = TRUE) > power){
      break
    } else {
      n.tmp <- n.tmp + 2
    }
  }
  return(n.tmp)
}

if (get.nn.replace) {
  ## Get sample size that yields expected power
  ## for each pair of (P_A, P_B) values considered 
  p_a.vec <- c(0.9, 0.9, 0.9, 0.9, 0.7, 0.7, 0.5, 0.3, 0.2)
  p_b.vec <- c(0.3, 0.5, 0.7, 0.8, 0.3, 0.5, 0.4, 0.1, 0.1)
  param.df <- data.frame(
    p_a = p_a.vec,
    p_b = p_b.vec)
  param.df$nn <- NA 
  set.seed(1)
  for (i in 1:nrow(param.df)){
    p_a.tmp <- param.df$p_a[i]
    p_b.tmp <- param.df$p_b[i]
    t1 <- Sys.time()
    n.tmp <- get.nn(p_a.tmp, p_b.tmp)
    t2 <- Sys.time()
    tt <- as.numeric(t2 - t1, units = "secs")
    print(paste0(n.tmp, "   | time: ", round(tt), " sec"))
    param.df[i, "nn"] <-  n.tmp
  }
  ## Save results to file 
  write.csv(param.df, file = param.df.path, quote = FALSE, row.names = FALSE)
}

## Read pre-computed param df 
param.df <- read.csv(param.df.path)




## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## FUNCTIONS TO SIMULATE
## 
## (1) Success in rejecting null, 
## (2) Number of failures in a trial,
## for design rules: 
## (1) complete, 
## (2) play-the-winner, 
## (3) drop-the-loser. 


## (1) complete
get.compl <- function(p_a, p_b, nn){
  ## Simulate number of successes in two treatment groups
  x1 <- rbinom(1, nn/2, p_a)
  x2 <- rbinom(1, nn/2, p_b)
  ## 
  res <- prop.test(
    x = c(x1, x2), n = c(nn/2, nn/2), 
    correct = FALSE, alternative = "two.sided")
  ## Whether or not success in rejecting null
  power.tmp <- as.numeric(res$p.value < 0.05)
  ## Number of failures
  failures.tmp <- nn - x1 - x2
  return(list(power = power.tmp, 
              failures = failures.tmp))
}


## (2) play-the-winner
## Setting: start with five balls of each type in the urn 
get.playwin <- function(p_a, p_b, nn){
  balls.A <- 5
  balls.B <- 5
  trt.A.success <- 0
  trt.A.failure <- 0
  trt.B.success <- 0
  trt.B.failure <- 0
  ## Iterate over each participant 
  for (i in 1:nn){
    ## Determine treatment type based on this iteration urn draw
    pr.draw.A <- balls.A / (balls.A + balls.B)
    is.draw.A <- as.logical(rbinom(1, 1, pr.draw.A))
    # ## Update urn count after this iteration urn draw, after the draw
    # if (is.draw.A) {
    #   balls.A <- balls.A - 1
    # } else {
    #   balls.B <- balls.B - 1
    # }
    ## Simulate whether or not treatment is successful
    pr.draw.success <- ifelse(is.draw.A, p_a, p_b)
    draw.success <- as.logical(rbinom(1, 1, pr.draw.success))
    ## Case: treatment was successful 
    if (draw.success){
      if (is.draw.A){
        balls.A <- balls.A + 1
        trt.A.success <- trt.A.success + 1
      } else {
        balls.B <- balls.B + 1
        trt.B.success <- trt.B.success + 1
      } 
      ## Case: treatment was not successful 
    } else {
      if (is.draw.A){
        balls.B <- balls.B + 1
        trt.A.failure <- trt.A.failure + 1
      } else {
        balls.A <- balls.A + 1
        trt.B.failure <- trt.B.failure + 1
      } 
    }
  }
  ## Determine total number of successes in each treatment group
  x1 <- trt.A.success
  x2 <- trt.B.success
  ## Determine total number of participants in each treatment group
  n1 <- trt.A.success + trt.A.failure
  n2 <- trt.B.success + trt.B.failure
  if (n1 + n2 != nn) stop("n1 + n2 != nn")
  res <- prop.test(x = c(x1, x2), n = c(n1, n2), correct = FALSE, 
                   alternative = "two.sided")
  ## Whether or not success in rejecting null
  power.tmp <- as.numeric(res$p.value < 0.05)
  ## Number of failures
  failures.tmp <- trt.A.failure + trt.B.failure
  return(list(power = power.tmp, 
              failures = failures.tmp))
}


## (3) drop-the-loser
## Setting: start with five type A, five type B, 1 type 0, draw with pr 1/3 
get.droploser <- function(p_a, p_b, nn){
  balls.A <- 5
  balls.B <- 5
  balls.0 <- 1
  trt.A.success <- 0
  trt.A.failure <- 0
  trt.B.success <- 0
  trt.B.failure <- 0
  i <- 1
  while (TRUE){
    ## Determine treatment type based on this iteration urn draw
    pr.draw.A <- balls.A / (balls.A + balls.B + balls.0)
    pr.draw.B <- balls.B / (balls.A + balls.B + balls.0)
    draw.unif <- runif(1)
    if (draw.unif < pr.draw.A){
      ## Case: treatment A
      draw.success <- as.logical(rbinom(1, 1, p_a))
      if (draw.success){
        trt.A.success <- trt.A.success + 1
      } else {
        trt.A.failure <- trt.A.failure + 1
        balls.A <- balls.A - 1
      }
    } else if (draw.unif < pr.draw.A + pr.draw.B) {
      ## Case: treatment B
      draw.success <- as.logical(rbinom(1, 1, p_b))
      if (draw.success){
        trt.B.success <- trt.B.success + 1
      } else {
        trt.B.failure <- trt.B.failure + 1
        balls.B <- balls.B - 1
      }
    } else {
      ## Case: urn 0
      balls.A <- balls.A + 1
      balls.B <- balls.B + 1
      next
    }
    i <- i + 1
    if (i > nn) break
  }
  ## Determine total number of successes in each treatment group
  x1 <- trt.A.success
  x2 <- trt.B.success
  ## Determine total number of participants in each treatment group
  n1 <- trt.A.success + trt.A.failure
  n2 <- trt.B.success + trt.B.failure
  if (n1 + n2 != nn) stop("n1 + n2 != nn")
  res <- prop.test(x = c(x1, x2), n = c(n1, n2), correct = FALSE, 
                   alternative = "two.sided")
  ## Whether or not success in rejecting null
  power.tmp <- as.numeric(res$p.value < 0.05)
  ## Number of failures
  failures.tmp <- trt.A.failure + trt.B.failure
  return(list(power = power.tmp, 
              failures = failures.tmp))
}



## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## SIMULATE RESULT: power, expected # of failures 

error.obj <- list(power = NA, failures = NA)
set.seed(1)
if (sim.replace){
  ## objects to store simulation results
  out.row_i   <- numeric()
  out.p_a     <- numeric()
  out.p_b     <- numeric()
  out.nn      <- numeric()
  out.power   <- numeric()
  out.failures <- numeric()
  out.method  <- numeric()
  for (row.i in 1:nrow(param.df)){
    message(paste0("row.i: ", row.i))
    vec.i <- unlist(param.df[row.i, ])
    p_a <- vec.i[1]
    p_b <- vec.i[2]
    nn  <- vec.i[3]
    for (j in 1:sim.rep.N){
      print(j)
      ## Simulate trial 
      out1 <- tryCatch({ get.compl(p_a, p_b, nn)     }, error = function(e) { error.obj }) 
      out2 <- tryCatch({ get.playwin(p_a, p_b, nn)   }, error = function(e) { error.obj }) 
      out3 <- tryCatch({ get.droploser(p_a, p_b, nn) }, error = function(e) { error.obj }) 
      ## Save simulation results 
      out.power    <- c(out.power, c(out1$power, out2$power, out3$power))
      out.failures <- c(out.failures, c(out1$failures, out2$failures, out3$failures))
      out.method   <- c(out.method, c("complete", "play_winner_rule", "drop_loser_rule"))
    }
    out.p_a   <- c(out.p_a, rep(p_a, 3 * sim.rep.N))
    out.p_b   <- c(out.p_b, rep(p_b, 3 * sim.rep.N))
    out.nn    <- c(out.nn,  rep(nn, 3 * sim.rep.N))
    out.row_i <- c(out.row_i, rep(row.i, 3 * sim.rep.N))
    if (length(out.power) != length(out.row_i)) stop("length(out.power) != length(out.row_i)")
  }
  
  ## Save results to data frame, write to file 
  out.df <- data.frame(
    row_i = out.row_i,
    p_a = out.p_a,
    p_b = out.p_b,
    nn = out.nn,
    power = out.power,
    failures = out.failures,
    method = out.method
  )
  write.csv(out.df, file = out.df.path, quote = FALSE, row.names = FALSE)
}

## Read pre-computed simulation results 
out.df <- read.csv(out.df.path)



## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## ANALYZE DATA, GENERATE PLOTS 

param.df
out.df

any(is.na(out.df))
sum(is.na(out.df$failures))

## Aggregate results data frame
out.df.agg <- 
  out.df %>%
  group_by(row_i, p_a, p_b, nn, method) %>%
  mutate(valid_obs = !is.na(power)) %>%
  summarize(power_mean = mean(power, na.rm = TRUE),
            power_sd = sd(power, na.rm = TRUE),
            failures_mean = mean(failures, na.rm = TRUE),
            failures_sd = sd(failures, na.rm = TRUE),
            n_valid_obs = sum(valid_obs)) %>%
  mutate(setting = paste0("P_a=", p_a, ", P_b=", p_b, ", n=", nn),
         # power_mean_err    = qt(0.975, df = 10000 - 1) * power_sd / sqrt(10000),
         # failures_mean_err = qt(0.975, df = 10000 - 1) * failures_sd / sqrt(10000),
         power_mean_err    = qnorm(0.975) * power_sd / sqrt(n_valid_obs),
         failures_mean_err = qnorm(0.975) * failures_sd / sqrt(n_valid_obs),
         power_mean_bar_UL = power_mean + power_mean_err,
         power_mean_bar_LL = power_mean - power_mean_err,
         failures_mean_bar_UL = failures_mean + failures_mean_err,
         failures_mean_bar_LL = failures_mean - failures_mean_err) %>%
  arrange(method, row_i) %>%
  as.data.frame()
out.df.agg$setting <- factor(as.character(out.df.agg$setting))
out.df.agg$method <- factor(
  as.character(out.df.agg$method),
  levels = c("complete", "play_winner_rule", "drop_loser_rule"),
  labels = c("Complete", "RPW rule", "DL rule"))


## Plot power 
ggplot(out.df.agg, 
       aes(x = method, y = power_mean, color = method, group = method)) +
  geom_hline(yintercept = 0.9, size = 0.2) + 
  geom_line(data = out.df.agg, 
            aes(x = method, y = power_mean, group = 1),
            inherit.aes = FALSE) + 
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = power_mean_bar_LL, ymax = power_mean_bar_UL),
                 width=.2) + 
  facet_wrap(~ setting, scales = "free", ncol = 3) + 
  theme_bw(base_size = 10) + 
  scale_y_continuous(limits = c(0.875, 0.926)) + 
  # theme(legend.position = "none") + 
  labs(x = "Method", 
       y = "Proportion of rejected nulls:\nbinomial mean and 95% CI of the mean bars",
       color = "Method")

plt.path <- file.path(project.dir, "figures/plot_power.jpeg")
ggsave(plt.path, width = 22, height = 12, units = "cm")


## Plot expected number of failures 
ggplot(out.df.agg, 
       aes(x = method, y = failures_mean, color = method, group = method)) +
  geom_line(data = out.df.agg, 
            aes(x = method, y = failures_mean, group = 1),
            inherit.aes = FALSE) + 
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = failures_mean_bar_LL, ymax = failures_mean_bar_UL),
                width=.2) + 
  facet_wrap(~ setting, scales = "free", ncol = 3) + 
  theme_bw(base_size = 10) + 
  # theme(legend.position = "none") + 
  labs(x = "Method", 
       y = "Mean # of failures:\nmean and 95% CI of the mean bars",
       color = "Method")

plt.path <- file.path(project.dir, "figures/plot_failures.jpeg")
ggsave(plt.path, width = 22, height = 12, units = "cm")


