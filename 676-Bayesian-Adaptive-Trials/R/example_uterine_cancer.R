
#' @description 
#' code written based on example provided in lecture of 
#' 340.676.11 Bayesian Adaptive Trials (Jun 22, 2021)

#' - observed outcome PFS ~ Exponential(\lambda)
#' - prior on rate \lambda ~ Gamma(1, 303 days)
#'   * mean = 
#'   * median = mean x log(2) = 1 x 303 x log(2) = 210.0236
#' - posterior \lambda | data ~ Gamma(1 + # Progressors, 303 + Exposure Time)
#' - calculate probability an arm is the best, for each arm 
#'   e.g. p_1 = P(\lambda_1 > \lambda_2 and \lambda_1 > \lambda_3)
#' - expected accrual rate = 3 days per patient

#' Parametrization
#' - The Gamma distribution with parameters shape = a and scale = s
#'   has E(X) = a*s


rm(list = ls())
options(scipen=999)

# expected value of outcome 
d_lam_scale <- c(303, 606, 606)
n_max_pats <- 195
d_days_per_pat <- 3
n_first_look_pats <- 45

# function to simulate arrival time 
simulate_arrival_time <- function(d_days_per_pat, n_max_pats){
  # n_max_pats <- 195; d_days_per_pat <- 3
  (1 : n_max_pats) * d_days_per_pat - (d_days_per_pat - 1)
}

# simulate arrival time vector of all patients
d_arrival_time = simulate_arrival_time(d_days_per_pat, n_max_pats)
df_all_data <- data.frame()

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# simulate first batch of participants ---------------------------------------

n_tmp_pats <- n_first_look_pats

# get group randomization vector 
d_rand_p <- rep(1, 3)/3
c_trt_group <- sample(c(1,2,3), prob = d_rand_p, size = n_tmp_pats, replace = TRUE)

# get the outcome 
d_outcome <- sapply(c_trt_group, function(val) rgamma(1, shape = 1, scale = d_lam_scale[val]))

# simulate current batch of participants 
df_tmp_data <- data.frame(
  n_pat_id = 1 : n_tmp_pats,
  d_arrival_time = d_arrival_time[1 : n_tmp_pats],
  c_trt_group = c_trt_group,
  d_outcome = d_outcome)
df_all_data <- rbind(df_all_data, df_tmp_data)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# simulate next batch of participants ---------------------------------------

#' Q: 
#' - do I cap exposure time at observation time (365 days)? 
#' - do folks who have progression event after 365 are hence never considered 
#'   as progressors and after some time just keep contributing 365 days? 
#'-  do I compute posterior p_1, p_2, p_3 probability using product of prob P(X>Y)*P(X>Z)

# compute time in the study 
d_tmp_time <- df_all_data$d_arrival_time[nrow(df_all_data)]

df_all_data <- df_all_data %>%
  mutate(d_exposure_time = d_tmp_time - d_arrival_time,
         d_exposure_time = ifelse(d_exposure_time > 365, 365, d_exposure_time), 
         n_is_progressor = ifelse(d_outcome <= d_exposure_time, 1, 0))
df_all_data










