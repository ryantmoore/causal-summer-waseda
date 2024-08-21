# Multiarm Bandits
# Ryan T. Moore
# First: 2019-02-12
# Last: 2023-08-01

# Preliminaries -----------------------------------------------------------

# Thomas Lotze (Square) and Markus Loecher (Berlin Sch Econ/Law):
library(bandit)
library(tidyverse)

# Calculating Posterior Probabilities -------------------------------------

# Clear two-arm trial:
successes <- c(50, 90)
n <- c(100, 100)

best_binomial_bandit(successes, n)


# Competitive two-arm trial:
successes <- c(50, 51)
n <- c(100, 100)

best_binomial_bandit(successes, n)


# Competitive two-arm trial:
successes <- c(50, 56)
n <- c(100, 100)

best_binomial_bandit(successes, n)


# Clear five-arm trial:

successes <- c(20, 30, 40, 50, 60)
n <- c(100, 100, 100, 100, 100)

best_binomial_bandit(successes, n) |> round(3)


# Competitive five-arm trial:

successes <- c(20, 30, 40, 50, 52)
n <- c(100, 100, 100, 100, 100)

best_binomial_bandit(successes, n) |> round(3)

# Similar: translating pre-election polls into win probabilities.
#
# (This could be an approval-voting poll of 100 respondents.)


# Exercise 1
#
# Demonstrate how posterior probabilities of being best 
# become more and less certain as numbers of trials 
# change -- absolutely and relatively.



# Simulating Binomial Bandits ---------------------------------------------

p_success <- c(0.2, 0.25, 0.3)

n_waves <- 4

n_per_wave <- 20

# Wave 1: Uniform draw over 3 arms

wave1_arms <- sample(p_success, 
                     size = n_per_wave, 
                     replace = TRUE)
wave1_arms
table(wave1_arms)

wave1_outcome <- rbinom(n_per_wave, 1, prob = wave1_arms)
wave1_outcome

df_wave1 <- tibble(wave1_arms, wave1_outcome)

table_wave1 <- table(df_wave1)
table_wave1

successes <- table_wave1[, "1"]
n <- rowSums(table_wave1)

best_binomial_bandit(successes, n)

posterior_prob_best <- best_binomial_bandit(successes, n)


# Wave 2: Thompson sampling

wave2_arms <- sample(p_success, size = n_per_wave,
                     prob = posterior_prob_best,
                     replace = TRUE)

table(wave2_arms)

wave2_outcome <- rbinom(n_per_wave, 1, prob = wave2_arms)

df_wave2 <- tibble(wave2_arms, wave2_outcome)

table_wave2 <- table(df_wave2)

successes <- table_wave2[, "1"]
n <- rowSums(table_wave2)

best_binomial_bandit(successes, n)


# Wave 3...

p_success <- c(.2, .25, .3)
n_waves <- 4
n_per_wave <- 20

# Set option for quiet summarise():
options(dplyr.summarise.inform = FALSE)

my_ts_bandit <- function(p_success, n_waves, n_per_wave){

  n_arms <- length(p_success)

  arm_id <- 1:n_arms

  df_arm_id <- tibble(arm_id, p_success)

  df_results <- crossing(wave = 1:n_waves, arm_id = arm_id, post_prob_best = NA_real_)

  df_results$arm <- rep(p_success, n_waves)

  df_results <- df_results %>% select(wave, arm_id, arm, post_prob_best)

  # Wave 1:

  wave1_arms <- sample(arm_id, size = n_per_wave, replace = TRUE)

  cat("Wave 1 is assigned!\n")

  wave1_outcome <- rbinom(n_per_wave, 1, prob = p_success[wave1_arms])

  df_wave1 <- tibble(wave1_arms, wave1_outcome)

  sf_wave1 <- df_wave1 %>% group_by(wave1_arms) %>%
    summarise(s = sum(wave1_outcome), n = n()) %>%
    right_join(df_arm_id, by = c("wave1_arms" = "arm_id")) %>%
    arrange(wave1_arms) %>%
    select(wave1_arms, p_success, s, n) %>%
    replace_na(list(s = 0, n = 0))

  successes <- sf_wave1$s
  n <- sf_wave1$n

  posterior_prob_best <- best_binomial_bandit(successes, n)

  df_results[1:n_arms, "post_prob_best"] <- posterior_prob_best

  sf_wave_total <- sf_wave1

  if(n_waves > 1){

    for(i in 2:n_waves){

      cat("Wave", i, "\n")

      wave_arms <- sample(arm_id, size = n_per_wave,
                           prob = posterior_prob_best,
                           replace = TRUE)

      wave_outcome <- rbinom(n_per_wave, 1, prob = p_success[wave_arms])

      df_wave <- tibble(wave_arms, wave_outcome)

      sf_wave_tmp <- df_wave %>% group_by(wave_arms) %>%
        summarise(s = sum(wave_outcome), n = n()) %>%
        right_join(df_arm_id, by = c("wave_arms" = "arm_id")) %>%
        arrange(wave_arms) %>%
        select(wave_arms, p_success, s, n) %>%
        replace_na(list(s = 0, n = 0))

      sf_wave_total$s <- sf_wave_total$s + sf_wave_tmp$s
      sf_wave_total$n <- sf_wave_total$n + sf_wave_tmp$n

      successes <- sf_wave_total$s
      n <- sf_wave_total$n

      posterior_prob_best <- best_binomial_bandit(successes, n)

      rows_to_fill <- (n_arms * (i - 1) + 1):(n_arms * i)

      df_results[rows_to_fill, "post_prob_best"] <- posterior_prob_best
    }
  }

  return(df_results)
}

my_b <- my_ts_bandit(p_success = c(.2, .25, .3),
             n_waves = 4, n_per_wave = 20)

ggplot(my_b, aes(wave, post_prob_best)) +
  geom_line(aes(color = as.factor(arm)))


# Exercise 2
#
# Reproduce Figure 1, left panel of 
# Offer-Westort, Coppock, and Green 2021.
