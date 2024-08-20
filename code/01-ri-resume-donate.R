## RI Hypothesis Tests and Confidence Intervals
## Ryan T. Moore
## First: 2017-07-06
## Last: 2024-08-20

# Preliminaries -----------------------------------------------------------

# To install package {qss}, 
# 1. install.packages("devtools"), then
# 2. devtools::install_github("kosukeimai/qss-package")
# (See also https://github.com/kosukeimai/qss)

library(qss)
library(tidyverse)


# RI for Resume Experiment ------------------------------------------------

data(resume)

head(resume)

table(resume$race, resume$call)

# Calculate obs TE:
resume |>
  group_by(race) |>
  summarise(callback_rate = mean(call))

# Calculate obs TE:
obs_te <- mean(resume$call[resume$race == "black"]) -
  mean(resume$call[resume$race == "white"])

# How many ways to split 4870 in half?
choose(4870, 4870 / 2)

# ... Better sample from the randomization distribution, then.

# Core idea:
tc <- rep(0:1, 5)
sample(tc)
sample(tc)

# Then calculate and store est ATE for each assignment.

# Implementation:

n_samples <- 10000
df <- tibble(est_te = rep(NA, n_samples))
base_assignment <- rep(0:1, 4870 / 2)

for(idx in 1:n_samples){

  this_assignment <- sample(base_assignment)

  this_mean_tr <- mean(resume$call[this_assignment == 1])

  this_mean_co <- mean(resume$call[this_assignment == 0])

  df$est_te[idx] <- this_mean_tr - this_mean_co

}

# Results

summary(df)
ri_hist <- ggplot(df, aes(est_te)) + 
  geom_histogram(stat = "count") +
  labs(x = "Estimated Effect of Black Name on CV")

# pdf("figs/riHist_resume1.pdf")
ri_hist
# dev.off()

# pdf("figs/riHist_resume2.pdf")
ri_hist + 
  geom_vline(xintercept = obs_te, color = "red") +
  annotate("text", x = -.028, y = 300, label = "Observed!", color = "red", angle = 15)
# dev.off()

p_value <- mean(abs(df$est_te) >= abs(obs_te))
p_value

t.test(call ~ race, data = resume)


# RI for Gerber Green Donations Example, Box 3.7, page 65 -----------------


# Prep data
donate <- tibble(tr = rep(1:0, each = 10),
                 donation = c(500, 100, 100, 50, 25, 25, 0, 0, 0, 0, 
                              25, 20, 15, 15, 10, 5, 5, 5, 0, 0))

# Calculate observed difference in means
obs_te <- mean(donate$donation[donate$tr == 1]) -
  mean(donate$donation[donate$tr == 0])

# (Alternate implementation)
donation_by_assg <- donate |>
  group_by(tr) |>
  summarise(avg_donation = mean(donation))

donation_by_assg

obs_te <- donation_by_assg$avg_donation[2] - donation_by_assg$avg_donation[1]
obs_te

# Implement RI

n_samples <- 10000
df_est_te <- tibble(est_te = rep(NA, n_samples))
base_assignment <- rep(0:1, nrow(donate) / 2)

for(idx in 1:n_samples){
  this_assignment <- sample(base_assignment)
  this_mean_tr <- mean(donate$donation[this_assignment == 1])
  this_mean_co <- mean(donate$donation[this_assignment == 0])
  df_est_te$est_te[idx] <- this_mean_tr - this_mean_co
}

# Results

summary(df_est_te)
ri_hist <- ggplot(df_est_te, aes(est_te)) +
  geom_histogram(stat = "count") +
  labs(x = "Estimated Effect of Mailer on Donation")

# pdf("figs/riHist_donate1.pdf")
ri_hist
# dev.off()

# pdf("figs/riHist_donate2.pdf")
ri_hist + geom_vline(xintercept = obs_te, color = "red") +
  annotate("text", x = 62, y = 140, label = "Observed!", color = "red", angle = 15)
# dev.off()

p_value <- mean(df_est_te$est_te >= obs_te)
p_value

# 1-sided t-test
# (using "less", since R will take [group 0] - [group 1])
t.test(donation ~ tr, data = donate, alternative = "less")



# The RI Confidence Interval ----------------------------------------------

# Using the ri package:

# To install (if "unavailable for this R version"):
# 1. Go to https://cran.r-project.org/src/contrib/Archive/ri/
# 2. Save ri_0.9.tar.gz to, e.g., Desktop
# 3. (May need to gzip ri_0.9.tar)
# 4. At R prompt, run (Mac syntax)
# install.packages("~/Desktop/ri_0.9.tar.gz", repos = NULL, source = TRUE)

library(ri)
our_permutations <- genperms(donate$tr, maxiter = 1000)

# For a one-sided test:
# (where only *more* donation is evidence against the null)
lower <- -Inf
upper <- invert.ci(donate$donation, donate$tr, prob = 0.5,
                   perms = our_permutations,
                   targetp = 0.95)
c(lower, upper)

# For a two-sided test:
lower <- invert.ci(donate$donation, donate$tr, prob = 0.5,
                   perms = our_permutations,
                   targetp = 0.025)

upper <- invert.ci(donate$donation, donate$tr, prob = 0.5,
                   perms = our_permutations,
                   targetp = 0.975)

c(lower, upper)

# By hand, without the ri package:
ri_ci <- function(tau = 0, n_samples = 1000){

  # Initialise storage:
  df_tau_pval <- data.frame(tau = rep(NA, length(tau)),
                            pval = rep(NA, length(tau)))

  # for all tau values to test:
  for(tau_idx in 1:length(tau)){
    
    # Let y_obs be everyone's potential outcome under treatment:
    donate$donation_tr <- donate$donation
    # Then fix, with controls' potential outcome under treatment 
    # being their potential outcome under control, plus the tau:
    donate$donation_tr[donate$tr == 0] <- donate$donation[donate$tr == 0] + 
      tau[tau_idx]
    
    # Similar for potential outcomes under control:
    donate$donation_co <- donate$donation
    donate$donation_co[donate$tr == 1] <- donate$donation[donate$tr == 1] - 
      tau[tau_idx]
    
    # Initialise storage for testing *this* tau only:
    df <- data.frame(est_te = rep(NA, n_samples))
    
    # Initialise a 0/1 assignment vector to return to:
    base_assignment <- rep(0:1, nrow(donate) / 2)
    
    for(idx in 1:n_samples){
      this_assignment <- sample(base_assignment)
      this_mean_tr <- mean(donate$donation_tr[this_assignment == 1])
      this_mean_co <- mean(donate$donation_co[this_assignment == 0])
      df$est_te[idx] <- this_mean_tr - this_mean_co
    }
    
    # Calculate p-value for this tau:
    p_value <- mean(abs(df$est_te - tau[tau_idx]) >= abs(obs_te - tau[tau_idx]))
    
    # Store it:
    df_tau_pval[tau_idx, ] <- c(tau[tau_idx], p_value)
  }

  return(df_tau_pval)
}

df_taus <- ri_ci(-130:270, 100)

# Keep only those tau s.t. p > \alpha, and plot:

df_taus |> filter(pval > 0.05) |>
  summarise(lb = min(tau), ub = max(tau)) |> ggplot() +
  geom_segment(aes(x = lb, xend = ub, y = 0, yend = 0)) +
  geom_point(x = obs_te, y = 0) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "Possible Values of ATE")

# For sequentially blocked designs, see 
# blockTools::invertRIconfInt()
