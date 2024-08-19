# Randomization and Potential Outcomes code

# First, install.packages("tidyverse")

# Load helpful package:
library(tidyverse)

# Create a data frame with sample data:
df <- tibble(id = 1:5,
             y1 = c(10, 20, 25, 5, 0),
             y0 = c(12, 15, 10, 5, 4),
             te = y1 - y0)

# View it:
df

# True ATE:
ate <- mean(df$te)

# Specify one treatment assignment:
tr <- c(1, 0, 0, 0, 0)
# tr <- c(1, rep(0, 4))

# How to extract the observed outcomes y_obs?
# For the treateds:
df$y1[tr == 1]
# For the controls:
df$y0[tr == 0]

# Calculate the TE estimate for this assignment:
# (mean of obs treated) - (mean of obs controls)
# (using the correct potential outcomes)
te_obs <- mean(df$y1[tr == 1]) - mean(df$y0[tr == 0])

# Show te_obs:
te_obs

# Specify a single random assignment:
tr <- sample(0:1, size = nrow(df), replace = TRUE)

# Calculate the TE estimate for this assignment:
mean(df$y1[tr == 1]) - mean(df$y0[tr == 0])


# Repeat for all possible assignments -------------------------------------

# 'For' loops -------------------------------------------------------------

# i is a variable:

for(i in 1:10){
  print(i)
}


# Create an empty storage vector:
est_ates <- rep(NA, 5)

for(i in 1:5){
  
  tr <- rep(0, 5)
  tr[i] <- 1
  
  te_obs <- mean(df$y1[tr == 1]) - mean(df$y0[tr == 0])
  
  est_ates[i] <- te_obs
}

est_ates

# Mean of estimated ATEs:
mean(est_ates)

# Identical to true ATE? (up to numerical tolerance):
all.equal(mean(est_ates), ate)

# mean(est_ates) == ate is very demanding, subject to floating point arithmetic...

