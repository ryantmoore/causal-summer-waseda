# Load packages -----------------------------------------------------------

library(estimatr)
library(tidyverse)

# Load resume data --------------------------------------------------------

resume <- read_csv("https://j.mp/2sDjsHI")

# Summarise data ----------------------------------------------------------



# Estimate causal effect --------------------------------------------------

lm_out <- lm_robust()

lm_out

summary()


# Calculate confidence intervals ------------------------------------------



# Calculate power ---------------------------------------------------------

# Power = 1 - Pr(Type II error)
# Power = 1 - Pr(False negative)
# Power = 1 - Pr()

# 1. How powerful was our test?
# (What was it's probability of detecting an effect this big?)

power.prop.test()

# 2. How small an effect could this test have detected (w/ prob 0.8)?

# 3. How big did our sample need to be to detect this difference, 
# with probability 0.8?

# 4. What if we only needed a 0.6 chance of detecting this effect?

# 5. What if we had only had 1/10th the data? 
# - what would our probability be of detecting this effect?
# - what's the smallest effect we could have detected?

# 6. How many observations would we need to detect a decrease of 0.01
# (from the callback rate of putatively white-sounding resumes) with 
# probability 0.8?




