
# Preliminaries -----------------------------------------------------------

library(cjoint)
library(tidyverse)

data("immigrationconjoint")
data("immigrationdesign")


# Estimate AMCE of Language Skills ----------------------------------------

# Immigrant's language skill is randomly assigned:
immigrationconjoint |> 
  count(`Language Skills`) |> 
  mutate(prop = n / sum(n))

lm_lang <- lm(Chosen_Immigrant ~ `Language Skills`, 
              data = immigrationconjoint)

# AMCE of going from fluent English to using an interpreter is -.16



# Using amce()  -----------------------------------------------------------

# Run AMCE estimator using all attributes in the design
conj_out <- amce(Chosen_Immigrant ~  Gender + Education + `Language Skills` +
                   `Country of Origin` + Job + `Job Experience` + `Job Plans` +
                   `Reason for Application` + `Prior Entry`, 
                 data = immigrationconjoint,
                 cluster = TRUE, 
                 respondent.id = "CaseID", 
                 design = immigrationdesign)

# Print summary:
summary(conj_out)

# Plot results:

plot(conj_out)


# Add interaction with respondent characteristic --------------------------

conj_out_rc <- amce(Chosen_Immigrant ~ Job + `Country of Origin` +
                      ethnocentrism:Job + ethnocentrism:`Country of Origin`,
                data = immigrationconjoint, na.ignore = TRUE,
                cluster = FALSE, 
                design = immigrationdesign,
                respondent.varying = "ethnocentrism")

# Print summary
summary(conj_out_rc)

# Plot results:
plot(conj_out_rc)


# Design Exercise ---------------------------------------------------------

# For index of all package functions/data:
help(package = "cjoint")

# For help/examples on one function:
library(cjoint)
help(makeDesign)

# (PS: to specify a function from a particular package)
# cjoint::makeDesign()