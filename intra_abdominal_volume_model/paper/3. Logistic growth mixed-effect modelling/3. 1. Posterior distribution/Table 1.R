# Table 1

library(rjags)
load("./paper/3. Logistic growth mixed-effect modelling/3. 1. Posterior distribution/rsamps_logistic_growth_by_individual_with_covariables_age_two_c_simul.RData")
rsamps_logistic_growth_model <- rsamps_logistic_growth_by_individual_with_covariables_age_two_c_simul

n <- 198 # number of individuals
summary(rsamps_logistic_growth_model)$statistics[c((1 + n) : (11 + n)),]
summary(rsamps_logistic_growth_model)$quantiles[c((1 + n) : (11 + n)),]
