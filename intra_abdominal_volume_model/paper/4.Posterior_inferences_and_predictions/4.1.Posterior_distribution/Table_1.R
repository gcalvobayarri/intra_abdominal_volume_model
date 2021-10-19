# Table 1

library(rjags)
load("./paper/4.Posterior_inferences_and_predictions/4.1.Posterior_distribution/rsamps_logistic_growth_by_individual_with_covariables_age_two_c_simul.RData")
rsamps_logistic_growth_model <- rsamps_logistic_growth_by_individual_with_covariables_age_two_c_simul

n <- 198 # number of individuals
summary(rsamps_logistic_growth_model)$statistics[c((1 + n) : (11 + n)),]
summary(rsamps_logistic_growth_model)$quantiles[c((1 + n) : (11 + n)),]
