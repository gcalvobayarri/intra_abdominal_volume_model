# Figure 6: Posterior distribution of the asymptotic parameter.
# 1. Saving posterior iterations-------------------------
library(rjags)
load("./paper/4.Posterior_inferences_and_predictions/4.1.Posterior_distribution/rsamps_logistic_growth_by_individual_with_covariables_age_two_c_simul.RData")
rsamps_logistic_growth_model <- rsamps_logistic_growth_by_individual_with_covariables_age_two_c_simul
n <- 198
I <- 1000
C <- 3

beta0_a <- c()
betaW_a <- c()
betaAge_a <- c()
sigma_a <- c()
u_a <- c()

for(c in 1 : C){
  for(i in 1 : I){
    beta0_a[(c-1) * I + i] <- unlist(rsamps_logistic_growth_model[c][i, 1 + n])

    betaW_a[(c-1) * I + i] <- unlist(rsamps_logistic_growth_model[c][i, 6 + n])
   
    betaAge_a[(c-1) * I + i] <- unlist(rsamps_logistic_growth_model[c][i, 4 + n])

    sigma_a[(c-1) * I + i] <- unlist(rsamps_logistic_growth_model[c][i, 10 + n])
    u_a[(c-1) * I + i] <- rnorm(1, 0, sd = sigma_a[(c-1) * I + i])
  }
}

# a = beta0_a + u_a + betaW_a + betaAge_a * Age

# 2. Histogram of a mean age individual-----------------

Expectation_a_mean_age_men <- c()
Expectation_a_mean_age_women <- c()

for(i in 1 : (I * C)){
  Expectation_a_mean_age_men[i] <- beta0_a[i] + u_a[i]
  Expectation_a_mean_age_women[i] <- beta0_a[i] + u_a[i] + betaW_a[i]
}

df_a <- data.frame(Expectation_a_mean_age_men, Expectation_a_mean_age_women)

# 3. 1. Men-------------------
library(ggplot2)
ggplot(df_a, aes(Expectation_a_mean_age_men))+geom_histogram(aes(y=..density..), alpha = 0)+
  geom_density(col=4, size = 1) + labs(x="a", y='Density') +
  coord_cartesian(xlim = c(-2,13), ylim = c(0, 0.25))+ theme_minimal()

# 3. 2. Women-----------------
ggplot(df_a, aes(Expectation_a_mean_age_women))+geom_histogram(aes(y=..density..), alpha = 0)+
  geom_density(col=3, size = 1) + labs(x="a", y='Density') +
  coord_cartesian(xlim = c(-2,13), ylim = c(0, 0.25))+ theme_minimal()

