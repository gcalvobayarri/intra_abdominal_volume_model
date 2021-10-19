# Figure 5
# 1. Loading posterior iterations-------------------------
library(rjags)
load("./paper/3. Logistic growth mixed-effect modelling/3. 1. Posterior distribution/rsamps_logistic_growth_by_individual_with_covariables_age_two_c_simul.RData")
rsamps_logistic_growth_model <- rsamps_logistic_growth_by_individual_with_covariables_age_two_c_simul

I <- 1000
C <- 3
n <- 198

# mu_j = (beta0^a + u_a + beta_w^a *Iw + beta_A^a * age )
# / (1 + exp(-(beta0^b + u_b + beta_w^b *Iw + beta_A^b * age + (beta0^c + beta_w^c *Iw) * Pia)))

beta0_a <- c()
beta0_b <- c()
beta0_c <- c()
betaW_a <- c()
betaW_b <- c()
betaW_c <- c()
betaAge_a <- c()
betaAge_b <- c()
sigma_a <- c()
u_a <- c()
sigma_b <- c()
u_b <- c()
sigma <- c()

for(c in 1 : C){
  for(i in 1 : I){
    beta0_a[(c-1) * I + i] <- unlist(rsamps_logistic_growth_model[c][i, 1 + n])
    beta0_b[(c-1) * I + i] <- unlist(rsamps_logistic_growth_model[c][i, 2 + n])
    beta0_c[(c-1) * I + i] <- unlist(rsamps_logistic_growth_model[c][i, 3 + n])
    
    betaW_a[(c-1) * I + i] <- unlist(rsamps_logistic_growth_model[c][i, 6 + n])
    betaW_b[(c-1) * I + i] <- unlist(rsamps_logistic_growth_model[c][i, 7 + n])
    betaW_c[(c-1) * I + i] <- unlist(rsamps_logistic_growth_model[c][i, 8 + n])
    
    betaAge_a[(c-1) * I + i] <- unlist(rsamps_logistic_growth_model[c][i, 4 + n])
    betaAge_b[(c-1) * I + i] <- unlist(rsamps_logistic_growth_model[c][i, 5 + n])
    
    sigma_a[(c-1) * I + i] <- unlist(rsamps_logistic_growth_model[c][i, 10 + n])
    u_a[(c-1) * I + i] <- rnorm(1, 0, sd = sigma_a[(c-1) * I + i])
    sigma_b[(c-1) * I + i] <- unlist(rsamps_logistic_growth_model[c][i, 11 + n])
    u_b[(c-1) * I + i] <- rnorm(1, 0, sd = sigma_b[(c-1) * I + i])
    
    sigma[(c-1) * I + i] <- unlist(rsamps_logistic_growth_model[c][i, 9 + n])
  }
}


# 2. Reading data----------------------
load("./paper/2. Intra-abdominal volume and intra-abdominal pressure data/data_without_nas_simulated.RData")

library(sqldf)
measures <- sqldf("SELECT (count(*)), id, sexo, edad, ipp FROM data_without_nas_simulated GROUP BY id ORDER BY edad")
sex_without_nas <- sqldf("SELECT DISTINCT(id), sexo, ipp FROM data_without_nas_simulated ORDER BY ipp, id")
names(sex_without_nas) <- c("id", "Sex", "ipp")
age_without_nas <- sqldf("SELECT DISTINCT(id), edad, ipp FROM data_without_nas_simulated ORDER BY ipp, id")
names(age_without_nas) <- c("id", "Age", "ipp")

age_standard <- (age_without_nas$Age-mean(age_without_nas$Age, na.rm = T))/ sd(age_without_nas$Age, na.rm = T)
sex <- sex_without_nas$Sex
individual_sizes <- measures$`(count(*))`

X <- matrix(NA, nrow = n, ncol = 75) # PIA matrix
N <- 0
for(i in 1 : n){
  for(j in 1 : individual_sizes[i]){
    X[i,j] <- data_without_nas_simulated$sim_pia[N + j]
  }
  N <- N + individual_sizes[i]
}

X_standard <- (X-mean(X, na.rm = T))/ sd(X, na.rm = T)


# 3. Posterior predictive for 64.65 years old patients-----------
PIA_standard <- seq(min(X_standard, na.rm = T), max(X_standard, na.rm = T), by = 0.03)
Expectation_mean_age_men <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)
Expectation_mean_age_women <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)
Predictive_mean_age_men <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)
Predictive_mean_age_women <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)
Age_mean <- 0

for(i in 1 : (I * C)){
  for(j in 1 : length(PIA_standard)){
    Expectation_mean_age_men[j, i] <- (beta0_a[i] + u_a[i] + betaAge_a[i] * Age_mean ) /
      (1 + exp(-(beta0_b[i] + u_b[i] + betaAge_b[i] * Age_mean + (beta0_c[i]) * PIA_standard[j])))
    Expectation_mean_age_women[j, i] <- (beta0_a[i] + u_a[i] + betaW_a[i] + betaAge_a[i] * Age_mean ) /
      (1 + exp(-(beta0_b[i] + betaW_b[i] + u_b[i] + betaAge_b[i] * Age_mean + (beta0_c[i] + betaW_c[i]) * PIA_standard[j])))
    
    Predictive_mean_age_men[j, i] <- rnorm(1, Expectation_mean_age_men[j, i], sd=sigma[i])
    Predictive_mean_age_women[j, i] <- rnorm(1, Expectation_mean_age_women[j, i], sd=sigma[i])
  }
}

# 4. Plots-----------------
# 4.1. Plotting men--------
Predictive_mean_age_men_q025 <- apply(Predictive_mean_age_men, 1, quantile, probs = c(0.025), na.rm = T)
Predictive_mean_age_men_q975 <- apply(Predictive_mean_age_men, 1, quantile, probs = c(0.975), na.rm = T)
Predictive_mean_age_men_mean <- apply(Predictive_mean_age_men, 1, mean, na.rm = T)



plot(seq(min(X, na.rm = T), max(X, na.rm = T), length.out = length(PIA_standard)),Predictive_mean_age_men_mean, type = 'l', ylim = c(0, 15), xlab = 'Intra-abdominal pressure', ylab = 'Intra-abdominal volume')
polygon(c(seq(min(X, na.rm = T), max(X, na.rm = T), length.out = length(PIA_standard)), rev(seq(min(X, na.rm = T), max(X, na.rm = T), length.out = length(PIA_standard)))), c(Predictive_mean_age_men_q025, rev(Predictive_mean_age_men_q975)), 
        col = 4, border = NA)
lines(seq(min(X, na.rm = T), max(X, na.rm = T), length.out = length(PIA_standard)), Predictive_mean_age_men_mean)


# 4.2. Plotting women------
Predictive_mean_age_women_q025 <- apply(Predictive_mean_age_women, 1, quantile, probs = c(0.025), na.rm = T)
Predictive_mean_age_women_q975 <- apply(Predictive_mean_age_women, 1, quantile, probs = c(0.975), na.rm = T)
Predictive_mean_age_women_mean <- apply(Predictive_mean_age_women, 1, mean, na.rm = T)


plot(seq(min(X, na.rm = T), max(X, na.rm = T), length.out = length(PIA_standard)), Predictive_mean_age_women_mean, type = 'l', ylim = c(0, 15), xlab = 'Intra-abdominal pressure', ylab = 'Intra-abdominal volume')
polygon(c(seq(min(X, na.rm = T), max(X, na.rm = T), length.out = length(PIA_standard)), rev(seq(min(X, na.rm = T), max(X, na.rm = T), length.out = length(PIA_standard)))), c(Predictive_mean_age_women_q025, rev(Predictive_mean_age_women_q975)), 
        col = 3, border = NA)
lines(seq(min(X, na.rm = T), max(X, na.rm = T), length.out = length(PIA_standard)), Predictive_mean_age_women_mean)
