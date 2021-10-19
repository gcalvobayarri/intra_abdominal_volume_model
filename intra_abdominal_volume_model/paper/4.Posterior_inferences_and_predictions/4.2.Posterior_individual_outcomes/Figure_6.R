# Figure 6
# 1. Loading posterior iterations-------------------------
library(rjags)
load("./paper/4.Posterior_inferences_and_predictions/4.1.Posterior_distribution/rsamps_logistic_growth_by_individual_with_covariables_age_two_c_simul.RData")
rsamps_logistic_growth_model <- rsamps_logistic_growth_by_individual_with_covariables_age_two_c_simul

I <- 1000
C <- 3
n <- 198

# mu_j = (beta0^a + u_a + beta_w^a *Iw + beta_A^a * age )
# / (1 + exp(-(beta0^b + u_b + beta_w^b *Iw + beta_A^b * age + (beta0^c + beta_w^c *Iw) * Pia)))

# summary(rsamps_logistic_growth_model)$statistics[(n + 1) :(n + 11),]

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





# 2. Reading data-----------
load("./paper/2.Intra_abdominal_volume_and_intra_abdominal_pressure_data/data_without_nas_simulated.RData")

library(sqldf)
age_without_nas <- sqldf("SELECT DISTINCT(id), edad, ipp FROM data_without_nas_simulated ORDER BY ipp, id")
names(age_without_nas) <- c("id", "Age", "ipp")
measures <- sqldf("SELECT (count(*)), id, sexo, edad, ipp FROM data_without_nas_simulated GROUP BY id ORDER BY edad")
individual_sizes <- measures$`(count(*))`

X <- matrix(NA, nrow = n, ncol = 75) # PIA matrix
N <- 0
for(i in 1 : n){
  for(j in 1 : individual_sizes[i]){
    X[i,j] <- data_without_nas_simulated$sim_pia[N + j]
  }
  N <- N + individual_sizes[i]
}

# escala variables continuas
X_standard <- (X-mean(X, na.rm = T))/ sd(X, na.rm = T)


PIA_standard <- seq(min(X_standard, na.rm = T), max(X_standard, na.rm = T), by = 0.01)

# 3. Expectations by age-----------------
#85
Expectation_85_age_men <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)
Expectation_85_age_women <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)
Predictive_85_age_men <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)
Predictive_85_age_women <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)

#75
Expectation_75_age_men <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)
Expectation_75_age_women <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)
Predictive_75_age_men <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)
Predictive_75_age_women <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)

#average
Expectation_mean_age_men <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)
Expectation_mean_age_women <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)
Predictive_mean_age_men <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)
Predictive_mean_age_women <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)

#55
Expectation_55_age_men <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)
Expectation_55_age_women <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)
Predictive_55_age_men <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)
Predictive_55_age_women <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)

#45
Expectation_45_age_men <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)
Expectation_45_age_women <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)
Predictive_45_age_men <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)
Predictive_45_age_women <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)

#35
Expectation_35_age_men <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)
Expectation_35_age_women <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)
Predictive_35_age_men <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)
Predictive_35_age_women <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)

#25
Expectation_25_age_men <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)
Expectation_25_age_women <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)
Predictive_25_age_men <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)
Predictive_25_age_women <- matrix(data = NA, nrow = length(PIA_standard), ncol = I * C)

mn <- mean(age_without_nas$Age)
dv <- sd(age_without_nas$Age)
Age_85 <- (85 - mn) / dv
Age_75 <- (75 - mn) / dv
Age_mean <- 0
Age_55 <- (55 - mn) / dv
Age_45 <- (45 - mn) / dv
Age_35 <- (35 - mn) / dv
Age_25 <- (25 - mn) / dv

for(i in 1 : (I * C)){
  for(j in 1 : length(PIA_standard)){
    
    #85
    Expectation_85_age_men[j, i] <- (beta0_a[i] + u_a[i] + betaAge_a[i] * Age_85 ) /
      (1 + exp(-(beta0_b[i] + u_b[i] + betaAge_b[i] * Age_85 + (beta0_c[i]) * PIA_standard[j])))
    Expectation_85_age_women[j, i] <- (beta0_a[i] + u_a[i] + betaW_a[i] + betaAge_a[i] * Age_85 ) /
      (1 + exp(-(beta0_b[i] + betaW_b[i] + u_b[i] + betaAge_b[i] * Age_85 + (beta0_c[i] + betaW_c[i]) * PIA_standard[j])))
    
    Predictive_85_age_men[j, i] <- rnorm(1, Expectation_85_age_men[j, i], sd=sigma[i])
    Predictive_85_age_women[j, i] <- rnorm(1, Expectation_85_age_women[j, i], sd=sigma[i])
    
    
    #75
    Expectation_75_age_men[j, i] <- (beta0_a[i] + u_a[i] + betaAge_a[i] * Age_75 ) /
      (1 + exp(-(beta0_b[i] + u_b[i] + betaAge_b[i] * Age_75 + (beta0_c[i]) * PIA_standard[j])))
    Expectation_75_age_women[j, i] <- (beta0_a[i] + u_a[i] + betaW_a[i] + betaAge_a[i] * Age_75 ) /
      (1 + exp(-(beta0_b[i] + betaW_b[i] + u_b[i] + betaAge_b[i] * Age_75 + (beta0_c[i] + betaW_c[i]) * PIA_standard[j])))
    
    Predictive_75_age_men[j, i] <- rnorm(1, Expectation_75_age_men[j, i], sd=sigma[i])
    Predictive_75_age_women[j, i] <- rnorm(1, Expectation_75_age_women[j, i], sd=sigma[i])
    
    #Aveerage
    Expectation_mean_age_men[j, i] <- (beta0_a[i] + u_a[i] + betaAge_a[i] * Age_mean ) /
      (1 + exp(-(beta0_b[i] + u_b[i] + betaAge_b[i] * Age_mean + (beta0_c[i]) * PIA_standard[j])))
    Expectation_mean_age_women[j, i] <- (beta0_a[i] + u_a[i] + betaW_a[i] + betaAge_a[i] * Age_mean ) /
      (1 + exp(-(beta0_b[i] + betaW_b[i] + u_b[i] + betaAge_b[i] * Age_mean + (beta0_c[i] + betaW_c[i]) * PIA_standard[j])))
    
    Predictive_mean_age_men[j, i] <- rnorm(1, Expectation_mean_age_men[j, i], sd=sigma[i])
    Predictive_mean_age_women[j, i] <- rnorm(1, Expectation_mean_age_women[j, i], sd=sigma[i])
    
    #55
    Expectation_55_age_men[j, i] <- (beta0_a[i] + u_a[i] + betaAge_a[i] * Age_55 ) /
      (1 + exp(-(beta0_b[i] + u_b[i] + betaAge_b[i] * Age_55 + (beta0_c[i]) * PIA_standard[j])))
    Expectation_55_age_women[j, i] <- (beta0_a[i] + u_a[i] + betaW_a[i] + betaAge_a[i] * Age_55 ) /
      (1 + exp(-(beta0_b[i] + betaW_b[i] + u_b[i] + betaAge_b[i] * Age_55 + (beta0_c[i] + betaW_c[i]) * PIA_standard[j])))
    
    Predictive_55_age_men[j, i] <- rnorm(1, Expectation_55_age_men[j, i], sd=sigma[i])
    Predictive_55_age_women[j, i] <- rnorm(1, Expectation_55_age_women[j, i], sd=sigma[i])
    
    #45
    Expectation_45_age_men[j, i] <- (beta0_a[i] + u_a[i] + betaAge_a[i] * Age_45 ) /
      (1 + exp(-(beta0_b[i] + u_b[i] + betaAge_b[i] * Age_45 + (beta0_c[i]) * PIA_standard[j])))
    Expectation_45_age_women[j, i] <- (beta0_a[i] + u_a[i] + betaW_a[i] + betaAge_a[i] * Age_45 ) /
      (1 + exp(-(beta0_b[i] + betaW_b[i] + u_b[i] + betaAge_b[i] * Age_45 + (beta0_c[i] + betaW_c[i]) * PIA_standard[j])))
    
    Predictive_45_age_men[j, i] <- rnorm(1, Expectation_45_age_men[j, i], sd=sigma[i])
    Predictive_45_age_women[j, i] <- rnorm(1, Expectation_45_age_women[j, i], sd=sigma[i])
    
    #35
    Expectation_35_age_men[j, i] <- (beta0_a[i] + u_a[i] + betaAge_a[i] * Age_35 ) /
      (1 + exp(-(beta0_b[i] + u_b[i] + betaAge_b[i] * Age_35 + (beta0_c[i]) * PIA_standard[j])))
    Expectation_35_age_women[j, i] <- (beta0_a[i] + u_a[i] + betaW_a[i] + betaAge_a[i] * Age_35 ) /
      (1 + exp(-(beta0_b[i] + betaW_b[i] + u_b[i] + betaAge_b[i] * Age_35 + (beta0_c[i] + betaW_c[i]) * PIA_standard[j])))
    
    Predictive_35_age_men[j, i] <- rnorm(1, Expectation_35_age_men[j, i], sd=sigma[i])
    Predictive_35_age_women[j, i] <- rnorm(1, Expectation_35_age_women[j, i], sd=sigma[i])
    
    #25
    Expectation_25_age_men[j, i] <- (beta0_a[i] + u_a[i] + betaAge_a[i] * Age_25 ) /
      (1 + exp(-(beta0_b[i] + u_b[i] + betaAge_b[i] * Age_25 + (beta0_c[i]) * PIA_standard[j])))
    Expectation_25_age_women[j, i] <- (beta0_a[i] + u_a[i] + betaW_a[i] + betaAge_a[i] * Age_25 ) /
      (1 + exp(-(beta0_b[i] + betaW_b[i] + u_b[i] + betaAge_b[i] * Age_25 + (beta0_c[i] + betaW_c[i]) * PIA_standard[j])))
    
    Predictive_25_age_men[j, i] <- rnorm(1, Expectation_25_age_men[j, i], sd=sigma[i])
    Predictive_25_age_women[j, i] <- rnorm(1, Expectation_25_age_women[j, i], sd=sigma[i])
  }
}

# 4. Plots------------
# 4.1. Plotting men--------

#85
Predictive_85_age_men_mean <- apply(Predictive_85_age_men, 1, mean, na.rm = T)

#75
Predictive_75_age_men_mean <- apply(Predictive_75_age_men, 1, mean, na.rm = T)

#Average
Predictive_mean_age_men_mean <- apply(Predictive_mean_age_men, 1, mean, na.rm = T)

#55
Predictive_55_age_men_mean <- apply(Predictive_55_age_men, 1, mean, na.rm = T)

#45
Predictive_45_age_men_mean <- apply(Predictive_45_age_men, 1, mean, na.rm = T)

#35
Predictive_35_age_men_mean <- apply(Predictive_35_age_men, 1, mean, na.rm = T)

#25
Predictive_25_age_men_mean <- apply(Predictive_25_age_men, 1, mean, na.rm = T)



# library("RColorBrewer")
# display.brewer.all()

mean_predictive_data <- data.frame(predictive = c(Predictive_85_age_men_mean, Predictive_75_age_men_mean, Predictive_mean_age_men_mean,
                                                  Predictive_55_age_men_mean, Predictive_45_age_men_mean, Predictive_35_age_men_mean,
                                                  Predictive_25_age_men_mean), pressure =
                                     rep(seq(min(X, na.rm = T), max(X, na.rm = T), length.out = length(PIA_standard)), 7),
                                   age = rep(c('85', '75', 'average age', '55', '45', 
                                               '35', '25 years old'), each =length(PIA_standard)))

library(ggplot2)

ggplot(data = mean_predictive_data, aes(x = pressure, y = predictive, colour = age)) + geom_line(size=1) +
  scale_color_brewer(aesthetics = 'colour', name = 'PuBu')+
  theme_test(base_size = 20, base_line_size = 15/20)+
  labs(x="IAP", y="IAV")+
  theme(legend.title = element_blank()) +
  coord_cartesian(ylim = c(0, 10))


# 4.2. Plotting women------
#85
Predictive_85_age_women_mean <- apply(Predictive_85_age_women, 1, mean, na.rm = T)

#75
Predictive_75_age_women_mean <- apply(Predictive_75_age_women, 1, mean, na.rm = T)

#Average
Predictive_mean_age_women_mean <- apply(Predictive_mean_age_women, 1, mean, na.rm = T)

#55
Predictive_55_age_women_mean <- apply(Predictive_55_age_women, 1, mean, na.rm = T)

#45
Predictive_45_age_women_mean <- apply(Predictive_45_age_women, 1, mean, na.rm = T)

#35
Predictive_35_age_women_mean <- apply(Predictive_35_age_women, 1, mean, na.rm = T)

#25
Predictive_25_age_women_mean <- apply(Predictive_25_age_women, 1, mean, na.rm = T)



# library("RColorBrewer")
# display.brewer.all()

mean_predictive_data_women <- data.frame(predictive = c(Predictive_85_age_women_mean, Predictive_75_age_women_mean, Predictive_mean_age_women_mean,
                                                        Predictive_55_age_women_mean, Predictive_45_age_women_mean, Predictive_35_age_women_mean,
                                                        Predictive_25_age_women_mean), pressure =
                                           rep(seq(min(X, na.rm = T), max(X, na.rm = T), length.out = length(PIA_standard)), 7),
                                         age = rep(c('85', '75', 'average age', '55', '45', 
                                                     '35', '25 years old'), each =length(PIA_standard)))

library(ggplot2)

ggplot(data = mean_predictive_data_women, aes(x = pressure, y = predictive, colour = age)) + geom_line(size=1) +
  scale_color_brewer(aesthetics = 'colour', type = 'seq', palette = 2)+
  theme_test(base_size = 20, base_line_size = 15/20)+
  labs(x="IAP", y="IAV")+
  theme(legend.title = element_blank()) +
  coord_cartesian(ylim = c(0, 10))
