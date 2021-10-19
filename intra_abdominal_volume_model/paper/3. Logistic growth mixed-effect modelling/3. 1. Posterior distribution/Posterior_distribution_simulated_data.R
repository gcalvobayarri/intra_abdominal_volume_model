# Posterior distribution


# 1. Reading data--------------
load("./paper/2. Intra-abdominal volume and intra-abdominal pressure data/data_without_nas_simulated.RData")

library(sqldf)
measures <- sqldf("SELECT (count(*)), id, sexo, edad, ipp FROM data_without_nas_simulated GROUP BY id ORDER BY edad")
sex_without_nas <- sqldf("SELECT DISTINCT(id), sexo, ipp FROM data_without_nas_simulated ORDER BY ipp, id")
names(sex_without_nas) <- c("id", "Sex", "ipp")
age_without_nas <- sqldf("SELECT DISTINCT(id), edad, ipp FROM data_without_nas_simulated ORDER BY ipp, id")
names(age_without_nas) <- c("id", "Age", "ipp")



# 2. Preparing data ----------
sex <- sex_without_nas$Sex

n <- 198
individual_sizes <- measures$`(count(*))`
Y <- matrix(NA, nrow = n, ncol = 75) #volume matrix
N <- 0
for(i in 1 : n){
  for(j in 1 : individual_sizes[i]){
    Y[i,j] <- data_without_nas_simulated$sim_vol[N + j]
  }
  N <- N + individual_sizes[i]
}

X <- matrix(NA, nrow = n, ncol = 75) # PIA matrix
N <- 0
for(i in 1 : n){
  for(j in 1 : individual_sizes[i]){
    X[i,j] <- data_without_nas_simulated$sim_pia[N + j]
  }
  N <- N + individual_sizes[i]
}

# standardizing variables
X_standard <- (X-mean(X, na.rm = T))/ sd(X, na.rm = T)

age_standard <- (age_without_nas$Age-mean(age_without_nas$Age, na.rm = T))/ sd(age_without_nas$Age, na.rm = T)


# 3. Modelling------------------
library(rjags)
library(mcmcplots)

data_for_the_model <- list(Y = Y, X = X_standard,  N = n, 
                           individual_sizes = individual_sizes,
                           sex = sex,
                           age = age_standard)



inits <- function() {
  list(betaW = runif(3, -10, 10), betaA = runif(2, -10, 10), beta0 = runif(3, 0, 10),
       sigma = runif(1, 0, 10),  sigmaA = runif(1, 0, 10), sigmaB = runif(1, 0, 10)
  )
}
parameters <- c("beta0", "betaW", "betaA", "sigmaA", "sigmaB", "sigma", "PDA", "u_a", "u_b")


result <- jags.model("./paper/3. Logistic growth mixed-effect modelling/3. 1. Posterior distribution/Model/logistic_growth_curve_model.txt", 
                     data_for_the_model, inits, n.chains = 3,
                     n.adapt = 0)

update(result, n.iter = 100000)

rsamps_logistic_growth_by_individual_with_covariables_age_two_c_simul <- coda.samples(result, parameters, n.iter = 100000,  thin = 100)

mcmcplot(rsamps_logistic_growth_by_individual_with_covariables_age_two_c_simul)

summary(rsamps_logistic_growth_by_individual_with_covariables_age_two_c_simul)$statistics[c((3 + n - 2 ) : (3 + n + 8)),]

summary(rsamps_logistic_growth_by_individual_with_covariables_age_two_c_simul)$quantiles[c((3 + n - 2 ) : (3 + n + 8)),]

# dic_logistic_growth_by_individual_with_covariables_age_two_c <- dic.samples(result, n.iter = 1000000, thin = 1000)

save(rsamps_logistic_growth_by_individual_with_covariables_age_two_c_simul, 
     file = "./paper/3. Logistic growth mixed-effect modelling/3. 1. Posterior distribution/rsamps_logistic_growth_by_individual_with_covariables_age_two_c_simul.RData")
# load("./paper/Logistic growth mixed-effect modelling/Posterior distribution/rsamps_logistic_growth_by_individual_with_covariables_age_two_c_simul.RData")
