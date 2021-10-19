library(rjags)
load("./paper/4.Posterior_inferences_and_predictions/4.1.Posterior_distribution/rsamps_logistic_growth_by_individual_with_covariables_age_two_c_simul.RData")
rsamps_logistic_growth_model <- rsamps_logistic_growth_by_individual_with_covariables_age_two_c_simul
I <- 1000
C <- 3
n <- 198

# mu_j = (beta0^a + u_a + beta_w^a *Iw + beta_A^a * age )
# / (1 + exp(-(beta0^b + u_b + beta_w^b *Iw + beta_A^b * age + (beta0^c + beta_w^c *Iw) * Pia)))

# summary(rsamps_logistic_growth_model)$statistics[(2*n + 12) :(3 * n + 11),]

beta0_a <- c()
beta0_b <- c()
beta0_c <- c()
betaW_a <- c()
betaW_b <- c()
betaW_c <- c()
betaAge_a <- c()
betaAge_b <- c()
sigma <- c()
u_a <- matrix(data = NA, nrow = n, ncol = C * I)
u_b <- matrix(data = NA, nrow = n, ncol = C * I)

# Tengo que sacar también los efectos aleatorios a y b...

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
    
    sigma[(c-1) * I + i] <- unlist(rsamps_logistic_growth_model[c][i, n + 9])
    
    
    for(nn in 1 : n){
      u_a[nn, (c-1) * I + i] <- unlist(rsamps_logistic_growth_model[c][i, n + 11 + nn])
      u_b[nn, (c-1) * I + i] <- unlist(rsamps_logistic_growth_model[c][i, 2 * n + 11 + nn])
      
    }
    
  }
}

draws_posterior <- cbind(beta0_a, beta0_b, beta0_c, betaW_a, betaW_b, betaW_c, betaAge_a, betaAge_b, sigma,  t(u_a), t(u_b))

#draws_posterior
# beta0_a column 1
# beta0_b column 2
# beta0_c column 3

# betaW_a column 4
# betaW_b column 5
# betaW_c column 6

# betaAge_a column 7
# betaAge_b column 8

# sigma column 9

# u_a columns 10 - 207
# u_b columns 208 - 405
