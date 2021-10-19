# 10. 4. 1 Ntzoufras CPO
library(rjags)
source('./paper/5.Model_checking/draws.R') #saved in draws_posterior

source('./paper/5.Model_checking/data_i.R') #saved in data_i_IAV

source('./paper/5.Model_checking/suma_logaritmica_de_elementos.R') #function


# 1st calculate ppo_i 10.3.4

log_cpo_i <- function(data_i, draws, log = TRUE) { #draws filas como iter, columnas n?mero de par?metros
  mu <- c()
  sigma <- draws[, 9]
  # mu_j = (beta0^a + u_a + beta_w^a *Iw + beta_A^a * age )
  # / (1 + exp(-(beta0^b + u_b + beta_w^b *Iw + beta_A^b * age + (beta0^c + beta_w^c *Iw) * Pia)))
  
  
  for(i in 1 : dim(draws)[1]){
    #print(i/3000*100)
    mu[i] <-  (draws[i, 1] + draws[i, 9 + data_i$id] + draws[i, 4] * data_i$sexo + 
                 draws[i, 7] * data_i$stand_age) / # numerador a
      ( 1 + exp(- (draws[i, 2] + draws[i, 207 + data_i$id] + draws[i, 5] * data_i$sexo +
                     draws[i, 8] * data_i$stand_age + # b
                     (draws[i, 3] + draws[i, 6] * data_i$sexo) * data_i$stand_presion   )   )  ) # c * X
    
  }
  
  log_likelihood <- dnorm(x = data_i$y, mean = mu, sd = sigma, log = log)
  
  inv_loglik <- - log_likelihood
  
  mean_inv_loglik <- - log(dim(draws)[1]) + suma_logaritmica_de_elementos(inv_loglik)
  
  log_cpo <- - mean_inv_loglik
  
  return(log_cpo) 
}

log_cpo <- c()
for(s in 1 : dim(data_i_IAV)[1]){
  log_cpo[s] <- log_cpo_i(data_i_IAV[s, ], draws_posterior)
}

save(log_cpo, file = './paper/5.Model_checking/log_cpo.RData')
