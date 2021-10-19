# 10. 4. 2 Ntzoufras PIT
library(rjags)
source('./paper/5.Model_checking/draws.R') #saved in draws_posterior

source('./paper/5.Model_checking/data_i.R') #saved in data_i_IAV

source('./paper/5.Model_checking/suma_logaritmica_de_elementos.R') #function


size_data <- dim(data_i_IAV)[1]
size_draws <- dim(draws_posterior)[1]
# weights

log_weights_i <- function(data_i, draws, log = TRUE) { #draws filas como iter, columnas n?mero de par?metros
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
  
  log_w <- inv_loglik - suma_logaritmica_de_elementos(inv_loglik)
  
  
  
  return(log_w) 
}

# una obs.
size_pred <- 1000
y_pred <- matrix(data = NA, nrow = size_data, ncol = size_pred)

PIT <- c()
for(obs in 1 : size_data){
  
  
  
  prob_w <- exp(log_weights_i(data_i_IAV[obs, ], draws_posterior)) # alguna es distinto de 0 si hacemos exp
  
  
  for(i in 1 : size_pred){
    idx <- sample(1:dim(draws_posterior)[1],1, prob = prob_w)
    
    sigma <- draws_posterior[idx, 9]
    # mu_j = (beta0^a + u_a + beta_w^a *Iw + beta_A^a * age )
    # / (1 + exp(-(beta0^b + u_b + beta_w^b *Iw + beta_A^b * age + (beta0^c + beta_w^c *Iw) * Pia)))
    
    
    
    #print(i/3000*100)
    mu <-  (draws_posterior[idx, 1] + draws_posterior[idx, 9 + data_i_IAV$id[obs]] + draws_posterior[idx, 4] * data_i_IAV$sexo[obs] + 
              draws_posterior[idx, 7] * data_i_IAV$stand_age[obs]) / # numerador a
      ( 1 + exp(- (draws_posterior[idx, 2] + draws_posterior[idx, 207 + data_i_IAV$id[obs]] + draws_posterior[idx, 5] * data_i_IAV$sexo[obs] +
                     draws_posterior[idx, 8] * data_i_IAV$stand_age[obs] + # b
                     (draws_posterior[idx, 3] + draws_posterior[idx, 6] * data_i_IAV$sexo[obs]) * data_i_IAV$stand_presion[obs]   )   )  ) # c * X
    y_pred[obs,i] <- rnorm(1, mean = mu, sd = sigma)
    
    
    
  }
  
  PIT[obs] <- sum(y_pred[obs,] <= data_i_IAV$y[obs]) / size_pred
}

save(PIT, file = './paper/5.Model_checking/PIT.RData')
