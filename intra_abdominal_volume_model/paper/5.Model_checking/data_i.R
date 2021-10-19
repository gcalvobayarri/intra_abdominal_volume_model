load("./paper/2.Intra_abdominal_volume_and_intra_abdominal_pressure_data/data_without_nas_simulated.RData")

n <- 198

library(sqldf)
measures <- sqldf("SELECT (count(*)), id, sexo, edad, ipp FROM data_without_nas_simulated GROUP BY id ORDER BY edad")
individual_sizes <- measures$`(count(*))`

Y <- matrix(NA, nrow = n, ncol = max(individual_sizes)) # PIA matrix
N <- 0
for(i in 1 : n){
  for(j in 1 : individual_sizes[i]){
    Y[i,j] <- data_without_nas_simulated$sim_vol[N + j]
  }
  N <- N + individual_sizes[i]
}



medida_numero <- c()
for(i in 1 : n){
  medida_numero <- c(medida_numero, 1 : individual_sizes[i])
}

data_i_IAV <- data.frame(y = data_without_nas_simulated$sim_vol, x = data_without_nas_simulated$sim_pia, 
                         id = unlist(Map(rep, 1 : n, individual_sizes)), edad = data_without_nas_simulated$edad,
                         sexo = as.numeric(as.character(data_without_nas_simulated$sexo)),
                         medidas = unlist(Map(rep, individual_sizes, individual_sizes)), 
                         medida_numero = medida_numero)


stand_age <- (data_i_IAV$edad - mean(data_i_IAV$edad)) / sd(data_i_IAV$edad)
stand_presion <- (data_i_IAV$x - mean(data_i_IAV$x)) / sd(data_i_IAV$x)

data_i_IAV <- data.frame(data_i_IAV, stand_age = stand_age, stand_presion = stand_presion)
