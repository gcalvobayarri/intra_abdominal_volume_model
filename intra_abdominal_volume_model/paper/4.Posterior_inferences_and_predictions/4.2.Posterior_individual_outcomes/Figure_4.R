# Figure 4

library(rjags)
load("./paper/4.Posterior_inferences_and_predictions/4.1.Posterior_distribution/rsamps_logistic_growth_by_individual_with_covariables_age_two_c_simul.RData")
rsamps_logistic_growth_model <- rsamps_logistic_growth_by_individual_with_covariables_age_two_c_simul

# Credible intervals ADP-----------------

n <- 198
I <- 1000
C <- 3

ADP <- matrix(rep(NA, n * I * C), nrow = n)

for(c in 1 : C){
  for(nn in 1 : n){
    for(i in 1 : I){
      ADP[nn, (c-1) * I + i] <- unlist(rsamps_logistic_growth_model[c][i, nn])
    }
  }
}

U <- c(); L <- c(); M <- c()

ADP_sort <- matrix(rep(NA, n * I * C), nrow = nn)
for(nn in 1 : n){
  ADP_sort[nn,] <- sort(ADP[nn,])
  U[nn] <- ADP_sort[nn, 2925]
  L[nn] <- ADP_sort[nn, 75]
  M[nn] <- mean(ADP_sort[nn,])
}

# Data frames-----------------------
load("./paper/2.Intra_abdominal_volume_and_intra_abdominal_pressure_data/data_without_nas_simulated.RData")

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

df_ADP_age <- data.frame(x = 1:n, M = M, L = L, U = U, age_standard, 
                          age_real = age_standard * sd(age_without_nas$Age, na.rm = T) + mean(age_without_nas$Age, na.rm = T), 
                          sex, M_real = M * sd(X, na.rm = T) + mean(X, na.rm = T), 
                          L_real = L * sd(X, na.rm = T) + mean(X, na.rm = T), U_real = U * sd(X, na.rm = T) + mean(X, na.rm = T))

df_ADP_age_men <- subset(df_ADP_age, sex == 0)

df_ADP_age_women <- subset(df_ADP_age, sex == 1)

# Plots-----------------------------
# install.packages("ggplot2")
require(ggplot2)

ggplot(df_ADP_age_men, aes(x = age_real, y = M_real)) +
  geom_point(size = 0.5, color = 4) +
  geom_errorbar(aes(ymax = U_real, ymin = L_real), color = 4)+
  scale_color_manual( values = 4)+
  #scale_fill_manual(values=rainbow(6)) +
  theme(axis.title.x=element_blank())+
  labs(x = 'Age',y = expression(ADP))+
  coord_cartesian(ylim = c(5, 18), xlim = c(23,92))+
  theme_minimal()+
  theme(axis.line = element_line(color="black", size = 0.5, lineend = "round"))

ggplot(df_ADP_age_women, aes(x = age_real, y = M_real)) +
  geom_point(size = 0.5, color = 3) +
  geom_errorbar(aes(ymax = U_real, ymin = L_real), color = 3)+
  scale_color_manual( values = 4)+
  #scale_fill_manual(values=rainbow(6)) +
  theme(axis.title.x=element_blank())+
  labs(x = 'Age',y = expression(ADP))+
  coord_cartesian(ylim = c(5, 18), xlim = c(23,92))+
  theme_minimal()+
  theme(axis.line = element_line(color="black", size = 0.5, lineend = "round"))
