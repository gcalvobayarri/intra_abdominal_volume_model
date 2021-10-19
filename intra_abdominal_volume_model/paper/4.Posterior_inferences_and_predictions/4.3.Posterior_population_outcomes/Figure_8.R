# Figure 7
# 1. Saving posterior iterations-------------------------
library(rjags)
load("./paper/4.Posterior_inferences_and_predictions/4.1.Posterior_distribution/rsamps_logistic_growth_by_individual_with_covariables_age_two_c_simul.RData")
rsamps_logistic_growth_model <- rsamps_logistic_growth_by_individual_with_covariables_age_two_c_simul
n <- 198
I <- 1000
C <- 3

beta0_a <- c()
beta0_b <- c()
beta0_c <- c()
betaW_a <- c()
betaW_b <- c()
betaW_c <- c()
betaAge_a <- c()
betaAge_b <- c()
sigma_a <- c()
sigma_b <- c()
u_a <- c()
u_b <- c()

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
    
  }
}


# PDA (y coordenada) = (- (log(5 - 2 * sqrt(6)) + (beta0_b + u_b + betaW_b + betaAge_b * Age) ) / (beta0_c + betaW_c)  )

# 2. Marginal expectation point cloud, mean age------------------
load("./paper/2.Intra_abdominal_volume_and_intra_abdominal_pressure_data/data_without_nas_simulated.RData")

library(sqldf)
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

Posterior_ADP_age_men_y <- c()
Posterior_ADP_age_men_x <- c()

Posterior_ADP_age_women_y <- c()
Posterior_ADP_age_women_x <- c()

for(i in 1 : (I * C)){
  Posterior_ADP_age_men_y[i] <- (beta0_a[i] + u_a[i]) * (3 + sqrt(6)) / 6
  Posterior_ADP_age_men_x[i] <- (- (log(5 - 2 * sqrt(6)) + (beta0_b[i] + u_b[i]) ) / 
                                     (beta0_c[i])  )
  Posterior_ADP_age_women_y[i] <- (beta0_a[i] + u_a[i] + betaW_a[i]) * (3 + sqrt(6)) / 6
  Posterior_ADP_age_women_x[i] <- (- (log(5 - 2 * sqrt(6)) + (beta0_b[i] + u_b[i] + betaW_b[i]) ) / 
                                       (beta0_c[i] + betaW_c[i])  )
  
}

df_ADP_xy_men <- data.frame(x = Posterior_ADP_age_men_x, y = Posterior_ADP_age_men_y,
                            x_real = Posterior_ADP_age_men_x * sd(X, na.rm = T) + mean(X, na.rm = T))
df_ADP_xy_women <- data.frame(x = Posterior_ADP_age_women_x, y = Posterior_ADP_age_women_y,
                              x_real = Posterior_ADP_age_women_x * sd(X, na.rm = T) + mean(X, na.rm = T))

# 3. Plots-----------
# Men--------------
library(gridExtra)
library(ggplot2)
hist_top <- ggplot(df_ADP_xy_men, aes(x_real))+geom_histogram(aes(y=..density..), alpha = 0)+
  geom_density(col=4, size = 1) + labs(x="", y="") +
  coord_cartesian(xlim = c(3,16))+ theme_minimal(base_size = 20, base_line_size = 1.5)+
  theme( 
    axis.text.y=element_text(colour = 'white'),           
  )

empty <- ggplot()+geom_point(aes(1,1), colour="white")+
  theme(axis.ticks=element_blank(), 
        panel.background=element_blank(), 
        axis.text.x=element_blank(), axis.text.y=element_blank(),           
        axis.title.x=element_blank(), axis.title.y=element_blank())

scatter <- ggplot(df_ADP_xy_men, (aes(x=x_real, y=y))) +
  #stat_binhex(colour="white") +
  theme_minimal(base_size = 20, base_line_size = 1.5) +
  geom_point(alpha=0.1) +
  geom_density_2d(colour=4, size = 1) +
  scale_fill_gradient(low="white", high="blue") +
  labs(x="ADP pressure", y="ADP volume") +
  coord_cartesian(ylim = c(-1,12), xlim = c(3,16))+
  geom_hline(yintercept = mean(df_ADP_xy_men$y), colour  = 4, size = 1) +
  geom_vline(xintercept = mean(df_ADP_xy_men$x_real), colour  = 4, size = 1)

hist_right <- ggplot(df_ADP_xy_men, aes(y))+
  theme_minimal(base_size = 20, base_line_size = 1.5) +geom_histogram(aes(y=..density..), breaks=seq(-1, 12, by=.5), alpha = .0) +
  labs(x="", y="") + 
  geom_density(col=4, size = 1) + coord_flip() +
  theme( 
    axis.text.x=element_text(colour = 'white')
  )

grid.arrange(hist_top, empty, scatter, hist_right, ncol=2, nrow=2, widths=c(4, 1.5), heights=c(1.5, 4))

# Women--------------
library(gridExtra)
hist_top <- ggplot(df_ADP_xy_women, aes(x_real))+geom_histogram(aes(y=..density..), alpha = 0)+
  geom_density(col=3, size = 1) + labs(x="", y="") +
  coord_cartesian(xlim = c(3,16))+ theme_minimal(base_size = 20, base_line_size = 1.5)+
  theme( 
    axis.text.y=element_text(colour = 'white'),           
  )

empty <- ggplot()+geom_point(aes(1,1), colour="white")+
  theme(axis.ticks=element_blank(), 
        panel.background=element_blank(), 
        axis.text.x=element_blank(), axis.text.y=element_blank(),           
        axis.title.x=element_blank(), axis.title.y=element_blank())

scatter <- ggplot(df_ADP_xy_women, (aes(x=x_real, y=y))) +
  #stat_binhex(colour="white") +
  theme_minimal(base_size = 20, base_line_size = 1.5) +
  geom_point(alpha=0.1) +
  geom_density_2d(colour=3, size = 1) +
  scale_fill_gradient(low="white", high="blue") +
  labs(x="ADP pressure", y="ADP volume") +
  coord_cartesian(ylim = c(-1,12), xlim = c(3,16))+
  geom_hline(yintercept = mean(df_ADP_xy_women$y), colour  = 3, size = 1) +
  geom_vline(xintercept = mean(df_ADP_xy_women$x_real), colour  = 3, size = 1)

hist_right <- ggplot(df_ADP_xy_women, aes(y))+
  theme_minimal(base_size = 20, base_line_size = 1.5) +geom_histogram(aes(y=..density..), breaks=seq(-1, 12, by=.5), alpha = .0) +
  labs(x="", y="") +
  geom_density(col=3, size = 1) + coord_flip()+
  theme( 
    axis.text.x=element_text(colour = 'white'),           
  )

grid.arrange(hist_top, empty, scatter, hist_right, ncol=2, nrow=2, widths=c(4, 1.5), heights=c(1.5, 4))
