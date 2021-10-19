load("./paper/2. Intra-abdominal volume and intra-abdominal pressure data/data_without_nas_simulated.RData")


# Figure 1--------------------------
library(sqldf)
library(ggplot2)
measures <- sqldf("SELECT (count(*)), id, sexo, edad FROM data_without_nas_simulated GROUP BY id ORDER BY edad")
names(measures) <- c("Measures", "Patients", "sexo", "edad")

measures_hombres <- sqldf(("SELECT * FROM measures WHERE sexo = 0 ORDER BY edad "))
measures_mujeres <- sqldf(("SELECT * FROM measures WHERE sexo = 1 ORDER BY edad "))

measures_hombres$Patients <- factor(measures_hombres$Patients, levels = measures_hombres$Patients[order(measures_hombres$edad)])
ggplot(data = measures_hombres, aes(x=Patients, y=Measures)) + 
  geom_bar(fill = 4, stat="identity", position="dodge")+
  scale_x_discrete(labels = rep("",118))+
  coord_cartesian(ylim = c(0,75))

measures_mujeres$Patients <- factor(measures_mujeres$Patients, levels = measures_mujeres$Patients[order(measures_mujeres$edad)])
ggplot(data=measures_mujeres, aes(x=Patients, y=Measures)) + 
  geom_bar(fill = 3, stat="identity", position="dodge")+
  scale_x_discrete(labels = rep("",80))+
  coord_cartesian(ylim = c(0,75))


# Figure 2--------------------------
data_hombres <- sqldf(("SELECT * FROM data_without_nas_simulated WHERE sexo = 0 ORDER BY id "))
data_mujeres <- sqldf(("SELECT * FROM data_without_nas_simulated WHERE sexo = 1 ORDER BY id "))

# Spaghetti plot for men
ggplot(data_hombres, aes(sim_pia, sim_vol, group = id)) + 
  geom_point(colour = 4) +
  geom_line(colour = 4) +
  #   theme(legend.title = element_blank())+
  coord_cartesian(ylim = c(0,17), xlim = c(0,16))+
  theme(legend.position = "none")+
  theme_minimal()

# Spaghetti plot for women
ggplot(data_mujeres, aes(sim_pia, sim_vol, group = id)) + 
  geom_point(colour = 3) +
  geom_line(colour = 3) +
  #   theme(legend.title = element_blank())+
  coord_cartesian(ylim = c(0,17), xlim = c(0,16))+
  theme(legend.position = "none")+
  theme_minimal()
