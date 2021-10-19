load("./paper/2. Intra-abdominal volume and intra-abdominal pressure data/data_without_nas_simulated.RData")
library(sqldf)

# Age ---------------------
age_data <- sqldf("SELECT DISTINCT(id), edad, ipp, sexo FROM data_without_nas_simulated ORDER BY ipp, id")
names(age_data) <- c("id", "Age", "ipp", "Sex")

# Numerical summary global age
summary(age_data$Age)

# Numerical summary men age
summary(age_data$Age[age_data$Sex==0])

# Numerical summary women age
summary(age_data$Age[age_data$Sex==1])


# IAP --------------------
# Numerical summary intra-abdominal pressure
summary(data_without_nas_simulated$sim_pia)

# IAV --------------------
# Numerical summary intra-abdominal volume
summary(data_without_nas_simulated$sim_vol)
