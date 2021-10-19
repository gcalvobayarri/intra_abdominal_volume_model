# Figure 3

x <- seq(from = 0, to = 20, length.out = 1000)
y <- 5 / (1 + exp(- (-10 + 1 * x))) #a = 5, b = -10, c = 1

ADP_x_point <- - (log(5 - 2 * sqrt(6)) + (-10)) / 1
ADP_y_point <- 5 * (3 + sqrt(6)) / 6

IP_x_point <- 10 / 1; IP_y_point <- 5 / 2

MDP_x_point <- - (log(2 - sqrt(3)) + (-10)) / 1
MDP_y_point <- 5 * (3 + sqrt(3)) / 6

MAP_x_point <- - (log(2 + sqrt(3)) + (-10)) / 1
MAP_y_point <- 5 * (3 - sqrt(3)) / 6

data_points <- data.frame(x = c(ADP_x_point, IP_x_point, MDP_x_point, MAP_x_point),
                          y = c(ADP_y_point, IP_y_point, MDP_y_point, MAP_y_point),
                          point = c('ADP', 'IP', 'MDP', 'MAP'))

data_curve <- data.frame(x, y)

library(ggplot2)
ggplot(data = data_curve, aes(x=x, y=y)) + 
  geom_line() +
  theme_minimal()+
  geom_point(data = data_points, aes(x=x, y=y)) +
  annotate(geom="text", x=ADP_x_point + 0.75, y=ADP_y_point, col="black", 
           label='ADP', parse=T) +
  annotate(geom="text", x=IP_x_point + 0.55, y=IP_y_point, col="black", 
           label='IP', parse=T)+
  annotate(geom="text", x=MDP_x_point + 0.75, y=MDP_y_point, col="black", 
           label='MDP', parse=T)+
  annotate(geom="text", x=MAP_x_point + 0.75, y=MAP_y_point, col="black", 
           label='MAP', parse=T)
