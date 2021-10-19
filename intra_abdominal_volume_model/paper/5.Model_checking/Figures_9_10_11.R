load('./paper/5.Model_checking/PIT.RData')

load('./paper/5.Model_checking/log_cpo.RData')


par(mar=c(5,5,2,2)+0.1)

# Figure 9
hist(exp(log_cpo), freq = T, col = 'white', cex.lab=1.5, cex.axis=1.5, main = NULL, xlab = 'CPO')

# Figure 10
hist(PIT, freq = F, col = 'white', cex.lab=1.5, cex.axis=1.5, main = NULL)


# Figure 11
source('./paper/5.Model_checking/data_i.R') #saved in data_i_IAV
par(mar=c(5,5,2,2)+0.1)
plot(data_i_IAV$x, PIT, cex.lab=1.5, cex.axis=1.5, main = NULL,
     xlab = 'IAP', lwd = 3)
