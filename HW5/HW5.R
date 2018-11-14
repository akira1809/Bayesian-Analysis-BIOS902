#HW 5

#the csv file from the coda generated in winBUGS
data <- read.csv("C:\\akira\\data\\coda_flat_prior.csv", header = TRUE)
data1 <- read.csv("C:\\akira\\data\\coda_normal_prior.csv", header = TRUE)
data2 <- read.csv("C:\\akira\\data\\coda_hierarchical.csv", header = TRUE)

#head(data)

library(ggplot2)
library(gridExtra)

#histogram of the slope beta1

#flat prior
p_beta1 <- ggplot(data = data, aes(x = beta1, y = ..density..)) + 
  geom_histogram(binwidth = 0.1, color = "black", fill = "blue") + 
  geom_vline(aes(xintercept=mean(beta1)),
             color="red", linetype="dashed", size=1)  + 
  geom_density(alpha=.5, fill="black") + 
  labs(title=expression(paste(beta[1], 
    " : assume flat prior")), x = expression(beta[1]))

#normal weak prior
p1_beta1 <- ggplot(data = data1, aes(x = beta1, y = ..density..)) + 
  geom_histogram(binwidth = 0.1, color = "black", fill = "blue") + 
  geom_vline(aes(xintercept=mean(beta1)),
             color="red", linetype="dashed", size=1)  + 
  geom_density(alpha=.5, fill="black") + 
  labs(title=expression(paste(beta[1], 
  " : assume weak normal prior")), x = expression(beta[1]))

#hierarchical model
p2_beta1 <- ggplot(data = data2, aes(x = beta1, y = ..density..)) + 
  geom_histogram(binwidth = 0.1, color = "black", fill = "blue") + 
  geom_vline(aes(xintercept=mean(beta1)),
             color="red", linetype="dashed", size=1)  + 
  geom_density(alpha=.5, fill="black") + 
  labs(title=expression(paste(beta[1], 
    " : assume hierarchical model")), x = expression(beta[1]))

#histogram of model predict for year 2016

#flat prior
p_yp <- ggplot(data = data, aes(x = y_p, y = ..density..)) + 
  geom_histogram(binwidth = 10, color = "black", fill = "blue") + 
  geom_vline(aes(xintercept=mean(y_p)),
             color="red", linetype="dashed", size=1)  + 
  geom_density(alpha=.5, fill="black")  + 
  labs(title = expression(paste(y[p], 
                  " 2016: flat prior")), 
       x = expression(y[p]))  

#normal weak prior

p1_yp <- ggplot(data = data1, aes(x = yp, y = ..density..)) + 
  geom_histogram(binwidth = 10, color = "black", fill = "blue") + 
  geom_vline(aes(xintercept=mean(yp)),
             color="red", linetype="dashed", size=1)  + 
  geom_density(alpha=.5, fill="black") + 
  labs(title = expression(paste(y[p], 
      " 2016: weak normal prior")), 
       x = expression(y[p])) 

#hierarchical model

p2_yp <- ggplot(data = data2, aes(x = yp, y = ..density..)) + 
  geom_histogram(binwidth = 10, color = "black", fill = "blue") + 
  geom_vline(aes(xintercept=mean(yp)),
             color="red", linetype="dashed", size=1)  + 
  geom_density(alpha=.5, fill="black") + 
  labs(title = expression(paste(y[p], 
      " 2016: hierarchical model")), 
       x = expression(y[p])) 

grid.arrange(p_beta1, p1_beta1, p2_beta1, nrow = 2)
grid.arrange(p_yp, p1_yp, p2_yp, nrow = 2)

