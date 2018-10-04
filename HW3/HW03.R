#BIOS902
#HW3

#Question 5.3

library(ggplot2)
library(tidyr)
library(gridExtra)
library(cowplot)
library(scales)
library(directlabels)

#Question 5.3

#part (a)

#Input the data
school <- c("A", "B", "C", "D", "E", "F", "G", "H")
effect <- c(28, 8, -3, 7, -1, 1, 18, 12) # those y_j's 
sigma <- c(15, 10, 16, 11, 9, 11, 10, 18) # those sigma_j's
data <- data.frame(school = school, effect = effect, sigma = sigma)

#preliminary quantities for computing posterior inferences
J <- length(school) #number of theta

tau <- seq(0.01, 40, 0.01) #set a range of tau's. Start from 0.01 to avoid singularity

n <- length(tau) #just an intermediate value

hat_mu <- rep(NA, n); InvV_mu <- rep(NA, n) #initialize the two values in equation 5.20

for (i in 1:n){
   hat_mu[i] <- sum(effect/(sigma^2 + (tau[i])^2))/sum(1/(sigma^2 + (tau[i])^2))
   InvV_mu[i] <- sum(1/(sigma^2 + (tau[i])^2))
}#define values from equation (5.20), for computation of posterior inference later

margin_post_tau <- rep(NA, n)
for (i in 1:n){
  margin_post_tau[i] <- sqrt(1/InvV_mu[i])*prod(exp(-0.5*(effect - hat_mu[i])^2/
                        (sigma^2 + (tau[i])^2))/sqrt(sigma^2 + (tau[i])^2))
} #compute marginal posterior density for tau given data y

#store the results in data frame
inference <- data.frame(tau = tau, margin_post_tau = margin_post_tau)

#plot the posterior marginal density p(tau|y) (Figure 5.5)
ggplot(data = inference, aes(x=tau, y=margin_post_tau)) +
  geom_line() + 
  theme(
    axis.text.y = element_blank()) + #hide the unnormalized values
  scale_x_continuous(breaks = seq(0, 40, 5))  + #set the steps size to be 5 
  labs(title=expression(paste("Marginal posterior desity"," p(",tau,"|y)")))+
  labs(y = "density")

# figure 5.6
theta <- matrix(NA, n, J) #initialize the theta values
for (i in 1:n){
  for (j in 1:J){
    theta[i, j] <- (effect[j]/(sigma[j])^2 + 
                      hat_mu[i]/(tau[i])^2)/(1/(sigma[j])^2 + 1/(tau[i])^2)
  }
} #fill in the corresponding value of theta_hat for different grid nubmers of tau
theta <- cbind(theta, tau) #bind y value with x value
colnames(theta) <- c(school, "tau") #name the variables
theta <- as.data.frame(theta) #make it a data frame
new_theta <- gather(theta, school_name, trt_effect,
                    A:H, factor_key=TRUE) #change data to long format

ggplot(data = new_theta, aes(x = tau, y = trt_effect, colour = school_name)) + 
  geom_line() + 
  labs(title = expression(paste("Estimate of E("
      ,theta[j],"|",tau,", y) by schools")),y = 
        "Estimated Treatment Effects") # plot figure 5.6

#figure 5.7
sd_theta <- matrix(NA, n, J) #initialize the theta values
for (i in 1:n){
  for (j in 1:J){
    sd_theta[i, j] <- sqrt((sigma[j]^2/(sigma[j]^2 + 
                            tau[i]^2))^2/(sum(1/(tau[i]^2 + sigma^2))) + 
                              1.0/(1.0/(sigma[j]^2) + 1.0/(tau[i]^2)))
  }
} #fill in the corresponding value of sd_theta for different grid nubmers of tau
sd_theta <- cbind(sd_theta, tau) #bind y value with x value
colnames(sd_theta) <- c(school, "tau") #name the variables
sd_theta <- as.data.frame(sd_theta) #make it a data frame
new_sd_theta <- gather(sd_theta, school_name, post_sd,
                    A:H, factor_key=TRUE) #change data to long format
ggplot(data = new_sd_theta, aes(x = tau, y = post_sd, colour = school_name)) + 
  geom_line() + 
  labs(title = expression(paste("posterior standard deviations: sd("
      ,theta[j],"|",tau,", y) by schools")),y = 
         "posterior standard deviations", colour = "School") + 
  ylim (0, 20)#Figure 5.7

#draw posterior tau, mu, and theta sequentially
set.seed(34) #set seed number
size <- 200 #set simulation size
post_tau <- sample(tau, size = size, replace = TRUE, 
       prob = margin_post_tau)#draw posterior tau
hat_mu_post <- rep(NA, size) #initialize mean for posterior mu
sd_mu_post <- rep(NA, size) #initialize sd for posterior mu
for (i in 1:size){
  hat_mu_post[i] <- sum(effect/(sigma^2 + (post_tau[i])^2))/sum(1/(sigma^2 + (post_tau[i])^2))
  sd_mu_post[i] <- 1/sqrt(sum(1/(sigma^2 + (post_tau[i])^2)))
}#generate posterior mean and sd for mu
post_mu <- rnorm(n= size, mean= hat_mu_post, 
                 sd = sd_mu_post) #generate posterior mu
hat_theta_post <- matrix(NA, size, J) #initialize mean for posterior theta
sd_theta_post <- matrix(NA, size, J) #initialize sd for posterior theta
for (j in 1:J){
  for (i in 1:size){
    hat_theta_post[i, j] <- (effect[j]/sigma[j]^2 + post_mu[i]/post_tau[i]^2)/
      (1/sigma[j]^2 + 1/post_tau[i]^2)
    sd_theta_post[i, j] <- sqrt(1/(1/sigma[j]^2 + 1/post_tau[i]^2))
  }
}
post_theta <- matrix(NA, size, J) #initialize sample for posterior theta
for (j in 1:J){
  post_theta[, j] <- rnorm(n = size, mean = hat_theta_post[, j], 
                          sd = sd_theta_post[, j])
} #generate posteior theta for each school

post_quant <- matrix(NA, J, 5) #initialize for quantile summary
for (j in 1:J){
  post_quant[j, ] <- quantile(x = post_theta[, j], probs = 
              c(0.025, 0.25, 0.5, 0.75, 0.975))
} #compute posterior quantiles for theta in different schools
post_quant <- cbind(school, round(post_quant, digits = 1)) #get school names
post_quant <- as.data.frame(post_quant) #convert to data.frame
colnames(post_quant) <- c("School", "2.5%", "25%", "median", "75%", "97.5%")
print(post_quant) #show table 5.3

#check the effect in school A and the largest effect
post_theta <- as.data.frame(post_theta) #convert posterior simulation to data.frame
colnames(post_theta) <- school
post_theta$max <- apply(post_theta, 1, max)

hist_A <-ggplot(data = post_theta, 
                aes(x=A)) + #histogram of posterior effect for school A
  labs(x = "Effect in School A")+
  xlim(c(-20, 60))+
  geom_histogram(binwidth = 2, color = "black", fill = "blue")
hist_max <-ggplot(data = post_theta, 
                  aes(x=max)) + #histogram of posterior effect for school A
  labs(x = "Largest effect")+
  xlim(c(-20, 60))+
  geom_histogram(binwidth = 2, color = "black", fill = "blue") 
#grid.arrange(hist_A, hist_max, nrow = 1)
plot_grid(hist_A, hist_max, labels = NULL)

#compute probability of school j is the best
best_prob <- as.data.frame(matrix(ncol= J, nrow=0)) #assign space for storing results
for (j in 1:J){
  best_prob[1, j] <- percent(mean(post_theta[, j] == post_theta[, 9]))
} #compute the probability of each school has best effects
colnames(best_prob) <- school #retrieve school names
best_prob
pair_prob <- as.data.frame(matrix(ncol= J, nrow = J)) #assign space for storing results
for (j in 1:J){
  for(i in 1:J){
    pair_prob[i, j] <- percent(mean(post_theta[, j] > post_theta[, i]))
  }
}
colnames(pair_prob) <- c("A >", "B >", "C >", "D >", "E >", "F >", "G >", "H >")
row.names(pair_prob) <- school
pair_prob #pairwise comparison on probabilities which school has better effects

#part (b)
set.seed(34)
post_theta_b <- as.data.frame(matrix(ncol= J, nrow = size))
for (j in 1:J){
  post_theta_b[, j] <- rnorm(n = size, mean = effect[j], 
                           sd = sigma[j])
} #generate posteior theta for each school, in part b
post_theta_b$max <- apply(post_theta_b, 1, max)

#compute probability of school j is the best
best_prob_b <- as.data.frame(matrix(ncol= J, nrow=0)) #assign space for storing results
for (j in 1:J){
  best_prob_b[1, j] <- percent(mean(post_theta_b[, j] == post_theta_b[, J + 1]))
} #compute the probability of each school has best effects
colnames(best_prob_b) <- school #retrieve school names
best_prob_b
pair_prob_b <- as.data.frame(matrix(ncol= J, nrow = J)) #assign space for storing results
for (j in 1:J){
  for(i in 1:J){
    pair_prob_b[i, j] <- percent(mean(post_theta_b[, j] > post_theta_b[, i]))
  }
}
colnames(pair_prob_b) <- c("A >", "B >", "C >", "D >", "E >", "F >", "G >", "H >")
row.names(pair_prob_b) <- school
pair_prob_b #pairwise comparison on probabilities which school has better effects

#Question 5.14

#create the data
y <- c(16+58,9+90,10+48,13+57,19+103,20+57,18+86,17+112,35+273,55+64)

#create contour plot for (alpha, beta)
a <- seq(3,7,4/1000) #grid value for log(alpha/beta)
b <- seq(-8,-1,7/1000) #grid value for log(beta)
z <- matrix(0,length(a),length(b)) #matrix to store density values at grids
size <- length(a)

 for (i in 1:size){
   for (j in 1:size){
     t1<-exp(a[i]+b[j]) #alpha
     t2<-exp(b[j]) #beta
    z[i,j]<- 
       sum(lgamma(t1+y)+log(t2^t1)-lgamma(t1)-log((t2+1)^(t1+y)))
   }
 }
 z<-z-max(z)
 z<-exp(z) ##these two lines are just for numeric stableness

post_hyper <- data.frame(alpha = numeric(1001^2), 
            beta= numeric(1001^2), 
            post_density = numeric(1001^2))#create dataframe for contour map
post_hyper$alpha <- rep(a, each = size)
post_hyper$beta <- rep(b, size)
post_hyper$post_density <- as.vector(t(z))#fill a vector from z by rows

#normalize the grid values so it become probability density
post_hyper$post_density <- post_hyper$post_density/sum(post_hyper$post_density)

#make contour plot
hyper_contour <- ggplot(data = post_hyper, aes(post_hyper$alpha, 
        post_hyper$beta, 
        z = post_hyper$post_density)) + 
  geom_contour(aes(colour = ..level..)) + 
  labs(title = expression(paste("Contour plot of p(",log(alpha/beta),
      ",",log(beta),"|y)")))+
  xlab(expression(log(alpha/beta)))+
  ylab(expression(log(beta)))

direct.label(hyper_contour, list("angled.boxes"))

#simulate random draw of (alpha, beta) by successive substitution sampling
prob<-apply(z,1,sum)
prob<-prob/sum(prob)#normalize the value of z matrix

#create two vectors to store the simulation result.
aa <- rep(0,1100) #the first 100 is for burning out
bb <- rep(0,1100)

#code from others, not successive substitution sampling
# for (i in 1:1000) {
#   t <- sample(size,1,prob=prob)#draw a row number
#   aa[i]<- a[t] #find the log(alpha/beta) value corresponding to that row
#   bb[i]<- sample(b,1,prob=z[t,])#conditioning on row, draw log(beta)
# }

#successive subsitution sampling

t <- sample(size,1,prob=prob)#draw a row number
for (i in 1:1100){
  aa[i] <- a[t]#find the log(alpha/beta) value corresponding to that row
  s <- sample(size, 1, prob = z[t, ]) #draw a column number, conditioned on the previous row
  bb[i] <- b[s]#find the log(beta) value corresponding to that column
  t <- sample(size, 1, prob = z[, s]) #draw a row number, conditioned on the previous column
}

aa <- aa[101:1100]
bb <- bb[101:1100]

#make scatter plot
scatter_data <-data.frame(aa = numeric(1000), bb = numeric(1000))
scatter_data$aa <- aa #fill in x value
scatter_data$bb <- bb #fill in y value

ggplot(data= scatter_data, aes(x = aa, y = bb)) + 
  geom_point(colour = "blue") + 
  labs(title = expression(paste("Scatter plot of ( ",log(alpha/beta),
                                ",",log(beta)," )")))+
  xlab(expression(log(alpha/beta)))+
  ylab(expression(log(beta)))

#draw samples for theta_j
alpha<-exp(aa+bb)
beta<-exp(bb)
theta<-matrix(0,1000,10)
for(i in 1:1000){
  for(j in 1:10){
    theta[i,j]<-rgamma(1,alpha[i]+y[j],beta[i]+1)
  }
}

theta_plot <- data.frame(site = y, median = numeric(10), lquan = numeric(10), 
                uquan = numeric(10))
for(j in 1:10){
  theta_plot$median[j] <- median(theta[,j])
  theta_plot$lquan[j] <- quantile(theta[, j], 0.025)
  theta_plot$uquan[j] <- quantile(theta[, j], 0.975)
}#compute median, lower and upper quantile for each site

ggplot(data = theta_plot, aes(group = site)) + 
  geom_point(aes(x = site, y = median), colour = "blue", size = 4) + 
  geom_segment(aes(x = site, y = lquan, xend = site, yend = uquan), colour = "blue")+
  geom_abline(intercept = 0, slope = 1, colour = "blue")+
  labs(x = expression(paste("observed rate: ",y[j])), 
       y= expression(paste("posterior median and 95% posterior interval for ",theta)))

#other people's code without using ggplot
# plot(y,apply(theta,2,median),xlim=c(0,300),ylim=c(0,300),pch=19,
#      xlab=expression(paste("Observed Rate ",y)),
#      ylab=expression(paste("95% posterior interval for ",theta)))
# abline(0,1,lty=2)
# for(i in 1:10){
#   lines(x=c(y[i],y[i]),y=quantile(theta[,i],c(0.025,0.975)))
# }









