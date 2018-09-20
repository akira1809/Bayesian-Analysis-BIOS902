#Question 2.11

#part(a)
#create grid
theta <- seq(from = 0, to = 100, by = 0.01)
#input data
y <- c(43, 44, 45, 46.5, 47.5)
#create vector for unnoramlized posterior
post_unnorm <- rep(NA, 10001)
#compute the unnoramlized posterior density
for (j in 1:10001){
  post_unnorm[j] <- prod(1/(1 + (y - theta[j])^2))*0.01
}
#compute the normalizing constant
C <- sum(post_unnorm)
#compute the normalized posterior density
post_norm <- post_unnorm/C
#plot the normalized posterior density
plot(theta, post_norm, type="l", lty = 3, lwd = 2, xlab = "theta value",
     ylab = "posterior density", main = "normalized posterior density")
#output the high density section:
post_norm[4401:4501]
post_unnorm[4401:4501]

#part(b)

#draw 1000 samples from posterior density
set.seed(1)
theta_sample <- sample(theta, size = 1000, replace = TRUE, prob = post_norm)
hist(theta_sample)

#part(c)
set.seed(1)
y6 <- rcauchy(1000, location = theta_sample, scale = 1)
hist(y6, nclass = 100, xlim = c(-50, 100))

#Question 2

set.seed(35)

#input the data
y_0 <- 39; n_0 <- 674; y_1 <- 22; n_1 <- 680

#draw posterior sample for p0, p1
p_0 <- rbeta(10000, shape1 = 40, shape2 = 636)
p_1 <- rbeta(10000, shape1 = 23, shape2 = 659)

#compute posterior sample for odds ratio
odds_ratio <- p_1*(1 - p_0)/(p_0*(1 - p_1))

#make Bayesian inference
post_mean <- mean(odds_ratio)
post_var <- var(odds_ratio)
sort_oddsratio <- sort(odds_ratio)
post_interval <- c(quantile(sort_oddsratio, probs = 0.025), 
                   quantile(sort_oddsratio, probs = 0.975))

#print outcomes
cat("the posterior mean of odds ratio is", post_mean, "\n")
cat("the posterior variance of odds ratio is", post_var, "\n")
cat("the posterior interval of odds ratio is", post_interval)

hist(odds_ratio, prob = TRUE, col = "grey", 
     xlab = "odds ratio", nclass = 40)# prob=TRUE for probabilities not counts
lines(density(odds_ratio), col="blue", lwd=2) # add a density estimate with defaults

#Question 3

#input the data
y1 <- c(13.357, 14.928, 14.896, 15.297, 14.82, 12.067, 14.824, 13.865
        , 17.447)
y2 <- c(15.98, 14.206, 16.011, 17.25, 15.993, 15.722, 17.143
        , 15.23, 15.125, 16.609, 14.735, 15.881, 15.789)
n1 <- length(y1)
n2 <- length(y2)

set.seed(35)
#draw samples for sigma^2
var1 <- (n1 - 1)*var(y1)/rchisq(10000, df = n1- 1)
var2 <- (n2 - 1)*var(y2)/rchisq(10000, df = n2- 1)
#draw samples for mu
mu1 <- rnorm(10000, mean = mean(y1), sd = sqrt(var1/n1))
mu2 <- rnorm(10000, mean = mean(y2), sd = sqrt(var2/n2))
#compute mu_d
mu_d <- mu1 - mu2
#summarize Bayesian inference results

mu_d_mean <- mean(mu_d)
mu_d_var <- var(mu_d)
mu_d_sort <-sort(mu_d)
mu_d_interval <- c(quantile(mu_d_sort, probs = 0.025), 
                   quantile(mu_d_sort, probs = 0.975))

#print outcome
cat("the posterior mean of mean difference is", mu_d_mean, "\n")
cat("the posterior variance of  mean difference is", mu_d_var, "\n")
cat("the posterior interval of mean difference is", mu_d_interval)

hist(mu_d, prob = TRUE, col = "grey", 
     xlab = "mean difference", nclass = 40)# prob=TRUE for probabilities not counts
lines(density(mu_d), col="blue", lwd=2) # add a density estimate with defaults




