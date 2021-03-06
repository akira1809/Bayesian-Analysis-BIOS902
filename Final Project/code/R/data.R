#BIOS902 Final Project

#This file stores some of those data we need to run in winBUGS

#data from the original Berry-Berry paper

Nt <- 148; Nc <- 132

b <- c(rep(1, 5), rep(2, 7), 3, 4, rep(5, 3), rep(6, 11), 
       rep(7, 9), rep(8, 3))

Nae <- length(b)

B <- 8

alp.siggam <- 3; beta.siggam <- 1

mu.gam00 <- 0; tau2.gam00 <- 0.1; alp.taugam <- 3; beta.taugam <- 1

lam.alp <- 1; lam.beta <-1

mu.tta00 <- 0; tau2.tta00 <- 0.1; alp.tta0 <- 3; beta.tta0 <- 1;

alp.tta <- 3; beta.tta <- 1


j <- c(1:5, 1:7, 1, 1, 1:3, 1:11, 1:9, 1:3)

Y <- c(57, 34, 2, 3, 27, 7, 2, 2, 24, 3, 2, 19, 3, 0, 2, 2,
       75, 4, 4, 1, 13, 28, 2, 13, 15, 3, 2, 3, 4, 2, 2, 13, 
       6, 8, 4, 0, 1, 0, 18, 2)

X <- c(40, 26, 0, 1, 20, 2, 0, 0, 10, 1, 7, 19, 2, 2, 0, 2, 
       43, 1, 2, 2, 8, 20, 1,8, 14, 1, 1, 1, 0, 0, 1, 3, 2, 
       1, 2, 2, 2, 2, 14, 1)

#winBUGS data statement based on the above

list(Nae = 40, Nt = 148, Nc = 132, B = 8, alp.siggam = 3, 
     beta.siggam = 1, mu.gam00 = 0, tau2.gam00 = 0.1, alp.taugam = 3, 
     beta.taugam = 1, lam.alp = 1, lam.beta = 1, mu.tta00 = 0, 
     tau2.tta00 = 0.1, alp.tta0 = 3, beta.tta0 = 1, alp.tta = 3, 
     beta.tta = 1)
list(b = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3, 4, 5, 5, 5, 
           6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7,7, 7, 
           8, 8, 8),j = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 6, 7, 1, 1, 1, 2, 3, 
           1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 1, 2, 3, 4, 5, 6, 
           7, 8, 9, 1, 2, 3), X = c(40, 26, 0, 1, 20, 2, 0, 0, 10, 
          1, 7, 19, 2, 2, 0, 2, 43, 1, 2, 2, 8, 20, 1, 8, 14, 1, 1, 1, 
          0, 0, 1, 3, 2, 1, 2, 2, 2, 2, 14, 1), Y = c(57, 34, 2, 3, 27, 7, 
          2, 2, 24, 3, 2, 19, 3, 0, 2, 2, 75, 4, 4, 1, 13, 28, 2, 13, 
          15, 3, 2, 3, 4, 2, 2, 13, 6, 8, 4, 0, 1, 0, 18, 2))

#read coda information exported from OpenBugs and compute posterior
#drug effect probability

#this includes all 40 adverse effects
theta <- read.csv("C:\\akira\\data\\coda_Berry_all_AE.csv")

theta_positive <- (theta > 0)
theta_zero <- (theta ==0)

result1 <- apply(theta_positive, 2, mean)
result2 <- apply(theta_zero, 2, mean)

#data for my project

Nt <- 320; Nc <- 320;

b <- c(rep(1, 4), rep(2, 2), rep(3, 3), rep(4,2), rep(5, 9), rep(6, 2), 
       rep(7, 4), rep(8, 5))

Nae <- length(b)

B <- 8; C<- 10

j <- c(1:4, 1:2, 1:3, 1:2, 1:9, 1:2, 1:4, 1:5)

X <- c(1, 3, 2, 6, 1, 1, 1, 1, 0, 0, 1, 2, 2, 0, 1, 
       1, 0, 5, 10, 4, 1, 1, 0, 4, 1, 1, 2, 3, 0, 0, 1)
Y <- c(0, 3, 0, 8, 0, 0, 0, 0, 1, 2, 0, 0, 1, 1, 0, 
       3, 1, 5, 2, 2, 0, 0, 1, 1, 1, 1, 1, 0, 2, 1, 0)

alp.siggam <- 3; beta.siggam <- 1

mu.gam00 <- 0; tau2.gam00 <- 0.1; alp.taugam <- 3; beta.taugam <- 1

lam.alp <- 1; lam.beta <-1 

mu.tta00 <- 0; tau2.tta00 <- 0.1; alp.tta0 <- 3; beta.tta0 <- 1

alp.tta <- 3; beta.tta <- 1

list(Nae = 31, Nt = 320, Nc = 320, B = 8, C = 10, alp.siggam = 3, 
     beta.siggam = 1, mu.gam00 = 0, tau2.gam00 = 0.1, alp.taugam = 3, 
     beta.taugam = 1, lam.alp = 1, lam.beta = 1, mu.tta00 = 0, 
     tau2.tta00 = 0.1, alp.tta0 = 3, beta.tta0 = 1, alp.tta = 3, 
     beta.tta = 1)
list(b = c(1, 1, 1, 1, 2, 2, 3, 3, 3, 4, 4, 5, 5, 5, 5, 5, 5, 5, 
           5, 5, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 8), 
     j = c(1, 2, 3, 4, 1, 2, 1, 2, 3, 1, 2, 1, 2, 3, 4, 5, 6, 7, 
           8, 9, 1, 2, 1, 2, 3, 4, 1, 2, 3, 4, 5), 
     X = c(1, 3, 2, 6, 1, 1, 1, 1, 0, 0, 1, 2, 2, 0, 1, 1, 0, 5, 
           10, 4, 1, 1, 0, 4, 1, 1, 2, 3, 0, 0, 1), 
     Y = c(0, 3, 0, 8, 0, 0, 0, 0, 1, 2, 0, 0, 1, 1, 0, 3, 1, 5, 
           2, 2, 0, 0, 1, 1, 1, 1, 1, 0, 2, 1, 0))

#the following code read in the coda information from .csv file
#generated by OpenBUGs by fitting newdata to the three level
#hierarchical model
theta_hier_new <- read.csv("C:\\akira\\data\\coda_newdata.csv")

#posterior probability of theta > 0 and = 0
theta_new_positive <- (theta_hier_new > 0)
theta_new_zero <- (theta_hier_new ==0)

result1_new <- apply(theta_new_positive, 2, mean)
result2_new <- apply(theta_new_zero, 2, mean)

#the following code read in the coda information from .csv file
#generated by OpenBUGs by fitting newdata to the independent model
theta_ind_new <- read.csv("C:\\akira\\data\\coda_independent.csv")

theta_ind_positive <- (theta_ind_new > 0)
theta_ind_zero <- (theta_ind_new ==0)

#posterior probability of theta >0 and theta = 0
result1_ind <- apply(theta_ind_positive, 2, mean)
result2_ind <- apply(theta_ind_zero, 2, mean)

