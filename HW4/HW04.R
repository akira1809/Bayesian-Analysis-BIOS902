#HW4

#Question 1

library(ggplot2)

X <- c(1:22); meanX <- mean(X); N <- 365; Time <- 22
mu0 <- 11.74; mu1 <-0.1017; gamma0 <- -0.2923; gamma1 <- -0.02348

Control <- function(x){
  d <-mu0 +mu1*(x - meanX)
}
Treatment <- function(x){
  d <- mu0 + gamma0 + (mu1 + gamma1)*(x - meanX)
}

p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + #create dummy dataset
  stat_function(fun=Control, geom="line", aes(colour="Control")) +
  stat_function(fun=Treatment, geom="line", aes(colour="Treatment")) + 
  labs(y = "HGB") + 
  #theme_grey() +
  scale_x_continuous(limits = c(1, 22)) +
  #scale_y_continuous(limits = c(10.5, 13.5))
  scale_color_manual(name = "Fitted Group Means",
                     values = c("blue", "red"), # Color specification
                     labels = c("Control", "Treatment"))

#read in the raw data of Question 1
Q1data <- read.table("C:\\akira\\data\\Q1_rawdata.txt", header = FALSE)

#create the data frame to be used in ggplot
Q1data_control <- subset(Q1data, Q1data$V1 == 1)
Q1data_treatment <- subset(Q1data, Q1data$V1 == 2)
Observed_control <- colMeans(Q1data_control, na.rm = TRUE)
Observed_treatment <- colMeans(Q1data_treatment, na.rm = TRUE)
Observed_control <- Observed_control[2:23]
Observed_treatment <- Observed_treatment[2:23]
Q1observed <- data.frame("Control" = Observed_control, "Treatment" = Observed_treatment, 
                         x = 1:22)
library(tidyr)
Q1observed_long <- gather(Q1observed, Treatment, HGB, 
                          Control:Treatment, factor_key = TRUE)
p2 <- ggplot(Q1observed_long, mapping = aes(x = x, y = HGB, colour = Treatment)) + 
      geom_line() + scale_x_continuous(limits = c(1, 22)) +
      scale_color_manual(name = "Observed Group Means",
                     values = c("blue", "red"), # Color specification
                     labels = c("Control", "Treatment"))
library(gridExtra)

grid.arrange(p2, p, nrow = 2)

#Question 2.
Q2data <- read.table("C:\\akira\\data\\copresence.txt", header = TRUE)

Xmean <- mean(Q2data$X)

beta01 <- -0.4045; beta11 <- -1.718 #logit link
beta02 <- -0.7148; beta12 <- -1.248 #log-log link

library(gtools)#for logit and inv.logit
library(LaplacesDemon) #for cloglog and invcloglog

logit <- function(x){
  d <- inv.logit(beta01 + beta11*(x - Xmean))
}
loglog <- function(x){
  d <- invcloglog(beta02 + beta12*(x - Xmean))
}

p2 <- ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + #create dummy dataset
  stat_function(fun=logit, geom="line", aes(colour="logit")) +
  stat_function(fun=loglog, geom="line", aes(colour="complementary log-log")) + 
  labs(y = "P(co-presence)") + 
  #theme_grey() +
  scale_x_continuous(limits = c(0.1, 5)) +
  #scale_y_continuous(limits = c(10.5, 13.5))
  scale_color_manual(name = "link functions",
                     values = c("blue", "red"), # Color specification
                     labels = c("logit", "complementary log-log"))
p2
