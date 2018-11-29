#This file prepare proper data in the format that can be implemented
#by c212 package to fit the three level hierarchical model proposed by 
#Berry & Berry

data_Berry <- data.frame("B" = rep(c(rep(1, 5), rep(3, 7), 5, 6, rep(8, 3), rep(9, 11), 
                                 rep(10, 9), rep(11, 3)), 2),
            "j" = rep(c(1:5, 1:7, 1, 1, 1:3, 1:11, 1:9, 1:3), 2),
            "AE" = rep(c(1:5, 1:7, 1, 1, 1:3, 1:11, 1:9, 1:3), 2),
            "Group" = c(rep(1, 40), rep(2, 40)), 
            "Count" = c(c(40, 26, 0, 1, 20, 2, 0, 0, 10, 1, 7, 19, 2, 2, 0, 2, 
                          43, 1, 2, 2, 8, 20, 1,8, 14, 1, 1, 1, 0, 0, 1, 3, 2, 
                          1, 2, 2, 2, 2, 14, 1), 
                        c(57, 34, 2, 3, 27, 7, 2, 2, 24, 3, 2, 19, 3, 0, 2, 2,
                          75, 4, 4, 1, 13, 28, 2, 13, 15, 3, 2, 3, 4, 2, 2, 13, 
                          6, 8, 4, 0, 1, 0, 18, 2)), 
            "Total" = c(rep(132, 40), rep(148, 40)))

library(c212)

fit <- c212.BB(data_Berry, burnin = 1000, iter = 11000, nchains = 3, 
               theta_algorithm = "MH", sim_type = "SLICE", global.sim.params = 
                 data.frame(type = c("MH", "MH", "MH", "MH",
                                     "SLICE", "SLICE", "SLICE"),
               param = c("sigma_MH_alpha", "sigma_MH_beta", "sigma_MH_gamma",
                         "sigma_MH_theta", "w_alpha", "w_beta", "w_gamma"),
               value = c(3, 3, 0.2, 0.2, 1, 1, 1), control = c(0, 0, 0, 0, 6, 6, 6),
               stringsAsFactors = FALSE),
               sim.params = NULL, initial_values = NULL,
               hyper_params = list(mu.gamma.0.0 = 0,
               tau2.gamma.0.0 = 10, mu.theta.0.0 = 0, tau2.theta.0.0 = 10,
               alpha.gamma.0.0 = 3, beta.gamma.0.0 = 1, alpha.theta.0.0 = 3,
               beta.theta.0.0 = 1, alpha.gamma = 3,
               beta.gamma = 1, alpha.theta = 3, beta.theta = 1,
              lambda.alpha = 1.0, lambda.beta = 1.0), global.pm.weight = 0.5,
              pm.weights = NULL, adapt_params = data.frame(min_w = 0.25, chains = 3, 
              burnin = 20000, iter = 40000), adapt_phase=0
               )
#Post probability for theta

mean(fit$theta[1, 2, 4,] > 0) #body system 2, AE 4(Diarrhea)
mean(fit$theta[1, 5, 3,] > 0) #body system 8, AE 3(Irritability)
mean(fit$theta[1, 7, 4,] > 0) #body system 10, AE 4(Rash)
mean(fit$theta[1, 7, 6,] > 0) #body system 10, AE 6(Rash, measles/rubella-like)

#Comparison with OpenBugs coda information

four_AE <- read.csv("C:\\akira\\data\\coda_Berry_four_AE.csv")
mean(four_AE$Diarrhea>0)
mean(four_AE$Irritability>0)
mean(four_AE$Rash>0)
mean(four_AE$Rash_measles_rubella_like>0)

#Pepare signal safety data in proper format for three level
#hierarchical model in R

data_new <- data.frame("B" = rep(c(rep(1, 4), rep(2, 2), rep(3, 3), rep(4, 2), 
              rep(5, 9), rep(6, 2), rep(7, 4), rep(8, 5)), 2),
            "j" = rep(c(1:4, 1:2, 1:3, 1:2, 1:9, 1:2, 1:4, 1:5), 2),
            "AE" = rep(c(1:4, 1:2, 1:3, 1:2, 1:9, 1:2, 1:4, 1:5), 2),
            "Group" = c(rep(1, 31), rep(2, 31)), 
            "Count" = c(c(1, 3, 2, 6, 1, 1, 1, 1, 0, 0, 1, 2, 2, 0, 1, 1,
            0, 5, 10, 4, 1, 1, 0, 4, 1, 1, 2, 3, 0, 0, 1), 
           c(0, 3, 0, 8, 0, 0, 0, 0, 1, 2, 0, 0, 1, 1, 0,
             3, 1, 5, 2, 2, 0, 0, 1, 1, 1, 1, 1, 0, 2, 1, 0)), 
           "Total" = c(rep(320, 31), rep(320, 31)))
fit2 <- c212.BB(data_new, burnin = 1000, iter = 11000, nchains = 3, 
               theta_algorithm = "MH", sim_type = "SLICE", global.sim.params = 
                 data.frame(type = c("MH", "MH", "MH", "MH",
                                     "SLICE", "SLICE", "SLICE"),
                            param = c("sigma_MH_alpha", "sigma_MH_beta", "sigma_MH_gamma",
                                      "sigma_MH_theta", "w_alpha", "w_beta", "w_gamma"),
                            value = c(3, 3, 0.2, 0.2, 1, 1, 1), control = c(0, 0, 0, 0, 6, 6, 6),
                            stringsAsFactors = FALSE),
               sim.params = NULL, initial_values = NULL,
               hyper_params = list(mu.gamma.0.0 = 0,
                                   tau2.gamma.0.0 = 10, mu.theta.0.0 = 0, tau2.theta.0.0 = 10,
                                   alpha.gamma.0.0 = 3, beta.gamma.0.0 = 1, alpha.theta.0.0 = 3,
                                   beta.theta.0.0 = 1, alpha.gamma = 3,
                                   beta.gamma = 1, alpha.theta = 3, beta.theta = 1,
                                   lambda.alpha = 1.0, lambda.beta = 1.0), global.pm.weight = 0.5,
               pm.weights = NULL, adapt_params = data.frame(min_w = 0.25, chains = 3, 
                        burnin = 20000, iter = 40000), adapt_phase=0
)

#Post probability for theta > 0
mean(fit2$theta[1, 1, 1,] > 0)#Arrhythmia
mean(fit2$theta[1, 1, 2,] > 0)#Increased BP
mean(fit2$theta[1, 1, 3,] > 0)#Other CB AEs
mean(fit2$theta[1, 1, 4,] > 0)#Pre-eclampsia
mean(fit2$theta[1, 2, 1,] > 0)#Emesis
mean(fit2$theta[1, 2, 2,] > 0)#Other GI AEs
mean(fit2$theta[1, 3, 1,] > 0)#Depression
mean(fit2$theta[1, 3, 2,] > 0)#Headache
mean(fit2$theta[1, 3, 3,] > 0)#Other HNMB AEs
mean(fit2$theta[1, 4, 1,] > 0)#Gestational Diabete ellitus
mean(fit2$theta[1, 4, 2,] > 0)#Other MAN AEs
mean(fit2$theta[1, 5, 1,] > 0)#Chorioamnionitis
mean(fit2$theta[1, 5, 2,] > 0)#Decreased Fetal Movement
mean(fit2$theta[1, 5, 3,] > 0)#Endomyometritis
mean(fit2$theta[1, 5, 4,] > 0)#Miscarriage
mean(fit2$theta[1, 5, 5,] > 0)#Other PD AEs
mean(fit2$theta[1, 5, 6,] > 0)#Postpartum Hemorrhage
mean(fit2$theta[1, 5, 7,] > 0)#Premature Delivery
mean(fit2$theta[1, 5, 8,] > 0)#Premature ROM
mean(fit2$theta[1, 5, 9,] > 0)#Preterm Contractions
mean(fit2$theta[1, 6, 1,] > 0)#Other RESP AEs
mean(fit2$theta[1, 6, 2,] > 0)#Shortness of Breath
mean(fit2$theta[1, 7, 1,] > 0)#Other UG AEs
mean(fit2$theta[1, 7, 2,] > 0)#Pyelnephritis
mean(fit2$theta[1, 7, 3,] > 0)#Urinary Tract Infection
mean(fit2$theta[1, 7, 4,] > 0)#Vaginal Bleeding
mean(fit2$theta[1, 8, 1,] > 0)#Abdominal Pain
mean(fit2$theta[1, 8, 2,] > 0)#Other BODY AEs
mean(fit2$theta[1, 8, 3,] > 0)#PD012
mean(fit2$theta[1, 8, 4,] > 0)#Pelvic Pain
mean(fit2$theta[1, 8, 5,] > 0)#Polyhydramnios