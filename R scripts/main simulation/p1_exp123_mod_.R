library(ggplot2)
library(ggpubr)
library(gridExtra)
library(reshape2)
library(haven)
library(lsr)


load("RESULTS_train.Rdata")    #load object IV classical approach
RESULT_train <- RESULTS_train 
load("AllResults.Rdata")       #load object IV new definition from 3 statistical methods


# SPSS file for second mixed anova  ---------

incVar <- c("rho1", "rho2", "ratio", "N", "r12", "R2", "Estimates", "IV_MSE")
RESULT <- RESULT[RESULT$rho1 != 1 & RESULT$rho2 != 1 , incVar]
RESULT <- cbind(RESULT, "reps" = rep(1:100, each = 3))
RESULTS_wide  <- dcast(RESULT, rho1 + rho2 + ratio + N + r12 + R2 + reps ~ Estimates, value.var = "IV_MSE")


write_sav(RESULTS_wide, "RESULTS_test_wide.sav")

# plot What happens to the regression coefficients?    ----------
coefs <- RESULT[, -grep("MSE|train|design|study|Job", names(RESULT))]
M1coefs <- coefs[, -grep("M2", names(coefs))]
M2coefs <- coefs[, -grep("M1", names(coefs))]

meltM1 <- melt(M1coefs, measure.vars = colnames(M1coefs)[8:9])
meltM1$model <- "M1"
colnames(meltM1)[8] <- "coef"

meltM2 <- melt(M2coefs, measure.vars = colnames(M2coefs)[8:10])
meltM2$model <- "M2"
colnames(meltM2)[8] <- "coef"

# true b1 and b2 
design   <- expand.grid(ratio = unique(RESULT$ratio), R2 = seq(0.1, 0.4, by = 0.1),
                        r12 = seq(0.1, 0.9, by = 0.2), b1 = 0.0, b2 = 0.0) 

design[design$ratio == "2:1", c("b1", "b2")] <-  c(sqrt(design[design$ratio == "2:1", "R2"]  / ((5/4) + design[design$ratio == "2:1", "r12"] )), 
                                                   .5*design[design$ratio == "2:1", "b1"]) 
design[design$ratio == "1:1", c("b1", "b2")] <-  c(sqrt(design[design$ratio == "1:1", "R2"]/(2+2*design[design$ratio == "1:1", "r12"])), 
                                                   design[design$ratio == "1:1", "b1"])
design[design$ratio == "1:2", c("b1", "b2")] <-  c(sqrt(design[design$ratio == "1:2", "R2"]/(5+ 4*design[design$ratio == "1:0", "r12"])), 
                                                   2*design[design$ratio == "1:2", "b1"])
design[design$ratio == "1:0", c("b1", "b2")] <-  c(sqrt(design[design$ratio == "1:0", "R2"]), rep(0.0, 20))

design <- data.frame(design)

true <- subset(design, ratio == "1:1" & R2 == 0.2 & r12 == 0.1 , b1:b2)

mydata <- subset(meltM2, r12 == .1 & rho1 == 1 & ratio == "1:1" & R2 == .2 & N == 200)

result <- aggregate(value ~  Estimates + rho2 + coef, mean, 
                    data = mydata[mydata$rho2 != 1, ])
result$Estimates <- factor(result$Estimates, labels = c("ridge", "OLS", "SIMEX"))
levels(result$coef) <- c("hat(b)[0]", "hat(b)[1]", "hat(b)[2]")

yline.dat <- data.frame(coef = c("hat(b)[0]", "hat(b)[1]", "hat(b)[2]"),
                        true = c(0.0, unlist(true)))


ggplot(result, aes(x = rho2, y = value, linetype = Estimates)) + geom_line(show.legend = F ) + 
  geom_point() + 
  geom_hline(aes(yintercept = true), data = yline.dat, col = "red") +
  facet_grid(~coef, labeller = label_parsed) + 
  theme_apa()+
  labs(linetype = "Method", x = expression(rho[2]),  y = "coefficient value") 


ggsave("Model2_coefs_rho2.pdf", width = 8, height = 6)

mydata <- subset(meltM2, r12 == .1 & rho2 == 1 & ratio == "1:1" & R2 == .2 & N == 200)

result <- aggregate(value ~  Estimates + rho1 +coef , mean, data = mydata[mydata$rho1 != 1, ])
result$Estimates <- factor(result$Estimates, labels = c("ridge", "OLS", "SIMEX"))
levels(result$coef) <- c("hat(b)[0]", "hat(b)[1]", "hat(b)[2]")


ggplot(result, aes(x = rho1, y = value, linetype = Estimates)) + geom_line(show.legend = F ) + 
  geom_point() +
  geom_hline(aes(yintercept = true), data = yline.dat, col = "red") +
  facet_grid(~coef, labeller = label_parsed) + theme_apa()+
  labs(linetype = "Method", x = expression(rho[1]),  y = "coefficient value") 


ggsave("Model2_coefs_rho1.pdf", width = 8, height = 6)



# plot results comparing different prediction rules -----------
result <- aggregate(IV_MSE ~ rho2 + Estimates , mean,  
                    data = RESULT[RESULT$rho1 != 1 & RESULT$rho2 != 1, ])

ggplot(result, aes(x = rho2, y = IV_MSE)) + geom_line(aes(linetype = Estimates)) + 
  geom_point() +
  scale_y_continuous(breaks = seq(-.01, .13, by = .01), limits= c(-.01, .13)) +
  theme_apa()+ guides(linetype = FALSE) +
  labs(linetype = "Method", x = expression(rho[2]),  y = "Incremental Predictive Validity")

ggsave("IV_rho2_method.pdf", width = 5, height = 6)

result <- aggregate(IV_MSE ~ rho1 + Estimates , mean,  data = RESULT[RESULT$rho1 != 1 & RESULT$rho2 != 1, ])

ggplot(result, aes(x = rho1, y = IV_MSE)) + geom_line(aes(linetype = Estimates)) + 
  geom_point() +
  scale_y_continuous(breaks = seq(-.01, .13, by = .01), limits= c(-.01, .13)) +
  theme_apa()+ guides(linetype = FALSE) +
  labs(linetype = "Method", x = expression(rho[1]),  y = "Incremental Predictive Validity")

ggsave("IV_rho1_method.pdf", width = 5, height = 6)


result <- aggregate(IV_MSE ~ N + Estimates , mean,  data = RESULT[RESULT$rho1 != 1 & RESULT$rho2 != 1, ])
ggplot(result, aes(x = as.factor(N), y = IV_MSE, group = Estimates)) + geom_line(aes(linetype= Estimates))+ 
  geom_point() + scale_y_continuous(breaks = seq(-.01, .13, by = .01), limits= c(-.01, .13)) +
  theme_apa() + guides(linetype = FALSE) +
  labs(x = expression(N[cal]),  y = "Incremental Predictive Validity")

ggsave("IV_N_method.pdf", width = 5, height = 6)

# select only results from ordinary least squares ------
RESULT_test <- RESULT[RESULT$Estimates == "least_squares", ]
RESULT_test$reps <- 1:100


# MERGE results MSE train and test sets --------------------
RESULTS <- cbind(RESULT_test[, -grep("B|Inter|study|Job|R2train", names(RESULT_test))], 
                 RESULT_train[, grep("IV|MSE", names(RESULT_train))])

MSE1 <- melt(RESULTS[, c(1:8, 9, 13)], id= 1:8)
MSE1$type <- factor(MSE1$variable, labels = c("test", "train"))
names(MSE1)[10] <- "MSE1"

MSE2 <- melt(RESULTS[, c(1:8, 10, 14)], id= 1:8)
MSE2$type <- factor(MSE2$variable, labels = c("test", "train"))
names(MSE2)[10] <- "MSE2"

IVMSE <- melt(RESULTS[, c(1:8, 11, 15)], id= 1:8)
IVMSE$type <- factor(IVMSE$variable, labels = c("test", "train"))
names(IVMSE)[10] <- "IVMSE"

RESULTS <- cbind.data.frame(MSE1[, -9], MSE2 = MSE2[, 10], IVMSE = IVMSE[, 10])

RESULTS_melt <- melt(RESULTS, measure.vars = c("MSE1", "MSE2", "IVMSE"), variable.name = "PE")

write_sav(RESULTS_melt, "RESULTS_train_test.sav")

RESULTS_train <- RESULTS_melt[RESULTS_melt$type == "train", ]
names(RESULTS_train)[11] <- "value.train"
RESULTS_test <- RESULTS_melt[RESULTS_melt$type == "test", ]
names(RESULTS_test)[11] <- "value.test"

RESULTS_unmelt <- cbind.data.frame(RESULTS_train, value.test = RESULTS_test[, 11])
RESULTS_unmelt$type <- NULL

write_sav(RESULTS_unmelt, "RESULTS_train_test_wide.sav")


# Figures in article ---------------------------
# Ncal ------------------
result <- aggregate(value ~ type + N, mean, data = RESULTS_melt[RESULTS_melt$PE == "IVMSE", ])

ggplot(result, aes(x = N, y = value, linetype = type)) + geom_line(aes(group = type)) +
  geom_point() +  geom_line() + scale_y_continuous(breaks = seq(-.01, .14, by = .01), limits= c(-.01, .14)) +
  theme_apa() + guides(linetype = FALSE) +
  scale_x_continuous(breaks = c(50, 200, 500, 1000)) +
  labs(y = "Incremental Predictive Validity", x = expression(N[cal]))

ggsave("IV_train_test_N.pdf", width = 5, height = 6)



# rho2 vs R2 -----------------
result <- aggregate(value ~ rho2 + type + R2, mean, data = RESULTS_melt[RESULTS_melt$PE == "IVMSE",])

ggplot(result, aes(x = rho2, y = value, linetype = type)) + geom_line(aes(group = type)) + 
  geom_point() + scale_y_continuous(breaks = seq(-.01, .14, by = .01), limits= c(-.01, .14)) +
  facet_grid(~R2)+
  theme_apa() + guides(linetype = FALSE) +
  labs(linetype = "sample", x = expression(rho[2]),  y = "Incremental Predictive Validity")

ggsave("IV_train_test_rho2_R2.pdf", width = 10, height = 6)

# rho2 ratio
result <- aggregate(value ~ rho2 + type + ratio, mean, data = RESULTS_melt[RESULTS_melt$PE == "IVMSE",])

ggplot(result, aes(x = rho2, y = value, linetype = type)) + geom_line(aes(group = type)) + 
  geom_point() + scale_y_continuous(breaks = seq(-.01, .14, by = .01), limits= c(-.01, .14)) +
  facet_grid(~ratio)+
  theme_apa() + guides(linetype = FALSE) +
  labs(linetype = "sample", x = expression(rho[2]),  y = "Incremental Predictive Validity")

ggsave("IV_train_test_rho2_ratio.pdf", width = 10, height = 6)


# rho1 r12 -----------------
result <- aggregate(value ~ rho1 + type + r12, mean, data = RESULTS_melt[RESULTS_melt$PE == "IVMSE", ])

ggplot(result, aes(x = rho1, y = value, linetype = type)) + geom_line(aes(group = type)) + 
  geom_point() + scale_y_continuous(breaks = seq(-.01, .14, by = .01), limits= c(-.01, .14)) +
  facet_grid(~r12)+
  theme_apa() + guides(linetype = FALSE) +
  labs(linetype = "sample", x = expression(rho[1]),  y = "Incremental Predictive Validity")

ggsave("IV_train_test_rho1_r12.pdf", width = 10, height = 6)

# rho1 R2
result <- aggregate(value ~ rho1 + type + R2, mean, data = RESULTS_melt[RESULTS_melt$PE == "IVMSE", ])

ggplot(result, aes(x = rho1, y = value, linetype = type)) + geom_line(aes(group = type)) + 
  geom_point() + scale_y_continuous(breaks = seq(-.01, .14, by = .01), limits= c(-.01, .14)) +
  facet_grid(~R2)+
  theme_apa() + guides(linetype = FALSE) +
  labs(linetype = "sample", x = expression(rho[1]),  y = "Incremental Predictive Validity")

ggsave("IV_train_test_rho1_R2.pdf", width = 10, height = 6)




