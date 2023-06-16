library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
library(gridExtra)
library(reshape2)
library(haven)
library(lsr)
library(jtools)
library(moments)
library(descriptr)

# simulation design ----------
rho1   <- seq(.5, 1, by = .1)
rho2   <- rho1
N      <- c(50, 100, 200, 500, 1000)
ratio  <- c("1:0", "2:1" , "1:1" , "1:2")
r12    <- seq(.1, .9, by = .2)
R2     <- seq(.1, .4, by = .1)

design <- expand.grid("rho1" = rho1, "rho2" = rho2, "ratio" = ratio, "N" = N, "r12" = r12, "R2" = R2)

design$ndesign <- 1:nrow(design)

load("RESULTS_train.Rdata")    # within sample ols
load("RESULTS.Rdata")          # out of sample ols ridge and simex
RESULTS_train$study <- paste0("design", RESULTS_train$ndesign, "_rep", RESULTS_train$rep)
RESULTS_ols <- RESULTS %>%
  filter(Estimates=="least_squares") %>%
  mutate(study=paste0("design", ndesign, "_rep", rep)) %>%
  select(ndesign, rep, IV_MSE, rho1, rho2, ratio, N, r12, R2, study) %>%
  rename(IVMSEtest = "IV_MSE")

# spss table wide format with IV_MSE_train ols and IV_MSE ols -----------------
RESULTS_ols_wide <- merge(select(RESULTS_train, IVMSEtrain, study),
                          RESULTS_ols, by="study")                 
write_sav(RESULTS_ols_wide, "RESULTS_ols_wide.sav")

# long format of results from OLS ---------------------------------------------
RESULTS_ols_long <- RESULTS_ols %>% 
  rename(IV_MSE="IVMSEtest") %>%
  mutate(sample="test")
RESULTS_train <- RESULTS_train %>%
  rename(IV_MSE="IVMSEtrain") %>%
  mutate(sample="train")
RESULTS_ols_long <- rbind.data.frame(RESULTS_train[colnames(RESULTS_ols_long)],
                                     RESULTS_ols_long)
save(RESULTS_ols_long, file="RESULTS_ols_long.Rdata")
# mixed anova table
RESULTS_ols_long <- RESULTS_ols_long %>%
  mutate(rho1=factor(rho1),
         rho2=factor(rho2), N=factor(N),
         r12=factor(r12),
         R2=factor(R2))
RESULTS_ols_long$sample <- factor(RESULTS_ols_long$sample)
save(RESULTS_ols_long, file="RESULTS_ols_long.Rdata")
ols_anova <- aov(IV_MSE ~ (rho1+rho2+N+r12+ratio+R2+sample)^3 + Error(study|sample),
                 data=RESULTS_ols_long)

# spss table wide format with IV MSE ridge, ols, and simex
RESULTS_wide <- RESULTS %>%
  select(ndesign, rep, Estimates, IV_MSE, rho1, rho2, ratio,
         N, r12, R2) %>%
  pivot_wider(names_from = Estimates, values_from = IV_MSE)
write_sav(RESULTS_wide, "RESULTS_wide.sav")   

# MIXED ANOVA 1 --------------------------------
# table in article -----------------------------
library(xtable)
library(readxl)
p1_exp1 <- read_excel("p1_exp1_betweenSubjects.xlsx")
p1_exp1 <- cbind.data.frame(source = p1_exp1[ , 1], apply(p1_exp1[ , -1], 2, as.numeric))

efsize <- .05
print(xtable(rbind(subset(p1_exp1[order(p1_exp1$part_eta_sqr, decreasing = TRUE), -5],
                          p1_exp1[order(p1_exp1$part_eta_sqr, decreasing = TRUE), ]$part_eta_sqr > efsize), 
                   p1_exp1[23, -5 ]), caption = "ANOVA result of IV (averaged over approaches) using ordinary least squares estimates",
             label = "tab:in_out_ls_anova", digits = c(0,2,2,0,2,3)), include.rownames = FALSE)


# Figures in article ---------------------------

# Ncal ------------------
result <- aggregate(IV_MSE ~ sample + N, mean, data = RESULTS_ols_long)
ggplot(result, aes(x = N, y = IV_MSE, linetype = sample)) + geom_line(aes(group = sample)) +
  geom_point() +  geom_line() + scale_y_continuous(breaks = seq(-.01, .14, by = .01), limits= c(-.01, .14)) +
  theme_apa() + guides(linetype = FALSE) +
  scale_x_continuous(breaks = c(50, 200, 500, 1000)) +
  labs(y = "Incremental Predictive Validity", x = expression(N[cal]))

ggsave("IV_train_test_N.pdf", width = 5, height = 6)



# rho2 vs R2 -----------------
result <- aggregate(IV_MSE ~ rho2 + sample + R2, mean, data = RESULTS_ols_long)

ggplot(result, aes(x = rho2, y = IV_MSE, linetype = sample)) + geom_line(aes(group = sample)) + 
  geom_point() + scale_y_continuous(breaks = seq(-.01, .14, by = .01), limits= c(-.01, .14)) +
  facet_grid(~R2)+
  theme_apa() + guides(linetype = FALSE) +
  labs(linetype = "sample", x = expression(rho[2]),  y = "Incremental Predictive Validity")

ggsave("IV_train_test_rho2_R2.pdf", width = 10, height = 6)

# rho2 ratio
result <- aggregate(IV_MSE ~ rho2 + sample + ratio, mean, data = RESULTS_ols_long)

ggplot(result, aes(x = rho2, y = IV_MSE, linetype = sample)) + geom_line(aes(group = sample)) + 
  geom_point() + scale_y_continuous(breaks = seq(-.01, .14, by = .01), limits= c(-.01, .14)) +
  facet_grid(~ratio)+
  theme_apa() + guides(linetype = FALSE) +
  labs(linetype = "sample", x = expression(rho[2]),  y = "Incremental Predictive Validity")

ggsave("IV_train_test_rho2_ratio.pdf", width = 10, height = 6)


# rho1 r12 -----------------
result <- aggregate(IV_MSE ~ rho1 + sample + r12, mean, data = RESULTS_ols_long)

ggplot(result, aes(x = rho1, y = IV_MSE, linetype = sample)) + geom_line(aes(group = sample)) + 
  geom_point() + scale_y_continuous(breaks = seq(-.01, .14, by = .01), limits= c(-.01, .14)) +
  facet_grid(~r12)+
  theme_apa() + guides(linetype = FALSE) +
  labs(linetype = "sample", x = expression(rho[1]),  y = "Incremental Predictive Validity")

ggsave("IV_train_test_rho1_r12.pdf", width = 10, height = 6)

# rho1 R2
result <- aggregate(IV_MSE ~ rho1 + sample + R2, mean, data = RESULTS_ols_long)

ggplot(result, aes(x = rho1, y = IV_MSE, linetype = sample)) + geom_line(aes(group = sample)) + 
  geom_point() + scale_y_continuous(breaks = seq(-.01, .14, by = .01), limits= c(-.01, .14)) +
  facet_grid(~R2)+
  theme_apa() + guides(linetype = FALSE) +
  labs(linetype = "sample", x = expression(rho[1]),  y = "Incremental Predictive Validity")

ggsave("IV_train_test_rho1_R2.pdf", width = 10, height=6)

# MIXED ANOVA 2 ----------------------------------------------------------------
# plot What happens to the regression coefficients?    -------------------------
M2coefs <- RESULTS[, -grep("M1|MSE", names(RESULTS))]
M2coefs_long_rho1 <- M2coefs %>%
  # select conditions in figure in article
  filter(ratio=="1:1" & N==200 & R2==0.2 &  r12==0.1 & rho1==1) %>%
  pivot_longer(c(Inter_M2:b2_M2), names_to="coef", values_to="value") %>%
  mutate(coef=factor(coef, levels=c("Inter_M2", "b1_M2", "b2_M2")))
levels(M2coefs_long_rho1$coef)  <- c("hat(b)[0]", "hat(b)[1]", "hat(b)[2]")
levels(M2coefs_long_rho1$Estimates) <- c("ridge", "OLS", "SIMEX")

M2coefs_long_rho2 <- M2coefs %>%
  # select conditions in figure in article
  filter(ratio=="1:1" & N==200 & R2==0.2 &  r12==0.1 & rho2==1) %>%
  pivot_longer(c(Inter_M2:b2_M2), names_to="coef", values_to="value") %>%
  mutate(coef=factor(coef, levels=c("Inter_M2", "b1_M2", "b2_M2")))
levels(M2coefs_long_rho2$coef)  <- c("hat(b)[0]", "hat(b)[1]", "hat(b)[2]")
levels(M2coefs_long_rho2$Estimates) <- c("ridge", "OLS", "SIMEX")

# true b1 and b2 -------
design$b0 <- 0.0
design[design$ratio == "2:1", "b1"] <-  sqrt(design[design$ratio == "2:1", "R2"]  / ((5/4) + design[design$ratio == "2:1", "r12"] ))
design[design$ratio == "2:1", "b2"] <- .5*design[design$ratio == "2:1", "b1"]
design[design$ratio == "1:1", "b1"] <-  sqrt(design[design$ratio == "1:1", "R2"]/(2+2*design[design$ratio == "1:1", "r12"]))
design[design$ratio == "1:1", "b2"] <-  design[design$ratio == "1:1", "b1"]
design[design$ratio == "1:2", "b1"] <-  sqrt(design[design$ratio == "1:2", "R2"]/(5+ 4*design[design$ratio == "1:0", "r12"])) 
design[design$ratio == "1:2", "b2"] <-  2*design[design$ratio == "1:2", "b1"]
design[design$ratio == "1:0", "b1"] <-  sqrt(design[design$ratio == "1:0", "R2"])
design[design$ratio == "1:0", "b2"] <-  0.0

subdesign <- design %>%
  filter(ratio == "1:1" & R2 == 0.2 & r12 == 0.1 & N==200 &rho1 == 1 & rho2 == 1) %>%
  select(b0:b2) %>%
  pivot_longer(b0:b2, names_to = "coef", values_to = "true_value") %>%
  mutate(coef=as.factor(coef))
levels(subdesign$coef) <- c("hat(b)[0]", "hat(b)[1]", "hat(b)[2]")


# when rho1 = 1 ----------------------------------------------------------------

M2coefs_long_rho1 %>% 
  ggplot(aes(x=rho2, y=value, linetype = Estimates)) + 
  stat_summary(geom="point", fun="mean") +
  stat_summary(geom="line", fun="mean", show.legend = F) +
  geom_hline(data=subdesign, aes(yintercept = true_value), col = "red") +
  facet_grid(~coef, labeller = label_parsed) + 
  theme_apa()+
  labs(linetype = "Method", x = expression(rho[2]),  y = "coefficient value") 

ggsave("Model2_coefs_rho2.pdf", width = 8, height = 6)


# when rho2 = 1 ----------------------------------------------------

M2coefs_long_rho2  %>%
  ggplot(aes(x=rho1, y=value, linetype = Estimates)) + 
  stat_summary(geom="point", fun="mean") +
  stat_summary(geom="line", fun="mean", show.legend = F) +
  geom_hline(data=subdesign, aes(yintercept = true_value), col = "red") +
  facet_grid(~coef, labeller = label_parsed) + 
  theme_apa()+
  labs(linetype = "Method", x = expression(rho[1]),  y = "coefficient value") 


ggsave("Model2_coefs_rho1.pdf", width = 8, height = 6)


# plot results comparing different prediction rules -----------
# aggregate first, it takes to long for ggplot to compute the summary
result <- aggregate(IV_MSE ~ rho2 + Estimates , mean,  
                    data = RESULTS)
ggplot(result, aes(x = rho2, y = IV_MSE)) + geom_line(aes(linetype = Estimates)) + 
  geom_point() +
  scale_y_continuous(breaks = seq(-.01, .13, by = .01), limits= c(-.01, .13)) +
  theme_apa()+ guides(linetype = FALSE) +
  labs(linetype = "Method", x = expression(rho[2]),  y = "Incremental Predictive Validity")

ggsave("IV_rho2_method.pdf", width = 5, height = 6)

result <- aggregate(IV_MSE ~ rho1 + Estimates , mean,  data = RESULTS)

ggplot(result, aes(x = rho1, y = IV_MSE)) + geom_line(aes(linetype = Estimates)) + 
  geom_point() +
  scale_y_continuous(breaks = seq(-.01, .13, by = .01), limits= c(-.01, .13)) +
  theme_apa()+ guides(linetype = FALSE) +
  labs(linetype = "Method", x = expression(rho[1]),  y = "Incremental Predictive Validity")

ggsave("IV_rho1_method.pdf", width = 5, height = 6)


result <- aggregate(IV_MSE ~ N + Estimates , mean,  data = RESULTS)
ggplot(result, aes(x = as.factor(N), y = IV_MSE, group = Estimates)) + geom_line(aes(linetype= Estimates))+ 
  geom_point() + scale_y_continuous(breaks = seq(-.01, .13, by = .01), limits= c(-.01, .13)) +
  theme_apa() + guides(linetype = FALSE) +
  labs(x = expression(N[cal]),  y = "Incremental Predictive Validity")

ggsave("IV_N_method.pdf", width = 5, height = 6)

