library(Synth)     # version 1.1-5
library(ggplot2)   # version 3.3.0
library(dplyr)     # version 0.8.4
library(reshape2)  # version 1.4.3
library(tidyr)     # version 1.0.2
library(rdrobust)  # version 0.99.5
library(stargazer) # version 5.2.2
library(ggsci)     # version 2.9.0

setwd("./Data")

############### Wenzhou ##################
# ------------------- Table 1 ----------------------

region <- "Wenzhou"
Y <- read.csv("wenzhou.csv")
Y <- Y[Y$ctname == region, ]
wenzhou.rd <- Y[, c("fulldate", "confirmed")]
wenzhou.rd$date <- as.numeric(as.Date(wenzhou.rd$fulldate)) - 18281 - 12
wenzhou.rd$treatment <- c(rep(0, 11), rep(1, length(wenzhou.rd$date) - 11))
wenzhou.rd$ctcomfirm <- wenzhou.rd$confirmed

fit_1 <- lm(log(ctcomfirm) ~ date + treatment, data = wenzhou.rd)  # linear
fit_2 <- lm(log(ctcomfirm) ~ date * treatment, data = wenzhou.rd)  # linear interaction
fit_3 <- lm(log(ctcomfirm) ~ date + I(date^2) + treatment, data = wenzhou.rd)  # quadratic
fit_4 <- lm(log(ctcomfirm) ~ (date + I(date^2)) * treatment - I(date^2), data = wenzhou.rd)  # quadratic interaction
fit_5 <- lm(log(ctcomfirm) ~ (date + I(date^2)) * treatment, data = wenzhou.rd)  # quadratic interaction with quadratic date

wz_aic <- AIC(fit_1, fit_2, fit_3, fit_4, fit_5)
wz_bic <- BIC(fit_1, fit_2, fit_3, fit_4, fit_5)

#### ------------------- Table 2 ----------------------
stargazer(fit_1, fit_2, fit_3, fit_4, fit_5, type = "html", style = "all", out = "regression_wenzhou.html")




############### Shanghai ###################

region <- "Shanghai"
Y <- read.csv("shanghai.csv")
Y <- Y[Y$ctname == region, ]
shanghai.rd <- Y[, c("fulldate", "confirmed")]
shanghai.rd$date <- as.numeric(as.Date(shanghai.rd$fulldate)) - 18280 - 5
shanghai.rd$treatment <- c(rep(0, 4), rep(1, length(shanghai.rd$date) - 4))
shanghai.rd$ctcomfirm <- shanghai.rd$confirmed

fit_1 <- lm(log(ctcomfirm) ~ date + treatment, data = shanghai.rd)  # linear
fit_2 <- lm(log(ctcomfirm) ~ date * treatment, data = shanghai.rd)  # linear interaction
fit_3 <- lm(log(ctcomfirm) ~ date + I(date^2) + treatment, data = shanghai.rd)  # quadratic
fit_4 <- lm(log(ctcomfirm) ~ (date + I(date^2)) * treatment - I(date^2), data = shanghai.rd)  # quadratic interaction
fit_5 <- lm(log(ctcomfirm) ~ (date + I(date^2)) * treatment, data = shanghai.rd)  # quadratic interaction with quadratic date

sh_aic <- AIC(fit_1, fit_2, fit_3, fit_4, fit_5)
sh_bic <- BIC(fit_1, fit_2, fit_3, fit_4, fit_5)

dat_result <- data.frame(wz_AIC = wz_aic[, 2], wz_BIC = wz_bic[, 2], sh_AIC = sh_aic[, 2], sh_BIC = sh_bic[, 2])
write.csv(dat_result, file = "AIC_BIC.csv")

#### ------------------- Table 2 ----------------------
stargazer(fit_1, fit_2, fit_3, fit_4, fit_5, type = "html", style = "all", out = "regression_shanghai.html")

