library(Synth)     # version 1.1-5
library(ggplot2)   # version 3.3.2
library(dplyr)     # version 1.0.2
library(reshape2)  # version 1.4.4
library(tidyr)     # version 1.1.2
library(rdrobust)  # version 0.99.8
library(stargazer) # version 5.2.2

############# Wenzhou ##############
# read data
wenzhou.data <- read.csv("wzdat1014.csv", header = T, fileEncoding = "GBK")
index.wenzhou <- length(unique(wenzhou.data$ctname))
wenzhou.data$unit.num <- rep(1:index.wenzhou, each = 29)
wenzhou.data$ctname <- as.character(wenzhou.data$ctname)
wenzhou.data$date <- rep(as.numeric(as.Date(wenzhou.data$fulldate)[1:29]), times = index.wenzhou)
wenzhou.data$Population <- as.numeric(wenzhou.data$Population)
wenzhou.data$per.confirm <- wenzhou.data$confirmed/wenzhou.data$Population * 1e+05

index1 = 1:index.wenzhou
# Prepare data
dataprep.out.wenzhou<-
  dataprep(
    foo = wenzhou.data,
    predictors = c("density", "Meantem2020", "prop.elder", "pergdp", "pc1","pc2","pc3","pc4"),
    predictors.op = "mean",
    dependent = "per.confirm",
    unit.variable = "unit.num",
    time.variable = "date",
    special.predictors = NULL,
    treatment.identifier = 1,
    controls.identifier = index1[-1],
    time.predictors.prior = c(as.Date("2020-01-21"):as.Date("2020-01-31")),
    time.optimize.ssr =  c(as.Date("2020-01-21"):as.Date("2020-01-31")),
    unit.names.variable = "ctname",
    time.plot =  c(as.Date("2020-01-21"):as.Date("2020-02-05"))
  )

# Synth
synth.out.wenzhou <- synth(dataprep.out.wenzhou, optimxmethod = c("Nelder-Mead", "BFGS", "L-BFGS-B"))


############# Shanghai ##############

# read data
shanghai.data <- read.csv("shdat1014.csv", header = T, fileEncoding = "GBK")
index.shanghai <- length(unique(shanghai.data$ctname))
shanghai.data$unit.num <- rep(1:index.shanghai, each = 29)
shanghai.data$ctname <- as.character(shanghai.data$ctname)
shanghai.data$date <-  rep(as.numeric(as.Date(shanghai.data$fulldate)[1:29]), times = index.shanghai)
shanghai.data$Population <- as.numeric(shanghai.data$Population)
shanghai.data$per.confirm <- shanghai.data$confirmed / shanghai.data$Population * 1e05

index2 = 1:index.shanghai
dataprep.out.shanghai<-
  dataprep(
    foo = shanghai.data,
    predictors = c("density", "Meantem2020", "prop.elder", "pergdp", "pc1","pc2"),
    predictors.op = "mean",
    dependent = "per.confirm",
    unit.variable = "unit.num",
    time.variable = "date",
    special.predictors = NULL,
    treatment.identifier = 1,
    controls.identifier = index2[-1],
    time.predictors.prior = c(as.Date("2020-01-20"):as.Date("2020-01-23")),
    time.optimize.ssr =  c(as.Date("2020-01-20"):as.Date("2020-01-23")),
    unit.names.variable = "ctname",
    time.plot =  c(as.Date("2020-01-20"):as.Date("2020-02-04"))
  )

# Synth
synth.out.shanghai <- synth(dataprep.out.shanghai, optimxmethod = c("Nelder-Mead", "BFGS","L-BFGS-B"))
