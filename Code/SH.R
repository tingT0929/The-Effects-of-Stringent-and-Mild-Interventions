## Data
setwd("./Data/")
source("../Code/Epidemic_model.R")
region <- "Shanghai"
Y <- read.csv("shdat1014.csv")
Y <- Y[Y$ctname == region,]
R <- Y$confirmed

N <- Y$Population[1]

policy_change_1 <- 5
policy_change_2 <- 15

# Gibbs sampler
para <- c(1, 5.1, 9.5)
alp <- c(0.5, 0.2, 0.1)
I <- rep(1, length(R))
sigma_R_L <- 100
R_L <- R + 0.1
R_L[R_L < 10^(-10)] <- 10^(-10)

max_l <- -Inf
para_t <- list(para, I, R_L, sigma_R_L, alp, max_l)

para <- list()
para[[1]] <- para_t
k <- 30000
for(h in 2:k){
  para[[1]] <- gibbs(para[[1]], R, N)
  cat(c(h, floor(para[[1]][[1]][1:3]), para[[1]][[3]][1:3], para[[1]][[5]], para[[1]][[6]], para[[1]][[4]]), "\n")
}

for(h in 2:k){
  para[[h]] <- gibbs(para[[h-1]], R, N)
  cat(c(h, floor(para[[h]][[2]][1:4]), para[[h]][[3]][1:4], para[[h]][[5]], para[[h]][[6]], para[[h]][[4]]), "\n")
}

save(para, file = paste0(region, "_para", ".rda"), version = 2)

