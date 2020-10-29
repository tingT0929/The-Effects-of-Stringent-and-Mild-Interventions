## Data
setwd("./Data/")
source("../Code/Modeling_0808.R")
region <- "Wenzhou"
Y <- read.csv("wenzhou.csv")
Y <- Y[Y$ctname == region,]
R <- Y$confirmed

N <- Y$Population[1]

policy_change_1 <- 3
policy_change_2 <- 12

# Gibbs sampler
para <- c(1, 5.1, 9.5)
alp <- c(0.5, 0.2, 0.1)
I <- rep(2, length(R))
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
  cat(c(h, floor(para[[1]][[2]][1:3]), para[[1]][[3]][1:3], para[[1]][[5]], para[[1]][[6]], para[[1]][[4]]), "\n")
}

for(h in 2:k){
  para[[h]] <- gibbs(para[[h-1]], R, N)
  cat(c(h, floor(para[[h]][[2]][1:4]), para[[h]][[3]][1:4], para[[h]][[5]], para[[h]][[6]], para[[h]][[4]]), "\n")
}

save(para, file = paste0(region, "_para", ".rda"), version = 2)

## Plot
mark <- seq(1, length(para), 10)
smooth_dat <- lapply(mark, function(i){
  Y <- cbind(N - para[[i]][[2]] - para[[i]][[3]], para[[i]][[2]], para[[i]][[3]])
  return(Y)
})

confirm <- sapply(1:length(smooth_dat), function(i){
  smooth_dat[[i]][,3]
})

confirm <- sapply(1:length(R), function(i){
  quantile(confirm[i,], c(0.025, 0.5, 0.975))
})

## Policy 1
delay_2 <- lapply(mark, function(i){
  Y <- cbind(N - para[[i]][[2]] - para[[i]][[3]], para[[i]][[2]], para[[i]][[3]])
  
  a <- para[[i]][[1]]
  b <- para[[i]][[5]]
  for(j in policy_change_1:length(R)){
    a[1] <- f_alp(j-3, b)
    out <- as.numeric(ode(y = Y[j-1,], times = (j-1):j, eqn, parms = a, N = N)[2,-1])
    Y[j,] <- out
  }
  
  return(Y)
})


delay_2 <- sapply(1:length(smooth_dat), function(i){
  delay_2[[i]][,3]
})

delay_2 <- sapply(1:length(R), function(i){
  quantile(delay_2[i,], c(0.025, 0.5, 0.975))
})

delay_5 <- lapply(mark, function(i){
  Y <- cbind(N - para[[i]][[2]] - para[[i]][[3]], para[[i]][[2]], para[[i]][[3]])
  
  a <- para[[i]][[1]]
  b <- para[[i]][[5]]
  for(j in policy_change_1:length(R)){
    a[1] <- f_alp(j-5, b)
    out <- as.numeric(ode(y = as.numeric(Y[j-1,]), times = (j-1):j, eqn, parms = a, N = N)[2,-1])
    Y[j,] <- out
  }
  
  return(Y)
})

delay_5 <- sapply(1:length(smooth_dat), function(i){
  delay_5[[i]][,3]
})

delay_5 <- sapply(1:length(R), function(i){
  quantile(delay_5[i,], c(0.025, 0.5, 0.975))
})

## latex
delay_5[2,29] / confirm[2,29]
delay_2[2,29] / confirm[2,29]
delay_5[,29] 
delay_2[,29]

## Effectiveness
delay_2 <- lapply(mark, function(i){
  Y <- cbind(N - para[[i]][[2]] - para[[i]][[3]], para[[i]][[2]], para[[i]][[3]])
  
  a <- para[[i]][[1]]
  b <- para[[i]][[5]]
  b[3] <- b[2]
  for(j in policy_change_2:length(R)){
    a[1] <- f_alp(j-1, b)
    out <- as.numeric(ode(y = as.numeric(Y[j-1,]), times = (j-1):j, eqn, parms = a, N = N)[2,-1])
    Y[j,] <- out
  }
  
  return(Y)
})

delay_2 <- sapply(1:length(smooth_dat), function(i){
  delay_2[[i]][,3]
})

delay_2 <- sapply(1:length(R), function(i){
  quantile(delay_2[i,], c(0.025, 0.5, 0.975))
})

delay_5 <- lapply(mark, function(i){
  Y <- cbind(N - para[[i]][[2]] - para[[i]][[3]], para[[i]][[2]], para[[i]][[3]])
  
  a <- para[[i]][[1]]
  b <- para[[i]][[5]]
  b[2] <- b[1]
  b[3] <- b[1]
  for(j in policy_change_1:length(R)){
    a[1] <- f_alp(j-1, b)
    out <- as.numeric(ode(y = as.numeric(Y[j-1,]), times = (j-1):j, eqn, parms = a, N = N)[2,-1])
    Y[j,] <- out
  }
  
  return(Y)
})

delay_5 <- sapply(1:length(smooth_dat), function(i){
  delay_5[[i]][,3]
})

delay_5 <- sapply(1:length(R), function(i){
  quantile(delay_5[i,], c(0.025, 0.5, 0.975))
})


x <- c(1:length(R))
a <- 100000 / N
ggplot() + 
  geom_ribbon(aes(x = x, ymin = re_scale(delay_2[1,] * a), ymax = re_scale(delay_2[3,] * a)), color = pal_jco()(7)[2], fill = pal_jco()(7)[4], alpha = 0.05, linetype = 3) +
  geom_ribbon(aes(x = x, ymin = re_scale(delay_5[1,] * a), ymax = re_scale(delay_5[3,] * a)), color = pal_jco()(7)[4], fill = pal_jco()(7)[2], alpha = 0.05, linetype = 3) +
  geom_ribbon(aes(x = x, ymin = re_scale(confirm[1,] * a), ymax = re_scale(confirm[3,] * a)), color = pal_jco()(7)[1], fill = pal_jco()(7)[1], linetype = 3) +
  geom_line(aes(x = x, y = re_scale(delay_2[2,] * a), color = "Without implementation of all policies"), size = 1) +
  geom_line(aes(x = x, y = re_scale(delay_5[2,] * a), color = "Without implementation of the last policy"), size = 1) +
  geom_line(aes(x = x, y = re_scale(confirm[2,] * a), color = "Smoothed comfirmed cases"), size = 1) +
  # geom_point(aes(x = x, y = re_scale(R * a), color = "Observed comfirmed Cases")) + 
  # scale_color_manual(values = c('red', "orange", "purple")) +
  scale_color_manual(values = pal_jco()(7)[c(1, 2, 4)]) +
  # scale_color_jco() +
  scale_x_continuous(breaks = seq(2, length(R), 5), 
                     label = format(seq.Date(from = as.Date("2020-1-21"), by = "day", length.out = length(R)), "%m/%d")[seq(2, length(R), 5)]) +
  geom_vline(xintercept = c(policy_change_1, policy_change_2), linetype = 2, size = 1, alpha = 0.8, color = "grey") +
  theme_bw(base_family = "Times") +
  theme(panel.grid.minor = element_blank(),
        legend.position = "top",
        panel.border = element_blank(),
        # text = element_text(family = "STHeiti"),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Date", y = "Confirmed cases (per 100,000)", 
       # title = "The simulation of effectiveness for policy intervention", 
       colour = "", fill = "") + 
  guides(linetype = guide_legend(nrow = 1),
         color = guide_legend(nrow = 1, order = 1),
         fill = guide_legend(nrow = 1, order = 2),
         # shape = guide_legend(nrow = 1),
         size = guide_legend(nrow = 1),
         color = T) + 
  scale_y_continuous(breaks = re_scale(c(0, 10, 100, 1000, 10000, 100000)), 
                     label = c(0, 10, 100, 1000, 10000, "100000"), limits = re_scale(c(0, 100000)))

ggsave(paste0(region, "_Policy_effect.pdf"), width = 8.5, height = 6)

delay_5[2,29] / confirm[2,29]
delay_2[2,29] / confirm[2,29]
(delay_5[2,29] - delay_2[2,29]) / delay_5[2,29]
(delay_2[2,29] - confirm[2,29]) / delay_2[2,29]
delay_5[,29]
delay_2[,29]

