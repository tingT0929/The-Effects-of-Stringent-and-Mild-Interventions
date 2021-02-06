library(ggsci)

## Data
setwd("~/GitHub/The-Effects-of-Stringent-and-Mild-Interventions/Data/")
source("../Code/Epidemic_model.R")

re_scale <- function(x){
  A <- log(10) / 10
  a <-  (1 / A - 10) / 100
  b <- (10 - 1 / (2 * A)) / 5
  return((a * x ^ 2 + b * x) * (x < 10) + log(x + 10^ (-10)) * (x >= 10) / (log(10) / 10))
}

# ------------- Delay --------------
dat_plot <- data.frame()
for (i in 1:2) {
  if (i == 1) {
    load("Wenzhou_para.rda")
    region <- "Wenzhou"
    Y <- read.csv("wzdat1014.csv")
    
    policy_change_1 <- 3
    policy_change_2 <- 12
  } else {
    load("Shanghai_para.rda")
    region <- "Shanghai"
    Y <- read.csv("shdat1014.csv")
    
    policy_change_1 <- 5
    policy_change_2 <- 15
  }
  Y <- Y[Y$ctname == region,]
  R <- Y$confirmed
  
  N <- Y$Population[1]
  
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
  
  delay_3 <- lapply(mark, function(i){
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
  
  delay_3 <- sapply(1:length(smooth_dat), function(i){
    delay_3[[i]][,3]
  })
  
  delay_3 <- sapply(1:length(R), function(i){
    quantile(delay_3[i,], c(0.025, 0.5, 0.975))
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
  
  
  a <- 100000 / N
  
  if(i == 1){
    x <- 2:(length(R) + 1)
  }else{
    x <- 1:length(R)
  }
  
  dat_temp <- data.frame(x = x, confirm = re_scale(confirm[2,] * a), confirm_obs = re_scale(R * a), 
                         confirm_min = re_scale(confirm[1,] * a), confirm_max = re_scale(confirm[3,] * a),
                         delay3 = re_scale(delay_3[2,] * a), delay3_min = re_scale(delay_3[1,] * a), delay3_max = re_scale(delay_3[3,] * a),
                         delay5 = re_scale(delay_5[2,] * a), delay5_min = re_scale(delay_5[1,] * a), delay5_max = re_scale(delay_5[3,] * a))
  
  dat_plot <- rbind(dat_plot, dat_temp)
}

dat_plot$city <- factor(rep(c("Wenzhou", "Shanghai"), each = length(R)), levels = c("Wenzhou", "Shanghai"))

ggplot(dat_plot) + 
  geom_ribbon(aes(x = x, ymin = delay5_min, ymax = delay5_max), color = pal_jco()(7)[4], fill = pal_jco()(7)[4], alpha = 0.05, linetype = 3) +
  geom_ribbon(aes(x = x, ymin = delay3_min, ymax = delay3_max), color = pal_jco()(7)[2], fill = pal_jco()(7)[2], alpha = 0.05, linetype = 3) +
  # geom_ribbon(aes(x = x, ymin = confirm_min, ymax = confirm_max), color = '#636363', fill = '#636363', alpha = 1, linetype = 3) +
  geom_line(aes(x = x, y = delay5, color = "Delay 5 days"), size = 1) +
  geom_line(aes(x = x, y = delay3, color = "Delay 3 days"), size = 1) +
  geom_line(aes(x = x, y = confirm, color = "Latent positive individuals"), size = 1) +
  geom_point(aes(x = x, y = confirm_obs, color = "Observed positive individuals"), size = 1) +
  scale_color_manual(breaks = c("Delay 3 days", "Delay 5 days", "Latent positive individuals", "Observed positive individuals"),
                     values = c(pal_jco()(7)[2], pal_jco()(7)[4], 'gray', pal_jco()(7)[1])) +
  scale_x_continuous(breaks = seq(2, length(R), 5), label = format(seq.Date(from = as.Date("2020-1-20"), by = "day", length.out = (length(R) + 1)), "%m/%d")[seq(2, length(R), 5)]) + 
  # geom_vline(xintercept = policy_change_1, linetype = 2, size = 1, alpha = 0.8, color = "red") +
  theme_bw(base_family = "Times") +
  theme(panel.grid.minor = element_blank(),
        legend.position = "top",
        panel.border = element_blank(),
        text = element_text(size = 18),
        strip.background = element_rect(
          color = "white"),
        # text = element_text(family = "STHeiti"),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Date", y = "Positive individuals per 100,000", 
       # title = "The simulation of delay effect", 
       colour = "", fill = "") + 
  guides(linetype = guide_legend(nrow = 1),
         color = guide_legend(nrow = 1, order = 1),
         fill = guide_legend(nrow = 1, order = 2),
         # shape = guide_legend(nrow = 1),
         size = guide_legend(nrow = 1),
         color = T) +
  facet_wrap(~ city) +
  scale_y_continuous(breaks = re_scale(c(0, 10, 10^2, 10^3, 10^4)), 
                     label = c(0, 10, 10^2, 10^3, 10^4))

ggsave("SH_WZ_Policy_delay.pdf", width = 10, height = 4.5)



# ------------- Effectiveness -----------------
dat_plot2 <- data.frame()
for (i in 1:2) {
  if (i == 1) {
    load("Wenzhou_para.rda")
    region <- "Wenzhou"
    Y <- read.csv("wzdat1014.csv")
    
    policy_change_1 <- 3
    policy_change_2 <- 12
  } else {
    load("Shanghai_para.rda")
    region <- "Shanghai"
    Y <- read.csv("shdat1014.csv")
    
    policy_change_1 <- 5
    policy_change_2 <- 15
  }
  Y <- Y[Y$ctname == region,]
  R <- Y$confirmed
  
  N <- Y$Population[1]
  
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
  
  sec_1 <- lapply(mark, function(i){
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
  
  sec_1 <- sapply(1:length(smooth_dat), function(i){
    sec_1[[i]][,3]
  })
  
  sec_1 <- sapply(1:length(R), function(i){
    quantile(sec_1[i,], c(0.025, 0.5, 0.975))
  })
  
  sec_2 <- lapply(mark, function(i){
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
  
  sec_2 <- sapply(1:length(smooth_dat), function(i){
    sec_2[[i]][,3]
  })
  
  sec_2 <- sapply(1:length(R), function(i){
    quantile(sec_2[i,], c(0.025, 0.5, 0.975))
  })
  
  if(i == 1){
    x <- 2:(length(R) + 1)
  }else{
    x <- 1:length(R)
  }
  a <- 100000 / N
  
  dat_temp2 <- data.frame(x = x, confirm = re_scale(confirm[2,] * a), confirm_obs = re_scale(R * a), 
                          confirm_min = re_scale(confirm[1,] * a), confirm_max = re_scale(confirm[3,] * a),
                          without_last = re_scale(sec_1[2,] * a), without_last_min = re_scale(sec_1[1,] * a), without_last_max = re_scale(sec_1[3,] * a),
                          without_all = re_scale(sec_2[2,] * a), without_all_min = re_scale(sec_2[1,] * a), without_all_max = re_scale(sec_2[3,] * a))
  
  dat_plot2 <- rbind(dat_plot2, dat_temp2)
}

dat_plot2$city <- factor(rep(c("Wenzhou", "Shanghai"), each = length(R)), levels = c("Wenzhou", "Shanghai"))
dat_plot2$policy_date <- 4
dat_plot2$policy_date[dat_plot2$city == "Shanghai"] <- 5
dat_plot2$policy_date_2 <- 13
dat_plot2$policy_date_2[dat_plot2$city == "Shanghai"] <- 15

ggplot(dat_plot2) + 
  geom_ribbon(aes(x = x, ymin = without_all_min, ymax = without_all_max), color = pal_jco()(7)[4], fill = pal_jco()(7)[4], alpha = 0.05, linetype = 3) +
  geom_ribbon(aes(x = x, ymin = without_last_min, ymax = without_last_max), color = pal_jco()(7)[2], fill = pal_jco()(7)[2], alpha = 0.05, linetype = 3) +
  # geom_ribbon(aes(x = x, ymin = confirm_min, ymax = confirm_max), color = pal_jco()(7)[4], fill = pal_jco()(7)[4], linetype = 3) +
  geom_line(aes(x = x, y = without_all, color = "Scenario 1"), size = 1) +
  geom_line(aes(x = x, y = without_last, color = "Scenario 2"), size = 1) +
  geom_line(aes(x = x, y = confirm, color = "Latent positive individuals"), size = 1) +
  geom_point(aes(x = x, y = confirm_obs, color = "Observed positive individuals"), size = 1) +
  geom_vline(aes(xintercept = policy_date), linetype = "dashed") +
  geom_vline(aes(xintercept = policy_date_2), linetype = "dashed") +
  scale_color_manual(values = pal_jco()(7)[c(1, 2, 4)]) +
  # scale_color_jco() +
  scale_x_continuous(breaks = seq(2, length(R), 5), 
                     label = format(seq.Date(from = as.Date("2020-1-20"), by = "day", length.out = (length(R) + 1)), "%m/%d")[seq(2, length(R), 5)]) +
  scale_color_manual(breaks = c("Scenario 1", "Scenario 2", "Latent positive individuals", "Observed positive individuals"),
                     values = c(pal_jco()(7)[4], pal_jco()(7)[2], 'gray', pal_jco()(7)[1])) +
  theme_bw(base_family = "Times") +
  theme(panel.grid.minor = element_blank(),
        legend.position = "top",
        panel.border = element_blank(),
        text = element_text(size = 16),
        strip.background = element_rect(
          color = "white"),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Date", y = "Positive individuals per 100,000", 
       colour = "", fill = "") + 
  guides(linetype = guide_legend(nrow = 1),
         color = guide_legend(nrow = 1, order = 1),
         fill = guide_legend(nrow = 1, order = 2),
         size = guide_legend(nrow = 1),
         color = T) + 
  facet_wrap(~ city) +
  scale_y_continuous(breaks = re_scale(c(0, 10, 100, 1000, 10000, 100000)), 
                     label = c(0, 10, 100, 1000, 10000, "100000"), limits = re_scale(c(0, 100000)))

ggsave("SH_WZ_Policy_effect.pdf", width = 10, height = 4.5)



