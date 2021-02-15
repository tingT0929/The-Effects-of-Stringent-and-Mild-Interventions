## Setting
library(ggsci)

## Data
setwd("../Data/")
source("../Code/Epidemic_model.R")

f_sim <- function(k, alp) {
  alp[4] * (alp[3] / (1 + exp(2 * log(99) / alp[2] * (k - alp[1] - alp[2] / 2))) + 1 - alp[3])
}

# ------------- Delay --------------
dat_plot <- data.frame()
for (i in 1:2) {
  if (i == 1) {
    region <- "Wenzhou"
    load("Wenzhou_para.rda")
    
  } else {
    region <- "Shanghai"
    load("Shanghai_para.rda")
    
  }
  
  N <-  6537101  ## The average population of the states of the United States
  mark <- seq(1, length(para), 10)
  
  ## Simulation 1
  sim_path <- lapply(mark, function(i){
    Y <- cbind(N - para[[i]][[2]][1], para[[i]][[2]][1], 0, 0)
    
    a <- para[[i]][[1]]
    intervention_start_date <- 16
    intervention_effect_date <- 20
    for(j in 2:100){
      a[1] <- f_sim(j, c(intervention_start_date, intervention_effect_date, 0, para[[i]][[5]][2]))
     
      out <- as.numeric(ode(y = Y[j-1,-4], times = (j-1):j, eqn, parms = a, N = N)[2,-1])
      Y <- rbind(Y, c(out, N - sum(out)))
    }
    
    Z <- Y
    for(j in (floor(intervention_start_date)+1):100){
      a[1] <- f_sim(j, c(intervention_start_date, intervention_effect_date, 
                         (para[[i]][[5]][2] - para[[i]][[5]][3]) / para[[i]][[5]][2], para[[i]][[5]][2]))
      
      out <- as.numeric(ode(y = Z[j-1,-4], times = (j-1):j, eqn, parms = a, N = N)[2,-1])
      Z[j,] <- c(out, N - sum(out))
    }
    
    return(list(Y, Z))
  })
  
  confirm <- sapply(1:length(mark), function(i){
    sim_path[[i]][[1]][,3]
  })
  
  confirm <- sapply(1:100, function(i){
    quantile(confirm[i,], c(0.025, 0.5, 0.975))
  })
  
  confirm_1 <- sapply(1:length(mark), function(i){
    sim_path[[i]][[2]][,3]
  })
  
  confirm_1 <- sapply(1:100, function(i){
    quantile(confirm_1[i,], c(0.025, 0.5, 0.975))
  })
  
  dat_temp <- data.frame(x = 1:100, 
                         confirm = confirm[2,], 
                         confirm_min = confirm[1,], confirm_max = confirm[3,],
                         confirm_1 = confirm_1[2,], 
                         confirm_min_1 = confirm_1[1,], confirm_max_1 = confirm_1[3,])
  
  dat_plot <- rbind(dat_plot, dat_temp)
}

dat_plot$city <- factor(rep(c("Type 1", "Type 2"), each = 100), levels = c("Type 1", "Type 2"))

dat_plot_null <- dat_plot
dat_plot$confirm_min_1[c(1:16, 101:116)] <- NA
dat_plot$confirm_max_1[c(1:16, 101:116)] <- NA
ggplot(dat_plot[c(1:40, 101:140),]) + 
  geom_vline(aes(xintercept = 16), linetype = "dashed") +
  geom_vline(aes(xintercept = 36), linetype = "dashed") +
  geom_line(aes(x = x, y = confirm_1, color = "Under interventions"), size = 1) +
  geom_line(aes(x = x, y = confirm, color = "Simulated outbreak curve"), size = 1) +
  # geom_ribbon(aes(x = x, ymin = confirm_min, ymax = confirm_max), color = pal_jco()(7)[4], fill = pal_jco()(7)[4], alpha = 0.05, linetype = 3) +
  # geom_ribbon(aes(x = x, ymin = confirm_min_1, ymax = confirm_max_1), color = pal_jco()(7)[6], fill = pal_jco()(7)[6], alpha = 0.05, linetype = 3) +
  scale_color_manual(breaks = c("Simulated outbreak curve", "Under interventions"),
                     values = c(pal_jco()(7)[4], pal_jco()(7)[6])) +
  scale_x_continuous(breaks = seq(1, 100, 7)) +
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
  labs(x = "Day", y = "Positive individuals", 
       colour = "", fill = "") + 
  guides(linetype = guide_legend(nrow = 1),
         color = guide_legend(nrow = 1, order = 1),
         fill = guide_legend(nrow = 1, order = 2),
         # shape = guide_legend(nrow = 1),
         size = guide_legend(nrow = 1),
         color = T) +
  facet_wrap(~ city)

ggsave("sim_abroad.pdf", width = 10, height = 4.5)

