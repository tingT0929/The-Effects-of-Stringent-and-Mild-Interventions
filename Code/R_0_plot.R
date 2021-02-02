library(coda)
library(tidybayes)

## Data
setwd("~/GitHub/The-Effects-of-Stringent-and-Mild-Interventions/Data/")
load("Wenzhou_para.rda")
wz_para <- para
load("Shanghai_para.rda")
sh_para <- para

mark <- seq(1, length(para), 10)

x <- 1:3

R_0_sh <- sapply(mark, function(i){
  sh_para[[i]][[5]] / sum(1 / 9.5 + 1 / sh_para[[i]][[1]][2])
})

rownames(R_0_sh) <- sapply(x, function(i){paste0("place", "[", i, "]")})
R_0_sh <- mcmc(t(R_0_sh))

R_0_wz <- sapply(mark, function(i){
  wz_para[[i]][[5]] / sum(1 / 9.5 + 1 / 1 / wz_para[[i]][[1]][2])
})

rownames(R_0_wz) <- sapply(x, function(i){paste0("place", "[", i, "]")})
R_0_wz <- mcmc(t(R_0_wz))

name_i <- factor(c("Shanghai", ' Wenzhou'), ordered = F)
a1 <- as.data.frame(spread_draws(R_0_sh, place[i]))
a1$class <- rep(name_i[1], nrow(a1))
a2 <- as.data.frame(spread_draws(R_0_wz, place[i]))
a2$class <- rep(name_i[2], nrow(a2))
a <- rbind(a2, a1)

ggplot() +
  stat_eye(aes(x = i, y = place, fill = class, color = class), a) + 
  #  stat_eye(aes(x = i, y = age, fill = "Symptomatic"), spread_draws(sym_Susceptibility, age[i]),
  #           alpha = 0.9) +
  facet_wrap(.~class) +
  scale_x_continuous(breaks = 1:3, label = c("Period 1", "Period 2", "Period 3")) +
  theme_bw(base_family = "Times") +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        panel.border = element_blank(),
        # text = element_text(family = "STHeiti"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0),
        strip.background = element_rect(color = "white")) +
  labs(x = "Period", y = "Effective reproduction numbers", 
       title = "", 
       colour = "", fill = "") + 
  guides(linetype = guide_legend(nrow = 1),
         color = guide_legend(nrow = 1, order = 1),
         fill = guide_legend(nrow = 1, order = 2),
         # shape = guide_legend(nrow = 1),
         size = guide_legend(nrow = 1),
         color = T) + 
  scale_fill_manual(values = pal_jco()(10)[c(2, 1)]) +
  scale_color_manual(values = pal_jco()(10)[c(2, 1)]) 

ggsave(paste0("R_0.pdf"), width = 5, height = 2.5)

