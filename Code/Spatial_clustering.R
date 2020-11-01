library(adegenet)
library(spatialClust)
library(fields)
library(ClustGeo)
library(reshape2)
library(adespatial)
library(FactoMineR)
library(ggdendro)
library(dplyr)

# -----------------------------------------
## Data
setwd("./Data")
dat <- read.csv("latdata1014.csv")
dat <- dat[, -1]
city <- dat$ctname
scity <- city[c(1, 2)]
us <- read.csv("usedata1014.csv", header = T)[, -1]
us$fulldate <- as.Date(us$fulldate)



## PCA
end_day <- c(11, 5)
cluster_size <- c(2, 1)
component <- list()
cluster <- list()
alpha <- c(0.1, 0.1)
for(i in 1:2){
  dat_city_t <- dat[c(i, 4:nrow(dat)), ]
  x <- dat_city_t[, -c(1:3)]
  x <- x / matrix(dat_city_t$Population, length(dat_city_t$Population), ncol(x)) * 100000
  
  mark <- sapply(1:nrow(x), function(j){sum((x[j, 1:end_day[i]] != 0)) > 1}) # non-zero mark
  
  dat_city <- dat_city_t[mark, ]
  x_tol <- x[mark, ]
  lat <- dat_city$Latitude
  
  # Distance matrix
  D_0 <- dist(x_tol)
  D_1 <- as.dist(rdist(lat))
  
  # Choose alpha
  cr <- choicealpha(D_0, D_1, range.alpha = seq(0, 1, length.out = 6), K = 5, graph = TRUE)
  ggplot() +
    geom_line(aes(x = cr$range.alpha, y = cr$Q[, 1], color = "Based on D0"), size = 1) + 
    geom_line(aes(x = cr$range.alpha, y = cr$Q[, 2], color = "Based on D1"), size = 1) + 
    scale_x_continuous(breaks = cr$range.alpha) + 
    scale_y_continuous(breaks = seq(0, 1, 0.25), labels = c("0%", "25%", "50%", "75%", "100%")) + 
    theme_bw(base_family = "Times") +
    theme(panel.grid.minor = element_blank(),
          legend.position = "top",
          panel.border = element_blank(),
          text = element_text(size = 18),
          strip.background = element_rect(color = "white"),
          plot.title = element_text(hjust = 0.5)) +
    labs(x = "Mixing parameter", y = "Explained inertia", 
         colour = "", fill = "") + 
    guides(linetype = guide_legend(nrow = 1),
           color = guide_legend(nrow = 1, order = 1),
           fill = guide_legend(nrow = 1, order = 2),
           size = guide_legend(nrow = 1)) +
    scale_color_manual(values = c("black", "red")) + 
    geom_vline(aes(xintercept = 0.4), linetype = "dashed") + 
    ggsave(paste0("clus", ".pdf"), height = 4, width = 6)
    
  tree <- hclustgeo(D_0, D_1, alpha = 0.4)
  plot(tree)
  P5 <- cutree(tree, cluster_size[i])
  
  sum(P5 == 1)
  
############# Cluster Plot ###########
  df1 <- dendro_data(tree, type = "rectangle")
  df1$labels$label <- city[as.numeric(df1$labels$label)]
  sdf <- segment(df1)
  df1$labels$Group <- "B"

  if(i == 1) {
    sdf <- filter(sdf, x <= 45);
    ptree <- ggplot(sdf) +
      geom_segment(aes(x = x, y = y, xend = xend,yend = yend)) +
      geom_text(data = df1$labels[1:45, ], aes(x = x, y = y - 0.001, label = label[1:45]),
                angle = 90, hjust = 1, vjust = 0.3, size = 2.5 ) +
      scale_y_continuous(expand = c(-0.01, 0.01)) +
      scale_x_continuous(limits = c(0, 60)) +
      theme_bw() + labs(x = "", y = "") +
      geom_point(data = filter(df1$labels, label == scity[i]),
                 aes(x = x, y = y, color = Group,  shape = Group)) +
      scale_color_brewer("", palette = "Set1") +
      theme(legend.position = "none", panel.border = element_blank()) +
      geom_hline(yintercept = 0.01, lty = 2, col = 2)+
      geom_segment(data=data.frame(x = c(21.05, 55), y = c(0.015, 0.015), 
                                   xend = c(55, 55), yend = c(0.015, 0.001)),
                   aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_text(data = data.frame(x = 55, y = 0, lab = "......"), 
                aes(x = x, y = y, label = lab), size = 10)
    
  ggsave(paste0("ptree", scity[i], ".pdf"), height = 6, width = 12)
  }
  
  a <- P5[i]
  cluster_mark <- (P5 == a)
  cluster[[i]] <- dat_city[cluster_mark, ]
  x_t <- x_tol[cluster_mark, 1:(end_day[i] - 1)]
  fit <- PCA(x_t, ncp = ncol(x_t), scale.unit = T)
  a <- sum(fit$eig[, 2] > 10)
  a <- fit$ind$coord[, 1:max(a, 1)]
  component[[i]] <- cbind(as.character(dat_city$ctname)[cluster_mark], a, x_tol[cluster_mark, end_day[i]])
  colnames(component[[i]])[ncol(component[[i]])] <- "Dim.4"
}

sapply(1:2, function(i) nrow(cluster[[i]]))

wzcpa <- as.data.frame(component[[1]])
colnames(wzcpa) <- c("ctname", sapply(1:(ncol(wzcpa) - 1), function(k) paste0("pc", k)))                                                    

shcpa <- as.data.frame(component[[2]])
colnames(shcpa) <- c("ctname", sapply(1:(ncol(shcpa) - 1), function(k) paste0("pc", k)))  

wzdat <- filter(us, ctname != "Chongqing", ctname != "Shanghai") %>% 
  merge(wzcpa, by = "ctname")
shdat <- filter(us, ctname != "Chongqing", ctname != "Wenzhou") %>% 
  merge(shcpa, by = "ctname")

wzdat1 <- filter(wzdat, ctname == "Wenzhou") %>% arrange(fulldate)
wzdat2 <- filter(wzdat, ctname != "Wehzhou") %>% arrange(ctname, fulldate)
wzdat <- rbind(wzdat1, wzdat2) %>% unique()

shdat1 <- filter(shdat, ctname == "Shanghai") %>% arrange(fulldate)
shdat2 <- filter(shdat, ctname != "Shanghai") %>% arrange(ctname, fulldate)
shdat <- rbind(shdat1, shdat2) %>% unique()

write.csv(wzdat, "wzdat1014.csv")
write.csv(shdat, "shdat1014.csv")

