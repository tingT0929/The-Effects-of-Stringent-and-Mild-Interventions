library(Synth)     # version 1.1-5
library(ggplot2)   # version 3.3.0
library(dplyr)     # version 0.8.4
library(reshape2)  # version 1.4.3
library(tidyr)     # version 1.0.2
library(rdrobust)  # version 0.99.5
library(stargazer) # version 5.2.2
library(ggsci)     # version 2.9.0

setwd("/Users/robby/Documents/R/feiyan")

# read data
wenzhou.data <- read.csv("wzdat0831.csv", header = T, fileEncoding = "GBK")
index.wenzhou <- length(unique(wenzhou.data$ctname))
wenzhou.data$unit.num <- rep(1:index.wenzhou,each = 29)
wenzhou.data$ctname <- as.character(wenzhou.data$ctname)
wenzhou.data$date <-  rep(as.numeric(as.Date(wenzhou.data$fulldate)[1:29]),times = index.wenzhou)
wenzhou.data$Population <- as.numeric(wenzhou.data$Population)
wenzhou.data$per.confirm <- wenzhou.data$confirmed / wenzhou.data$Population * 100000
index.wenzhou
#### ------------------- Figure 1 ----------------------
index = 1:index.wenzhou
# Prepare data
dataprep.out.wenzhou<-
  dataprep(
    foo = wenzhou.data,
    predictors = c("density", "Meantem2020", "prop.elder", "pergdp", "pc1", "pc2", "pc3"),
    predictors.op = "mean",
    dependent = "per.confirm",
    unit.variable = "unit.num",
    time.variable = "date",
    special.predictors = NULL,
    treatment.identifier = 1,
    controls.identifier = index[-1],
    time.predictors.prior = c(as.Date("2020-01-21"):as.Date("2020-01-31")),
    time.optimize.ssr =  c(as.Date("2020-01-21"):as.Date("2020-01-31")),
    unit.names.variable = "ctname",
    time.plot =  c(as.Date("2020-01-21"):as.Date("2020-02-08"))
  )

# Synth
synth.out.wenzhou <- synth(dataprep.out.wenzhou)

# Extract data
line.wenzhou <- data.frame(per.confirm = c(as.numeric(dataprep.out.wenzhou$Y1plot), as.numeric(dataprep.out.wenzhou$Y0plot %*% synth.out.wenzhou$solution.w)), 
                           date = rep(as.Date(c(18282:18300), origin = "1970-01-01"), 2),
                           index = rep(c("Wenzhou", "Synthetic"), each = 19))
# Output weight and variables
synth.tables.wenzhou <- synth.tab(
  dataprep.res = dataprep.out.wenzhou,
  synth.res = synth.out.wenzhou)
print(synth.tables.wenzhou$tab.w[order(synth.tables.wenzhou$tab.w$w.weights), ])
write.csv(synth.tables.wenzhou$tab.w[order(synth.tables.wenzhou$tab.w$w.weights), ], "wenzhou0831.csv")
synth.tables.wenzhou$tab.pred
write.csv(synth.tables.wenzhou$tab.pred, "wenzhoupred0831.csv")

# trend
trend <- spread(line.wenzhou, index, per.confirm)
trend$Synthetic <- floor(trend$Synthetic * 925)
trend$Wenzhou <- trend$Wenzhou * 925
trend
line.wenzhou[line.wenzhou$date == "2020-02-08", ]

line.wenzhou$index <- factor(line.wenzhou$index, levels = c("Wenzhou", "Synthetic"))
# plot
ggplot(data = line.wenzhou, mapping = aes(x = date,y = per.confirm, group = index)) +
  geom_line(aes(color = index), size = 1) +
  geom_vline(aes(xintercept = as.Date("2020-02-01")), linetype = "dashed") +
  scale_x_date(date_labels = "%m/%d", date_breaks = "2 days") +
  theme_bw(base_family = "Times") +
  theme(panel.grid.minor = element_blank(),
        legend.position = "top",
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  scale_color_jco() +
  labs(x = "Date", y = "The postive number of COVID-19 per 100,000", 
       title = NULL,
       color = "")

ggsave("wenzhou_2lines.pdf")

# Placebo test
placeboline.wenzhou <- {}
index = 1:index.wenzhou
for (i in index) {
  dataprep.out<-
    dataprep(
      foo = wenzhou.data,
      predictors = c("density", "Meantem2020", "prop.elder", "pergdp", "pc1", "pc2", "pc3"),
      predictors.op = "mean",
      dependent = "per.confirm",
      unit.variable = "unit.num",
      time.variable = "date",
      special.predictors = NULL,
      treatment.identifier = i,
      controls.identifier = index[-i],
      time.predictors.prior = c(as.Date("2020-01-21"):as.Date("2020-01-31")),
      time.optimize.ssr =  c(as.Date("2020-01-21"):as.Date("2020-01-31")),
      unit.names.variable = "ctname",
      time.plot =  c(as.Date("2020-01-21"):as.Date("2020-02-08"))
    )
  try({
     synth.out <- synth(dataprep.out)
  placeboline.wenzhou <- c(placeboline.wenzhou, 
                           list(dataprep.out$Y1plot - dataprep.out$Y0plot %*% synth.out$solution.w))
  })
}
placebo.wenzhou <- as.data.frame(placeboline.wenzhou)
index.placebo.wenzhou <- ncol(placebo.wenzhou)
placebo.wenzhou2 <- gather(placebo.wenzhou, area, per.confirm)
placebo.wenzhou2$date <- rep(as.Date(c(18282:18300), origin = "1970-01-01"), index.placebo.wenzhou)

# Discards counties with MSE ten times higher than Wenzhou’s
befpolicy <- placebo.wenzhou2[placebo.wenzhou2$date %in% as.Date(c(18282:18292), origin = "1970-01-01"),]
befpolicy <- spread(befpolicy[,1:3],area,per.confirm)
mse.wenzhou <- apply(befpolicy[,2:(index.placebo.wenzhou + 1)]^2, 2, mean) # Calculate MSE
tentimes <- names(mse.wenzhou[mse.wenzhou > mse.wenzhou["X1"] * 10]) # Get the corresponding index
placebo.wenzhou2 <- placebo.wenzhou2[!placebo.wenzhou2$area %in% tentimes,]
placebo.wenzhou2$group <- factor(c(rep("Wenzhou",19), rep("Control Regions", (index.placebo.wenzhou-1-length(tentimes))*19)),
                                 levels = c("Wenzhou", "Control Regions"))

ggplot(data = placebo.wenzhou2,mapping=aes(x = date,y = per.confirm, group = area))+
   geom_line(aes(color = group, size = group, alpha = group))+
  scale_x_date(date_labels ="%m/%d",date_breaks = "2 days")+
  theme_bw(base_family = "Times") +
  theme(panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top",
        panel.border = element_blank(),
        text = element_text(family = "Times"),
        plot.title = element_text(hjust = 0.5))+
  scale_colour_discrete(name  ="",
                        breaks=c("Wenzhou", "Control Regions"),
                        labels=c("Wenzhou", "Control Regions")) +
  scale_size_discrete(name  ="",
                      breaks=c("Wenzhou", "Control Regions"),
                      labels=c("Wenzhou", "Control Regions")) +
  scale_colour_manual(values = c(pal_jco()(7)[1],  pal_jco()(7)[3]))+
  scale_alpha_manual(values = c(1, 0.3), guide = FALSE)+
  scale_size_manual(values = c(1, 0.5), guide = FALSE)+
  labs(x = "Date", y = "Gap in the postive number of COVID-19 per 100,000", 
       title = NULL,
       color = "")

ggsave("wenzhou0830.pdf")



#### shanghai#########

# read data
shanghai.data <- read.csv("shdat0831.csv", header = T, fileEncoding = "GBK")
index.shanghai <- length(unique(shanghai.data$ctname))
shanghai.data$unit.num <- rep(1:index.shanghai,each = 29)
shanghai.data$ctname <- as.character(shanghai.data$ctname)
shanghai.data$date <-  rep(as.numeric(as.Date(shanghai.data$fulldate)[1:29]),times = index.shanghai)
shanghai.data$Population <- as.numeric(shanghai.data$Population)
shanghai.data$per.confirm <- shanghai.data$confirmed / shanghai.data$Population * 100000
#### ------------------- Figure 1 ----------------------
index = 1:index.shanghai
dataprep.out.shanghai<-
  dataprep(
    foo = shanghai.data,
    predictors = c("density", "Meantem2020", "prop.elder", "pergdp", "pc1"),
    predictors.op = "mean",
    dependent = "per.confirm",
    unit.variable = "unit.num",
    time.variable = "date",
    special.predictors = NULL,
    treatment.identifier = 1,
    controls.identifier = index[-1],
    time.predictors.prior = c(as.Date("2020-01-20"):as.Date("2020-01-23")),
    time.optimize.ssr =  c(as.Date("2020-01-20"):as.Date("2020-01-23")),
    unit.names.variable = "ctname",
    time.plot =  c(as.Date("2020-01-20"):as.Date("2020-02-05"))
  )

# Synth
synth.out.shanghai <- synth(dataprep.out.shanghai)

# Extract data
line.shanghai <- data.frame(per.confirm = c(as.numeric(dataprep.out.shanghai$Y1plot), as.numeric(dataprep.out.shanghai$Y0plot %*% synth.out.shanghai$solution.w)), 
                           date = rep(as.Date(c(18281:18297), origin = "1970-01-01"), 2),
                           index = rep(c("Shanghai", "Synthetic"), each = 17))

# Output weight and variables
synth.tables.shanghai <- synth.tab(
  dataprep.res = dataprep.out.shanghai,
  synth.res = synth.out.shanghai)
print(synth.tables.shanghai$tab.w[order(synth.tables.shanghai$tab.w$w.weights),])
write.csv(synth.tables.shanghai$tab.w[order(synth.tables.shanghai$tab.w$w.weights),],"shanghai0831.csv")
synth.tables.shanghai$tab.pred
write.csv(synth.tables.shanghai$tab.pred,"shanghaipred0831.csv")

# trend
trend <- spread(line.shanghai,index,per.confirm)
trend$Synthetic <- floor(trend$Synthetic * shanghai.data$Population[1]) / 100000
trend$Shanghai <- trend$Shanghai * shanghai.data$Population[1] / 100000
trend
# write.csv(trend,"trend.csv",fileEncoding = "GBK")
line.shanghai <- line.shanghai[c(-17, -34), ]

# plot
ggplot(data = line.shanghai, mapping = aes(x = date,y = per.confirm, group = index)) +
  geom_line(aes(color = index), size = 1) +
  geom_vline(aes(xintercept = as.Date("2020-01-24")), linetype = "dashed")+
  scale_x_date(date_labels ="%m/%d", date_breaks = "2 days")+
  theme_bw(base_family = "Times") +
  theme(panel.grid.minor = element_blank(),
        legend.position = "top",
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  scale_color_jco() +
  labs(x = "Date", y = "The postive number of COVID-19 per 100,000", 
       title =NULL,
       color = "")

ggsave("shanghai_2lines.pdf")


# Placebo test
placeboline.shanghai <- {}
index = 1:index.shanghai
for (i in index) {
  dataprep.out<-
    dataprep(
      foo = shanghai.data,
      predictors = c("density", "Meantem2020", "prop.elder", "pergdp", "pc1"),
      predictors.op = "mean",
      dependent = "per.confirm",
      unit.variable = "unit.num",
      time.variable = "date",
      special.predictors = NULL,
      treatment.identifier = i,
      controls.identifier = index[-i],
      time.predictors.prior = c(as.Date("2020-01-20"):as.Date("2020-01-23")),
      time.optimize.ssr =  c(as.Date("2020-01-20"):as.Date("2020-01-23")),
      unit.names.variable = "ctname",
      time.plot =  c(as.Date("2020-01-20"):as.Date("2020-02-05"))
    )
  synth.out <- synth(dataprep.out)
  placeboline.shanghai <- c(placeboline.shanghai,list(dataprep.out$Y1plot - dataprep.out$Y0plot %*% synth.out$solution.w))
}
placebo.shanghai <- as.data.frame(placeboline.shanghai)
placebo.shanghai2<-gather(placebo.shanghai,area,per.confirm)
placebo.shanghai2$date = rep(as.Date(c(18281:18297),origin = "1970-01-01"),index.shanghai)
placebo.shanghai2$group = factor(c(rep("Shanghai",17),rep("Control Regions",(index.shanghai-1)*17)),
                                 levels = c("Shanghai", "Control Regions"))

placebo.shanghai2 <- placebo.shanghai2[placebo.shanghai2$date != "2020-02-05", ]
ggplot(data = placebo.shanghai2,mapping=aes(x = date,y = per.confirm, group = area))+
  geom_line(aes(color = group, alpha = group, size = group))+
  scale_x_date(date_labels ="%m/%d",date_breaks = "2 days")+
  theme_bw(base_family = "Times") +
  theme(panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top",
        panel.border = element_blank(),
        text = element_text(family = "Times"),
        plot.title = element_text(hjust = 0.5))+
  scale_colour_discrete(name  ="",
                        breaks=c("Shanghai", "Control Regions"),
                        labels=c("Shanghai", "Control Regions")) +
  scale_size_discrete(name  ="",
                      breaks=c("Shanghai", "Control Regions"),
                      labels=c("Shanghai", "Control Regions")) +
  scale_colour_manual(values = c(pal_jco()(7)[1], pal_jco()(7)[3]))+
  scale_alpha_manual(values = c(1, 0.3), guide = FALSE)+
  scale_size_manual(values = c(1, 0.5), guide = FALSE)+
  labs(x = "Date", y = "Gap in the postive number of COVID-19 per 100,000", 
       title = NULL,
       color = "")

ggsave("shanghai0830.pdf")



# ------------------------- Combine 2 cities ----------------------------
line.shanghai[line.shanghai$index == "Shanghai", "index"] <- "Observed"
line.shanghai$city <- "Shanghai"
line.wenzhou$index <- as.character(line.wenzhou$index)
line.wenzhou[line.wenzhou$index == "Wenzhou", "index"] <- "Observed"
line.wenzhou$city <- "Wenzhou"

placebo.shanghai2$group <- as.character(placebo.shanghai2$group)
placebo.shanghai2[placebo.shanghai2$group == "Treated Region", "group"] <- "Treated Region"

placebo.shanghai2$city <- "Shanghai"
placebo.wenzhou2$group <- as.character(placebo.wenzhou2$group)
placebo.wenzhou2[placebo.wenzhou2$group == "wenzhou", "group"] <- "Treated Region"
placebo.wenzhou2$city <- "Wenzhou"

line_2city <- rbind(line.shanghai, line.wenzhou)
placebo_2city <- rbind(placebo.shanghai2, placebo.wenzhou2)

line_2city$city <- factor(line_2city$city, levels = c("Wenzhou", "Shanghai"))
placebo_2city$city <- factor(placebo_2city$city, levels = c("Wenzhou", "Shanghai"))

placebo_2city$group <- factor(placebo_2city$group, levels = c("Treated Region", "Control Regions"))

line_2city$policy_date <- as.Date("2020-01-24")
line_2city[line_2city$city == "Wenzhou", "policy_date"] <- as.Date("2020-02-01")
# plot
ggplot(data = line_2city, mapping = aes(x = date,y = per.confirm, group = index)) +
  geom_line(aes(color = index), size = 1) +
  geom_vline(aes(xintercept = policy_date), linetype = "dashed") +
  scale_x_date(date_labels = "%m/%d", date_breaks = "3 days") +
  theme_bw(base_family = "Times") +
  theme(panel.grid.minor = element_blank(),
        legend.position = "top",
        panel.border = element_blank(),
        text = element_text(size = 16),
        strip.background = element_rect(color = "white"),
        plot.title = element_text(hjust = 0.5)) +
  scale_color_jco() +
  facet_wrap(~ city, scales = "free_x") +
  labs(x = "Date", y = "Postive individuals per 100,000", 
       title = NULL,
       color = "")
ggsave("SCM_SH_WZ.pdf", width = 9, height = 4.5)



ggplot(data = placebo_2city, mapping=aes(x = date,y = per.confirm, group = area))+
  geom_line(aes(color = group, alpha = group, size = group)) +
  scale_x_date(date_labels ="%m/%d",date_breaks = "3 days") +
  theme_bw(base_family = "Times") +
  theme(panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top",
        panel.border = element_blank(),
        strip.background = element_rect(color = "white"),
        # text = element_text(family = "Times"),
        text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5)) +
  scale_colour_discrete(name  ="",
                        breaks=c("Shanghai", "Control Regions"),
                        labels=c("Shanghai", "Control Regions")) +
  scale_size_discrete(name  ="",
                      breaks=c("Shanghai", "Control Regions"),
                      labels=c("Shanghai", "Control Regions")) +
  scale_colour_manual(values = c(pal_jco()(7)[1], pal_jco()(7)[3]))+
  scale_alpha_manual(values = c(1, 0.3), guide = FALSE)+
  scale_size_manual(values = c(1, 0.5), guide = FALSE)+
  facet_wrap(~ city, scales = "free_x") +
  labs(x = "Date", y = "Gap in the postive individuals per 100,000", 
       title = NULL,
       color = "")

ggsave("SCM_test_SH_WZ.pdf", width = 9, height = 4.5)

save(line_2city, placebo_2city, file = "SCM_2city_plot.rda")

