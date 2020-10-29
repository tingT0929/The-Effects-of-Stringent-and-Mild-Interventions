library(Synth)  # version 1.1-5
library(ggplot2)  # version 3.3.2
library(dplyr)  # version 1.0.2
library(reshape2)  # version 1.4.4
library(tidyr)  # version 1.1.2
library(rdrobust)  # version 0.99.8
library(stargazer)  # version 5.2.2

setwd("D:\\Documents\\GitHub\\intervention-effects\\Data")

# read data
wenzhou.data <- read.csv("wzdat1014.csv", header = T, fileEncoding = "GBK")
index.wenzhou <- length(unique(wenzhou.data$ctname))
wenzhou.data$unit.num <- rep(1:index.wenzhou, each = 29)
wenzhou.data$ctname <- as.character(wenzhou.data$ctname)
wenzhou.data$date <- rep(as.numeric(as.Date(wenzhou.data$fulldate)[1:29]), times = index.wenzhou)
wenzhou.data$Population <- as.numeric(wenzhou.data$Population)
wenzhou.data$per.confirm <- wenzhou.data$confirmed/wenzhou.data$Population * 1e+05
index.wenzhou
#### ------------------- Figure 1 ----------------------
index <- 1:index.wenzhou
# Prepare data
dataprep.out.wenzhou <- dataprep(foo = wenzhou.data, predictors = c("density", "Meantem2020", "prop.elder", 
    "pergdp", "pc1", "pc2", "pc3", "pc4"), predictors.op = "mean", dependent = "per.confirm", unit.variable = "unit.num", 
    time.variable = "date", special.predictors = NULL, treatment.identifier = 1, controls.identifier = index[-1], 
    time.predictors.prior = c(as.Date("2020-01-21"):as.Date("2020-01-31")), time.optimize.ssr = c(as.Date("2020-01-21"):as.Date("2020-01-31")), 
    unit.names.variable = "ctname", time.plot = c(as.Date("2020-01-21"):as.Date("2020-02-05")))

# Synth
synth.out.wenzhou <- synth(dataprep.out.wenzhou, optimxmethod = c("Nelder-Mead", "BFGS", "L-BFGS-B"))

# Extract data
line.wenzhou <- data.frame(per.confirm = c(as.numeric(dataprep.out.wenzhou$Y1plot), as.numeric(dataprep.out.wenzhou$Y0plot %*% 
    synth.out.wenzhou$solution.w)), date = rep(as.Date(c(18282:18297), origin = "1970-01-01"), 2), 
    index = rep(c("Wenzhou", "Synthetic"), each = 16))
# Output weight and variables
synth.tables.wenzhou <- synth.tab(dataprep.res = dataprep.out.wenzhou, synth.res = synth.out.wenzhou)
print(synth.tables.wenzhou$tab.w[order(synth.tables.wenzhou$tab.w$w.weights), ])
write.csv(synth.tables.wenzhou$tab.w[order(synth.tables.wenzhou$tab.w$w.weights), ], "wenzhou1007.csv")
synth.tables.wenzhou$tab.pred
write.csv(synth.tables.wenzhou$tab.pred, "wenzhoupred1007.csv")
write.csv(wenzhou.data[!duplicated(wenzhou.data$ctname), ], "wenzhouvariable.csv")

# trend
trend.wenzhou <- spread(line.wenzhou, index, per.confirm)
trend.wenzhou$Synthetic <- trend.wenzhou$Synthetic * wenzhou.data$Population[1]/1e+05
trend.wenzhou$Wenzhou <- trend.wenzhou$Wenzhou * wenzhou.data$Population[1]/1e+05
trend.wenzhou

write.csv(trend.wenzhou, "trend.wenzhou.csv", fileEncoding = "GBK")

# plot
ggplot(data = line.wenzhou, mapping = aes(x = date, y = per.confirm, group = index)) + 
    geom_line(aes(color = index), size = 1) + 
    geom_vline(aes(xintercept = as.Date("2020-02-01")), linetype = "dashed") + 
    scale_x_date(date_labels = "%m/%d", date_breaks = "3 days") + 
    theme_bw(base_family = "Times") + 
    theme(panel.grid.minor = element_blank(), 
    legend.position = "top", panel.border = element_blank(), text = element_text(family = "Times-Roman"), 
    plot.title = element_text(hjust = 0.5)) + 
    labs(x = "Date", y = "COVID-19 cases (per 100,000)", title = NULL, color = "")

# Placebo test
index <- 1:index.wenzhou
for (i in index) {
    dataprep.out <- dataprep(foo = wenzhou.data, predictors = c("density", "Meantem2020", "prop.elder", 
        "pergdp", "pc1", "pc2", "pc3", "pc4"), predictors.op = "mean", dependent = "per.confirm", 
        unit.variable = "unit.num", time.variable = "date", special.predictors = NULL, treatment.identifier = i, 
        controls.identifier = index[-i], time.predictors.prior = c(as.Date("2020-01-21"):as.Date("2020-01-31")), 
        time.optimize.ssr = c(as.Date("2020-01-21"):as.Date("2020-01-31")), unit.names.variable = "ctname", 
        time.plot = c(as.Date("2020-01-21"):as.Date("2020-02-05")))
    synth.out <- synth(dataprep.out, optimxmethod = c("Nelder-Mead", "BFGS", "L-BFGS-B"))
    placeboline.wenzhou <- c(placeboline.wenzhou, list(dataprep.out$Y1plot - dataprep.out$Y0plot %*% 
        synth.out$solution.w))
}

placebo.wenzhou <- as.data.frame(placeboline.wenzhou)
index.placebo.wenzhou <- ncol(placebo.wenzhou)
index.placebo.wenzhou
placebo.wenzhou2 <- gather(placebo.wenzhou, area, per.confirm)
placebo.wenzhou2$date <- rep(as.Date(c(18282:18297), origin = "1970-01-01"), index.placebo.wenzhou)

# Discards counties with MSE thousand times higher than Wenzhou’s
befpolicy <- placebo.wenzhou2[placebo.wenzhou2$date %in% as.Date(c(18282:18292), origin = "1970-01-01"), 
    ]
befpolicy <- spread(befpolicy[, 1:3], area, per.confirm)
mse.wenzhou <- apply(befpolicy[, 2:(index.placebo.wenzhou + 1)]^2, 2, mean)  # Calculate MSE
thstimes.wenzhou <- names(mse.wenzhou[mse.wenzhou > mse.wenzhou["X1"] * 1000])  # Get the corresponding index
thstimes.wenzhou
placebo.wenzhou2 <- placebo.wenzhou2[!placebo.wenzhou2$area %in% thstimes.wenzhou, ]
placebo.wenzhou2$group <- factor(c(rep("wenzhou", 16), rep("Control Regions", (index.placebo.wenzhou - 
    1 - length(thstimes.wenzhou)) * 16)), levels = c("wenzhou", "Control Regions"))

ggplot(data = placebo.wenzhou2, mapping = aes(x = date, y = per.confirm, group = area)) + 
    geom_line(aes(color = group)) + 
    scale_x_date(date_labels = "%m/%d", date_breaks = "3 days") + theme_bw(base_family = "Times") + 
    theme(panel.grid.minor = element_blank(), 
          panel.grid = element_blank(), 
          legend.position = "top", 
          panel.border = element_blank(), 
          text = element_text(family = "Times"), 
          plot.title = element_text(hjust = 0.5)) +
    scale_colour_discrete(name = "", breaks = c("wenzhou", "Control Regions"), 
                          labels = c("wenzhou", "Control Regions")) + 
    scale_size_discrete(name = "", breaks = c("wenzhou", "Control Regions"), 
                        labels = c("wenzhou", "Control Regions")) + 
    scale_colour_manual(values = c("#00BFC4", "grey")) + 
    scale_alpha_manual(values = c(1, 0.3), guide = FALSE) +
    scale_size_manual(values = c(1, 0.5), guide = FALSE) + 
    labs(x = "Date", y = "Gap in COVID-19 cases (per 100,000)", title = NULL, color = "")

ggsave("wenzhou0830.pdf")



#### shanghai#########

# read data
shanghai.data <- read.csv("shdat1014.csv", header = T, fileEncoding = "GBK")
index.shanghai <- length(unique(shanghai.data$ctname))
shanghai.data$unit.num <- rep(1:index.shanghai, each = 29)
shanghai.data$ctname <- as.character(shanghai.data$ctname)
shanghai.data$date <- rep(as.numeric(as.Date(shanghai.data$fulldate)[1:29]), times = index.shanghai)
shanghai.data$Population <- as.numeric(shanghai.data$Population)
shanghai.data$per.confirm <- shanghai.data$confirmed / shanghai.data$Population * 1e+05

#### ------------------- Figure 1 ----------------------
index <- 1:index.shanghai
index.shanghai
dataprep.out.shanghai <- dataprep(foo = shanghai.data, predictors = c("density", "Meantem2020", "prop.elder", 
    "pergdp", "pc1", "pc2"), predictors.op = "mean", dependent = "per.confirm", unit.variable = "unit.num", 
    time.variable = "date", special.predictors = NULL, treatment.identifier = 1, controls.identifier = index[-1], 
    time.predictors.prior = c(as.Date("2020-01-20"):as.Date("2020-01-23")), time.optimize.ssr = c(as.Date("2020-01-20"):as.Date("2020-01-23")), 
    unit.names.variable = "ctname", time.plot = c(as.Date("2020-01-20"):as.Date("2020-02-04")))

# Synth
synth.out.shanghai <- synth(dataprep.out.shanghai, optimxmethod = c("Nelder-Mead", "BFGS", "L-BFGS-B"))

# Extract data
line.shanghai <- data.frame(per.confirm = c(as.numeric(dataprep.out.shanghai$Y1plot), as.numeric(dataprep.out.shanghai$Y0plot %*% 
    synth.out.shanghai$solution.w)), date = rep(as.Date(c(18281:18296), origin = "1970-01-01"), 2), 
    index = rep(c("Shanghai", "Synthetic"), each = 16))

# Output weight and variables
synth.tables.shanghai <- synth.tab(dataprep.res = dataprep.out.shanghai, synth.res = synth.out.shanghai)
print(synth.tables.shanghai$tab.w[order(synth.tables.shanghai$tab.w$w.weights), ])
write.csv(synth.tables.shanghai$tab.w[order(synth.tables.shanghai$tab.w$w.weights), ], "shanghai1007.csv")
synth.tables.shanghai$tab.pred
write.csv(synth.tables.shanghai$tab.pred, "shanghaipred1007.csv")

# trend
trend.shanghai <- spread(line.shanghai, index, per.confirm)
trend.shanghai$Synthetic <- trend.shanghai$Synthetic * shanghai.data$Population[1] / 1e+05
trend.shanghai$Shanghai <- trend.shanghai$Shanghai * shanghai.data$Population[1] / 1e+05
trend.shanghai
write.csv(trend.shanghai, "trend.shanghai.csv", fileEncoding = "GBK")
write.csv(shanghai.data[!duplicated(shanghai.data$ctname), ], "shanghaivariable.csv")

# plot
ggplot(data = line.shanghai, mapping = aes(x = date, y = per.confirm, group = index)) + 
    geom_line(aes(color = index), size = 1) + 
    geom_vline(aes(xintercept = as.Date("2020-01-24")), linetype = "dashed") + 
    scale_x_date(date_labels = "%m/%d", date_breaks = "3 days") + 
    theme_bw(base_family = "Times") +
    theme(panel.grid.minor = element_blank(), 
          legend.position = "top", 
          panel.border = element_blank(), 
          text = element_text(family = "Times-Roman"), 
          plot.title = element_text(hjust = 0.5)) +
    labs(x = "Date", y = "COVID-19 cases (per 100,000)", title = NULL, color = "")

# Placebo test
index <- 1:index.shanghai
for (i in index) {
    dataprep.out <- dataprep(foo = shanghai.data, predictors = c("density", "Meantem2020", "prop.elder", 
        "pergdp", "pc1", "pc2"), predictors.op = "mean", dependent = "per.confirm", unit.variable = "unit.num", 
        time.variable = "date", special.predictors = NULL, treatment.identifier = i, controls.identifier = index[-i], 
        time.predictors.prior = c(as.Date("2020-01-20"):as.Date("2020-01-23")), time.optimize.ssr = c(as.Date("2020-01-20"):as.Date("2020-01-23")), 
        unit.names.variable = "ctname", time.plot = c(as.Date("2020-01-20"):as.Date("2020-02-04")))
    synth.out <- synth(dataprep.out, optimxmethod = c("Nelder-Mead", "BFGS", "L-BFGS-B"))
    # synth.out <- synth(dataprep.out,custom.v = as.numeric(synth.out.shanghai$solution.v))
    placeboline.shanghai <- c(placeboline.shanghai, list(dataprep.out$Y1plot - dataprep.out$Y0plot %*% 
        synth.out$solution.w))
}
placebo.shanghai <- as.data.frame(placeboline.shanghai)
index.placebo.shanghai <- ncol(placebo.shanghai)

placebo.shanghai2 <- gather(placebo.shanghai, area, per.confirm)
placebo.shanghai2$date <- rep(as.Date(c(18281:18296), origin = "1970-01-01"), index.placebo.shanghai)

# Discards counties with MSE thousand times higher than Shanghai’s
befpolicy <- placebo.shanghai2[placebo.shanghai2$date %in% as.Date(c(18281:18284), origin = "1970-01-01"), 
    ]
befpolicy <- spread(befpolicy[, 1:3], area, per.confirm)
befpolicy
mse.shanghai <- apply(befpolicy[, 2:(index.placebo.shanghai + 1)]^2, 2, mean)  # Calculate MSE
thstimes.shanghai <- names(mse.shanghai[mse.shanghai > mse.shanghai["X1"] * 1000])  # Get the corresponding index
thstimes.shanghai

placebo.shanghai2 <- placebo.shanghai2[!placebo.shanghai2$area %in% thstimes.shanghai, ]
placebo.shanghai2$group <- factor(c(rep("shanghai", 16), 
                                    rep("Control Regions", (index.placebo.shanghai - 1 - length(thstimes.shanghai)) * 16)), 
                                  levels = c("shanghai", "Control Regions"))

ggplot(data = placebo.shanghai2, mapping = aes(x = date, y = per.confirm, group = area)) + 
    geom_line(aes(color = group, alpha = group, size = group)) 
scale_x_date(date_labels = "%m/%d", date_breaks = "3 days") + 
    theme_bw(base_family = "Times") + 
    theme(panel.grid.minor = element_blank(), 
          panel.grid = element_blank(), 
          legend.position = "top", 
          panel.border = element_blank(),
          text = element_text(family = "Times"), 
          plot.title = element_text(hjust = 0.5)) +
    annotate(geom = "text", label = "02 / 03", x = as.Date("2020-02-03"), y = 4, hjust = -0.35, vjust = -1.3) + 
    scale_colour_discrete(name = "", breaks = c("shanghai", "Control Regions"), 
                          labels = c("shanghai", "Control Regions")) + 
    scale_size_discrete(name = "", breaks = c("shanghai", "Control Regions"), 
    labels = c("shanghai", "Control Regions")) + 
    scale_colour_manual(values = c("#00BFC4", "grey")) + 
    scale_alpha_manual(values = c(1, 0.3), guide = FALSE) + 
    scale_size_manual(values = c(1, 0.5), 
    guide = FALSE) + 
    labs(x = "Date", y = "Gap in COVID-19 cases (per 100,000)", title = NULL, color = "")

ggsave("shanghai1007.pdf")
