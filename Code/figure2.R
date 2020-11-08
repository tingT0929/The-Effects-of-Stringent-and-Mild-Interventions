
############# Wenzhou ##############
# Extract data
line.wenzhou <- data.frame(per.confirm = c(as.numeric(dataprep.out.wenzhou$Y1plot), 
                                           as.numeric(dataprep.out.wenzhou$Y0plot %*% synth.out.wenzhou$solution.w)), 
                           date = rep(as.Date(c(18282:18297), origin = "1970-01-01"), 2),
                           index = rep(c("Wenzhou", "Synthetic"), each = 16))
# trend
trend.wenzhou <- spread(line.wenzhou, index, per.confirm)
trend.wenzhou
trend.wenzhou$Synthetic[16] / trend.wenzhou$Wenzhou[16]
trend.wenzhou$Synthetic <- trend.wenzhou$Synthetic * wenzhou.data$Population[1] / 1e+05
trend.wenzhou$Wenzhou <- trend.wenzhou$Wenzhou * wenzhou.data$Population[1] / 1e+05
trend.wenzhou

# placebo test
placeboline.wenzhou <- {}
index = 1:index.wenzhou
for (i in index) {
  dataprep.out<-
    dataprep(
      foo = wenzhou.data,
      predictors = c("density", "Meantem2020", "prop.elder", "pergdp", "pc1","pc2","pc3","pc4"),
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
      time.plot =  c(as.Date("2020-01-21"):as.Date("2020-02-05"))
    )
  synth.out <- synth(dataprep.out, optimxmethod = c("Nelder-Mead", "BFGS", "L-BFGS-B"))
  placeboline.wenzhou <- c(placeboline.wenzhou, list(dataprep.out$Y1plot - 
                                                       dataprep.out$Y0plot %*% synth.out$solution.w))
}

placebo.wenzhou <- as.data.frame(placeboline.wenzhou)
index.placebo.wenzhou <- ncol(placebo.wenzhou)
index.placebo.wenzhou
placebo.wenzhou2 <- gather(placebo.wenzhou, area, per.confirm)
placebo.wenzhou2$date <- rep(as.Date(c(18282:18297), origin = "1970-01-01"), index.placebo.wenzhou)

# Discards counties with MSE thousand times higher than Wenzhou’s
befpolicy <- placebo.wenzhou2[placebo.wenzhou2$date %in% as.Date(c(18282:18292), origin = "1970-01-01"), ]
befpolicy <- spread(befpolicy[, 1:3], area, per.confirm)
mse.wenzhou <- apply(befpolicy[, 2:(index.placebo.wenzhou + 1)]^2, 2, mean) # Calculate MSE
thstimes.wenzhou <- names(mse.wenzhou[mse.wenzhou > mse.wenzhou["X1"] * 1000]) # Get the corresponding index
thstimes.wenzhou
placebo.wenzhou2 <- placebo.wenzhou2[!placebo.wenzhou2$area %in% thstimes.wenzhou, ]
placebo.wenzhou2$group <- factor(c(rep("wenzhou", 16), rep("Control Regions", 
                                                           (index.placebo.wenzhou - 1 - length(thstimes.wenzhou))*16)),
                                 levels = c("wenzhou", "Control Regions"))


############# Shanghai ##############
# Extract data
line.shanghai <- data.frame(per.confirm = c(as.numeric(dataprep.out.shanghai$Y1plot), 
                                            as.numeric(dataprep.out.shanghai$Y0plot %*% synth.out.shanghai$solution.w)), 
                            date = rep(as.Date(c(18281:18296), origin = "1970-01-01"), 2),
                            index = rep(c("Shanghai", "Synthetic"), each = 16))

# trend
trend.shanghai <- spread(line.shanghai, index,per.confirm)
trend.shanghai
trend.shanghai$Synthetic[16] / trend.shanghai$Shanghai[16]
trend.shanghai$Synthetic <- trend.shanghai$Synthetic * shanghai.data$Population[1] / 1e05
trend.shanghai$Shanghai <- trend.shanghai$Shanghai * shanghai.data$Population[1] / 1e05
trend.shanghai

# placebo test
placeboline.shanghai <- {}
index = 1:index.shanghai
for (i in index) {
  dataprep.out<-
    dataprep(
      foo = shanghai.data,
      predictors = c("density", "Meantem2020", "prop.elder", "pergdp", "pc1","pc2"),
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
      time.plot =  c(as.Date("2020-01-20"):as.Date("2020-02-04"))
    )
  synth.out <- synth(dataprep.out, optimxmethod = c("Nelder-Mead", "BFGS", "L-BFGS-B"))
  placeboline.shanghai <- c(placeboline.shanghai,
                            list(dataprep.out$Y1plot - dataprep.out$Y0plot %*% synth.out$solution.w))
}
placebo.shanghai <- as.data.frame(placeboline.shanghai)
index.placebo.shanghai <- ncol(placebo.shanghai)

placebo.shanghai2 <- gather(placebo.shanghai, area, per.confirm)
placebo.shanghai2$date <- rep(as.Date(c(18281:18296), origin = "1970-01-01"), index.placebo.shanghai)
# Discards counties with MSE ten thousand times higher than Shanghai’s
befpolicy <- placebo.shanghai2[placebo.shanghai2$date %in% as.Date(c(18281:18284), origin = "1970-01-01"), ]
befpolicy <- spread(befpolicy[, 1:3], area, per.confirm)
befpolicy
mse.shanghai <- apply(befpolicy[, 2:(index.placebo.shanghai + 1)]^2, 2, mean) # Calculate MSE
thstimes.shanghai <- names(mse.shanghai[mse.shanghai > mse.shanghai["X1"] * 10000]) # Get the corresponding index
thstimes.shanghai
placebo.shanghai2 <- placebo.shanghai2[!placebo.shanghai2$area %in% thstimes.shanghai, ]
placebo.shanghai2$group <- factor(c(rep("shanghai",16), rep("Control Regions", 
                                                            (index.placebo.shanghai-1-length(thstimes.shanghai))*16)),
                                  levels = c("shanghai", "Control Regions"))


# ------------------------- Combine 2 cities ----------------------------
line.shanghai[line.shanghai$index == "Shanghai", "index"] <- "Observed"
line.shanghai$city <- "Shanghai"
line.wenzhou$index <- as.character(line.wenzhou$index)
line.wenzhou[line.wenzhou$index == "Wenzhou", "index"] <- "Observed"
line.wenzhou$city <- "Wenzhou"

placebo.shanghai2$group <- as.character(placebo.shanghai2$group)
placebo.shanghai2[placebo.shanghai2$group == "shanghai", "group"] <- "Treated Region"
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


# -------------------------------- Plot -----------------------------------
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

