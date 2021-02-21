#### ------------------- Figure D.1 ----------------------

RDplott_wz <- rdplot(log(wenzhou.rd$ctcomfirm), wenzhou.rd$date, c = 0, p = 2, binselect = "esmv", 
    kernel = "uniform", x.label = "date", y.label = "Cumulative confirmed cases")

RDplott_sh <- rdplot(log(shanghai.rd$ctcomfirm), shanghai.rd$date, c = 0, p = 2, binselect = "esmv", 
    kernel = "uniform", x.label = "date", y.label = "Cumulative confirmed cases")
len_xy <- length(RDplott_sh$vars_poly$rdplot_x)
x_plot_r_sh2 <- RDplott_sh$vars_poly$rdplot_x[(1 + len_xy/2):len_xy]
y_hat_r_sh2 <- RDplott_sh$vars_poly$rdplot_y[(1 + len_xy/2):len_xy]

RDplott_sh <- rdplot(log(shanghai.rd$ctcomfirm), shanghai.rd$date, c = 0, p = 1, binselect = "esmv", 
    kernel = "uniform", x.label = "date", y.label = "Cumulative confirmed cases")
# ----------

vars_bins <- RDplott_wz$vars_bins

rdplot_mean_bin <- vars_bins$rdplot_mean_bin
rdplot_mean_y <- vars_bins$rdplot_mean_y
rdplot_cil_bin <- vars_bins$rdplot_ci_l
rdplot_cir_bin <- vars_bins$rdplot_ci_r

len_xy <- length(RDplott_wz$vars_poly$rdplot_x)
x_plot_l <- RDplott_wz$vars_poly$rdplot_x[1:(len_xy/2)]
x_plot_r <- RDplott_wz$vars_poly$rdplot_x[(1 + len_xy/2):len_xy]
y_hat_l <- RDplott_wz$vars_poly$rdplot_y[1:(len_xy/2)]
y_hat_r <- RDplott_wz$vars_poly$rdplot_y[(1 + len_xy/2):len_xy]

data_bins_wz <- data.frame(rdplot_mean_bin, rdplot_mean_y, rdplot_cil_bin, rdplot_cir_bin, city = "Wenzhou")
data_poly_wz <- data.frame(x_plot_l, y_hat_l, x_plot_r, y_hat_r, city = "Wenzhou")

# ----- Shanghai
vars_bins <- RDplott_sh$vars_bins

rdplot_mean_bin <- vars_bins$rdplot_mean_bin
rdplot_mean_y <- vars_bins$rdplot_mean_y
rdplot_cil_bin <- vars_bins$rdplot_ci_l
rdplot_cir_bin <- vars_bins$rdplot_ci_r

len_xy <- length(RDplott_sh$vars_poly$rdplot_x)
x_plot_l <- RDplott_sh$vars_poly$rdplot_x[1:(len_xy/2)]
y_hat_l <- RDplott_sh$vars_poly$rdplot_y[1:(len_xy/2)]

data_bins_sh <- data.frame(rdplot_mean_bin, rdplot_mean_y, rdplot_cil_bin, rdplot_cir_bin, city = "Shanghai")
data_poly_sh <- data.frame(x_plot_l, y_hat_l, x_plot_r = x_plot_r_sh2, y_hat_r = y_hat_r_sh2, city = "Shanghai")


data_bins <- rbind(data_bins_wz, data_bins_sh)
data_poly <- rbind(data_poly_wz, data_poly_sh)

data_bins$city <- factor(data_bins$city, levels = c("Wenzhou", "Shanghai"))
data_poly$city <- factor(data_poly$city, levels = c("Wenzhou", "Shanghai"))

# save(data_bins, data_poly, file = 'RD_2city_plot.rda') load('RD_2city_plot.rda')

ggplot() + theme_bw(base_family = "Times") + 
    geom_line(data = data_poly, aes(x = x_plot_l, y = y_hat_l, 
                                    col = "2"), size = 1, na.rm = TRUE) + 
    geom_line(data = data_poly, aes(x = x_plot_r, y = y_hat_r, 
                                    col = "2"), size = 1, na.rm = TRUE) + 
    geom_point(data = data_bins, aes(x = rdplot_mean_bin, y = rdplot_mean_y, 
                                     col = "1"), size = 2, na.rm = TRUE) + 
    labs(x = "Median of rate variable (time) within each bin", 
    y = "Mean of log (total positive individuals) within each bin") + 
    scale_color_jco() + 
    theme(legend.position = "None", panel.border = element_blank(), 
          text = element_text(size = 16), 
          strip.background = element_rect(color = "white")) + 
    facet_wrap(~city, scales = "free_x") + 
    geom_vline(xintercept = 0, size = 1, linetype = 2)

ggsave(filename = "RD2_border.pdf", width = 10, height = 5.5)
