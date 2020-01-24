#RHP pretreatment QQplot

shapiro.test(RHP_)

RHP_pt_slope <- diff(quantile(RHP_control_values$RHP_pretreatment, c(0.25, 0.75), na.rm = T)) / 
  diff(qnorm(c(0.25, 0.75)))

RHP_pt_intercept <- quantile(RHP_control_values$RHP_pretreatment, 0.25, na.rm = T) - RHP_pt_slope * qnorm(0.25)

ggplot(RHP_control_values, aes(sample = RHP_pretreatment)) +
  stat_qq(pch = 1, colour = "black",  fill = "black", alpha = 0.35, size = 4) +
  stat_qq(pch = 19, colour = "black",  fill = "black", alpha = 0.15, size = 4) +
  geom_abline(aes(slope = RHP_pt_slope, intercept = RHP_pt_intercept), size = 1, col = "red", alpha = 0.5, lty = 1) +
  labs(y = "Latency (s)", x = "Theoretical distribution",
       title = "QQplot: Ramped hot plate", 
       subtitle = "Pre-treatment values; n = 387")  +
  scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 1)) +
  theme_bw() +
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))

#RHP pretreamtent Density plot

ggplot(RHP_control_values, aes(x = RHP_pretreatment)) +
  geom_histogram(aes(y = ..density..), fill = "orangered4", colour = "black", alpha = 0.5, bins = 20) +
  geom_density(colour = "black",  alpha = 0.75, size = 1, bw = 4) +
  scale_x_continuous() +
  labs(x = "Latency (s)", y = "Density", 
       title = "Density plot: Ramped hot plate",
       subtitle = "Pre-treatment values; n = 387") +
  theme_bw() +
  theme(legend.title = element_blank(), 
        text = element_text(size = 14), axis.text = element_text(size = 14)) 

#SHP vehicle QQplot

SHP_vehicle <- qplot_controls %>% 
  filter(Method == "CTL-vehicle (intravenous)" | 
           Method == "CTL-vehicle (intravenous time-course)" | 
             Method == "Untreated and Air treatment")  %>%
  filter(!is.na(hot_plate)) %>%
  dplyr::select(hot_plate, Method)
SHP_vehicle$hot_plate <- as.numeric(SHP_vehicle$hot_plate)


ggplot(SHP_vehicle, aes(x = Method, y = hot_plate)) +
  geom_point() +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", colour = "black")

SHPslope <- diff(quantile(SHP_vehicle$hot_plate, c(0.25, 0.75), na.rm = T)) / 
  diff(qnorm(c(0.25, 0.75)))

SHPintercept <- quantile(SHP_vehicle$hot_plate, 0.25, na.rm = T) - SHPslope * qnorm(0.25)

SHP_q <- ggplot(SHP_vehicle, aes(sample = hot_plate)) +
  stat_qq(pch = 1, colour = "black",  fill = "black", alpha = 0.35, size = 4) +
  stat_qq(pch = 19, colour = "black",  fill = "black", alpha = 0.15, size = 4) +
  geom_abline(aes(slope = SHPslope, intercept = SHPintercept), col = "red", alpha = 0.5, lty = 1) +
  scale_y_continuous(breaks = seq(0, 80, 10)) +
  scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 1)) +
  labs(y = "Latency (s)", x = "Theoretical distribution",
       title = "QQplot: Standard hot plate",
       subtitle = "Normal distribution; n = 215") +
  theme_bw() +
  theme(text = element_text(size = 14), axis.text = element_text(size = 14, colour = "black"))

#SHP control population density plot

SHP_d <- ggplot(SHP_vehicle, aes(x = hot_plate)) +
  geom_histogram(aes(y = ..density..), fill = "cadetblue4", colour = "black", alpha = 0.5, bins = 14) +
  geom_density(colour = "black",  alpha = 0.2, fill = "black", bw = 5) +
  scale_x_continuous() +
  labs(x = "Latency (s)", y = "Density", 
       title = "Density plot: Standard hot plate",
       subtitle = "Normal distribution; n = 215") +
  theme_bw() +
  theme(legend.title = element_blank(), 
        text = element_text(size = 14), axis.text = element_text(size = 14)) 

#SHP untreated QQplot

SHP_untreated <- qplot_controls %>% 
  filter(Method == "Untreated and Air treatment") %>%
  filter(!is.na(hot_plate)) %>%
  dplyr::select(hot_plate)

SHPslope_untreated <- diff(quantile(SHP_untreated$hot_plate, c(0.25, 0.75), na.rm = T)) / 
  diff(qnorm(c(0.25, 0.75)))

SHPintercept_untreated <- quantile(SHP_untreated$hot_plate, 0.25, na.rm = T) - SHPslope_untreated * qnorm(0.25)

ggplot(SHP_untreated, aes(sample = hot_plate)) +
  stat_qq(pch = 1, colour = "black",  fill = "black", alpha = 0.35, size = 4) +
  stat_qq(pch = 19, colour = "black",  fill = "black", alpha = 0.15, size = 4) +
  geom_abline(aes(slope = SHPslope_untreated, intercept = SHPintercept_untreated), col = "red", alpha = 0.5, lty = 1) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 80, 10)) +
  scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 1)) +
  labs(y = "Latency (s)", x = "Theoretical distribution",
       title = "QQplot: Standard hot plate",
       subtitle = "Pre-treatment; n = 84") +
  theme_bw() +
  theme(text = element_text(size = 14), axis.text = element_text(size = 14, colour = "black"))

#SHP pre-treatment density plot

ggplot(SHP_untreated, aes(x = hot_plate)) +
  geom_histogram(aes(y = ..density..), fill = "cadetblue4", colour = "black", alpha = 0.5, bins = 15) +
  geom_density(colour = "black", alpha = 0.75, size = 1, bw = 5) +
  scale_x_continuous(breaks = seq(0, 70, 10)) +
  labs(x = "Latency (s)", y = "Density",
       title = "Density plot: Standard hot plate",
       subtitle = "Pre-treatment; n = 84") +
  theme_bw() +
  theme(legend.title = element_blank(), 
        text = element_text(size = 14), axis.text = element_text(size = 14)) 

#SHP log transform untreated

logSHP_untreated <- log_qplot_controls %>% 
  filter(Method == "Untreated and Air treatment") %>%
  filter(!is.na(hot_plate)) %>%
  dplyr::select(hot_plate)

logHPslope_untreated <- diff(quantile(logSHP_untreated$hot_plate, c(0.25, 0.75), na.rm = T)) / 
  diff(qnorm(c(0.25, 0.75)))

logHPintercept_untreated <- quantile(logSHP_untreated$hot_plate, 0.25, na.rm = T) - logHPslope_untreated * qnorm(0.25)

ggplot(logSHP_untreated, aes(sample = hot_plate)) +
  stat_qq(pch = 1, colour = "black",  fill = "black", alpha = 0.35, size = 4) +
  stat_qq(pch = 19, colour = "black",  fill = "black", alpha = 0.15, size = 4) +
  geom_abline(aes(slope = logHPslope_untreated, intercept = logHPintercept_untreated), 
              size = 1, col = "red", alpha = 0.5, lty = 1) +
  scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 1)) +
  labs(y = "Log Latency (s)", x = "Theoretical distribution",
       title = "QQplot: Standard hot plate",
       subtitle = "Pre-treatment logarithmic transform; n = 84") +
  theme_bw() +
  theme(text = element_text(size = 14), axis.text = element_text(size = 14, colour = "black")) 

#SHP log transform pre-treatment density plot

ggplot(SHP_untreated, aes(x = hot_plate)) +
  geom_histogram(aes(y = ..density..), fill = "cadetblue4", colour = "black", alpha = 0.5, bins = 15) +
  geom_density(colour = "black", alpha = 0.75, size = 1, bw = 5) +
  scale_x_continuous(breaks = seq(0, 70, 10)) +
  labs(x = "Latency (s)", y = "Density",
       title = "Density plot: Standard hot plate",
       subtitle = "Pre-treatment; n = 84") +
  theme_bw() +
  theme(legend.title = element_blank(), 
        text = element_text(size = 14), axis.text = element_text(size = 14)) 

#SHP control population log transform QQplot

log_SHP_controls <- log_qplot_controls %>% 
  filter(Method == "CTL-vehicle (intravenous)" | 
           Method == "CTL-vehicle (intravenous time-course)" | 
           Method == "Untreated and Air treatment")  %>%
  filter(!is.na(hot_plate)) %>%
  dplyr::select(hot_plate, Method)

logHPslope <- diff(quantile(log_SHP_controls$hot_plate, c(0.25, 0.75), na.rm = T)) / 
  diff(qnorm(c(0.25, 0.75)))

logHPintercept <- quantile(log_SHP_controls$hot_plate, 0.25, na.rm = T) - logHPslope * qnorm(0.25)

SHP_qlog <- ggplot(log_SHP_controls, aes(sample = hot_plate)) +
  stat_qq(pch = 1, colour = "black",  fill = "black", alpha = 0.35, size = 4) +
  stat_qq(pch = 19, colour = "black",  fill = "black", alpha = 0.1, size = 4) +
  geom_abline(aes(slope = logHPslope, intercept = logHPintercept), 
              size = 1, col = "red", alpha = 0.5, lty = 1) +
  labs(y = "Log Latency (s)", x = "Theoretical distribution",
       title = " ",
       subtitle = "Log-transformed distribution; n = 215") +
  theme_bw() +
  theme(text = element_text(size = 14), axis.text = element_text(size = 14, colour = "black"))

SHP_dlog <- ggplot(log_SHP_controls, aes(x = hot_plate)) +
  geom_histogram(aes(y = ..density..), fill = "cadetblue4", 
                 colour = "black", alpha = 0.5, bins = 14) +
  geom_density(colour = "black",  alpha = 0.2, fill = "black", bw = 0.09) +
  scale_x_continuous() +
  labs(x = "Log Latency (s)", y = "Density", 
       title = " ",
       subtitle = "Log-transformed distribution; n = 215") +
  theme_bw() +
  theme(legend.title = element_blank(), 
        text = element_text(size = 14), axis.text = element_text(size = 14)) 

gSHP_q <- ggplotGrob(SHP_q)
gSHP_d <- ggplotGrob(SHP_d)
gSHP_qlog <- ggplotGrob(SHP_qlog)
gSHP_dlog <- ggplotGrob(SHP_dlog)

gSHP_q$width <- gSHP_qlog$width
gSHP_q$height <- gSHP_qlog$height
gSHP_d$width <- gSHP_dlog$width
gSHP_d$height <- gSHP_dlog$height

grid.draw(rbind(ggplotGrob(RHP_q), ggplotGrob(RHP_d), 
                ggplotGrob(RHP_qlog), ggplotGrob(RHP_dlog), 
                size="last", ncol = 2))
row1 <- rbind(ggplotGrob(RHP_q),
              ggplotGrob(RHP_qlog),
              size = "last")
row2 <- rbind(ggplotGrob(RHP_d),
              ggplotGrob(RHP_dlog),
              size = "last")
matrix_plot_RHP <- arrangeGrob(cbind(row1, row2, size = "last"))
grid.draw(matrix_plot_RHP)


grid.newpage()
grid.arrange(gSHP_q, gSHP_d, gSHP_qlog, gSHP_dlog, ncol = 2)











#CV calculation SHP
BPS_SHP_CV  %>%
  mutate(min = min(logSHP),
         corrected_latency = logSHP - min) %>%
  summarize(mean = mean(corrected_latency),
            sd = sd(corrected_latency),
            cv = (sd/mean * 100))
