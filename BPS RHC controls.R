RHP_comparison <- RHP_controls_comparison_R %>%
  melt() %>%
  filter(!is.na(value))

RHP_comparison %>%
  mutate(min = min(value),
         corrected_latency = value - min) %>%
  dplyr::summarize(mean = mean(corrected_latency),
            sd = sd(corrected_latency),
            cv = (sd/mean * 100))

shapiro.test(RHP_comparison$value)



ggplot(RHP_comparison, aes(x = "", y = value)) +
  geom_point(position = position_jitter(), pch = 19, alpha = 0.2, size = 3) +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", colour = "black", alpha = 0.75, width = 0.5) +
  stat_summary(fun.y = mean, geom = "point", colour = "orangered4", alpha = 0.75, size = 3) +
  facet_wrap(~ variable) +
  theme_bw() +
  labs(x = "Variable", y = "Latency (s)") +
  theme(text = element_text(size = 14), 
        axis.text = element_text(size = 14, colour = "black"))

#RHP pretreatment QQplot

RHP_pt_slope <- diff(quantile(RHP_comparison$value, c(0.25, 0.75), na.rm = T)) / 
  diff(qnorm(c(0.25, 0.75)))

RHP_pt_intercept <- quantile(RHP_comparison$value, 0.25, na.rm = T) - RHP_pt_slope * qnorm(0.25)

RHP_q <- ggplot(RHP_comparison, aes(sample = value)) +
  stat_qq(pch = 1, colour = "black",  fill = "black", alpha = 0.35, size = 4) +
  stat_qq(pch = 19, colour = "black",  fill = "black", alpha = 0.15, size = 4) +
  geom_abline(aes(slope = RHP_pt_slope, intercept = RHP_pt_intercept), size = 1, col = "red", alpha = 0.5, lty = 1) +
  labs(y = "Latency (s)", x = "Theoretical distribution",
       title = "QQplot: Ramped hot plate", 
       subtitle = "Normal distribution; n = 456")  +
  scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 1)) +
  scale_y_continuous(limits = c(120, 180)) +
  theme_bw() +
  theme(text = element_text(size = 14), axis.text = element_text(size = 14))

#RHP pretreamtent Density plot

RHP_d <- ggplot(RHP_comparison, aes(x = value)) +
  geom_histogram(aes(y = ..density..), fill = "orangered4", colour = "black", alpha = 0.5, bins = 20) +
  geom_density(colour = "black",  alpha = 0.2, fill = "black", bw = 3) +
  scale_x_continuous() +
  labs(x = "Latency (s)", y = "Density", 
       title = "Density plot: Ramped hot plate",
       subtitle = "Normal distribution; n = 456") +
  theme_bw() +
  theme(legend.title = element_blank(), 
        text = element_text(size = 14), axis.text = element_text(size = 14)) 

#RHP log pretreatment QQplot

log_RHP_dataset %>%
  mutate(min = min(value),
         corrected_latency = value - min) %>%
  summarize(mean = mean(corrected_latency),
            sd = sd(corrected_latency),
            cv = (sd/mean * 100))

log_RHP_dataset <- RHP_control_values %>%
  melt(id = c("RHP_pretreatment", "RHP_controls")) %>%
  dplyr::select(variable, value) %>%
  filter(!is.na(value))

logRHP_pt_slope <- diff(quantile(log_RHP_dataset$value, c(0.25, 0.75), na.rm = T)) / 
  diff(qnorm(c(0.25, 0.75)))

logRHP_pt_intercept <- quantile(log_RHP_dataset$value, 0.25, na.rm = T) - logRHP_pt_slope * qnorm(0.25)

RHP_qlog <- ggplot(log_RHP_dataset, aes(sample = value)) +
  stat_qq(pch = 1, colour = "black",  fill = "black", alpha = 0.35, size = 4) +
  stat_qq(pch = 19, colour = "black",  fill = "black", alpha = 0.15, size = 4) +
  geom_abline(aes(slope = logRHP_pt_slope, intercept = logRHP_pt_intercept), size = 1, col = "red", alpha = 0.5, lty = 1) +
  labs(y = "Log Latency (s)", x = "Theoretical distribution",
       title = " ", 
       subtitle = "Log-transformed distribution; n = 456")  +
  scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 1)) +
  theme_bw() +
  theme(text = element_text(size = 14), axis.text = element_text(size = 14)) 

#RHP log pretreamtent Density plot

RHP_dlog <- ggplot(log_RHP_dataset, aes(x = value)) +
  geom_histogram(aes(y = ..density..), fill = "orangered4", colour = "black", alpha = 0.5, bins = 20) +
  geom_density(colour = "black",  alpha = 0.2, fill = "black", bw = 0.015) +
  scale_x_continuous() +
  labs(x = "Log Latency (s)", y = "Density", 
       title = " ",
       subtitle = "Log-transformed distribution; n = 456") +
  theme_bw() +
  theme(legend.title = element_blank(), 
        text = element_text(size = 14), axis.text = element_text(size = 14)) 


gRHP_q <- ggplotGrob(RHP_q)
gRHP_d <- ggplotGrob(RHP_d)
gRHP_qlog <- ggplotGrob(RHP_qlog)
gRHP_dlog <- ggplotGrob(RHP_dlog)

gRHP_q$width <- gRHP_qlog$width
gRHP_q$height <- gRHP_qlog$height
gRHP_d$width <- gRHP_dlog$width
gRHP_d$height <- gRHP_dlog$height


grid.newpage()
grid.arrange(gRHP_q, gRHP_d, gRHP_qlog, gRHP_dlog, ncol = 2)






