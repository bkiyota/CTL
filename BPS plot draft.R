
# SHP data
SHP_BPS_controls <- BPS_control_population_comparison %>%
  select(Combined,  Assay) %>%
  filter(Assay == "Standard hot plate") %>%
  mutate(log10_Combined = log10(Combined))


#SHP control population: qqplot-density plot
SHP_combined_slope <- 
  diff(quantile(SHP_BPS_controls$Combined, c(0.25, 0.75), na.rm = T)) / 
  diff(qnorm(c(0.25, 0.75)))

SHP_combined_intercept <- 
  quantile(SHP_BPS_controls$Combined, 0.25, na.rm = T) - SHP_combined_slope * qnorm(0.25)

SHP_combined_panel1 <- ggplot(SHP_BPS_controls, aes(sample = Combined)) +
  stat_qq(pch = 1, colour = "black",  fill = "black", alpha = 0.35, size = 4) +
  stat_qq(pch = 19, colour = "black",  fill = "black", alpha = 0.15, size = 4) +
  geom_abline(aes(slope = SHP_combined_slope, intercept = SHP_combined_intercept), size = 1, col = "red", alpha = 0.5, lty = 1) +
  labs(y = "Latency (s)", x = "Theoretical distribution",
       title = "Standard hot plate (untreated + vehicle-treated)",
       subtitle = "Quantile-Quantile and Density plots")  +
  scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 1)) +
  scale_y_continuous(breaks = seq(0, 60, 10)) +
  theme_bw() +
  theme(text = element_text(family = "Century Gothic", size = 14), 
        axis.text = element_text(family = "Century Gothic", size = 14)) +
  annotate(geom = "text", label = "n = 215", x = -2, y = 60, 
           size = 5, family = "Century Gothic")

SHP_y_dens <- axis_canvas(SHP_combined_panel1, axis = "y") +
  geom_vridgeline(data = SHP_BPS_controls, aes(y = Combined, x = 0, width = ..density..),
                  stat = "ydensity", alpha = 0.7, size = 0.75, trim = F, fill = "seagreen", bw = 3) 

SHP_normal_pp1 <- insert_yaxis_grob(SHP_combined_panel1, SHP_y_dens, grid::unit(0.2, "null"),
                         position = "right")

ggdraw(SHP_normal_pp1)

# SHP log transform: qqplot-density plot

SHP_log10_combined_slope <- 
  diff(quantile(SHP_BPS_controls$log10_Combined, c(0.25, 0.75), na.rm = T)) / 
  diff(qnorm(c(0.25, 0.75)))

SHP_log10_combined_intercept <- 
  quantile(SHP_BPS_controls$log10_Combined, 0.25, na.rm = T) - SHP_log10_combined_slope * qnorm(0.25)

SHP_log10_combined_panel1 <- ggplot(SHP_BPS_controls, aes(sample = log10_Combined)) +
  stat_qq(pch = 1, colour = "black",  fill = "black", alpha = 0.35, size = 4) +
  stat_qq(pch = 19, colour = "black",  fill = "black", alpha = 0.15, size = 4) +
  geom_abline(aes(slope = SHP_log10_combined_slope, intercept = SHP_log10_combined_intercept), 
              size = 1, col = "red", alpha = 0.5, lty = 1) +
  labs(y = "Log Latency (s)", x = "Theoretical distribution",
       title = "Standard hot plate (untreated + vehicle-treated)",
       subtitle = "Log-transformation: Quantile-Quantile and Density plots")  +
  scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 1)) +
  scale_y_continuous() +
  theme_bw() +
  theme(text = element_text(family = "Century Gothic", size = 14), 
        axis.text = element_text(family = "Century Gothic", size = 14)) +
  annotate(geom = "text", label = "n = 215", x = -2, y = 1.75, 
           size = 5, family = "Century Gothic")

SHP_log10_y_dens <- axis_canvas(SHP_log10_combined_panel1, axis = "y") +
  geom_vridgeline(data = SHP_BPS_controls, aes(y = log10_Combined, x = 0, width = ..density..),
                  stat = "ydensity", alpha = 0.7, size = 0.75, trim = F, fill = "seagreen", bw = .06) 

SHP_log10_normal_pp1 <- insert_yaxis_grob(SHP_log10_combined_panel1, SHP_log10_y_dens, grid::unit(0.2, "null"),
                                    position = "right")

ggdraw(SHP_log10_normal_pp1)

# RHP data

RHP_BPS_controls <- BPS_control_population_comparison %>%
  select(Combined,  Assay) %>%
  filter(Assay == "Ramped hot plate" & !is.na(Combined)) %>%
  mutate(log10_Combined = log10(Combined))

#RHP control population: qqplot-density plot

RHP_combined_slope <- 
  diff(quantile(RHP_BPS_controls$Combined, c(0.25, 0.75), na.rm = T)) / 
  diff(qnorm(c(0.25, 0.75)))

RHP_combined_intercept <- 
  quantile(RHP_BPS_controls$Combined, 0.25, na.rm = T) - RHP_combined_slope * qnorm(0.25)

RHP_combined_panel1 <- ggplot(RHP_BPS_controls, aes(sample = Combined)) +
  stat_qq(pch = 1, colour = "black",  fill = "black", alpha = 0.35, size = 4) +
  stat_qq(pch = 19, colour = "black",  fill = "black", alpha = 0.15, size = 4) +
  geom_abline(aes(slope = RHP_combined_slope, intercept = RHP_combined_intercept), size = 1, col = "red", alpha = 0.5, lty = 1) +
  labs(y = "Latency (s)", x = "Theoretical distribution",
       title = "Ramped hot plate (untreated + vehicle-treated)",
       subtitle = "Quantile-Quantile and Density plots")  +
  scale_x_continuous(breaks = seq(-3, 3, 1)) +
  scale_y_continuous() +
  theme_bw() +
  theme(text = element_text(family = "Century Gothic", size = 14), 
        axis.text = element_text(family = "Century Gothic", size = 14)) +
  annotate(geom = "text", label = "n = 456", x = -2, y = 180, 
           size = 5, family = "Century Gothic")

RHP_y_dens <- axis_canvas(RHP_combined_panel1, axis = "y") +
  geom_vridgeline(data = RHP_BPS_controls, aes(y = Combined, x = 0, width = ..density..),
                  stat = "ydensity", alpha = 0.75, size = 0.75, trim = F, fill = "tomato3", bw = 3) 

RHP_normal_pp1 <- insert_yaxis_grob(RHP_combined_panel1, RHP_y_dens, grid::unit(0.2, "null"),
                                    position = "right")

ggdraw(RHP_normal_pp1)

# RHP log transform: qqplot-density plot

RHP_log10_combined_slope <- 
  diff(quantile(RHP_BPS_controls$log10_Combined, c(0.25, 0.75), na.rm = T)) / 
  diff(qnorm(c(0.25, 0.75)))

RHP_log10_combined_intercept <- 
  quantile(RHP_BPS_controls$log10_Combined, 0.25, na.rm = T) - RHP_log10_combined_slope * qnorm(0.25)

RHP_log10_combined_panel1 <- ggplot(RHP_BPS_controls, aes(sample = log10_Combined)) +
  stat_qq(pch = 1, colour = "black",  fill = "black", alpha = 0.35, size = 4) +
  stat_qq(pch = 19, colour = "black",  fill = "black", alpha = 0.15, size = 4) +
  geom_abline(aes(slope = RHP_log10_combined_slope, intercept = RHP_log10_combined_intercept), 
              size = 1, col = "red", alpha = 0.5, lty = 1) +
  labs(y = "Log Latency (s)", x = "Theoretical distribution",
       title = "Ramped hot plate (untreated + vehicle-treated)",
       subtitle = "Log-transformation: Quantile-Quantile and Density plots")  +
  scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 1)) +
  scale_y_continuous() +
  theme_bw() +
  theme(text = element_text(family = "Century Gothic", size = 14), 
        axis.text = element_text(family = "Century Gothic", size = 14)) +
  annotate(geom = "text", label = "n = 456", x = -2, y = 2.25, 
           size = 5, family = "Century Gothic")

RHP_log10_y_dens <- axis_canvas(RHP_log10_combined_panel1, axis = "y") +
  geom_vridgeline(data = RHP_BPS_controls, aes(y = log10_Combined, x = 0, width = ..density..),
                  stat = "ydensity", alpha = 0.75, size = 0.75, trim = F, fill = "tomato3", bw = .01) 

RHP_log10_normal_pp1 <- insert_yaxis_grob(RHP_log10_combined_panel1, RHP_log10_y_dens, grid::unit(0.2, "null"),
                                          position = "right")

ggdraw(RHP_log10_normal_pp1)


