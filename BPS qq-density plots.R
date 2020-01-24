# SHP control subsets

SHP_untreated_vehicle <- SHP_BPS_control_raw_data_ %>%
  gather(key = "Subset", value = "Latency", -Experiment, -rep_measurement) %>%
  filter(!is.na(Latency)) %>%
  mutate(latency_correction = (Latency - 9) / 111,
         log_Latency = log10(Latency),
         log_latency_correction = (log_Latency - log10(9)) / 1.125)

# SHP combined subsets

SHP_combined_BPS <- SHP_untreated_vehicle %>%
  mutate(latency_correction = (Latency - 9) / 111,
         log_Latency = log10(Latency),
         log_latency_correction = (log_Latency - log10(9)) / 1.125)

#SHP scatteprlot
ggplot(SHP_untreated_vehicle, aes(x = Subset, y = Latency)) +
  geom_point(position = position_jitter(width = 0.25), size = 3, alpha = 0.5) +
  stat_summary(fun.y = mean, geom = "point", colour = "red", size = 3) +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", width = 0.25, alpha = 0.75) +
  theme_bw() +
  labs(title = "Standard hot plate")

# SHP subset histogram
ggplot(SHP_untreated_vehicle, aes(x = log10(Latency))) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "black", alpha = 0.85) +
  geom_density() +
  facet_wrap(~Subset) +
  theme_bw() +
  labs(title = "Standard hot plate (Logarithmic transform)")

bartlett.test(Latency ~ Subset, data = SHP_untreated_vehicle)
t.test(Latency ~ Subset, data = SHP_untreated_vehicle, var.equal = F)
cohen.d(d = SHP_untreated_vehicle$Latency, f = SHP_untreated_vehicle$Subset, 
        hedges.correction = T, conf.level = 0.95)

# RHP control subsets 

RHP_untreated_vehicle <- RHP_BPS_control_raw_data %>%
  gather(key = "Subset", value = "Latency", -Experiment, -rep_measurement) %>%
  filter(!is.na(Latency)) %>%
  mutate(latency_correction = (Latency - 131.19) / 94.08,
         log_Latency = log10(Latency),
         log_latency_correction = (log_Latency - log10(131.19)) / 0.235)

RHP_combined_BPS <- RHP_untreated_vehicle %>%
  mutate(latency_correction = (Latency - 131.19) / 94.08,
         log_Latency = log10(Latency),
         log_latency_correction = (log_Latency - log10(131.19)) / 0.235)

## Scatterplot comparing the two
ggplot(RHP_untreated_vehicle, aes(x = Subset, y = Latency)) +
  geom_point(position = position_jitter(width = 0.25), size = 3, alpha = 0.15) +
  stat_summary(fun.y = mean, geom = "point", colour = "red", size = 3) +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", width = 0.25, alpha = 0.75) +
  theme_bw() +
  labs(title = "Ramped hot plate")



## RHP Histogram
ggplot(RHP_untreated_vehicle, aes(x = log10(Latency))) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "black", alpha = 0.85) +
  geom_density() +
  facet_wrap(~Subset) +
  theme_bw() +
  labs(title = "Ramped hot plate (logarithmic transform)")

bartlett.test(Latency ~ Subset, data = RHP_untreated_vehicle)
t.test(Latency ~ Subset, data = RHP_untreated_vehicle, var.equal = T)
cohen.d(d = RHP_untreated_vehicle$Latency, f = RHP_untreated_vehicle$Subset, 
        hedges.correction = T, conf.level = 0.95)

# SHP subsets

## SHP combined subsets

SHP_normal_combined_slope <- 
  diff(quantile(SHP_combined_BPS$latency_correction, c(0.25, 0.75), na.rm = T)) / 
  diff(qnorm(c(0.25, 0.75)))

SHP_normal_combined_intercept <- 
  quantile(SHP_combined_BPS$latency_correction, 0.25, na.rm = T) - 
  SHP_normal_combined_slope * qnorm(0.25)

SHP_normal_combined_panel1 <- ggplot(SHP_combined_BPS, aes(sample = latency_correction)) +
  stat_qq(pch = 1, colour = "black",  fill = "black", alpha = 0.35, size = 4) +
  stat_qq(pch = 19, colour = "black",  fill = "black", alpha = 0.15, size = 4) +
  geom_abline(aes(slope = SHP_normal_combined_slope, intercept = SHP_normal_combined_intercept),
              size = 1, col = "red", alpha = 0.5, lty = 1) +
  labs(y = "Latency (s)", x = "Theoretical distribution",
       title = "Standard hot plate (untreated + vehicle)",
       subtitle = "Quantile-Quantile and Density plots")  +
  scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 1)) +
  scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, 0.2)) +
  theme_bw() +
  theme(text = element_text(family = "Century Gothic", size = 14), 
        axis.text = element_text(family = "Century Gothic", size = 14)) +
  annotate(geom = "text", label = "n = 108", x = -2, y = 0.75, 
           size = 5, family = "Century Gothic")

SHP_normal_combined_dens <- axis_canvas(SHP_normal_combined_panel1, axis = "y") +
  geom_vridgeline(data = SHP_combined_BPS, aes(y = latency_correction, x = 0, width = ..density..),
                  stat = "ydensity", alpha = 0.7, size = 0.75, trim = F, fill = "seagreen") 

SHP_normal_combined_pp1 <- insert_yaxis_grob(SHP_normal_combined_panel1, SHP_normal_combined_dens, grid::unit(0.2, "null"),
                                    position = "right")

ggdraw(SHP_normal_combined_pp1)

### SHP combined log transform

SHP_log_combined_slope <- 
  diff(quantile(SHP_combined_BPS$log_latency_correction, c(0.25, 0.75), na.rm = T)) / 
  diff(qnorm(c(0.25, 0.75)))

SHP_log_combined_intercept <- 
  quantile(SHP_combined_BPS$log_latency_correction, 0.25, na.rm = T) - 
  SHP_log_combined_slope * qnorm(0.25)

SHP_log_combined_panel1 <- ggplot(SHP_combined_BPS, aes(sample = log_latency_correction)) +
  stat_qq(pch = 1, colour = "black",  fill = "black", alpha = 0.35, size = 4) +
  stat_qq(pch = 19, colour = "black",  fill = "black", alpha = 0.15, size = 4) +
  geom_abline(aes(slope = SHP_log_combined_slope, intercept = SHP_log_combined_intercept),
              size = 1, col = "red", alpha = 0.5, lty = 1) +
  labs(y = "Latency (s)", x = "Theoretical distribution",
       title = "Standard hot plate (untreated + vehicle)",
       subtitle = "Log-transform: Quantile-Quantile and Density plots")  +
  scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 1)) +
  scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, .2)) +
  theme_bw() +
  theme(text = element_text(family = "Century Gothic", size = 14), 
        axis.text = element_text(family = "Century Gothic", size = 14)) +
  annotate(geom = "text", label = "n = 108", x = -2, y = 0.75, 
           size = 5, family = "Century Gothic")

SHP_log_combined_dens <- axis_canvas(SHP_log_combined_panel1, axis = "y") +
  geom_vridgeline(data = SHP_combined_BPS, aes(y = log_latency_correction, x = 0, width = ..density..),
                  stat = "ydensity", alpha = 0.7, size = 0.75, trim = F, fill = "seagreen") 

SHP_log_combined_pp1 <- insert_yaxis_grob(SHP_log_combined_panel1, SHP_log_combined_dens, grid::unit(0.2, "null"),
                                             position = "right")

ggdraw(SHP_log_combined_pp1)

plot_grid(SHP_normal_combined_pp1, SHP_log_combined_pp1, align = "h")


## SHP untreated untransformed

SHP_normal_untreated_BPS <- SHP_untreated_vehicle %>%
  filter(Subset == "SHP_untreated") 

SHP_normal_untreated_slope <- 
  diff(quantile(SHP_normal_untreated_BPS$latency_correction, c(0.25, 0.75), na.rm = T)) / 
  diff(qnorm(c(0.25, 0.75)))

SHP_normal_untreated_intercept <- 
  quantile(SHP_normal_untreated_BPS$latency_correction, 0.25, na.rm = T) - 
  SHP_normal_untreated_slope * qnorm(0.25)

SHP_normal_untreated_panel1 <- ggplot(SHP_normal_untreated_BPS, aes(sample = latency_correction)) +
  stat_qq(pch = 1, colour = "black",  fill = "black", alpha = 0.35, size = 4) +
  stat_qq(pch = 19, colour = "black",  fill = "black", alpha = 0.15, size = 4) +
  geom_abline(aes(slope = SHP_normal_untreated_slope, intercept = SHP_normal_untreated_intercept),
              size = 1, col = "red", alpha = 0.5, lty = 1) +
  labs(y = "Latency (s)", x = "Theoretical distribution",
       title = "Standard hot plate (untreated)",
       subtitle = "Quantile-Quantile and Density plots")  +
  scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 1)) +
  scale_y_continuous(limits = c(0, 0.5)) +
  theme_bw() +
  theme(text = element_text(family = "Century Gothic", size = 14), 
        axis.text = element_text(family = "Century Gothic", size = 14)) +
  annotate(geom = "text", label = "n = ", x = -2, y = 0.18, 
           size = 5, family = "Century Gothic")

SHP_normal_untreated_dens <- axis_canvas(SHP_normal_untreated_panel1, axis = "y") +
  geom_vridgeline(data = SHP_normal_untreated_BPS, aes(y = latency_correction, x = 0, width = ..density..),
                  stat = "ydensity", alpha = 0.7, size = 0.75, trim = F, fill = "seagreen") 

SHP_normal_untreated_pp1 <- insert_yaxis_grob(SHP_normal_untreated_panel1, SHP_normal_untreated_dens, grid::unit(0.2, "null"),
                                    position = "right")

ggdraw(SHP_normal_untreated_pp1)

### SHP untreated log transformed

SHP_log_untreated_BPS <- SHP_untreated_vehicle %>%
  filter(Subset == "SHP_untreated") 

SHP_log_untreated_slope <- 
  diff(quantile(SHP_log_untreated_BPS$log_latency_correction, c(0.25, 0.75), na.rm = T)) / 
  diff(qnorm(c(0.25, 0.75)))

SHP_log_untreated_intercept <- 
  quantile(SHP_log_untreated_BPS$log_latency_correction, 0.25, na.rm = T) - 
  SHP_log_untreated_slope * qnorm(0.25)

SHP_log_untreated_panel1 <- ggplot(SHP_log_untreated_BPS, aes(sample = log_latency_correction)) +
  stat_qq(pch = 1, colour = "black",  fill = "black", alpha = 0.35, size = 4) +
  stat_qq(pch = 19, colour = "black",  fill = "black", alpha = 0.15, size = 4) +
  geom_abline(aes(slope = SHP_log_untreated_slope, intercept = SHP_log_untreated_intercept),
              size = 1, col = "red", alpha = 0.5, lty = 1) +
  labs(y = "Latency (s)", x = "Theoretical distribution",
       title = "Standard hot plate (untreated)",
       subtitle = "Log-transform: Quantile-Quantile and Density plots")  +
  scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 1)) +
  scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, 0.2)) +
  theme_bw() +
  theme(text = element_text(family = "Century Gothic", size = 14), 
        axis.text = element_text(family = "Century Gothic", size = 14)) +
  annotate(geom = "text", label = "n = ", x = -2, y = 0.15, 
           size = 5, family = "Century Gothic")

SHP_log_untreated_dens <- axis_canvas(SHP_log_untreated_panel1, axis = "y") +
  geom_vridgeline(data = SHP_log_untreated_BPS, aes(y = log_latency_correction, x = 0, width = ..density..),
                  stat = "ydensity", alpha = 0.7, size = 0.75, trim = F, fill = "seagreen") 

SHP_log_untreated_pp1 <- insert_yaxis_grob(SHP_log_untreated_panel1, SHP_log_untreated_dens, grid::unit(0.2, "null"),
                                              position = "right")

ggdraw(SHP_log_untreated_pp1)

## SHP vehicle nontransformed

SHP_normal_vehicle_BPS <- SHP_untreated_vehicle %>%
  filter(Subset == "SHP_vehicle") 

SHP_normal_vehicle_slope <- 
  diff(quantile(SHP_normal_vehicle_BPS$latency_correction, c(0.25, 0.75), na.rm = T)) / 
  diff(qnorm(c(0.25, 0.75)))

SHP_normal_vehicle_intercept <- 
  quantile(SHP_normal_vehicle_BPS$latency_correction, 0.25, na.rm = T) - 
  SHP_normal_vehicle_slope * qnorm(0.25)

SHP_normal_vehicle_panel1 <- ggplot(SHP_normal_vehicle_BPS, aes(sample = latency_correction)) +
  stat_qq(pch = 1, colour = "black",  fill = "black", alpha = 0.35, size = 4) +
  stat_qq(pch = 19, colour = "black",  fill = "black", alpha = 0.15, size = 4) +
  geom_abline(aes(slope = SHP_normal_vehicle_slope, intercept = SHP_normal_vehicle_intercept),
              size = 1, col = "red", alpha = 0.5, lty = 1) +
  labs(y = "Latency (s)", x = "Theoretical distribution",
       title = "Standard hot plate (vehicle)",
       subtitle = "Quantile-Quantile and Density plots")  +
  scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 1)) +
  scale_y_continuous(limits = c(0, 0.5)) +
  theme_bw() +
  theme(text = element_text(family = "Century Gothic", size = 14), 
        axis.text = element_text(family = "Century Gothic", size = 14)) +
  annotate(geom = "text", label = "n = ", x = -2, y = 0.18, 
           size = 5, family = "Century Gothic")

SHP_normal_vehicle_dens <- axis_canvas(SHP_normal_vehicle_panel1, axis = "y") +
  geom_vridgeline(data = SHP_normal_vehicle_BPS, aes(y = latency_correction, x = 0, width = ..density..),
                  stat = "ydensity", alpha = 0.7, size = 0.75, trim = F, fill = "seagreen") 

SHP_normal_vehicle_pp1 <- insert_yaxis_grob(SHP_normal_vehicle_panel1, SHP_normal_vehicle_dens, grid::unit(0.2, "null"),
                                              position = "right")

ggdraw(SHP_normal_vehicle_pp1)

### SHP vehicle log transform

SHP_log_vehicle_BPS <- SHP_untreated_vehicle %>%
  filter(Subset == "SHP_vehicle") 

SHP_log_vehicle_slope <- 
  diff(quantile(SHP_log_vehicle_BPS$log_latency_correction, c(0.25, 0.75), na.rm = T)) / 
  diff(qnorm(c(0.25, 0.75)))

SHP_log_vehicle_intercept <- 
  quantile(SHP_log_vehicle_BPS$log_latency_correction, 0.25, na.rm = T) - 
  SHP_log_vehicle_slope * qnorm(0.25)

SHP_log_vehicle_panel1 <- ggplot(SHP_log_vehicle_BPS, aes(sample = log_latency_correction)) +
  stat_qq(pch = 1, colour = "black",  fill = "black", alpha = 0.35, size = 4) +
  stat_qq(pch = 19, colour = "black",  fill = "black", alpha = 0.15, size = 4) +
  geom_abline(aes(slope = SHP_log_vehicle_slope, intercept = SHP_log_vehicle_intercept),
              size = 1, col = "red", alpha = 0.5, lty = 1) +
  labs(y = "Latency (s)", x = "Theoretical distribution",
       title = "Standard hot plate (vehicle)",
       subtitle = "Log-transform: Quantile-Quantile and Density plots")  +
  scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 1)) +
  scale_y_continuous(limits = c(0, .8), breaks = seq(0, 0.8, 0.2)) +
  theme_bw() +
  theme(text = element_text(family = "Century Gothic", size = 14), 
        axis.text = element_text(family = "Century Gothic", size = 14)) +
  annotate(geom = "text", label = "n = ", x = -2, y = 0.18, 
           size = 5, family = "Century Gothic")

SHP_log_vehicle_dens <- axis_canvas(SHP_log_vehicle_panel1, axis = "y") +
  geom_vridgeline(data = SHP_log_vehicle_BPS, aes(y = log_latency_correction, x = 0, width = ..density..),
                  stat = "ydensity", alpha = 0.7, size = 0.75, trim = F, fill = "seagreen") 

SHP_log_vehicle_pp1 <- insert_yaxis_grob(SHP_log_vehicle_panel1, SHP_log_vehicle_dens, grid::unit(0.2, "null"),
                                            position = "right")

ggdraw(SHP_log_vehicle_pp1)

## RHP normal combined 

RHP_normal_combined_slope <- 
  diff(quantile(RHP_combined_BPS$latency_correction, c(0.25, 0.75), na.rm = T)) / 
  diff(qnorm(c(0.25, 0.75)))

RHP_normal_combined_intercept <- 
  quantile(RHP_combined_BPS$latency_correction, 0.25, na.rm = T) - 
  RHP_normal_combined_slope * qnorm(0.25)

RHP_normal_combined_panel1 <- ggplot(RHP_combined_BPS, aes(sample = latency_correction)) +
  stat_qq(pch = 1, colour = "black",  fill = "black", alpha = 0.35, size = 4) +
  stat_qq(pch = 19, colour = "black",  fill = "black", alpha = 0.15, size = 4) +
  geom_abline(aes(slope = RHP_normal_combined_slope, intercept = RHP_normal_combined_intercept),
              size = 1, col = "red", alpha = 0.5, lty = 1) +
  labs(y = "Latency (s)", x = "Theoretical distribution",
       title = "Ramped hot plate (untreated + vehicle)",
       subtitle = "Quantile-Quantile and Density plots")  +
  scale_x_continuous(limits = c(-3.5, 3.5), breaks = seq(-3, 3, 1)) +
  scale_y_continuous(limits = c(0, 0.7), breaks = seq(0, 0.7, .1)) +
  theme_bw() +
  theme(text = element_text(family = "Century Gothic", size = 14), 
        axis.text = element_text(family = "Century Gothic", size = 14)) +
  annotate(geom = "text", label = "n = 910", x = -2, y = 0.7, 
           size = 5, family = "Century Gothic")

RHP_normal_combined_dens <- axis_canvas(RHP_normal_combined_panel1, axis = "y") +
  geom_vridgeline(data = RHP_combined_BPS, aes(y = latency_correction, x = 0, width = ..density..),
                  stat = "ydensity", alpha = 0.7, size = 0.75, trim = F, fill = "tomato3") 

RHP_normal_combined_pp1 <- insert_yaxis_grob(RHP_normal_combined_panel1, RHP_normal_combined_dens, grid::unit(0.2, "null"),
                                             position = "right")

ggdraw(RHP_normal_combined_pp1)

### RHP combined log transform

RHP_log_combined_slope <- 
  diff(quantile(RHP_combined_BPS$log_latency_correction, c(0.25, 0.75), na.rm = T)) / 
  diff(qnorm(c(0.25, 0.75)))

RHP_log_combined_intercept <- 
  quantile(RHP_combined_BPS$log_latency_correction, 0.25, na.rm = T) - 
  RHP_log_combined_slope * qnorm(0.25)

RHP_log_combined_panel1 <- ggplot(RHP_combined_BPS, aes(sample = log_latency_correction)) +
  stat_qq(pch = 1, colour = "black",  fill = "black", alpha = 0.35, size = 4) +
  stat_qq(pch = 19, colour = "black",  fill = "black", alpha = 0.15, size = 4) +
  geom_abline(aes(slope = RHP_log_combined_slope, intercept = RHP_log_combined_intercept),
              size = 1, col = "red", alpha = 0.5, lty = 1) +
  labs(y = "Latency (s)", x = "Theoretical distribution",
       title = "Ramped hot plate (untreated + vehicle)",
       subtitle = "Log-transform: Quantile-Quantile and Density plots")  +
  scale_x_continuous(limits = c(-3.5, 3.5), breaks = seq(-3, 3, 1)) +
  scale_y_continuous(limits = c(0, 0.7), breaks = seq(0, 0.7, .1)) +
  theme_bw() +
  theme(text = element_text(family = "Century Gothic", size = 14), 
        axis.text = element_text(family = "Century Gothic", size = 14)) +
  annotate(geom = "text", label = "n = 910", x = -2, y = 0.7, 
           size = 5, family = "Century Gothic")

RHP_log_combined_dens <- axis_canvas(RHP_log_combined_panel1, axis = "y") +
  geom_vridgeline(data = RHP_combined_BPS, aes(y = log_latency_correction, x = 0, width = ..density..),
                  stat = "ydensity", alpha = 0.7, size = 0.75, trim = F, fill = "tomato3") 

RHP_log_combined_pp1 <- insert_yaxis_grob(RHP_log_combined_panel1, RHP_log_combined_dens, grid::unit(0.2, "null"),
                                          position = "right")

ggdraw(RHP_log_combined_pp1)

plot_grid(RHP_normal_combined_pp1, RHP_log_combined_pp1, align = "h")

## RHP untreated non transformed

RHP_normal_untreated_BPS <- RHP_untreated_vehicle %>%
  filter(Subset == "RHP_untreated") 

RHP_normal_untreated_slope <- 
  diff(quantile(RHP_normal_untreated_BPS$latency_correction, c(0.25, 0.75), na.rm = T)) / 
  diff(qnorm(c(0.25, 0.75)))

RHP_normal_untreated_intercept <- 
  quantile(RHP_normal_untreated_BPS$latency_correction, 0.25, na.rm = T) - 
  RHP_normal_untreated_slope * qnorm(0.25)

RHP_normal_untreated_panel1 <- ggplot(RHP_normal_untreated_BPS, aes(sample = latency_correction)) +
  stat_qq(pch = 1, colour = "black",  fill = "black", alpha = 0.35, size = 4) +
  stat_qq(pch = 19, colour = "black",  fill = "black", alpha = 0.15, size = 4) +
  geom_abline(aes(slope = RHP_normal_untreated_slope, intercept = RHP_normal_untreated_intercept),
              size = 1, col = "red", alpha = 0.5, lty = 1) +
  labs(y = "Latency (s)", x = "Theoretical distribution",
       title = "Ramped hot plate (untreated)",
       subtitle = "Quantile-Quantile and Density plots")  +
  scale_x_continuous(limits = c(-3.5, 3.5), breaks = seq(-3, 3, 1)) +
  scale_y_continuous(limits = c(0, 0.7), breaks = seq(0, 0.7, 0.1)) +
  theme_bw() +
  theme(text = element_text(family = "Century Gothic", size = 14), 
        axis.text = element_text(family = "Century Gothic", size = 14)) +
  annotate(geom = "text", label = "n = 910", x = -2, y = 0.7, 
           size = 5, family = "Century Gothic")

RHP_normal_untreated_dens <- axis_canvas(RHP_normal_untreated_panel1, axis = "y") +
  geom_vridgeline(data = RHP_normal_untreated_BPS, aes(y = latency_correction, x = 0, width = ..density..),
                  stat = "ydensity", alpha = 0.7, size = 0.75, trim = F, fill = "tomato3") 

RHP_normal_untreated_pp1 <- insert_yaxis_grob(RHP_normal_untreated_panel1, RHP_normal_untreated_dens, grid::unit(0.2, "null"),
                                              position = "right")

ggdraw(RHP_normal_untreated_pp1)


plot_grid(RHP_)
### RHP untreated log transform

RHP_log_untreated_BPS <- RHP_untreated_vehicle %>%
  filter(Subset == "RHP_untreated") 

RHP_log_untreated_slope <- 
  diff(quantile(RHP_log_untreated_BPS$log_latency_correction, c(0.25, 0.75), na.rm = T)) / 
  diff(qnorm(c(0.25, 0.75)))

RHP_log_untreated_intercept <- 
  quantile(RHP_log_untreated_BPS$log_latency_correction, 0.25, na.rm = T) - 
  RHP_log_untreated_slope * qnorm(0.25)

RHP_log_untreated_panel1 <- ggplot(RHP_log_untreated_BPS, aes(sample = log_latency_correction)) +
  stat_qq(pch = 1, colour = "black",  fill = "black", alpha = 0.35, size = 4) +
  stat_qq(pch = 19, colour = "black",  fill = "black", alpha = 0.15, size = 4) +
  geom_abline(aes(slope = RHP_log_untreated_slope, intercept = RHP_log_untreated_intercept),
              size = 1, col = "red", alpha = 0.5, lty = 1) +
  labs(y = "Latency (s)", x = "Theoretical distribution",
       title = "Ramped hot plate (untreated)",
       subtitle = "Log-transform: Quantile-Quantile and Density plots")  +
  scale_x_continuous(limits = c(-3.5, 3.5), breaks = seq(-3, 3, 1)) +
  scale_y_continuous(limits = c(0, 0.6), breaks = seq(0, 0.6, 0.1)) +
  theme_bw() +
  theme(text = element_text(family = "Century Gothic", size = 14), 
        axis.text = element_text(family = "Century Gothic", size = 14)) +
  annotate(geom = "text", label = "n = ", x = -2, y = 0.15, 
           size = 5, family = "Century Gothic")

RHP_log_untreated_dens <- axis_canvas(RHP_log_untreated_panel1, axis = "y") +
  geom_vridgeline(data = RHP_log_untreated_BPS, aes(y = log_latency_correction, x = 0, width = ..density..),
                  stat = "ydensity", alpha = 0.7, size = 0.75, trim = F, fill = "tomato3") 

RHP_log_untreated_pp1 <- insert_yaxis_grob(RHP_log_untreated_panel1, RHP_log_untreated_dens, grid::unit(0.2, "null"),
                                           position = "right")

ggdraw(RHP_log_untreated_pp1)

## RHP vehicle nontransformed

RHP_normal_vehicle_BPS <- RHP_untreated_vehicle %>%
  filter(Subset == "RHP_vehicle") 

RHP_normal_vehicle_slope <- 
  diff(quantile(RHP_normal_vehicle_BPS$latency_correction, c(0.25, 0.75), na.rm = T)) / 
  diff(qnorm(c(0.25, 0.75)))

RHP_normal_vehicle_intercept <- 
  quantile(RHP_normal_vehicle_BPS$latency_correction, 0.25, na.rm = T) - 
  RHP_normal_vehicle_slope * qnorm(0.25)

RHP_normal_vehicle_panel1 <- ggplot(RHP_normal_vehicle_BPS, aes(sample = latency_correction)) +
  stat_qq(pch = 1, colour = "black",  fill = "black", alpha = 0.35, size = 4) +
  stat_qq(pch = 19, colour = "black",  fill = "black", alpha = 0.15, size = 4) +
  geom_abline(aes(slope = RHP_normal_vehicle_slope, intercept = RHP_normal_vehicle_intercept),
              size = 1, col = "red", alpha = 0.5, lty = 1) +
  labs(y = "Latency (s)", x = "Theoretical distribution",
       title = "Ramped hot plate (vehicle)",
       subtitle = "Quantile-Quantile and Density plots")  +
  scale_x_continuous(limits = c(-3.5, 3.5), breaks = seq(-3, 3, 1)) +
  scale_y_continuous(limits = c(0, 0.6), breaks = seq(0, 0.6, 0.1)) +
  theme_bw() +
  theme(text = element_text(family = "Century Gothic", size = 14), 
        axis.text = element_text(family = "Century Gothic", size = 14)) +
  annotate(geom = "text", label = "n = ", x = -2, y = 0.18, 
           size = 5, family = "Century Gothic")

RHP_normal_vehicle_dens <- axis_canvas(RHP_normal_vehicle_panel1, axis = "y") +
  geom_vridgeline(data = RHP_normal_vehicle_BPS, aes(y = latency_correction, x = 0, width = ..density..),
                  stat = "ydensity", alpha = 0.7, size = 0.75, trim = F, fill = "tomato3")

RHP_normal_vehicle_pp1 <- insert_yaxis_grob(RHP_normal_vehicle_panel1, RHP_normal_vehicle_dens, grid::unit(0.2, "null"),
                                            position = "right")

ggdraw(RHP_normal_vehicle_pp1)

### RHP vehicle log transform

RHP_log_vehicle_BPS <- RHP_untreated_vehicle %>%
  filter(Subset == "RHP_vehicle") 

RHP_log_vehicle_slope <- 
  diff(quantile(RHP_log_vehicle_BPS$log_latency_correction, c(0.25, 0.75), na.rm = T)) / 
  diff(qnorm(c(0.25, 0.75)))

RHP_log_vehicle_intercept <- 
  quantile(RHP_log_vehicle_BPS$log_latency_correction, 0.25, na.rm = T) - 
  RHP_log_vehicle_slope * qnorm(0.25)

RHP_log_vehicle_panel1 <- ggplot(RHP_log_vehicle_BPS, aes(sample = log_latency_correction)) +
  stat_qq(pch = 1, colour = "black",  fill = "black", alpha = 0.35, size = 4) +
  stat_qq(pch = 19, colour = "black",  fill = "black", alpha = 0.15, size = 4) +
  geom_abline(aes(slope = RHP_log_vehicle_slope, intercept = RHP_log_vehicle_intercept),
              size = 1, col = "red", alpha = 0.5, lty = 1) +
  labs(y = "Latency (s)", x = "Theoretical distribution",
       title = "Ramped hot plate (vehicle)",
       subtitle = "Log-transform: Quantile-Quantile and Density plots")  +
  scale_x_continuous(limits = c(-3.5, 3.5), breaks = seq(-3, 3, 1)) +
  scale_y_continuous(limits = c(0, .6), breaks = seq(0, 0.6, 0.1)) +
  theme_bw() +
  theme(text = element_text(family = "Century Gothic", size = 14), 
        axis.text = element_text(family = "Century Gothic", size = 14)) +
  annotate(geom = "text", label = "n = ", x = -2, y = 0.18, 
           size = 5, family = "Century Gothic")

RHP_log_vehicle_dens <- axis_canvas(RHP_log_vehicle_panel1, axis = "y") +
  geom_vridgeline(data = RHP_log_vehicle_BPS, aes(y = log_latency_correction, x = 0, width = ..density..),
                  stat = "ydensity", alpha = 0.7, size = 0.75, trim = F, fill = "tomato3") 

RHP_log_vehicle_pp1 <- insert_yaxis_grob(RHP_log_vehicle_panel1, RHP_log_vehicle_dens, grid::unit(0.2, "null"),
                                         position = "right")

ggdraw(RHP_log_vehicle_pp1)



