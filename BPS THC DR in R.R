logSHP_DR <- BPS_SHP_THC_DR_data %>% dplyr::select(-Assay)
logSHP_DR_scaled <- logSHP_DR %>%
  mutate(Effect_correction = (Effect - 9) / 111,
         log_Effect_correction = (log_Effect - 0.9542425) / 1.125)

leveneTest(log_Effect_correction ~ factor(Dose), data = logSHP_DR_scaled)
bartlett.test(log_Effect_correction ~ factor(Dose), data = logSHP_DR_scaled)

aov(log_Effect_correction ~ factor(Dose), 
    data = logSHP_DR_scaled)



logSHP_DR %>% group_by(Dose) %>%
  summarize(n = n())


# SHP THC normal 
log_SHP_DR_data_ANOVA <- BPS_SHP_THC_DR_data %>% dplyr::select(-Assay)
logSHP_DR %>% group_by(Dose)

logSHPfit <- drm(log_Effect ~ Dose, data = logSHP_DR, 
              fct = LL.4(fixed = c(NA, NA, 2.079181, NA), 
                         names = c("Slope","Lower Limit","Upper Limit","ED50")))
logSHPline <- expand.grid(Dose = exp(seq(log(max(logSHP_DR$Dose)),
                                      log(min(logSHP_DR$Dose)),length=100))) 

oneway.test()

logSHP <- predict(logSHPfit,newdata=logSHPline,interval="confidence") 
logSHPline$p <- logSHP[,1]
logSHPline$pmin <- logSHP[,2]
logSHPline$pmax <- logSHP[,3]

logTHC_SHP_graph <- ggplot(logSHP_DR, aes(x = Dose, y = log_Effect)) +
  geom_point(colour = "black", fill = "black", alpha = 0.25, size = 2) + 
  geom_line(data = logSHPline, aes(x = Dose,y = p)) + 
  theme_bw() +
  labs(title = "Standard hot plate: pure THC", 
       subtitle = "Upper limit constraint = 120; n = 4-20 per group", x = "Dose (mg/kg)", y = "Log Latency (s)") +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", colour = "black", width = 0.25) +
  stat_summary(fun.data = mean_se, geom = "errorbar", colour = "seagreen", alpha = 0.75, width = 0.2) +
  stat_summary(fun.y = mean, geom = "point", colour = "seagreen", alpha = 0.85, size = 3, pch = 15) +
  geom_ribbon(data = logSHPline, aes(x = Dose,y = p, ymin = pmin, ymax = pmax), alpha = 0.15) +
  scale_x_continuous(trans = "log10", breaks = c(0.01, 0.1, 1, 10), 
                     labels =c("Vehicle", "0.1", "1.0", "10")) +
  theme(text = element_text(family = "Century Gothic", size = 14),
        axis.text = element_text(family = "Century Gothic", size = 14)) +
  geom_abline(slope = 0, intercept = 2.079181, lty = 3, alpha = 0.8) +
  scale_y_continuous() 

# SHP THC DR scaled 0 to 1 (log-transofrm)

log_SHP_DR_data_ANOVA <- BPS_SHP_THC_DR_data %>% dplyr::select(-Assay)
logSHP_DR %>% group_by(Dose)

leveneTest(log_Effect_correction ~ factor(Dose), data = logRHP_DR_scaled)
bartlett.test(log_Effect_correction ~ factor(Dose), data = logRHP_DR_scaled)


logSHP_scaled_fit <- drm(log_Effect_correction ~ Dose, data = logSHP_DR_scaled, 
                 fct = LL.4(fixed = c(NA, NA, 1, NA), 
                            names = c("Slope","Lower Limit","Upper Limit","ED50")))
logSHP_scaled_line <- expand.grid(Dose = exp(seq(log(max(logSHP_DR_scaled$Dose)),
                                         log(min(logSHP_DR_scaled$Dose)),length=100))) 

logSHP_scaled <- predict(logSHP_scaled_fit,newdata=logSHP_scaled_line,interval="confidence") 
logSHP_scaled_line$p <- logSHP_scaled[,1]
logSHP_scaled_line$pmin <- logSHP_scaled[,2]
logSHP_scaled_line$pmax <- logSHP_scaled[,3]

logTHC_SHP_scaled_graph <- ggplot(logSHP_DR_scaled, aes(x = Dose, y = log_Effect_correction)) +
  geom_point(colour = "black", fill = "black", alpha = 0.25, size = 2) + 
  geom_line(data = logSHP_scaled_line, aes(x = Dose,y = p)) + 
  theme_bw() +
  labs(title = "Standard hot plate: pure THC", 
       subtitle = "n = 4-20 per group", x = "Dose (mg/kg)", y = "Proportion of effect") +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", colour = "black", width = 0.25) +
  stat_summary(fun.data = mean_se, geom = "errorbar", colour = "seagreen", alpha = 0.75, width = 0.2, size = 1) +
  stat_summary(fun.y = mean, geom = "point", colour = "seagreen", alpha = 0.85, size = 3, pch = 15) +
  geom_ribbon(data = logSHP_scaled_line, aes(x = Dose,y = p, ymin = pmin, ymax = pmax), alpha = 0.15) +
  scale_x_continuous(trans = "log10", breaks = c(0.01, 0.1, 1, 10), 
                     labels =c("Vehicle", "0.1", "1.0", "10")) +
  theme(text = element_text(family = "Century Gothic", size = 14),
        axis.text = element_text(family = "Century Gothic", size = 14)) +
  geom_abline(slope = 0, intercept = 1, lty = 3, alpha = 0.8) +
  scale_y_continuous(limits = c(0, 1.2), breaks = seq(0, 1, 0.2))

ED(logSHP_scaled_fit, c(25, 50, 75), interval = "delta")


## SHP THC DR normal scaled 0 to 1

log_SHP_DR_data_ANOVA <- BPS_SHP_THC_DR_data %>% dplyr::select(-Assay)
logSHP_DR %>% group_by(Dose)

logSHP_normal_scaled_fit <- drm(Effect_correction ~ Dose, data = logSHP_DR_scaled, 
                         fct = LL.4(fixed = c(NA, NA, 1, NA), 
                                    names = c("Slope","Lower Limit","Upper Limit","ED50")))
logSHP_normal_scaled_line <- expand.grid(Dose = exp(seq(log(max(logSHP_DR_scaled$Dose)),
                                                 log(min(logSHP_DR_scaled$Dose)),length=100))) 

logSHP_normal_scaled <- predict(logSHP_normal_scaled_fit,newdata=logSHP_normal_scaled_line,interval="confidence") 
logSHP_normal_scaled_line$p <- logSHP_normal_scaled[,1]
logSHP_normal_scaled_line$pmin <- logSHP_normal_scaled[,2]
logSHP_normal_scaled_line$pmax <- logSHP_normal_scaled[,3]

logTHC_SHP_normal_scaled_graph <- ggplot(logSHP_DR_scaled, aes(x = Dose, y = Effect_correction)) +
  geom_point(colour = "black", fill = "black", alpha = 0.25, size = 2) + 
  geom_line(data = logSHP_normal_scaled_line, aes(x = Dose,y = p)) + 
  theme_bw() +
  labs(title = "Standard hot plate: pure THC", 
       subtitle = "n = 4-20 per group", x = "Dose (mg/kg)", y = "Scaled Latency (s)") +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", colour = "black", width = 0.25) +
  stat_summary(fun.data = mean_se, geom = "errorbar", colour = "seagreen", alpha = 0.75, width = 0.2, size = 1) +
  stat_summary(fun.y = mean, geom = "point", colour = "seagreen", alpha = 0.85, size = 3, pch = 15) +
  geom_ribbon(data = logSHP_normal_scaled_line, aes(x = Dose,y = p, ymin = pmin, ymax = pmax), alpha = 0.15) +
  scale_x_continuous(trans = "log10", breaks = c(0.01, 0.1, 1, 10), 
                     labels =c("Vehicle", "0.1", "1.0", "10")) +
  theme(text = element_text(family = "Century Gothic", size = 14),
        axis.text = element_text(family = "Century Gothic", size = 14)) +
  geom_abline(slope = 0, intercept = 1, lty = 3, alpha = 0.8) +
  scale_y_continuous(limits = c(-0.2, 1.3), breaks = seq(0, 1, 0.2))

# RHP

RHP_DR %>% group_by(Dose) %>% summarize(n = n())
RHP_DR$Dose <- as.numeric(as.character(RHP_DR$Dose))



RHP_DR <- THC_DR_data_v1 %>% filter(Assay == "Rampled hot plate" | Assay == "Ramped hot plate")
RHPfit <- drm(Effect ~ Dose, data = RHP_DR, 
              fct = LL.4(fixed = c(NA, NA, 225, NA), names = c("Slope","Lower Limit","Upper Limit","ED50")))
RHPline <- expand.grid(Dose = exp(seq(log(max(RHP_DR$Dose)),
                                      log(min(RHP_DR$Dose)),length=100))) 
RHP <- predict(RHPfit,newdata=RHPline,interval="confidence") 
RHPline$p <- RHP[,1]
RHPline$pmin <- RHP[,2]
RHPline$pmax <- RHP[,3]
THC_RHP_graph <- ggplot(RHP_DR, aes(x = Dose, y = Effect)) +
  geom_point(colour = "black", fill = "black", alpha = 0.25, size = 3) + 
  geom_line(data = RHPline, aes(x = Dose,y = p)) + 
  theme_bw() +
  labs(title = "Ramped hot plate: pure THC",
       subtitle = "Upper limit constraint = 225; n = 8-16 per group", x = "Dose (mg/kg)", y = "Latency (s)") +
  scale_x_continuous(trans = "log10", breaks = c(0.01, 0.1, 1.0, 10), 
                     labels =c("Vehicle", "0.1", "1.0",  "10")) +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", colour = "black", width = 0.25) +
  stat_summary(fun.data = mean_se, geom = "errorbar", colour = "orangered4", alpha = 0.5, width = 0.2) +
  stat_summary(fun.y = mean, geom = "point", colour = "orangered4", alpha = 0.85, size = 3, shape = 15) +
  geom_ribbon(data = RHPline, aes(x = Dose,y = p, ymin = pmin, ymax = pmax), alpha = 0.2) +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 14)) +
  geom_abline(slope = 0, intercept = 225, lty = 3, alpha = 0.8, colour = "black") +
  scale_y_continuous(limits = c(130, 260), breaks = seq(120, 260, 20))

####
#
#
#
#
#log RHP THC dose-response
logRHP_DR_scaled <- BPS_RHP_THC_DR %>%
  mutate(Effect_correction = (Effect - 131.19) / 93.790,
         log_Effect_correction = (log_Effect - 2.117901) / 0.235)

leveneTest(log_Effect_correction ~ factor(Dose), data = logRHP_DR_scaled)
bartlett.test(log_Effect_correction ~ factor(Dose), data = logRHP_DR_scaled)


logRHPfit_scaled <- drm(log_Effect_correction ~ Dose, data = logRHP_DR_scaled, 
              fct = LL.4(fixed = c(NA, NA, 1, NA), names = c("Slope","Lower Limit","Upper Limit","ED50")))
logRHPline_scaled <- expand.grid(Dose = exp(seq(log(max(logRHP_DR_scaled$Dose)),
                                      log(min(logRHP_DR_scaled$Dose)),length=100))) 
logRHP_scaled <- predict(logRHPfit_scaled,newdata=logRHPline_scaled,interval="confidence") 
logRHPline_scaled$p <- logRHP_scaled[,1]
logRHPline_scaled$pmin <- logRHP_scaled[,2]
logRHPline_scaled$pmax <- logRHP_scaled[,3]

logTHC_scaled_RHP_graph <- ggplot(logRHP_DR_scaled, aes(x = Dose, y = log_Effect_correction)) +
  geom_point(colour = "black", fill = "black", alpha = 0.25, size = 2) + 
  geom_line(data = logRHPline_scaled, aes(x = Dose,y = p)) + 
  theme_bw() +
  labs(title = "Ramped hot plate: pure THC",
       subtitle = "n = 8-16 per group", x = "Dose (mg/kg)", y = "Proportion of effect") +
  scale_x_continuous(trans = "log10", breaks = c(0.01, 0.1, 1.0, 10), 
                     labels =c("Vehicle", "0.1", "1.0",  "10")) +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", colour = "black", width = 0.25) +
  stat_summary(fun.data = mean_se, geom = "errorbar", colour = "tomato3", alpha = 0.75, width = 0.2, size = 1) +
  stat_summary(fun.y = mean, geom = "point", colour = "tomato3", alpha = 0.85, size = 3, shape = 15) +
  geom_ribbon(data = logRHPline_scaled, aes(x = Dose,y = p, ymin = pmin, ymax = pmax), alpha = 0.15) +
  theme(text = element_text(family = "Century Gothic", size = 14),
        axis.text = element_text(family = "Century Gothic", size = 14),
        title = element_text(family = "Century Gothic", size = 14, face = "bold")) +
  geom_abline(slope = 0, intercept = 1, lty = 3, alpha = 0.8, colour = "black") +
  scale_y_continuous(limits = c(0, 1.2), breaks = seq(0, 1, 0.2))

ED(logRHPfit_scaled, c(25, 50, 75), interval = "delta")


plot_grid(logTHC_SHP_scaled_graph, logTHC_scaled_RHP_graph, align = "h")

summary(logRHPfit_scaled)
summary(logSHP_scaled_fit)






#log RHP THC dose-response non-transformed
logRHP_DR_scaled <- BPS_RHP_THC_DR %>%
  mutate(Effect_correction = (Effect - 131.19) / 93.790,
         log_Effect_correction = (log_Effect - 2.117901) / 0.235)


logRHPfit_normal_scaled<- drm(Effect_correction ~ Dose, data = logRHP_DR_scaled, 
                       fct = LL.4(fixed = c(NA, NA, 1, NA), names = c("Slope","Lower Limit","Upper Limit","ED50")))
logRHPline_normal_scaled <- expand.grid(Dose = exp(seq(log(max(logRHP_DR_scaled$Dose)),
                                                log(min(logRHP_DR_scaled$Dose)),length=100))) 
logRHP_normal_scaled <- predict(logRHPfit_normal_scaled,newdata=logRHPline_normal_scaled,interval="confidence") 
logRHPline_normal_scaled$p <- logRHP_normal_scaled[,1]
logRHPline_normal_scaled$pmin <- logRHP_normal_scaled[,2]
logRHPline_normal_scaled$pmax <- logRHP_normal_scaled[,3]

logTHC_normal_scaled_RHP_graph <- ggplot(logRHP_DR_scaled, aes(x = Dose, y = Effect_correction)) +
  geom_point(colour = "black", fill = "black", alpha = 0.25, size = 2) + 
  geom_line(data = logRHPline_normal_scaled, aes(x = Dose,y = p)) + 
  theme_bw() +
  labs(title = "Ramped hot plate: pure THC",
       subtitle = "n = 8-16 per group", x = "Dose (mg/kg)", y = "Scaled Latency (s)") +
  scale_x_continuous(trans = "log10", breaks = c(0.01, 0.1, 1.0, 10), 
                     labels =c("Vehicle", "0.1", "1.0",  "10")) +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", colour = "black", width = 0.25) +
  stat_summary(fun.data = mean_se, geom = "errorbar", colour = "tomato3", alpha = 0.75, width = 0.2, size = 1) +
  stat_summary(fun.y = mean, geom = "point", colour = "tomato3", alpha = 0.85, size = 3, shape = 15) +
  geom_ribbon(data = logRHPline_normal_scaled, aes(x = Dose,y = p, ymin = pmin, ymax = pmax), alpha = 0.15) +
  theme(text = element_text(family = "Century Gothic", size = 14),
        axis.text = element_text(family = "Century Gothic", size = 14)) +
  geom_abline(slope = 0, intercept = 1, lty = 3, alpha = 0.8, colour = "black") +
  scale_y_continuous(limits = c(0, 1.2), breaks = seq(0, 1, 0.2))



ED(logSHPfit, c(25, 50, 75), interval = "delta")
ED(SHPfit, c(25, 50, 75), interval = "delta")
ED(logRHPfit, c(25, 50, 75), interval = "delta")


row_SHP <- cbind(ggplotGrob(THC_SHP_graph),
              ggplotGrob(logmorphine_SHP_graph),
              size = "last")
row_RHP <- cbind(ggplotGrob(logTHC_RHP_graph),
              ggplotGrob(logmorphine_RHP_graph),
              size = "last")
matrix_plot_DR <- arrangeGrob(rbind(row_SHP, row_RHP, size = "last"))
grid.draw(matrix_plot_DR)

