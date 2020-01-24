
params.df <- cbind(data.frame(param=gsub(":\\(Intercept\\)","",
                                         rownames(summary(logmorphine_SHPfit)$coefficient)),stringsAsFactors=F),
                   data.frame(summary(logmorphine_SHPfit)$coefficient))
rownames(params.df) <- NULL
ann.df <- data.frame(Parameter=gsub(" Limit","",params.df$param),
                     Value=signif(params.df[,2],3),stringsAsFactors=F)
rownames(ann.df) <- NULL

thm <- ttheme_minimal(
  core=list(fg_params = list(hjust=rep(c(0, 1), each=4), 
                             x=rep(c(0.15, 0.85), each=4)),
            bg_params = list(fill = NA)),
  colhead=list(bg_params=list(fill = NA)))

ggdraw(logmorphine_SHP_graph) + draw_grob(tableGrob(ann.df, rows=NULL, theme=thm), 
                      x=0.26, y=0.41, width=0.25, height=0.5)

ED(logmorphine_SHPfit, c(25, 50, 75), interval = "delta")


#SHP morphine DR

SHP_morphine_DR <- BPS_morphine_DR_data %>%
  filter(Assay == "SHP") %>%
  melt(id = c("Assay")) %>%
  filter(!is.na(value)) 

leveneTest(log_latency_correction ~ factor(Dose), data = logSHP_morphine_DR)
bartlett.test(log_latency_correction ~ factor(Dose), data = logSHP_morphine_DR)

SHP_morphine_anova <- aov(log_latency_correction ~ factor(Dose), data = logSHP_morphine_DR)

distBCMod <- caret::BoxCoxTrans(logSHP_morphine_DR$log_latency_correction)
print(distBCMod)

par(mfrow=c(2,2)) 
plot(SHP_morphine_anova)

SHP_morphine_anova <- aov(dist_new ~ factor(Dose), data = logSHP_morphine_DR)
par(mfrow=c(2,2)) 
plot(SHP_morphine_anova)

logSHP_morphine_DR <- cbind(logSHP_morphine_DR, dist_new=predict(distBCMod, logSHP_morphine_DR$log_latency_correction)) 
head(logSHP_morphine_DR)

het_corrected_SHP_anova <- Anova(SHP_morphine_anova, type ="II", white.adjust = T)

SHP_morphine_DR$variable <- as.numeric(as.character(SHP_morphine_DR$variable))

SHP_morphine_DR <- SHP_morphine_DR %>% dplyr::rename(Dose = variable)
SHP_morphine_DR <- SHP_morphine_DR %>% dplyr::rename(Latency = value)

morphine_SHPfit <- drm(Latency ~ Dose, data = SHP_morphine_DR, 
                          fct = LL.4(fixed = c(NA, NA, 120, NA), 
                                     names = c("Slope","Lower Limit","Upper Limit","ED50")))
morphine_SHPline <- expand.grid(Dose = exp(seq(log(max(SHP_morphine_DR$Dose)),
                                                  log(min(SHP_morphine_DR$Dose)),length=100))) 
morphine_SHP <- predict(morphine_SHPfit,newdata=morphine_SHPline,interval="confidence") 
morphine_SHPline$p <- morphine_SHP[,1]
morphine_SHPline$pmin <- morphine_SHP[,2]
morphine_SHPline$pmax <- morphine_SHP[,3]
morphine_SHP_graph <- ggplot(SHP_morphine_DR, aes(x = Dose, y = Latency)) +
  geom_point(colour = "black", fill = "black", alpha = 0.25, size = 3) + 
  geom_line(data = morphine_SHPline, aes(x = Dose,y = p)) + 
  theme_bw() +
  labs(title = "Standard hot plate: Morphine", 
       subtitle = "Upper limit constraint = 120; n = 7 per group", x = "Dose (mg/kg)", y = "Latency (s)") +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", colour = "black", width = 0.25) +
  stat_summary(fun.data = mean_se, geom = "errorbar", colour = "cadetblue4", alpha = 0.5, width = 0.2) +
  stat_summary(fun.y = mean, geom = "point", colour = "cadetblue4", alpha = 0.85, size = 3, pch = 15) +
  geom_ribbon(data = morphine_SHPline, aes(x = Dose,y = p, ymin = pmin, ymax = pmax), alpha = 0.2) +
  scale_x_continuous(trans = "log10", breaks = c(0.01, 0.1, 1, 10), 
                     labels =c("Vehicle", "0.1", "1.0", "10")) +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 14)) +
  geom_abline(slope = 0, intercept = 120, lty = 3, alpha = 0.8) +
  scale_y_continuous(limits = c(-10, 155), breaks = seq(0, 150, 30))

#
#
#
#
#
#log transformed SHP morphine DR

logSHP_morphine_DR <- BPS_morphine_DR_data %>%
  filter(Assay == "logSHP") %>%
  melt(id = c("Assay")) %>%
  filter(!is.na(value)) %>%
  mutate(log_latency_correction = (value - 0.9542425) / 1.125)

leveneTest(log_latency_correction ~ factor(Dose), data = logSHP_morphine_DR)
bartlett.test(log_latency_correction ~ factor(Dose), data = logSHP_morphine_DR)

logSHP_morphine_DR$variable <- as.numeric(as.character(logSHP_morphine_DR$variable))

logSHP_morphine_DR <- logSHP_morphine_DR %>% dplyr::rename(Dose = variable)
logSHP_morphine_DR <- logSHP_morphine_DR %>% dplyr::rename(Latency = value)

logmorphine_SHPfit <- drm(log_latency_correction ~ Dose, data = logSHP_morphine_DR, 
              fct = LL.4(fixed = c(NA, NA, 1, NA), 
                         names = c("Slope","Lower Limit","Upper Limit","ED50")))
logmorphine_SHPline <- expand.grid(Dose = exp(seq(log(max(logSHP_morphine_DR$Dose)),
                                      log(min(logSHP_morphine_DR$Dose)),length=100))) 
logmorphine_SHP <- predict(logmorphine_SHPfit,newdata=logmorphine_SHPline,interval="confidence") 
logmorphine_SHPline$p <- logmorphine_SHP[,1]
logmorphine_SHPline$pmin <- logmorphine_SHP[,2]
logmorphine_SHPline$pmax <- logmorphine_SHP[,3]

logmorphine_SHP_graph <- ggplot(logSHP_morphine_DR, aes(x = Dose, y = log_latency_correction)) +
  geom_point(colour = "black", fill = "black", alpha = 0.25, size = 3) + 
  geom_line(data = logmorphine_SHPline, aes(x = Dose,y = p)) + 
  theme_bw() +
  labs(title = "Standard hot plate: Morphine", 
       subtitle = "n = 7 per group", x = "Dose (mg/kg)", y = "Proportion of effect") +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", colour = "black", width = 0.25) +
  stat_summary(fun.data = mean_se, geom = "errorbar", colour = "seagreen", alpha = 0.75, width = 0.2, size = 1) +
  stat_summary(fun.y = mean, geom = "point", colour = "seagreen", alpha = 0.85, size = 3, pch = 15) +
  geom_ribbon(data = logmorphine_SHPline, aes(x = Dose,y = p, ymin = pmin, ymax = pmax), alpha = 0.2) +
  scale_x_continuous(trans = "log10", breaks = c(0.01, 0.1, 1, 10), 
                     labels =c("Vehicle", "0.1", "1.0", "10")) +
  theme(text = element_text(size = 14, family = "Century Gothic"),
        axis.text = element_text(size = 14, family = "Century Gothic")) +
  geom_abline(slope = 0, intercept = 1, lty = 3, alpha = 0.8) +
  scale_y_continuous(limits = c(0, 1.24), breaks = seq(0, 1, 0.2)) 

ED(logmorphine_SHPfit, c(25, 50, 75), interval = "delta")

#
#
#
#
#
#
#RHP morphine Dose-response graph

RHP_morphine_DR <- BPS_morphine_DR_data %>%
  filter(Assay == "logRHP") %>%
  melt(id = c("Assay")) %>%
  filter(!is.na(value)) %>%
  mutate(log_latency_correction = (value - 2.117901) / 0.235)

leveneTest(log_latency_correction ~ factor(Dose), data = RHP_morphine_DR)
bartlett.test(log_latency_correction ~ factor(Dose), data = RHP_morphine_DR)

RHP_morphine_DR$variable <- as.numeric(as.character(RHP_morphine_DR$variable))

RHP_morphine_DR <- RHP_morphine_DR %>% dplyr::rename(Dose = variable)
RHP_morphine_DR <- RHP_morphine_DR %>% dplyr::rename(Latency = value)

morphine_RHPfit <- drm(log_latency_correction ~ Dose, data = RHP_morphine_DR, 
                       fct = LL.4(fixed = c(NA, NA, 1, NA), 
                                  names = c("Slope","Lower Limit","Upper Limit","ED50")))
morphine_RHPline <- expand.grid(Dose = exp(seq(log(max(RHP_morphine_DR$Dose)),
                                               log(min(RHP_morphine_DR$Dose)),length=100))) 
morphine_RHP <- predict(morphine_RHPfit,newdata=morphine_RHPline,interval="confidence") 
morphine_RHPline$p <- morphine_RHP[,1]
morphine_RHPline$pmin <- morphine_RHP[,2]
morphine_RHPline$pmax <- morphine_RHP[,3]

morphine_RHP_graph <- ggplot(RHP_morphine_DR, aes(x = Dose, y = log_latency_correction)) +
  geom_point(colour = "black", fill = "black", alpha = 0.25, size = 3) + 
  geom_line(data = morphine_RHPline, aes(x = Dose,y = p)) + 
  theme_bw() +
  labs(title = "Ramped hot plate: Morphine", 
       subtitle = "n = 8 per group", x = "Dose (mg/kg)", y = "Proportion of effect") +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", colour = "black", width = 0.25) +
  stat_summary(fun.data = mean_se, geom = "errorbar", colour = "tomato3", alpha = 0.75, width = 0.2, size = 1) +
  stat_summary(fun.y = mean, geom = "point", colour = "tomato3", alpha = 0.85, size = 3, pch = 15) +
  geom_ribbon(data = morphine_RHPline, aes(x = Dose,y = p, ymin = pmin, ymax = pmax), alpha = 0.2) +
  scale_x_continuous(trans = "log10", breaks = c(0.01, 0.1, 1, 10), 
                     labels =c("Vehicle", "0.1", "1.0", "10")) +
  theme(text = element_text(size = 14, family = "Century Gothic"),
        axis.text = element_text(size = 14, family = "Century Gothic")) +
  geom_abline(slope = 0, intercept = 1, lty = 3, alpha = 0.8) +
  scale_y_continuous(limits = c(0, 1.24), breaks = seq(0, 1, 0.2))

ED(morphine_RHPfit, c(25, 50, 75), interval = "delta")

#
#

plot_grid(logmorphine_SHP_graph, morphine_RHP_graph, align = "h")
summary(logmorphine_SHPfit)
summary(morphine_RHPfit)

#
#
#
#log RHP morphine DR

logRHP_morphine_DR <- BPS_morphine_DR_data %>%
  filter(Assay == "logRHP") %>%
  melt(id = c("Assay")) %>%
  filter(!is.na(value)) 
logRHP_morphine_DR$variable <- as.numeric(as.character(logRHP_morphine_DR$variable))

logRHP_morphine_DR <- logRHP_morphine_DR %>% dplyr::rename(Dose = variable)
logRHP_morphine_DR <- logRHP_morphine_DR %>% dplyr::rename(Latency = value)

logmorphine_RHPfit <- drm(Latency ~ Dose, data = logRHP_morphine_DR, 
                       fct = LL.4(fixed = c(NA, NA, 2.352, NA), 
                                  names = c("Slope","Lower Limit","Upper Limit","ED50")))
logmorphine_RHPline <- expand.grid(Dose = exp(seq(log(max(logRHP_morphine_DR$Dose)),
                                               log(min(logRHP_morphine_DR$Dose)),length=100))) 
logmorphine_RHP <- predict(logmorphine_RHPfit,newdata=logmorphine_RHPline,interval="confidence") 
logmorphine_RHPline$p <- logmorphine_RHP[,1]
logmorphine_RHPline$pmin <- logmorphine_RHP[,2]
logmorphine_RHPline$pmax <- logmorphine_RHP[,3]
logmorphine_RHP_graph <- ggplot(logRHP_morphine_DR, aes(x = Dose, y = Latency)) +
  geom_point(colour = "black", fill = "black", alpha = 0.25, size = 3) + 
  geom_line(data = logmorphine_RHPline, aes(x = Dose,y = p)) + 
  theme_bw() +
  labs(title = "Ramped hot plate: Morphine", 
       subtitle = "Upper limit constraint = 225; n = 8 per group", x = "Dose (mg/kg)", y = "Log Latency (s)") +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", colour = "black", width = 0.25) +
  stat_summary(fun.data = mean_se, geom = "errorbar", colour = "orangered4", alpha = 0.5, width = 0.2) +
  stat_summary(fun.y = mean, geom = "point", colour = "orangered4", alpha = 0.85, size = 3, pch = 15) +
  geom_ribbon(data = logmorphine_RHPline, aes(x = Dose,y = p, ymin = pmin, ymax = pmax), alpha = 0.2) +
  scale_x_continuous(trans = "log10", breaks = c(0.01, 0.1, 1, 10), 
                     labels =c("Vehicle", "0.1", "1.0", "10")) +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 14)) +
  geom_abline(slope = 0, intercept = 2.352, lty = 3, alpha = 0.8) +
  scale_y_continuous(limits = c(2.1, 2.41), breaks = seq(2.1, 2.5, 0.1))


modelFit(logmorphine_SHPfit, method = "cum")
modelFit(morphine_SHPfit, method = "cum")
modelFit(morphine_RHPfit, method = "cum")
modelFit(SHPfit, method = "cum")
modelFit(logSHPfit, method = "cum")
modelFit(RHPfit, method = "cum")
modelFit(logmorphine_RHPfit, method = "cum")


RHPmorphine_aov <- aov(Latency ~ Dose, data = RHP_morphine_DR)
plot(RHPmorphine_aov, 3)
