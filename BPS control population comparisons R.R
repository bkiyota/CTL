RHPcontrols <- BPS_control_population_comparison %>%
  filter(Assay == "Ramped hot plate") %>%
  melt(id = c("Assay")) %>%
  filter(!is.na(value)) 

RHPcontrols$variable <- factor(RHPcontrols$variable, levels = c("Pre-treatment", "Vehicle-injected", "Combined"))

RHPcontrols %>%
  group_by(variable) %>%
  dplyr::summarize(n = n(),
            mean = mean(value),
            sd = sd(value))

RHPsubsets <- RHPcontrols %>%
  filter(variable != "Combined")
bartlett.test(value ~ variable, data = RHPsubsets)
t.test(value ~ variable, data = RHPsubsets, conf.level = 0.95, var.equal = T)
cohen.d(d = RHPsubsets$value, f = RHPsubsets$variable, hedges.correction = T, conf.level = 0.95)

RHPcontrol_plot <-  ggplot(RHPcontrols, aes(x = variable, y = value)) +
  geom_point(position = position_jitter(width = .1), alpha = 0.45, size = 1, pch = 1) +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", size = 1,
               width = 0.35, alpha = 0.75, colour = "orangered4") +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", size = 1,
               width = 0.35, alpha = 0.25, colour = "black", lty = "twodash") +
  stat_summary(fun.y = mean, geom = "point", pch = 15, colour = "orangered4", alpha = 0.85, size = 3) +
  theme_bw() +
  labs(x = " ", y = "Latency (s)", title = " ",
       subtitle = "Ramped hot plate groups") +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 14, colour = "black")) +
  annotate("text", x = "Pre-treatment", y = 178, label = "n=387") +
  annotate("text", x = "Vehicle-injected", y = 178, label = "n=69") +
  annotate("text", x = "Combined", y = 178, label = "n=456")






SHPcontrols <- BPS_control_population_comparison %>%
  filter(Assay == "Standard hot plate") %>%
  melt(id = c("Assay")) %>%
  filter(!is.na(value))

SHPcontrols$variable <- factor(SHPcontrols$variable, levels = c("Pre-treatment", "Vehicle-injected", "Combined"))

SHPcontrols %>%
  group_by(variable) %>%
  dplyr::summarize(n = n(),
            mean = mean(value),
            sd = sd(value))

SHPcontrol_plot <- ggplot(SHPcontrols, aes(x = variable, y = value)) +
  geom_point(position = position_jitter(width = .1), alpha = 0.45, size = 1, pch = 1) +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", size = 1,
               width = 0.35, alpha = 0.75, colour = "cadetblue4") +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", size = 1,
               width = 0.35, alpha = 0.25, colour = "black", lty = "twodash") +
  stat_summary(fun.y = mean, geom = "point", pch = 15, colour = "cadetblue4", alpha = 0.85, size = 3) +
  theme_bw() +
  labs(x = " ", y = "Latency (s)", title = "Subserts composing the 'control' populations",
       subtitle = "Standard hot plate groups") +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 14, colour = "black")) +
  annotate("text", x = "Pre-treatment", y = 45, label = "n=84") +
  annotate("text", x = "Vehicle-injected", y = 55, label = "n=131") +
  annotate("text", x = "Combined", y = 55, label = "n=215") +
  scale_y_continuous(limits = c(0, 61), breaks = seq(0, 60, 10))


SHPsubsets <- SHPcontrols %>%
  filter(variable != "Combined")
bartlett.test(value ~ variable, data = SHPsubsets)
t.test(value ~ variable, data = SHPsubsets, conf.level = 0.95, var.equal = F)
cohen.d(d = SHPsubsets$value, f = SHPsubsets$variable, hedges.correction = T, conf.level = 0.95)


Control_plot_row <- cbind(ggplotGrob(SHPcontrol_plot),
              ggplotGrob(RHPcontrol_plot),
              size = "last")
grid.draw(Control_plot_row)



