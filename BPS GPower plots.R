BPS_G_power_plots$Treatment <-  factor(BPS_G_power_plots$Treatment, ordered = T, 
       levels = c("pure THC", "Morphine"))
RHP_G_power <- BPS_G_power_plots %>%
  dplyr::select(-X__1) 

ggplot(RHP_G_power, aes(x = Sample_size, y = RHP, group = Groups)) +
  geom_point(aes(pch = factor(Groups), colour = factor(Groups)), size = 3, alpha = 0.75) +
  facet_wrap(~ Treatment) +
  theme_bw() +
  scale_shape_manual(values = c(16, 15, 17, 18, 21), name = "Treatment\ngroups") +
  scale_x_continuous(limits = c(10, 60), breaks = seq(10, 60, 10)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", 
                                "#E6AB02", "steelblue"), name = "Treatment\ngroups") +
  labs(x = "Total sample size", y = "Power (1 - beta error probability)",
       title = "G*Power analysis for the ramped hot plate assay",
       subtitle = "Based on an alpha value of 0.05") +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 14, colour = "black"),
        strip.text.x = element_text(size = 14)) +
  geom_abline(slope = 0, intercept = 0.8, lty = 3, colour = "black", alpha = 0.75)

ggplot(RHP_G_power, aes(x = Sample_size, y = RHP, group = Groups)) +
  geom_point(aes(pch = factor(Groups), colour = factor(Groups)), size = 3, alpha = 0.75) +
  facet_wrap(~ Treatment) +
  theme_bw() +
  scale_shape_manual(values = c(16, 15, 17, 18, 21), name = "Treatment\ngroups") +
  scale_x_continuous(limits = c(10, 60), breaks = seq(10, 60, 10)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", 
                                "#E6AB02", "steelblue"), name = "Treatment\ngroups") +
  xlab("Total sample size") +
  ylab(expression("Power (1 - "*beta*" error probability)")) +
  labs(title = "G*Power analysis for the ramped hot plate assay",
       subtitle = expression("Based on "*alpha*" = 0.05; Effect size: THC ("*omega^2*" = 0.88), morphine ("*omega^2*" = 0.77)")) +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 14, colour = "black"),
        strip.text.x = element_text(size = 14, colour = "black"),
        strip.background = element_rect(fill = alpha("paleturquoise4", 0.5))) +
  geom_abline(slope = 0, intercept = 0.8, lty = 3, colour = "black", alpha = 0.75)



#SHP G*Power plots

SHP_G_power <- BPS_G_power_plots %>%
  dplyr::select(-X__1) 

ggplot(SHP_G_power, aes(x = Sample_size, y = SHP, group = Groups)) +
  geom_point(aes(pch = factor(Groups), colour = factor(Groups)), size = 3, alpha = 0.75) +
  facet_wrap(~ Treatment) +
  theme_bw() +
  scale_shape_manual(values = c(16, 15, 17, 18, 21), name = "Treatment\ngroups") +
  scale_x_continuous(limits = c(10, 60), breaks = seq(10, 60, 10)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", 
                                "#E6AB02", "steelblue"), name = "Treatment\ngroups") +
  labs(x = "Total sample size", y = "Power (1 - beta error probability)",
       title = "G*Power analysis for the standard hot plate assay",
       subtitle = "Based on an alpha value of 0.05") +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 14, colour = "black"),
        strip.text.x = element_text(size = 14)) +
  geom_abline(slope = 0, intercept = 0.8, lty = 3, colour = "black", alpha = 0.75)
