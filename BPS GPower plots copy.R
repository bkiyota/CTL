BPS_G_power_plots$Treatment <-  factor(BPS_G_power_plots$Treatment, ordered = T, 
       levels = c("pure THC", "Morphine"))
RHP_G_power <- BPS_G_power_plots 

ggplot(RHP_G_power, aes(x = Sample_size, y = logRHP, group = Groups)) +
  geom_point(aes(pch = factor(Groups), colour = factor(Groups)), size = 3, alpha = 0.75) +
  facet_grid(alpha ~ Treatment) +
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

#RHP final plot for alpha = 0.05

ggplot(RHP_G_power, aes(x = Sample_size, y = logRHP, group = Groups)) +
  geom_point(aes(pch = factor(Groups), colour = factor(Groups)), size = 3, alpha = 0.75) +
  facet_grid(alpha ~ Treatment) +
  theme_bw() +
  scale_shape_manual(values = c(16, 15, 17, 18, 21), name = "Treatment\ngroups") +
  scale_x_continuous(limits = c(10, 80), breaks = seq(10, 80, 10)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", 
                                "#E6AB02", "steelblue"), name = "Treatment\ngroups") +
  xlab("Total sample size") +
  ylab(expression("Power (1 - "*beta*" error probability)")) +
  labs(title = "G*Power analysis: Ramped hot plate (log-transformed)",
       subtitle = expression("Rows determined by "*alpha*" value; Effect size: THC ("*omega^2*" = 0.81), morphine ("*omega^2*" = 0.78)")) +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 14, colour = "black"),
        strip.text.x = element_text(size = 14, colour = "black"),
        strip.text.y = element_text(size = 14, colour = "black"),
        strip.background = element_rect(fill = alpha("paleturquoise4", 0.5))) +
  geom_abline(slope = 0, intercept = 0.8, lty = 3, colour = "black", alpha = 0.75) +
  geom_abline(slope = 0, intercept = 0.9, lty = 1, colour = "black", alpha = 0.75)





#RHP plot for alpha = 0.01

RHP_G_power_.01 <- BPS_G_power_plots %>%
  filter(alpha == 0.01)

ggplot(RHP_G_power_.01, aes(x = Sample_size, y = RHP, group = Groups)) +
  geom_point(aes(pch = factor(Groups), colour = factor(Groups)), size = 3, alpha = 0.75) +
  facet_wrap(~ Treatment) +
  theme_bw() +
  scale_shape_manual(values = c(16, 15, 17, 18, 21), name = "Treatment\ngroups") +
  scale_x_continuous(limits = c(10, 80), breaks = seq(10, 80, 10)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", 
                                "#E6AB02", "steelblue"), name = "Treatment\ngroups") +
  xlab("Total sample size") +
  ylab(expression("Power (1 - "*beta*" error probability)")) +
  labs(title = " ",
       subtitle = expression("Based on "*alpha*" = 0.01; Effect size: THC ("*omega^2*" = 0.88), morphine ("*omega^2*" = 0.77)")) +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 14, colour = "black"),
        strip.text.x = element_text(size = 14, colour = "black"),
        strip.background = element_rect(fill = alpha("paleturquoise4", 0.5))) +
  geom_abline(slope = 0, intercept = 0.8, lty = 3, colour = "black", alpha = 0.75) +
  geom_abline(slope = 0, intercept = 0.9, lty = 1, colour = "black", alpha = 0.75)

#SHP G*Power plots for alpha = 0.05

SHP_G_power_.05 <- BPS_G_power_plots %>%
  filter(alpha == 0.05)

ggplot(SHP_G_power_.05, aes(x = Sample_size, y = SHP, group = Groups)) +
  geom_point(aes(pch = factor(Groups), colour = factor(Groups)), size = 3, alpha = 0.75) +
  facet_wrap(~ Treatment) +
  theme_bw() +
  scale_shape_manual(values = c(16, 15, 17, 18, 21), name = "Treatment\ngroups") +
  scale_x_continuous(limits = c(10, 80), breaks = seq(10, 80, 10)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", 
                                "#E6AB02", "steelblue"), name = "Treatment\ngroups") +
  xlab("Total sample size") +
  ylab(expression("Power (1 - "*beta*" error probability)")) +
  labs(title = "G*Power analysis for the standard hot plate assay",
       subtitle = expression("Based on "*alpha*" = 0.05; Effect size: THC ("*omega^2*" = 0.71), morphine ("*omega^2*" = 0.78)")) +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 14, colour = "black"),
        strip.text.x = element_text(size = 14, colour = "black"),
        strip.background = element_rect(fill = alpha("paleturquoise4", 0.5))) +
  geom_abline(slope = 0, intercept = 0.8, lty = 3, colour = "black", alpha = 0.75) +
  geom_abline(slope = 0, intercept = 0.9, lty = 1, colour = "black", alpha = 0.75)

logSHP_G_power_.05 <- BPS_G_power_plots %>%
  filter(alpha == 0.05)

ggplot(SHP_G_power_.05, aes(x = Sample_size, y = logSHP, group = Groups)) +
  geom_point(aes(pch = factor(Groups), colour = factor(Groups)), size = 3, alpha = 0.75) +
  facet_wrap(~ Treatment) +
  theme_bw() +
  scale_shape_manual(values = c(16, 15, 17, 18, 21), name = "Treatment\ngroups") +
  scale_x_continuous(limits = c(10, 80), breaks = seq(10, 80, 10)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", 
                                "#E6AB02", "steelblue"), name = "Treatment\ngroups") +
  xlab("Total sample size") +
  ylab(expression("Power (1 - "*beta*" error probability)")) +
  labs(title = "G*Power analysis for the standard hot plate assay (log-transformed)",
       subtitle = expression("Based on "*alpha*" = 0.05; Effect size: THC ("*omega^2*" = 0.67), morphine ("*omega^2*" = 0.71)")) +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 14, colour = "black"),
        strip.text.x = element_text(size = 14, colour = "black"),
        strip.background = element_rect(fill = alpha("paleturquoise4", 0.5))) +
  geom_abline(slope = 0, intercept = 0.8, lty = 3, colour = "black", alpha = 0.75) +
  geom_abline(slope = 0, intercept = 0.9, lty = 1, colour = "black", alpha = 0.75)

#SHP plot for alpha = 0.01

SHP_G_power_.01 <- BPS_G_power_plots_copy
SHP_G_power_.01$alpha <- factor(SHP_G_power_.01$alpha, ordered = T, levels = c(0.05, 0.01))

ggplot(SHP_G_power_.01, aes(x = Sample_size, y = SHP, group = Groups)) +
  geom_point(aes(pch = factor(Groups), colour = factor(Groups)), size = 3, alpha = 0.75) +
  facet_grid(alpha ~ Treatment) +
  theme_bw() +
  scale_shape_manual(values = c(16, 15, 17, 18, 21), name = "Treatment\ngroups") +
  scale_x_continuous(limits = c(10, 80), breaks = seq(10, 80, 10)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", 
                                "#E6AB02", "steelblue"), name = "Treatment\ngroups") +
  xlab("Total sample size") +
  ylab(expression("Power (1 - "*beta*" error probability)")) +
  labs(title = " ",
       subtitle = expression("Rows determined by "*alpha*" value; Effect size: THC ("*omega^2*" = 0.71), morphine ("*omega^2*" = 0.78)")) +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 14, colour = "black"),
        strip.text.x = element_text(size = 14, colour = "black"),
        strip.background = element_rect(fill = alpha("paleturquoise4", 0.5))) +
  geom_abline(slope = 0, intercept = 0.8, lty = 3, colour = "black", alpha = 0.75) +
  geom_abline(slope = 0, intercept = 0.9, lty = 1, colour = "black", alpha = 0.75)

logSHP_G_power_.01 <- BPS_G_power_plots_copy
logSHP_G_power_.01$alpha <- factor(logSHP_G_power_.01$alpha, ordered = T, levels = c(0.05, 0.01))
logSHP_G_power_.01$Treatment <- factor(logSHP_G_power_.01$Treatment, ordered = T, levels = c("pure THC", "Morphine"))

ggplot(logSHP_G_power_.01, aes(x = Sample_size, y = logSHP, group = Groups)) +
  geom_point(aes(pch = factor(Groups), colour = factor(Groups)), size = 2, alpha = 0.75) +
  geom_line(aes(colour = factor(Groups))) +
  facet_grid(alpha ~ Treatment) +
  theme_bw() +
  scale_shape_manual(values = c(16, 15, 17, 18, 21), name = "Treatment\ngroups") +
  scale_x_continuous(limits = c(10, 80), breaks = seq(10, 80, 10)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", 
                                "#E6AB02", "steelblue"), name = "Treatment\ngroups") +
  xlab("Total sample size") +
  ylab(expression("Power (1 - "*beta*" error probability)")) +
  labs(title = "G*Power analysis: Standard hot plate (log-transformed)",
       subtitle = expression("Rows determined by "*alpha*" value; Effect size: THC ("*omega^2*" = 0.67), morphine ("*omega^2*" = 0.71)")) +
  theme(text = element_text(size = 14, family = "Century Gothic"),
        axis.text = element_text(size = 14, colour = "black", family = "Century Gothic"),
        strip.text.x = element_text(size = 14, colour = "black", family = "Century Gothic"),
        strip.text.y = element_text(size = 14, colour = "black", family = "Century Gothic"),
        strip.background = element_rect(fill = alpha("seagreen", 0.75)),
        legend.text = element_text(size = 14, family = "Century Gothic")) +
  geom_abline(slope = 0, intercept = 0.8, lty = 3, colour = "black", alpha = 0.75) +
  geom_abline(slope = 0, intercept = 0.9, lty = 1, colour = "black", alpha = 0.75)

ggplot(logSHP_G_power_.01, aes(x = Sample_size, y = logRHP, group = Groups)) +
  geom_point(aes(pch = factor(Groups), colour = factor(Groups)), size = 2, alpha = 0.75) +
  geom_line(aes(colour = factor(Groups))) +
  facet_grid(alpha ~ Treatment) +
  theme_bw() +
  scale_shape_manual(values = c(16, 15, 17, 18, 21), name = "Treatment\ngroups") +
  scale_x_continuous(limits = c(10, 80), breaks = seq(10, 80, 10)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", 
                                "#E6AB02", "steelblue"), name = "Treatment\ngroups") +
  xlab("Total sample size") +
  ylab(expression("Power (1 - "*beta*" error probability)")) +
  labs(title = "G*Power analysis: Ramped hot plate (log-transformed)",
       subtitle = expression("Rows determined by "*alpha*" value; Effect size: THC ("*omega^2*" = 0.81), morphine ("*omega^2*" = 0.78)")) +
  theme(text = element_text(size = 14, family = "Century Gothic"),
        axis.text = element_text(size = 14, colour = "black", family = "Century Gothic"),
        strip.text.x = element_text(size = 14, colour = "black", family = "Century Gothic", face = "bold"),
        strip.text.y = element_text(size = 14, colour = "black", family = "Century Gothic", face = "bold"),
        strip.background = element_rect(fill = alpha("tomato3", 0.75)),
        legend.text = element_text(size = 14, family = "Century Gothic")) +
  geom_abline(slope = 0, intercept = 0.8, lty = 3, colour = "black", alpha = 0.75) +
  geom_abline(slope = 0, intercept = 0.9, lty = 1, colour = "black", alpha = 0.75)


### Combined G*Powah


#### So, I need a factored column, with each factored unit being 455 long; i.e. as long as 
#### the dataset for one drug using one protocol

x_1 <- rep(1, 455)
x_2 <- rep(2, 455)
x_3 <- rep(3, 455)
x_4 <- rep(4, 455)

binded_x_ <- cbind(x_1,
                   x_2,
                   x_3,
                   x_4) 

binded_df <- as.data.frame(binded_x_)
melted_df <- melt(binded_df) %>% 
  dplyr::select(-variable)
melted_df$value <- factor(melted_df$value, levels = c(1, 2, 3, 4))

Combined_power_plot <- logSHP_G_power_.01 %>%
  filter(alpha == 0.05) %>%
  dplyr::select(-SHP, -RHP, -alpha) %>%
  gather(key = "Protocol", value = "Value", -Sample_size, -Groups, -Treatment) %>%
  separate(Protocol, into = c("empty", "Protocol"), sep = "log") %>%
  dplyr::select(-empty) %>%
  mutate(Protocol = gsub("SHP", "Standard\nhot plate", Protocol),
         Protocol = gsub("RHP", "Ramped\nhot plate", Protocol))

Combined_power_plot$Protocol <- factor(Combined_power_plot$Protocol,
                                       levels = c("Standard\nhot plate", "Ramped\nhot plate"))

combined_melt <- cbind(Combined_power_plot, melted_df)
Power_plot <- as.data.frame(combined_melt)
Power_plot_2 <- Power_plot %>%
  mutate(line_SHP_THC = ifelse(value == 1, 32, NA),
         line_RHP_THC = ifelse(value == 2, 28, NA),
         line_SHP_morphine = ifelse(value == 3, 23, NA),
         line_RHP_morphine = ifelse(value == 4, 25, NA)) %>%
  gather(key = "factored_lines", value = "FLine",
         -Sample_size, -Groups, -Treatment, -Protocol, -Value, -value)

Power_plot_2$FLine <- factor(Power_plot_2$FLine, levels = c("32", "28", "23", "25"))

vline_df <- data.frame(z = levels(Power_plot_2$Treatment),
                                  vl = c(32, 28, 23, 25))

# This is for grouping of the data in order to allow for lines specific to each panel

mean.data <- aggregate(x = Power_plot_2$Value, # use the y values
                       by = Power_plot_2[c("Protocol", "Treatment")], # group by Group and then by Subgroup
                       FUN = function(x) {
                         signif(mean(x), 4) # calculate the mean keep 4 significant numbers
                       }
)

colnames(mean.data) <- c("Protocol", "Treatment", "Average")




Gpower_plot <- ggplot(Power_plot_2, aes(x = Sample_size, y = Value, group = Groups)) +
  geom_point(aes(pch = factor(Groups)), colour = "black", size = 2, alpha = 0.25) +
  geom_point(data = subset(Power_plot_2, Groups == 5), aes(x = Sample_size, y = Value), 
             colour = "red", pch = 18, alpha = 0.5, size = 2) +
  geom_line(colour = "black", alpha = 0.5) +
  geom_line(data = subset(Power_plot_2, Groups == 5), aes(x = Sample_size, y = Value), 
             colour = "red", alpha = 0.5) +
  facet_grid(Protocol ~ Treatment) +
  theme_bw() +
  scale_shape_manual(values = c(16, 15, 17, 18, 21), name = "Treatment\ngroups") +
  scale_x_continuous(limits = c(10, 80), breaks = seq(10, 80, 10)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", 
                                "#E6AB02", "steelblue"), name = "Treatment\ngroups") +
  xlab("Total sample size") +
  ylab(expression("Power (1 - "*beta*" error probability)")) +
  theme(text = element_text(size = 14, family = "Century Gothic"),
        axis.text = element_text(size = 14, colour = "black", family = "Century Gothic"),
        strip.text.x = element_text(size = 14, colour = "white", family = "Century Gothic", face = "bold"),
        strip.text.y = element_text(size = 14, colour = "black", angle = 0,
                                    family = "Century Gothic", face = "bold"),
        strip.background = element_rect(fill = alpha("royalblue4", 0.75)),
        legend.text = element_text(size = 14, family = "Century Gothic")) +
 geom_vline(data = mean.data, mapping = aes(xintercept = c(32, 23, 28, 25)),
             alpha = 0.75, size = .5) +
  geom_abline(slope = 0, intercept = 0.8, lty = 3, colour = "black", size = 0.5, alpha = 0.75)


# This is going to try aND change the background colour
g <- ggplot_gtable(ggplot_build(Gpower_plot))
stripr <- which(grepl('strip-r', g$layout$name))
fills <- alpha(c("seagreen", "tomato3"), 0.75)
k <- 1
for (i in stripr) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g)

