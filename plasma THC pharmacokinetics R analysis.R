plasma_data <- Plasma_literature_data_Huestis_1992 %>% 
  melt(id = c("time_min", "time_hours", "THC_content", "Subject", "Smoking period", "log_time_min"))

plasma_data <- plasma_data %>% dplyr::rename(Cannabinoid = variable)
plasma_data <- plasma_data %>% dplyr::rename(Concentration = value)
plasma_data <- plasma_data %>% dplyr::rename(smoking_period = 'Smoking period')


ggplot(plasma_data, aes(x = time_min, y = Concentration, colour = Cannabinoid)) +
  geom_point(aes(pch = factor(smoking_period))) +
  coord_trans(x = "log10") +
  scale_x_log10() +
  facet_wrap(~ THC_content)

ggplot(plasma_data, aes(x = log_time_min, y = Concentration, colour = Cannabinoid)) +
  geom_point(alpha = 0.5, size = 2) +
  stat_summary(fun.y = mean, geom = "line", aes(group = Cannabinoid), size = 1) +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", aes(group = Cannabinoid), alpha = 0.5) +
  scale_y_continuous(breaks = seq(-60, 360, 40)) +
  scale_x_continuous() +
  annotation_logticks(sides = "b") +
  facet_wrap(~ THC_content, ncol = 1, scales = "free_x") +
  theme_bw() +
  geom_abline(slope = 0, intercept = 0, lty = 3) +
  annotate("rect", xmin = 0, xmax = 1, ymin = -50, ymax = 360, fill = "darkgrey", alpha = 0.2) +
  coord_cartesian(ylim = c(-40, 340)) +
  labs(x = "Log time (minutes)", y = "Concentration (ng/ml)", 
       title = "Inhalation pharmacokinetics of THC and its metabolites",
       subtitle = "N = 6; The results from 1.75% and 3.55% THC cannabis cigarettes are depicted ") +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 14, colour = "black"),
        strip.text.x = element_text(size = 14))


plasma_data %>%
  filter(Cannabinoid == "THC") %>%
  ggplot(aes(x = log_time_min, y = Concentration)) +
  geom_point(aes(colour = Subject)) +
  geom_line(aes(colour = Subject)) +
  facet_wrap(~ THC_content)

plasma_data %>%
  filter(Cannabinoid == "THC") %>%
  ggplot(aes(x = log_time_min, y = Concentration)) +
  geom_point(aes(colour = factor(THC_content)), alpha = 0.5, size = 3) +
  geom_line(aes(colour = factor(THC_content))) +
  facet_wrap(~ Subject, ncol = 2, scales = "free_x") +
  scale_color_manual(values = c("orangered4", "cadetblue4"), 
                     name = "%w/w of\npotential THC in\ncannabis cigarette") +
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50)) +
  theme_bw() +
  annotation_logticks(sides = "b") +
  labs(x = "Log time (minutes)", y = "Concentration of THC (ng/ml)", 
       title = "Inhalation pharmacokinetics of THC in 6 individual patients (Huestis et al., 1992b)",
       subtitle = "N = 6; The results from 1.75% and 3.55% THC cannabis cigarettes are depicted (cigarette = ~900 mg)") +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 14, colour = "black"),
        strip.text.x = element_text(size = 14))

subject_max_THC_time <- plasma_data %>%
  filter(Cannabinoid == "THC" & time_min > 0) %>% 
  group_by(Subject, time_min) %>%
  summarize(max = max(Concentration))

plasma_data %>% filter(THC_content == 3.55 & Cannabinoid == "THC" & time_min > 9 & time_min < 60) %>%
  ggplot(aes(x = time_sec, y = Concentration)) +
  geom_point() +
  facet_wrap(~ Subject)

subject_B <- plasma_data %>% 
  filter(Subject == "B" & Cannabinoid == "THC" & THC_content == 3.55 & time_min >= 10 & time_min < 60) %>%
  select(time_min, Concentration)
ggplot(subject_B, aes(x = time_min, Concentration)) +
  geom_point() +
  stat_smooth()

subject_C <- plasma_data %>% 
  filter(Subject == "C" & Cannabinoid == "THC" & THC_content == 3.55 & time_min >= 9 & time_min < 60) %>%
  select(time_sec, Concentration)

subject_E <- plasma_data %>% 
  filter(Subject == "E" & Cannabinoid == "THC" & THC_content == 3.55 & time_min >= 9 & time_min < 60) %>%
  select(time_sec, Concentration)

subject_F <- plasma_data %>% 
  filter(Subject == "F" & Cannabinoid == "THC" & THC_content == 3.55 & time_min >= 10 & time_min < 60) %>%
  select(time_sec, Concentration)

subject_G <- plasma_data %>% 
  filter(Subject == "G" & Cannabinoid == "THC" & THC_content == 3.55 & time_min >= 9 & time_min < 60) %>%
  select(time_sec, Concentration)

subject_H <- plasma_data %>% 
  filter(Subject == "H" & Cannabinoid == "THC" & THC_content == 3.55 & time_min >= 5 & time_min < 60) %>%
  select(time_sec, Concentration)

#11-OH-THC analysis
##The plan is to fit this data with a curve and then subtract from the THC concentrations at the respective time points to
##to see if that will have some influence on the plateau in plasma concentrations

OH_THC_lm <- plasma_data %>% filter(Cannabinoid == "11-OH-THC" & time_min < 22.5)

OH_THC_lm %>% filter(THC_content == 3.55) %>%
ggplot(aes(x = time_sec, y = Concentration)) +
  geom_point() 

OHTHC_means <- OH_THC_lm %>% filter(time_sec > 0) %>%
  group_by(time_min) %>%
  summarize(mean = mean(Concentration))

OH_THC_lm %>% filter(time_sec > 0) %>%
  group_by(time_min) %>%
  summarize(mean = mean(Concentration)) %>%
  ggplot(aes(x = time_min, y = mean)) +
  geom_point()

OH_THC_lm_3.55 <- OH_THC_lm %>% filter(time_sec > 0 & THC_content == 3.55)


OH_THC_lm %>%filter(time_sec > 0) %>%
ggplot(aes(x = time_sec, y = mean)) +
  geom_point() +
  stat_smooth(method = "lm")


plasma_data <- plasma_data %>% mutate(time_sec = 60 * time_min)

OH_THC_lm_analysis <- lm(mean ~ time_sec, data = OH_THC_lm)
summary(OH_THC_lm_analysis)

plasma_data %>% 
  filter(Cannabinoid == "11-OH-THC" & time_sec > 0) %>%
  ggplot(aes(x = log_time_sec, y = Concentration)) +
  geom_point() +
  stat_smooth()

max_OH_THC <- plasma_data %>% 
  filter(Cannabinoid == "11-OH-THC" & time_sec > 0) %>% 
  group_by(time_min) %>%
  summarize(max = max(Concentration))




