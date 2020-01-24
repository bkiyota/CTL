 C_knot_calculation <- plasma_data %>% filter(Cannabinoid == "THC" & THC_content == 3.55
                                              & time_min < 12 & time_min > 0) %>%
  select(Subject, Cannabinoid, time_sec, Concentration) %>%
  group_by(Subject, time_sec) 
 
Subject_pharmacokinetic_analysis %>% group_by(Subject) %>%
  summarize(average = mean(C_absolute_per_puff))
 
 ggplot(Subject_pharmacokinetic_analysis, aes(x = time_exhale_sec, y = C_absolute_per_puff)) +
   geom_point(size = 3, alpha = 0.75) + 
   stat_smooth(method = "loess") +
   facet_wrap(~ Subject, scales = "free_x") +
   scale_x_continuous(breaks = seq(0, 588, 84)) +
   scale_y_continuous(breaks = seq(0, 300, 100)) +
   labs(x = "Time (seconds)", y = "Peak THC concentration per puff (ng/ml)",
        title = "Estimated THC intake per puff (2 s inhale-10 s hold period)",
        subtitle = "Subjects (B, C, E, F, G, H) took 8 puffs from a 900 mg cigarette (3.55% THC) ; each puff was 84 s apart") +
   theme_bw() +
   theme(text = element_text(size = 14),
         axis.text = element_text(size = 14, colour = "black"),
         strip.text.x = element_text(size = 14)) +
   geom_abline(slope = 0, intercept = 0, lty = 3)
 
#Linear regression for individuals
 
B_Co <- Subject_pharmacokinetic_analysis %>% filter(Subject == "B") 
B_Co_lm <- lm(C_absolute_per_puff ~ time_exhale_sec, data = B_Co)
summary(B_Co_lm)

C_Co <- Subject_pharmacokinetic_analysis %>% filter(Subject == "C") 
C_Co_lm <- lm(C_absolute_per_puff ~ time_exhale_sec, data = C_Co)
summary(C_Co_lm)

E_Co <- Subject_pharmacokinetic_analysis %>% filter(Subject == "E") 
E_Co_lm <- lm(C_absolute_per_puff ~ time_exhale_sec, data = E_Co)
summary(E_Co_lm)

F_Co <- Subject_pharmacokinetic_analysis %>% filter(Subject == "F") 
F_Co_lm <- lm(C_absolute_per_puff ~ time_exhale_sec, data = F_Co)
summary(F_Co_lm)

G_Co <- Subject_pharmacokinetic_analysis %>% filter(Subject == "G") 
G_Co_lm <- lm(C_absolute_per_puff ~ time_exhale_sec, data = G_Co)
summary(G_Co_lm)

H_Co <- Subject_pharmacokinetic_analysis %>% filter(Subject == "H") 
H_Co_lm <- lm(C_absolute_per_puff ~ time_exhale_sec, data = H_Co)
summary(H_Co_lm)

 
 ggplot(Pharmacokinetic_model_based_on_actual_C_knots, aes(x = time_sec, y = Concentration)) +
   geom_line(aes(colour = Subject), alpha = 0.5, size = 1) +
   stat_summary(fun.y = mean, geom = "line", colour = "black", size = 1) +
   stat_summary(fun.data = mean_sdl, geom = "errorbar", colour = "black", alpha = 0.15) +
   theme_classic()
 
model_of_actual_C_knots <- Corrected_model_based_on_actual_C_knots %>% 
  melt(id = c("time_sec", "Puff"))

model_of_actual_C_knots <- model_of_actual_C_knots %>%
  dplyr::rename(Subject = variable)

model_of_actual_C_knots <- model_of_actual_C_knots %>%
  dplyr::rename(Concentration = value)

ggplot(model_of_actual_C_knots, aes(x = time_sec, y = Concentration, colour = Subject)) +
  geom_line(alpha = 0.75, size = 1) +
  stat_summary(fun.y = mean, geom = "line", colour = "black", alpha = 0.75, size = 1, lty = "twodash") +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", colour = "black", alpha = 0.12, width = 0.25) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")) +
  scale_x_continuous(breaks = seq(0, 1000, 100)) +
  scale_y_continuous(breaks = seq(0, 400, 50)) +
  geom_abline(slope = 0, intercept = 0, lty = 3, colour = "black") +
  labs(x = "Time (seconds)", y = "Concentration of THC (ng/ml)",
       title = "Cannabinoid pharmacokinetics for all subjects",
       subtitle = "8 puffs from a 900 mg cannabis cigarette (3.55% THC); 84 s between each peak") +
  theme_bw() +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 14, colour = "black"),
        strip.text.x = element_text(size = 14)) 









