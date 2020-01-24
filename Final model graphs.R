theoretical_model <- Model_based_on_theoretical_C_knots %>% 
  melt(id = c("time_sec", "Puff"))

theoretical_model <- theoretical_model %>% dplyr::rename(Subject = variable)
theoretical_model <- theoretical_model %>% dplyr::rename(Concentration = value)

ggplot(theoretical_model, aes(x = time_sec, y = Concentration, colour = Subject)) +
  geom_line(alpha = 0.75, size = 1) +
  stat_summary(fun.y = mean, geom = "line", colour = "black", alpha = 0.75, size = 1, lty = "twodash") +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", colour = "black", alpha = 0.12, width = 0.25) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "black")) +
  scale_x_continuous(breaks = seq(0, 1000, 100)) +
  scale_y_continuous(breaks = seq(0, 400, 50)) +
  geom_abline(slope = 0, intercept = 0, lty = 3, colour = "black") +
  labs(x = "Time (seconds)", y = "Concentration of THC (ng/ml)",
       title = "Theoretical cannabinoid pharmacokinetics for all subjects",
       subtitle = "8 puffs from a 900 mg cannabis cigarette (3.55% THC); 84 s between each peak") +
  theme_bw() +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 14, colour = "black"),
        strip.text.x = element_text(size = 14)) 



#nlme attempt
theoretical_model_no_avg <- theoretical_model %>%
  filter(Subject == "Average")

## factor puff variable
theoretical_model_no_avg$Puff <- factor(theoretical_model_no_avg$Puff, ordered = T, 
                                        levels = c("1", "2", "3", "4", "5", "6", "7", "8"))

lme_theoretical <- lme(Concentration ~  time_sec + Subject + Puff, 
                       data = theoretical_model_no_avg,
                       method = "ML",
                       na.action = "na.omit",
                       random = ~1|theoretical_model_no_avg$Subject)


compare_subjects_models <- Comparison_of_models %>%
  melt(id = c("time_sec", "Puff", "Model"))

compare_subjects_models <- compare_subjects_models %>% dplyr::rename(Subject = variable)
compare_subjects_models <- compare_subjects_models %>% dplyr::rename(Concentration = value)

ggplot(compare_subjects_models, aes(x = time_sec, y = Concentration)) +
  geom_line(aes(colour = Model), size = 1, alpha = 0.75) +
  scale_colour_manual(values = c("orangered4", "cadetblue4")) +
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50)) +
  stat_summary(fun.y = mean, geom = "line", colour = "black", lty = "twodash", alpha = 0.25, size = 1, aes(group = Model)) +
  facet_wrap(~ Subject, ncol = 2, scales = "free_x") +
  labs(x = "Time (seconds)", y = "Concentration of THC (ng/ml)",
       title = "Comparing the models of THC intake per inhalation") +
  theme_bw() +
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 14, colour = "black"),
        strip.text.x = element_text(size = 14)) 

ggplot(compare_subjects_models, aes(x = time_sec, y = Concentration, colour = Subject)) +
  geom_line(size = 1, alpha = 0.75) +
  facet_wrap(~ Model, ncol = 1, scales = "free_x") +
  scale_color_manual(values = c("dodgerblue3", "indianred2", "forestgreen", "darkorchid3", "goldenrod2", "slategray")) +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", colour = "black", alpha = 0.05) +
  theme_bw() +
  theme(text = element_text(size = 14), 
        axis.text = element_text(size = 14, colour = "black"),
        strip.text.x = element_text(size = 14)) +
  scale_y_continuous(breaks = seq(0, 350, 50)) +
  scale_x_continuous(breaks = seq(0, 1000, 100)) +
  labs(x = "Time (seconds)", y = "Concentration of THC (ng/ml)",
       title = "Human THC pharmacokinetic model on repeated inhalation (N = 6)",
       subtitle = "Subjects took 8 puffs from a 900 mg cannabis cigarette (3.55% THC); 2 s inhalation/10 s hold period/72 s rest period")







