ggplot(attempt_model_1, aes(x = time, y = C_1)) +
  geom_point(size = 0.2) +
  geom_line()

#linear regression on 11-OH-THC

ggplot(attempt_model_1, aes(x = time, y = OH_THC_lm)) +
  geom_point()

ggplot(attempt_model_1, aes(x = time, y = C_1_minus_OH_THC_lm)) +
  geom_point(size = 0.2) +
  geom_line()

#exponential one-phase association on 11-OH-THC

ggplot(attempt_model_1, aes(x = time, y = OH_THC_exponential)) +
  geom_point()

ggplot(attempt_model_1, aes(x = time, y = C_1_minus_OH_THC_exponential)) +
  geom_point(size = 0.2) +
  geom_point(mapping = aes(x = time, y = OH_THC_exponential), size = 0.2, alpha = 0.25, colour = "orangered4") +
  geom_point(mapping = aes(x = time, y = C_1), size = 0.2, alpha = 0.25, colour = "cadetblue4") +
  geom_line(mapping = aes(x = time, y = C_1), alpha = 0.25, colour = "cadetblue4") +
  geom_line() +
  geom_line(mapping = aes(x = time, y = THC_COOH_exponential)) +
  geom_line(mapping = aes(x = time, y = THC_actual_C_knots), colour = "green") +
  labs(x = "Time (seconds)",  y = "Concentration (ng/ml)",
       title = "Preliminary model of THC pharmacokinetics",
       subtitle = "Metabolism of THC into 11-OH-THC is taken into account (red line)") +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 100)) +
  scale_y_continuous(breaks = seq(0, 120, 20)) +
  theme_classic() +
  theme(text = element_text(size = 14), 
        axis.text = element_text(size = 14, colour = "black"))

final_model <- attempt_model_1 
ggplot(final_model) +
  geom_line(mapping = aes(x = time, y = Theoretical_THC_C_knots), colour = "cadetblue4", lty = "dotdash", size = 1, alpha = 0.5) +
  geom_line(mapping = aes(x = time, y = THC_actual_C_knots), colour = "orangered4", lty = "dotdash", size = 1, alpha = 0.5) +
  geom_line(mapping = aes(x = time, y = OH_THC_exponential), colour = "darkseagreen", lty = "dotdash", size = 1, alpha = 0.5) +
  geom_line(mapping = aes(x = time, y = THC_COOH_exponential), colour = "darkgreen", lty = "dotdash", size = 1, alpha = 0.5) +
  geom_line(mapping = aes(x = time, y = THC_metabolites), size = 1, colour = "black", alpha = 0.75) +
  geom_line(mapping = aes(x = time, y = theoretical_corrected), colour = "cadetblue4", size = 1, alpha = 0.75) +
  geom_line(mapping = aes(x = time, y = actual_corrected), colour = "orangered4", size = 1, alpha = 0.75) +
  scale_x_continuous(limits = c(0, 1020), breaks = seq(0, 1020, 60)) + 
  scale_y_continuous(breaks = seq(0, 200, 20)) +
  labs(x = "Time (minutes)", y = "Concentration (ng/ml)") +
  theme_classic() +
  theme(text = element_text(size = 14), 
        axis.text = element_text(size = 14, colour = "black")) 

final_model_v2 <- attempt_model_1 %>%
  select(time, OH_THC_exponential, THC_COOH_exponential, THC_metabolites, 
         THC_actual_C_knots, Theoretical_THC_C_knots,
         theoretical_corrected, actual_corrected) %>%
  melt(id = c("time", "OH_THC_exponential", "THC_COOH_exponential", "THC_metabolites",
              "THC_actual_C_knots", "actual_corrected"))
final_model_v2 <- final_model_v2 %>% dplyr::rename(theoretical = variable)
final_model_v2 <- final_model_v2 %>% dplyr::rename(theoretical_concentrations = value)

final_model_v3 <- final_model_v2 %>% 
  melt(id = c("time", "theoretical", "theoretical_concentrations", 
              "OH_THC_exponential", "THC_COOH_exponential", "THC_metabolites")) 
final_model_v3 <- final_model_v3 %>% dplyr::rename(actual = variable)
final_model_v3 <- final_model_v3 %>% dplyr::rename(actual_concentrations = value)

final_model_v4 <- final_model_v3 %>% 
  melt(id = c("time", "OH_THC_exponential", "THC_COOH_exponential", "THC_metabolites", "actual", "theoretical")) 
final_model_v4 <- final_model_v4 %>% dplyr::rename(model = variable)
final_model_v4 <- final_model_v4 %>% dplyr::rename(model_concentrations = value)

final_model_v5 <- final_model_v4 %>% 
  melt(id = c("time", "actual", "theoretical", "model", "model_concentrations")) 

final_model_v6 <- final_model_v5
levels(final_model_v5$model)[levels(final_model_v5$model)=="theoretical_concentrations"] <- "Theoretical"
levels(final_model_v5$model)[levels(final_model_v5$model)=="actual_concentrations"] <- "Actual"
head(final_model_v6)

ggplot(final_model_v5, aes(x = time/60, y = model_concentrations)) +
  geom_line(alpha = 0.7) +
  geom_line(mapping = aes(x = time/60, y = value, colour = variable), alpha = 0.75, size = 1) +
  facet_wrap(~ model) +
  scale_colour_manual(values = c("orangered4", "#7570B3", "cadetblue4"), 
                      labels = c("11-OH-THC", "THC-COOH", "THC metabolites"), name = "") +
  scale_y_continuous(breaks = seq(0, 160, 20)) +
  scale_x_continuous(limits = c(0, 17), breaks = seq(0, 20, 2)) +
  theme_bw() +
  labs(x = "Time (minutes)", y = "Concentration (ng/ml)", title = "THC concentrations corrected for metabolism",
       subtitle = "Concentrations of metabolites are subtracted from the modeled THC concentrations; shaded area depicts the magnitude of the correction") +
  theme(text = element_text(size = 14), 
        axis.text = element_text(size = 14, colour = "black"),
        strip.background = element_rect(fill = "#666666"),
        strip.text = element_text(colour = "white", face = "bold", size = 14))





