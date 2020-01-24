CTL_control_populations_all_assays$Controls <- as.numeric(CTL_control_populations_all_assays$Controls)

filtered_CTLcontrol_values <- CTL_control_populations_all_assays %>%
  filter(!is.na(Controls)) 

filtered_CTLcontrol_values %>%
  group_by(Assay) %>%
  summarise(count = n()) 

filtered_CTLcontrol_values %>%
  group_by(Assay) %>%
  CV(Controls)

CV <- function(mean, sd) {
  (sd/mean) * 100
}

ggplot(filtered_CTLcontrol_values, aes(x = "Controls", y = Controls)) +
  geom_point(pch = 19, size = 3, colour = "black", fill = "black", alpha = 0.25, 
             position = position_jitter()) +
  stat_summary(fun.y = mean, geom = "point", size = 3, colour = "red", alpha = 0.75) +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", 
               width = 0.25, colour = "red", alpha = 0.75, size = 1) +
  facet_wrap(~ Assay, scales = "free_y", ncol = 4) +
  labs(x = NULL, y = "Response values",
       title = "Control populations for all Cannevert assays") +
  geom_abline(slope = 0, intercept = 0, col = "black", lty = 3) +
  theme_bw() +
  theme(text = element_text(size = 14), axis.text = element_text(size = 14, colour = "black"))

booktabs()
tabular((Heading("Assay")*factor(Assay)) ~ (n = 1) + Format(digits = 2) *
                   (Heading("Statistics")*(Controls)) * 
                   (mean + sd), data = filtered_CTLcontrol_values)



