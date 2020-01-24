THC_DR_data <- THC_dose_response_data_in_R %>%
  melt(id = c("Assay")) 

THC_DR_data <- THC_DR_data %>% dplyr::rename(Dose = variable)
THC_DR_data <- THC_DR_data %>% dplyr::rename(Effect = value)

THC_DR_data_v1 <- THC_DR_data %>%
  filter(!is.na(Effect)) %>% arrange(Assay)

SHP_THC_DR <- THC_DR_data_v1 %>%
  filter(Assay == "Standard hot plate")






SHPfit <- drm(Effect ~ Dose, data = SHP_DR, 
           fct = LL.4(names = c("Slope","Lower Limit","Upper Limit","ED50")))

pred.df <- expand.grid(Dose = exp(seq(log(max(THC_DR_data_v1$Dose)),
                                      log(min(THC_DR_data_v1$Dose)),length=100))) 

pred <- predict(fit,newdata=pred.df,interval="confidence") 
pred.df$p <- pred[,1]
pred.df$pmin <- pred[,2]
pred.df$pmax <- pred[,3]

ggplot(THC_DR_data_v1, aes(x = Dose, y = Effect)) +
  geom_point() + 
  geom_line(data = pred.df, aes(x = Dose,y = p)) + 
  theme_bw() + 
  facet_wrap(~Assay, scales = "free_y") +
  scale_x_continuous(trans = "log10")






ggplot(THC_DR_data_v1, aes(x = Dose, y = Effect)) +
  geom_point(alpha = 0.5, size = 2) +
  stat_function(fun = function(x){
    drm_y=function(x, drm){
      coef(drm)[2]+((coef(drm)[3]-coef(drm)[2])/(1+exp((coef(drm)[1]*(log(x)-log(coef(drm)[4]))))))
    }
    + drm_y(x,drm = drm(data = THC_DR_data_v1, Effect ~ Dose, Assay, fct=LL.4(), na.action = na.omit))
  }) +
  scale_x_continuous(trans = "log10") + 
  facet_wrap(~Assay, scales = "free")



ggplot(THC_DR_data_v1, aes(x = Dose, y = Effect)) +
  geom_point(alpha = 0.5, size = 2) +
  stat_function(fun = function(x){
    drm_y=function(x, drm){
      coef(drm)[2]+((coef(drm)[3]-coef(drm)[2])/(1+exp((coef(drm)[1]*(log(x)-log(coef(drm)[4]))))))
    }
    + drm_y(x,drm = drm(data = THC_DR_data_v1, Effect ~ Dose, Assay, fct=LL.4(), na.action = na.omit))
  }) +
  scale_x_continuous(trans = "log10") + 
  facet_wrap(~Assay, scales = "free")
  


SHP_DR <- THC_DR_data_v1 %>% filter(Assay == "Standard hot plate") 
  ggplot(SHP_DR, aes(x = Dose, y = Effect)) +
  geom_point() +
  stat_function(fun = function(x){
    drm_y=function(x, drm){
      coef(drm)[2]+((coef(drm)[3]-coef(drm)[2])/(1+exp((coef(drm)[1]*(log(x)-log(coef(drm)[4]))))))
    }
    + drm_y(x,drm = drm(data = SHP_DR, Effect ~ Dose, 
                        fct=LL.4(), na.action = na.omit))
  }) + 
    scale_x_continuous(trans = "log10")



ggpairs(THC_DR_data_v1, mapping = aes(x = Dose, y = Effect, colour = Assay))

ggpairs(CTL_controls_wide_layout, 
        diag = list(continuous = "barDiag"),
        lower = list(continuous = "smooth"))

ggpairs(THC_data_all_assays_wide_format, aes(x = Dose), 
        lower = list(continuous = my_fn))