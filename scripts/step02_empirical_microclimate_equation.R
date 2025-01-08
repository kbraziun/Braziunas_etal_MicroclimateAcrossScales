#####
#
## Fit empirical microclimate equations
#
#####

### load libraries
library(tidyverse)
library(corrplot)
library(ggpubr)
library(car) # qqplot
library(lme4) # lmer
library(lmerTest) 
library(MuMIn) # r2lmm
library(ModelMetrics) # rmse

### selected sessionInfo()
# R version 4.3.2 (2023-10-31 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19044)
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] ModelMetrics_1.2.2.2 MuMIn_1.47.5         lmerTest_3.1-3       lme4_1.1-35.1       
# [5] Matrix_1.6-3         car_3.1-2            carData_3.0-5        ggpubr_0.6.0        
# [9] corrplot_0.92        lubridate_1.9.3      forcats_1.0.0        stringr_1.5.1       
# [13] dplyr_1.1.4          purrr_1.0.2          readr_2.1.4          tidyr_1.3.0         
# [17] tibble_3.2.1         ggplot2_3.4.4        tidyverse_2.0.0     

###
# 1. load data
###

# monthly offset and predictors
pred.month <- read.csv("processed_data/microclimate_data_prep/micro_macro_data_monthly_clean.csv")

summary(pred.month)

# quick plots
pred.month %>%
  ggplot(aes(x=month,y=buff_minT, group=month)) +
  facet_wrap(~data_source) +
  geom_boxplot() +
  theme_bw()

pred.month %>%
  ggplot(aes(x=month,y=buff_maxT, group=month)) +
  facet_wrap(~data_source) +
  geom_boxplot() +
  theme_bw()

###
# 2. evaluate predictor collinearity, representation, pairwise relationships between predictors and responses
###

### pearson's rank correlation
corr.pred <- pred.month %>%
  ungroup() %>%
  dplyr::select(-c(plot_id:buff_maxT, count)) %>% 
  cor(method="pearson")

corrplot(corr.pred, type="upper", diag=FALSE)

corr.abs <- abs(corr.pred)
corr.abs
# highly correlated: macro temp and phenology, evergreen/conifer/broadleaved share

# use anova to eval correlation 


### representation across studies
pred.month %>%
  dplyr::select(-c(plot_id,year:buff_maxT,count)) %>%
  pivot_longer(c(macro_minT:tpi)) %>%
  ggplot() +
  facet_wrap(~name, scales="free") +
  geom_density(aes(x=value, color=data_source,fill=data_source), alpha=0.4) +
  theme_bw()

pred.month %>%
  dplyr::select(-c(plot_id,year:buff_maxT,count)) %>%
  pivot_longer(c(macro_minT:tpi)) %>%
  ggplot() +
  facet_wrap(~name, scales="free") +
  geom_boxplot(aes(x=data_source, y=value, fill=data_source)) +
  theme_bw() 

# summary data
pred.month %>%
  group_by(data_source) %>%
  tally()

pred.summall <- pred.month %>%
  pivot_longer(c(micro_minT:tpi)) %>%
  group_by(name) %>%
  summarise(mean = mean(value), sd = sd(value),
            min = min(value), max = max(value)) %>%
  mutate(data_source = "all")
  
pred.month %>% 
  pivot_longer(c(micro_minT:tpi)) %>%
  group_by(data_source,name) %>%
  summarise(mean = mean(value), sd = sd(value),
            min = min(value), max = max(value)) %>%
  rbind(pred.summall) %>%
  write.csv("processed_data/microclimate_data_prep/var_mean_sd.csv",row.names=FALSE)

### pairwise relationships between predictors and responses
# minT
pred.month %>%
  # only look at subset of predictors
  pivot_longer(c(macro_minT,northness,tpi,lai,stol)) %>%
  ggplot(aes(x=value, y=buff_minT,color=data_source)) +
  facet_wrap(~name, scales="free_x",ncol=2) +
  geom_point(alpha=0.02) +
  geom_smooth(method="lm", se=FALSE) +
  stat_cor(
    method="spearman",cor.coef.name="rho",p.digits=NA, label.sep=""
  ) +
  theme_bw()

# maxT
pred.month %>%
  # only look at subset of predictors
  pivot_longer(c(macro_maxT,northness,tpi,lai,stol)) %>%
  ggplot(aes(x=value, y=buff_maxT,color=data_source)) +
  facet_wrap(~name, scales="free_x",ncol=2) +
  geom_point(alpha=0.02) +
  geom_smooth(method="lm", se=FALSE) +
  stat_cor(
    method="spearman",cor.coef.name="rho",p.digits=NA, label.sep=""
  ) +
  theme_bw()
# some inconsistencies among studies, difference in intercepts

###
# 3. fit models, check assumptions
###

### subset to observations and predictors
pred.mod <- pred.month %>%
  dplyr::select(c(buff_minT,buff_maxT,macro_minT,macro_maxT, lai,stol,northness,tpi,data_source,plot_id))

### evaluate variables for potential transformations
hist(pred.mod$buff_minT)
hist(pred.mod$buff_maxT) # a bit skewed
hist(pred.mod$macro_minT)
hist(pred.mod$macro_maxT)
hist(pred.mod$lai) # a bit skewed
hist(pred.mod$lai^(1/3))
hist(pred.mod$stol)
hist(pred.mod$northness) # peaks on either end
hist(pred.mod$tpi) # narrow peak

### min buffer
# simple lm
mod.min <- lm(buff_minT ~  northness + tpi + lai + stol + macro_minT, data=pred.mod)

# with transformation of lai
pred.transform <- pred.mod %>%
  mutate(lai=lai^(1/3))
mod.min <- lm(buff_minT ~  northness + tpi + lai + stol + macro_minT, data=pred.transform)

# check lm assumptions
plot(mod.min,1)  
plot(mod.min,3)
car::qqPlot(resid(mod.min)) 
plot(mod.min,5) 
summary(mod.min)

# check for residual trends among data sources
pred.mod$resid <- resid(mod.min)
pred.mod$fitted <- predict(mod.min)

ggplot(aes(x=fitted, y=resid), data=pred.mod) +
  geom_boxplot(aes(color=data_source)) +
  theme_bw()

# lmm, adding data_source as random effect
# first check collinearity with fixed effects
mod.min <- lm(buff_minT ~  northness + tpi + lai + stol + macro_minT + data_source, data=pred.mod)
vif(mod.min) # GVIF, interpret the squared scaled version similarly to vif
vif(mod.min)^2

# fit lmm
mod.min <- lmer(buff_minT ~  northness + tpi + lai + stol + macro_minT + (1|data_source), data=pred.mod,REML=FALSE)
# mod.min <- lmer(buff_minT ~  northness + tpi + lai + stol + macro_minT + (1|data_source), data=pred.transform,REML=FALSE)

# check lmm assumptions
fm1F <- fortify.merMod(mod.min)
ggplot(fm1F, aes(.fitted, .resid)) + geom_point(colour="black",shape=21) +
  geom_hline(yintercept=0, lty=2) +
  geom_smooth(method="loess",se=FALSE, linewidth=0.5,color="red") +
  ylab("Residuals") +
  xlab("Fitted") +
  ggtitle("Residuals vs. Fitted")+theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=10,hjust = 0.5,face="bold"),
        axis.title = element_text(size=10,color="black"),
        axis.text=element_text(size=8,color="black"))
ggplot(data.frame(resid(mod.min)), aes(sample = resid(mod.min))) +
  stat_qq(geom="point",shape=21) +
  stat_qq_line(color = "black",lty=2) +
  ylab("Sample Quantiles") +
  xlab("Theoretical Quantiles") +
  ggtitle("Normal Q-Q Plot")+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=10,hjust = 0.5,face="bold"),
        axis.title = element_text(size=10,color="black"),
        axis.text=element_text(size=8,color="black"))

summary(mod.min)
fixef(mod.min)
VarCorr(mod.min)

# check for residual trends among data sources
pred.mod$resid <- resid(mod.min)
pred.mod$fitted <- predict(mod.min)

ggplot(aes(x=fitted, y=resid), data=pred.mod) +
  geom_boxplot(aes(color=data_source)) +
  theme_bw()

### max buffer
# simple lm
mod.max <- lm(buff_maxT ~  northness + tpi + lai + stol + macro_maxT, data=pred.mod)

# with transformation of lai
pred.transform <- pred.mod %>%
  mutate(lai=lai^(1/3))
mod.max <- lm(buff_maxT ~  northness + tpi + lai + stol + macro_maxT, data=pred.transform)

# check lm assumptions
plot(mod.max,1)  
plot(mod.max,3)
car::qqPlot(resid(mod.max)) 
plot(mod.max,5) 
summary(mod.max)

# check for residual trends among data sources
pred.mod$resid <- resid(mod.max)
pred.mod$fitted <- predict(mod.max)

ggplot(aes(x=fitted, y=resid), data=pred.mod) +
  geom_boxplot(aes(color=data_source)) +
  theme_bw()

# lmm, adding data_source as random effect
# first check collinearity with fixed effects
mod.max <- lm(buff_minT ~  northness + tpi + lai + stol + macro_maxT + data_source, data=pred.mod)
vif(mod.max) # GVIF, interpret the squared scaled version similarly to vif
vif(mod.max)^2

# fit lmm
mod.max <- lmer(buff_maxT ~  northness + tpi + lai + stol + macro_maxT + (1|data_source), data=pred.mod,REML=FALSE)
# mod.max <- lmer(buff_maxT ~  northness + tpi + lai + stol + macro_maxT + (1|data_source), data=pred.transform,REML=FALSE)

# check lmm assumptions
fm1F <- fortify.merMod(mod.max)
ggplot(fm1F, aes(.fitted, .resid)) + geom_point(colour="black",shape=21) +
  geom_hline(yintercept=0, lty=2) +
  geom_smooth(method="loess",se=FALSE, linewidth=0.5,color="red") +
  ylab("Residuals") +
  xlab("Fitted") +
  ggtitle("Residuals vs. Fitted")+theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=10,hjust = 0.5,face="bold"),
        axis.title = element_text(size=10,color="black"),
        axis.text=element_text(size=8,color="black"))
ggplot(data.frame(resid(mod.max)), aes(sample = resid(mod.max))) +
  stat_qq(geom="point",shape=21) +
  stat_qq_line(color = "black",lty=2) +
  ylab("Sample Quantiles") +
  xlab("Theoretical Quantiles") +
  ggtitle("Normal Q-Q Plot")+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=10,hjust = 0.5,face="bold"),
        axis.title = element_text(size=10,color="black"),
        axis.text=element_text(size=8,color="black"))

summary(mod.max)
fixef(mod.max)
VarCorr(mod.max)

# check for residual trends among data sources
pred.mod$resid <- resid(mod.max)
pred.mod$fitted <- predict(mod.max)

ggplot(aes(x=fitted, y=resid), data=pred.mod) +
  geom_boxplot(aes(color=data_source)) +
  theme_bw()

###
# 4. final models, model fit and evaluations
###

summary(pred.month) # 7755 monthly observations
length(unique(pred.month$plot_id)) # 497 plots

summary(pred.month)

### final models
# min
mod.min <- lmer(buff_minT ~  northness + tpi + lai + stol + macro_minT + (1|data_source), data=pred.month,REML=FALSE)

r.squaredGLMM(mod.min) # r2c=0.07, r2m=0.24
rmse(pred.month$buff_minT,predict(mod.min)) # 1.4
coef(mod.min)
summary(mod.min)

# max
mod.max <- lmer(buff_maxT ~  northness + tpi + lai + stol + macro_maxT  + (1|data_source), data=pred.month,REML=FALSE)

r.squaredGLMM(mod.max) # r2c=0.29, r2m=0.47
rmse(pred.month$buff_maxT,predict(mod.max)) # 2.7
coef(mod.max)
summary(mod.max)

### plot evaluations
month.plot <- pred.month %>%
  mutate(buff_minT_pred = predict(mod.min),
         buff_maxT_pred = predict(mod.max)) %>%
  # check model equation
  mutate(buff_minT_ck = ifelse(data_source=="diazCalafat",
                               0.565738 - 0.02475312 *macro_minT + 0.2626848*northness + 0.0158236*tpi + 0.0227305*lai - 0.203076*stol,
                               ifelse(data_source=="formica",
                                      2.144732 - 0.02475312*macro_minT + 0.2626848*northness + 0.0158236*tpi + 0.0227305*lai -0.203076*stol,
                                      ifelse(data_source=="zellweger",
                                             1.661728 -0.02475312*macro_minT + 0.2626848  *northness + 0.0158236*tpi +0.0227305*lai -0.203076 *stol,NA))),
         buff_maxT_ck = ifelse(data_source=="diazCalafat",
                               2.555752 - 0.1932246*macro_maxT - 0.5728993*northness + 0.01399113  *tpi - 0.3947826*lai + 0.4419002*stol,
                               ifelse(data_source=="formica",
                                      -1.238956 - 0.1932246*macro_maxT - 0.5728993*northness + 0.01399113*tpi -0.3947826*lai + 0.4419002*stol,
                                      ifelse(data_source=="zellweger",
                                             1.613153-0.1932246*macro_maxT -0.5728993*northness + 0.01399113*tpi -0.3947826*lai + 0.4419002*stol,NA))),
         buff_minT_diff = buff_minT_ck - buff_minT_pred,
         buff_maxT_diff = buff_maxT_ck - buff_maxT_pred) %>%
  mutate(micro_minT_pred = macro_minT + buff_minT_pred,
         micro_maxT_pred = macro_maxT + buff_maxT_pred)
summary(month.plot) # passes checks

ann <- data.frame(r2m=c(round(r.squaredGLMM(mod.min)[1],2),
                       round(r.squaredGLMM(mod.max)[1],2)),
                  r2c=c(round(r.squaredGLMM(mod.min)[2],2),
                        round(r.squaredGLMM(mod.max)[2],2)),
                  rmse=c(round(rmse(pred.month$buff_minT,predict(mod.min)),1),
                  round(rmse(pred.month$buff_maxT,predict(mod.max)),1)),
                  lab=c("(a)","(b)")) %>%
  mutate(lab=paste0(lab,"~italic(R)[m]^2==",r2m,"*','~","italic(R)[c]^2==",r2c,"*','~","RMSE==",rmse,"~degree*C"))

# model fits
ggplot(aes(x=buff_minT,y=buff_minT_pred),data=month.plot) +
  geom_point() +
  geom_abline(slope=1,intercept=0) +
  geom_smooth(method="lm") +
  xlim(range(range(month.plot$buff_minT), range(month.plot$buff_minT_pred))) +
  ylim(range(range(month.plot$buff_minT), range(month.plot$buff_minT_pred))) +
  xlab(expression("Observed Tmin"["offset"]~"("*degree*"C)")) +
  ylab(expression("Predicted Tmin"["offset"]~"("*degree*"C)")) +
  geom_text(aes(x=-Inf,y=Inf, label=lab),parse=TRUE,data=ann[1,],size=8/2.834646,hjust=-0.02,vjust=1.2) +
  theme_bw()

month.plot %>%
  pivot_longer(c(micro_minT,macro_minT,micro_minT_pred)) %>%
  mutate(name = factor(name, levels=c("macro_minT","micro_minT","micro_minT_pred"))) %>%
  ggplot() +
  geom_boxplot(aes(x=factor(month),y=value,color=factor(name))) +
  ylab(expression("Minimum temperature ("*degree*"C)")) +
  scale_color_manual(values=c("red","blue","black"), name="", labels=c("Macroclimate","Microclimate","Predicted microclimate")) +
  geom_text(aes(x=-Inf,y=Inf, label="(c)"),parse=TRUE,data=ann[1,],size=8/2.834646,hjust=-0.2,vjust=1.2) +
  scale_x_discrete(labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul ","Aug","Sep","Oct","Nov","Dec")) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(angle=45,hjust=1.8,vjust=1.7))

ggplot(aes(x=buff_maxT,y=buff_maxT_pred),data=month.plot) +
  geom_point() +
  geom_abline(slope=1,intercept=0) +
  geom_smooth(method="lm") +
  xlim(range(range(month.plot$buff_maxT), range(month.plot$buff_maxT_pred))) +
  ylim(range(range(month.plot$buff_maxT), range(month.plot$buff_maxT_pred))) +
  xlab(expression("Observed Tmax"["offset"]~"("*degree*"C)")) +
  ylab(expression("Predicted Tmax"["offset"]~"("*degree*"C)")) +
  geom_text(aes(x=-Inf,y=Inf, label=lab),parse=TRUE,data=ann[2,],size=8/2.834646,hjust=-0.02,vjust=1.2) +
  theme_bw()

month.plot %>%
  pivot_longer(c(micro_maxT,macro_maxT,micro_maxT_pred)) %>%
  mutate(name = factor(name, levels=c("macro_maxT","micro_maxT","micro_maxT_pred"))) %>%
  ggplot() +
  geom_boxplot(aes(x=factor(month),y=value,color=factor(name))) +
  ylab(expression("Maximum temperature ("*degree*"C)")) +
  scale_x_discrete(labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul ","Aug","Sep","Oct","Nov","Dec")) +
  scale_color_manual(values=c("red","blue","black"), name="", labels=c("Macroclimate","Microclimate","Predicted microclimate")) +
  geom_text(aes(x=-Inf,y=Inf, label="(d)"),parse=TRUE,data=ann[1,],size=8/2.834646,hjust=-0.2,vjust=1.2) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(angle=45,hjust=1.8,vjust=1.7))
