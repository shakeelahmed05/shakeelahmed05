library(devtools)
#install.packages("sf")  
Data=read.csv(file.choose())
str(Data)
#install.packages("lme4")
#install.packages("tidyverse")
#install.packages("afex")
#install.packages("Matrix")
#install.packages("ROCR")
library(ggplot2)
library(lme4)
library(tidyverse)
library(afex)
library(Matrix)
#install.packages("merTools")
library(merTools)
head(Data)
Data$modality <- ifelse(Data$modality == "Audio-only", 0, 1)
rt_full.mod <- lmer(RT ~ 1 + modality + 
                      (1|PID) + (1|stim), 
                    data = Data)
summary(rt_full.mod)
predicted <- predict(rt_full.mod)
df <- data.frame(Observed = Data$RT,
                 Predicted = predicted)
ggplot(df, aes(x = Observed, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 3, slope = 1,
              linetype = "dashed") +
  xlab("Observed Time") +
  ylab("Predicted Time")

qplot(PID, RT,  
      colour = PID, geom = "boxplot", data= Data)
AIC(rt_full.mod)
predict(rt_full.mod, newdata = Data[1:10, ]) ##10 predicted values
predictInterval(rt_full.mod, newdata = Data[1:10, ])## Prediction interval
predictInterval(rt_full.mod, newdata = Data[1:10, ], n.sims = 500, level = 0.9,
                stat = 'median') ##
feSims <- FEsim(rt_full.mod, n.sims = 100)
head(feSims)  ####The easiest are getting the posterior distributions of both fixed and random effect parameters.
plotFEsim(FEsim(rt_full.mod, n.sims = 100), level = 0.9, stat = 'median', intercept = FALSE)
reSims <- REsim(rt_full.mod, n.sims = 100)
head(reSims)
##caterpillar plots for the random-effect terms:
plotREsim(REsim(rt_full.mod, n.sims = 100), stat = 'median', sd = TRUE)
#Note that plotREsim highlights group levels 
#that have a simulated distribution that does not overlap 0 – these appear darker. The lighter bars represent grouping 
#levels that are not distinguishable from 0 in the data.
ranks <- expectedRank(rt_full.mod, groupFctr = "stim")
head(ranks)

impSim <- REimpact(rt_full.mod, Data[7, ], groupFctr = "stim", breaks = 5,
                   n.sims = 300, level = 0.9)
impSim
ggplot(impSim, aes(x = factor(bin), y = AvgFit, ymin = AvgFit - 1.96*AvgFitSE,
                   ymax = AvgFit + 1.96*AvgFitSE)) +
  geom_pointrange() + theme_bw() + labs(x = "Bin of `stim` term", y = "Predicted Fit")
