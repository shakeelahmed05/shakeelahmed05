
install.packages("lme4")
library(lme4)
library(readr)
DF<- read_csv("C:/Users/Admin/OneDrive - National University of Sciences & Technology/Desktop/DATA ANALYSIS WITH R/heart.csv")
head(DF)
model <- lmer(thalach ~ restecg+chol+age+sex+ (1 | thal),
              data = DF)
summary(model)
library(ggplot2)
predicted <- predict(model)
ggplot(DF, aes(x = thalach , y = predicted)) +
  geom_point() +
  geom_abline(intercept = 3, slope = 1,
              linetype = "dashed") +
  xlab("Observed MaxHr") +
  ylab("Predicted MaxHr")
# Load necessary package
library(lattice)
# Plot random effects
dotplot(ranef(model, condVar=TRUE))
##The plot shows the variability in intercepts across different thallium groups, 
##indicating how each group's intercept deviates from the overall average intercept.
