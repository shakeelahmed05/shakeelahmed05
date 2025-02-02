library(survival) # load it. You can also
library(help=survival) # see the list of available functions and data sets.
library(ggplot2)##For Survival Curve
library(ggfortify)#For Survival Curve
##load or creat your own data
stime <- c(2.2, 3, 8.4, 7.5)
status <- c(1,0,1,0)
Surv(stime, status)
library("survminer")
data("lung")
head(lung)

##Fitting KM Survival Curve for overall data
fit0 <- survfit(Surv(time, status) ~ 1, data = lung)
print(fit0)
#Fitting KM Survival Curve by SEX
fit1<- survfit(Surv(time, status) ~ sex, data = lung)

print(fit1)
# Summary of survival curves
summary(fit0)
summary(fit1)
# Access to the sort summary table
summary(fit0)$table
summary(fit1)$table
##Create a data frame including Important Results
DF <- data.frame(time = fit0$time,
                 n.risk = fit0$n.risk,
                 n.event = fit0$n.event,
                 n.censor = fit0$n.censor,
                 surv = fit0$surv,
                 upper = fit0$upper,
                 lower = fit0$lower
)
head(DF)

##Ploting Survival Curve with with C.I
# Change color, linetype by strata, risk.table color by strata
ggsurvplot(fit1,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))


ggsurvplot(
  fit1,                     # survfit object with calculated statistics.
  pval = TRUE,             # show p-value of log-rank test.
  conf.int = TRUE,         # show confidence intervals for 
  # point estimaes of survival curves.
  conf.int.style = "step",  # customize style of confidence intervals
  xlab = "Time in days",   # customize X axis label.
  break.time.by = 200,     # break X axis in time intervals by 200.
  ggtheme = theme_light(), # customize plot and risk table with a theme.
  risk.table = "abs_pct",  # absolute number and percentage at risk.
  risk.table.y.text.col = T,# colour risk table text annotations.
  risk.table.y.text = FALSE,# show bars instead of names in text annotations
  # in legend of risk table.
  ncensor.plot = TRUE,      # plot the number of censored subjects at time t
  surv.median.line = "hv",  # add the median survival pointer.
  legend.labs = 
    c("Male", "Female"),    # change legend labels.
  palette = 
    c("#E7B800", "#2E9FDF") # custom color palettes.
)
summary(fit1)$table
quantile(fit1,probs=c(0.25,0.50,0.75))
head(lung)
Model0 <- coxph(Surv(time, status)~sex+age, data = lung)
summary(Model0)
covariates <- c("sex", "age", "ph.ecog", "ph.karno", "pat.karno", "meal.cal", "wt.loss")
univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(time, status)~', x)))

univ_models <- lapply( univ_formulas, function(x){coxph(x, data = lung)})
# Extract data 
univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         p.value<-signif(x$wald["pvalue"], digits=2)
                         wald.test<-signif(x$wald["test"], digits=2)
                         beta<-signif(x$coef[1], digits=2);#coeficient beta
                         HR <-signif(x$coef[2], digits=2);#exp(beta)
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(beta, HR, wald.test, p.value)
                         names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                       "p.value")
                         return(res)
                         #return(exp(cbind(coef(x),confint(x))))
                       })
res <- t(as.data.frame(univ_results, check.names = FALSE))
as.data.frame(res)
# Plot the baseline survival function
ggsurvplot(survfit(Model1), data=DF, color = "#2E9FDF",
           ggtheme = theme_minimal())

# Create the new data  
new_df <- data.frame(sex = c(1, 2), 
                       age=rep(mean(lung$age, na.rm = TRUE), 2))

##New data
fit_Model1 <- survfit(Model0, newdata = new_df)
ggsurvplot(fit_Model1, data=lung, conf.int = TRUE, legend.labs=c("sex=1", "sex=2"),
           ggtheme = theme_minimal())
ggforest(Model0)

# Test proportional hazards assumption

##The proportional hazards (PH) assumption can be checked using 
##statistical tests and graphical diagnostics based on the scaled Schoenfeld residuals.
##In principle, the Schoenfeld residuals are independent of time. 
##A plot that shows a non-random pattern against time is evidence of violation of the PH assumption.
cox_zph <- cox.zph(Model0)
cox_zph
# Plot Schoenfeld residuals
plot(cox_zph)
##The proportional hazard assumption is supported by a non-significant relationship between residuals and time, 
##and refuted by a significant relationship.
ggcoxzph(cox_zph)
ggforest(
  Model0,
  data = NULL,
  main = "Hazard ratio",
  cpositions = c(0.1, 0.2, 0.3),
  fontsize = 0.7,
  refLabel = "reference",
  noDigits = 2
)
