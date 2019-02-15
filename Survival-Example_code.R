#### survival plot 
#install.packages("survminer")
library(survminer)
library(survival)

#? survival::lung 
# check censoring status

Lungfit <- survfit(Surv(time, status) ~ sex, data = lung)
plot(Lungfit, col=1:2)

ggsurvplot(Lungfit)

# customize plot area
ggsurvplot(Lungfit,  size = 1,  
           linetype = "strata", 
           risk.table = TRUE, fun='pct',
           risk.table.col = "strata", 
           break.time.by = 250, 
           xlab="Time in days",
           legend="bottom",
           legend.title="Sex",
           legend.labs=c("Male", "Female"),
           conf.int = TRUE, 
           pval = TRUE, 
           ggtheme=theme_gray() )

## check cumulative incidence (probability) of events ##


ggsurvplot(Lungfit,  size = 1,  
           linetype = "strata", 
           risk.table = TRUE, fun='event', ## change fun
           risk.table.col = "strata", 
           break.time.by = 250, 
           xlab="Time in days",
           legend="bottom",
           legend.title="Sex",
           legend.labs=c("Male", "Female"),
           conf.int = TRUE, 
           ggtheme=theme_gray() )

## make a forest plot of effects

lung$age <- ifelse(lung$age > 70, ">70","<= 70")
fit <- coxph( Surv(time, status) ~ sex + ph.ecog + age, data = lung)
fit

ggforest(fit)
