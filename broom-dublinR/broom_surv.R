library(survival)
library(dplyr)
library(ggplot2)
library(broom)

# Non-parametric survival estimator using no covariates (i.e. baseline survival curve)
kmfit <- survfit(Surv(time, status) ~ 1., data = lung)
glance(kmfit)
kmfit

# Semi-parametric Cox PH model using age and sex as predictors
cfit <- coxph(Surv(time, status) ~ age + sex, data = lung)
summary(cfit)
glance(cfit)

# Pass the Cox PH model to survfit, what does this curve represent?
# It plots a curve for a pseudo subject with covriate values equal to the mean of the covariate
# Users are strongly encouraged to use the newdata argument as the pseudo case can be nonsensical
sfit <- survfit(cfit, newdata = data.frame(age = 76, sex = 1))
sfit
glance(sfit)

tidy(kmfit)
tidy(sfit)
tidy(cfit)

ggplot(tidy(sfit), 
       aes(x = time, 
           y = estimate)) + 
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high), 
              alpha = 0.25)

			  
			  
dbplyr()