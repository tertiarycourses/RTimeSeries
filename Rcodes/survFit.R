library(survival)
library(survminer)

aml=read.csv(file.choose())
View(aml)
names(aml)
hist(aml$time)
plot(density(aml$time))
table(aml$cens)
table(aml$group)

# reusable survival objects

### censored

recsurv=Surv(aml$time,aml$cens)
recsurv  # + indicates censored data

# survfit fits survival curve with variaous methods
# kaplein - meier curve

fit_KM = survfit(recsurv~1, type="kaplan-meier", conf.type="log-log")
plot(fit_KM, xlab="time", ylab="prop")

# print restricted means
print(fit_KM, print.rmean=TRUE)   # restricted mean 18 are useful info the rest are censored

# cumulative events
plot(fit_KM, fun="event")  # opposite of survival (18 events)
                           # how many get the condition

### compare across the two groups
fit_KM2 = survfit(recsurv~group, data=aml)
ggsurvplot(fit_KM2, data =aml, pval = TRUE)


## Cox proportional hazard model

fit1 = coxph(recsurv ~ group, data=aml)
fit1

ggforest(fit1, data = aml)

# hazard ratios (HR) which are derived from the model for all covariates 
# that we included in the formula in coxph. 
# Briefly, an HR > 1 indicates an increased risk of death 
# (according to the definition of h(t)) if a specific condition is met by a patient. 
# An HR < 1, on the other hand, indicates a decreased risk. 


cox.zph(fit1)
plot(cox.zph(fit1))

########## example 2

View(ovarian)
ov2=ovarian
ov2$ecog.ps=factor(ov2$ecog.ps)
ov2$rx=factor(ov2$rx)
ov2$resid.ds=factor(ov2$resid.ds)

hist(ovarian$age) 

######3 lets create our own column

ov2$ageGp = ifelse (ov2$age >= 50, "old", "young")
ov2$ageGp =factor(ov2$ageGp)


surv_object <- Surv(time = ov2$futime, event = ov2$fustat)
surv_object 

fit1 <- survfit(surv_object ~ rx, data = ov2)
summary(fit1)
ggsurvplot(fit1, data = ov2, pval = TRUE)

# The log-rank p-value of 0.3 indicates a non-significant result 
# if you consider p < 0.05 to indicate statistical significance.


fit2 <- survfit(surv_object ~ resid.ds, data = ov2)
summary(fit2)
ggsurvplot(fit2, data = ov2, pval = TRUE)


fit3 <- survfit(surv_object ~ ageGp, data = ov2)
summary(fit3)
ggsurvplot(fit3, data = ov2, pval = TRUE)


fit2 <- coxph(surv_object ~ rx + resid.ds + ageGp + ecog.ps, 
                   data = ov2)
fit2

ggforest(fit2, data = ov2)

cox.zph(fit2)
plot(cox.zph(fit2))


### survival datasets
# bladder, cancer, colon, heart, lukemia, lung, rats, transplant, retinopathy, ovarian
