### Libraries
library(ggplot2)
library(sandwich)
library(lmtest)


### Load and merge data

data_randomiz <- read.csv("./Scenario/data/randomization.csv", header =TRUE)
data_subject <-read.csv("./Scenario/data/subject.csv", header=TRUE)
data_efficacy <-read.csv("./Scenario/data/efficacy.csv", header = TRUE)
data <- merge(data_randomiz, data_subject, by ="subject")
data <- merge(data, data_efficacy, by ="subject")

# correct data type
cols_to_factor <- c("arm","country","eye.colour","tissue.use")
data[cols_to_factor] <- lapply(data[cols_to_factor] , factor)


### Descriptive statistics

# categorical variables
table(data$arm, useNA="ifany")
table(data$arm, data$tissue.use, useNA="ifany")
table(data$arm, data$country, useNA="ifany")
table(data$arm, data$eye.colour, useNA="ifany")

# continuous variables
apply(data[!(colnames(data) %in% c(cols_to_factor, "subject"))],2,summary)

# plots per trial arm
df <- data.frame(prop.table(table(data$tissue.use,data$arm)))
names(df) <- c("tissue.use","arm","count")
ggplot(data=df, aes(x= tissue.use, y=count, fill=arm)) + geom_bar(stat="identity", position = "dodge")

df<-data.frame(prop.table(table(data$previous.year,data$arm)))
names(df) <- c("previous.year","arm","count")
ggplot(data=df, aes(x= previous.year, y=count, fill=arm)) + geom_bar(stat="identity", position = "dodge")

df<-data.frame(prop.table(table(data$nosebleeds,data$arm)))
names(df) <- c("nosebleeds","arm","count")
ggplot(data=df, aes(x= nosebleeds, y=count, fill=arm)) + geom_bar(stat="identity", position = "dodge")

ggplot(data=data, aes(x = arm, y = mucus.viscosity)) + geom_boxplot()


### Primamry hypothesis: Treatment effect

# poisson test for ratio 1 (unadjusted)
counts <-  aggregate(data$nosebleeds, by = list(data$arm), sum)$x
timeatrisk <- aggregate(data$duration, by =list(data$arm), sum)$x

p.test <- poisson.test(counts, timeatrisk, r=1, alternative ="two.sided", conf.level = 0.95)
p.test

# poisson regression (unadjusted)
pois.model.unadj <- glm(nosebleeds ~ 1 + arm, offset = log(duration), data=data, family = poisson(link=log))
summary(pois.model.unadj)
coeftest(pois.model.unadj, vcov = sandwich)

# poisson regression (adjusted at baseline)
pois.model.adj <- glm(nosebleeds ~ 1 + previous.year + arm, offset = log(duration), data=data, family = poisson(link=log))
summary(pois.model.adj)
coeftest(pois.model.adj, vcov = sandwich)

mean(data$nosebleeds)
var(data$nosebleeds)

# quasi-poisson regression (unadjusted)
qpois.model.unadj <- glm(nosebleeds ~ 1 + arm, offset = log(duration), data=data, 
                         family = quasipoisson(link=log))
summary(qpois.model.unadj) # dispersion parameter = 1.7 -> need to adjust se for dispersion

# quasi-poisson regression (adjusted)
qpois.model.adj <- glm(nosebleeds ~ 1 + previous.year + arm, offset = log(duration), data=data, 
                       family = quasipoisson(link=log))
summary(qpois.model.adj)


### Exploratory analysis

## Subgroup analysis: effect modification tissue::treatment

# tissue - arm interaction
qpois.model.tissue.arm.interact <- glm(nosebleeds ~ 1 + previous.year + tissue.use*arm, 
                                       offset = log(duration), 
                                       data=data, 
                                       family = quasipoisson(link=log))
summary(qpois.model.tissue.arm.interact) # treatment lost significance

# tissue
qpois.model.tissue <- glm(nosebleeds ~ 1 + previous.year + tissue.use, offset = log(duration), 
                          data=data, family = quasipoisson(link=log))
summary(qpois.model.tissue)

# tissue - arm main effects
qpois.model.tissue.arm <- glm(nosebleeds ~ 1 + previous.year + tissue.use + arm, offset = log(duration), 
                              data=data, family = quasipoisson(link=log))
summary(qpois.model.tissue.arm)

# test for interaction term
anova(qpois.model.tissue.arm, qpois.model.tissue.arm.interact, test="Chisq")
ggplot(data= data, aes(x = arm, color = tissue.use, group = tissue.use, y = nosebleeds)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")


## Predictive biomarker: mucus viscosity

# model with biomarker only
qpois.model.viscosity.active <- glm(nosebleeds ~ 1 + previous.year + mucus.viscosity, 
                                    offset = log(duration), 
                                    data=data, subset = which(data$arm=="ACTIVE"),
                                    family = quasipoisson(link=log))
summary(qpois.model.viscosity.active)

# main effects biomarker and treatment 
qpois.model.viscosity.arm <- glm(nosebleeds ~ 1 + previous.year + mucus.viscosity + arm, 
                                 offset = log(duration), 
                                 data=data, 
                                 family = quasipoisson(link=log))
summary(qpois.model.viscosity.arm)

# interaction biomarker::treatment
qpois.model.viscosity.interact <- glm(nosebleeds ~ 1 + previous.year + mucus.viscosity*arm, 
                                 offset = log(duration), 
                                 data=data, 
                                 family = quasipoisson(link=log))
summary(qpois.model.viscosity.interact)

qplot(x = mucus.viscosity, y = nosebleeds, data = data, color = arm) +
  geom_smooth(method = "lm") 






