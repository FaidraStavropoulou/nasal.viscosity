### Libraries
library(ggplot2)

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
df <- data.frame(table(data$tissue.use,data$arm))
names(df) <- c("tissue.use","arm","count")
ggplot(data=df, aes(x= tissue.use, y=count, fill=arm)) + geom_bar(stat="identity", position = "dodge")

df<-data.frame(table(data$previous.year,data$arm))
names(df) <- c("previous.year","arm","count")
ggplot(data=df, aes(x= previous.year, y=count, fill=arm)) + geom_bar(stat="identity", position = "dodge")

df<-data.frame(table(data$nosebleeds,data$arm))
names(df) <- c("nosebleeds","arm","count")
ggplot(data=df, aes(x= nosebleeds, y=count, fill=arm)) + geom_bar(stat="identity", position = "dodge")

ggplot(data=data, aes(x = arm, y = mucus.viscosity)) + geom_boxplot()


### Primamry hypothesis: Treatment effect

# poisson regression (unadjusted)
pois.model.unadj <- glm(nosebleeds ~ 1 + arm, offset = log(duration), data=data, family = poisson(link=log))
summary(pois.model.unadj)

# poisson regression (adjusted at baseline)
pois.model.adj <- glm(nosebleeds ~ 1 + previous.year + arm, offset = log(duration), data=data, family = poisson(link=log))
summary(pois.model.adj)

mean(data$nosebleeds)
var(data$nosebleeds)

