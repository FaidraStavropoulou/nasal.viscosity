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
