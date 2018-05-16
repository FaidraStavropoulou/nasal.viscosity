### Load and merge data

data_randomiz <- read.csv("./Scenario/data/randomization.csv", header =TRUE)
data_subject <-read.csv("./Scenario/data/subject.csv", header=TRUE)
data_efficacy <-read.csv("./Scenario/data/efficacy.csv", header = TRUE)
data <- merge(data_randomiz, data_subject, by ="subject")
data <- merge(data, data_efficacy, by ="subject")