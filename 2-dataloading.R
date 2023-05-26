## Load dataset

alldata <- read.csv("Data/Full Data.csv")

colnames(alldata)
testdata <- alldata[, c(1,14:23)]
write.csv(testdata, "Cleaned Data/PHQ-9 Scores.csv", row.names = FALSE)

colnames(alldata)
ysrdata <- alldata[, c(1,27:38)]

summary(as.factor(ysrdata$WithDep.class))
summary(as.factor(ysrdata$AnxDep.class))

write.csv(ysrdata, "Cleaned Data/YSR Scores.csv", row.names = FALSE)

item = c("Anhedonia", "Depressed Mood", "Sleep Changes", "Loss of Energy", "Appetite Changes", "Low Self-Esteem", "Trouble Concentrating", "Psychomotor Changes", "Suicide Ideation")