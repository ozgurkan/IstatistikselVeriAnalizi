library(readxl)
SdA_HW <- read_excel("C:/Users/ozgur/Desktop/HW/SdA-HW.xls")
View(SdA_HW)

delete_empty_column <- SdA_HW[!sapply(SdA_HW, function (x) all(is.na(x) | x == ""))]
View(delete_empty_column)

install.packages("zoo")
update.packages("zoo")
library(zoo)
interpolation <- delete_empty_column
interpolation$tpthrt <- na.approx(interpolation$tpthrt, method="linear")
interpolation$pkthrt <- na.approx(interpolation$pkthrt, method="linear")
interpolation$dfdrrt <- na.approx(interpolation$dfdrrt, method="linear")
interpolation$rrt <- na.approx(interpolation$rrt, method="linear")
interpolation$frt <- na.approx(interpolation$frt, method="linear")
interpolation <-na.approx(replace(interpolation, interpolation == 0, NA), method="linear")
View(interpolation)



type1 <-interpolation[ interpolation[,"type"]==1, ]
View(type1)
fivenum(type1[,"tpthrt"])
summary(type1[,"tpthrt"])

fivenum(type1[,"pkthrt"])
summary(type1[,"pkthrt"])

fivenum(type1[,"dfdrrt"])
summary(type1[,"dfdrrt"])

fivenum(type1[,"rrt"])
summary(type1[,"rrt"])

fivenum(type1[,"frt"])
summary(type1[,"frt"])



type2 <-interpolation[ interpolation[,"type"]==2, ]
View(type2)
fivenum(type2[,"tpthrt"])
summary(type2[,"tpthrt"])

fivenum(type2[,"pkthrt"])
summary(type2[,"pkthrt"])

fivenum(type2[,"dfdrrt"])
summary(type2[,"dfdrrt"])

fivenum(type2[,"rrt"])
summary(type2[,"rrt"])

fivenum(type2[,"frt"])
summary(type2[,"frt"])


boxplot(type1[,"tpthrt"],main="TYPE 1- TPTHRT")
boxplot(type1[,"pkthrt"],main="TYPE 1- PKTHRT")
boxplot(type1[,"dfdrrt"],main="TYPE 1- DFDRRT")
boxplot(type1[,"rrt"],main="TYPE 1- RRT")
boxplot(type1[,"frt"],main="TYPE 1- FRT")

boxplot(type2[,"tpthrt"],main="TYPE 2- TPTHRT")
boxplot(type2[,"pkthrt"],main="TYPE 2- PKTHRT")
boxplot(type2[,"dfdrrt"],main="TYPE 2- DFDRRT")
boxplot(type2[,"rrt"],main="TYPE 2- RRT")
boxplot(type2[,"frt"],main="TYPE 2- FRT")




hist(type1[,"tpthrt"],main="TYPE 1- TPTHRT")
hist(type1[,"pkthrt"],main="TYPE 1- PKTHRT")
hist(type1[,"dfdrrt"],main="TYPE 1- DFDRRT")
hist(type1[,"rrt"],main="TYPE 1- RRT")
hist(type1[,"frt"],main="TYPE 1- FRT")


hist(type2[,"tpthrt"],main="TYPE 2- TPTHRT")
hist(type2[,"pkthrt"],main="TYPE 2- PKTHRT")
hist(type2[,"dfdrrt"],main="TYPE 2- DFDRRT")
hist(type2[,"rrt"],main="TYPE 2- RRT")
hist(type2[,"frt"],main="TYPE 2- FRT")




normalizedtype1 <-type1
normalizedtype1 <- (type1[, c(2, 3, 4, 5,6)]-min(type1[, c(2, 3, 4, 5,6)]))/(max(type1[, c(2, 3, 4, 5,6)])-min(type1[, c(2, 3, 4, 5,6)]))
View(normalizedtype1)



normalizedtype2 <-type2
normalizedtype2 <- (type2[, c(2, 3, 4, 5,6)]-min(type2[, c(2, 3, 4, 5,6)]))/(max(type2[, c(2, 3, 4, 5,6)])-min(type2[, c(2, 3, 4, 5,6)]))
View(normalizedtype2)



plot(x=1:100,y=normalizedtype1[,"tpthrt"],type = 'l',col = "red",main="TPTHRT")
lines(x=1:100, y=normalizedtype2[,"tpthrt"],type = 'l',col = "blue")
# Add a legend to the plot
legend("topright", legend=c("TYPE 1", "TYPE 2"),
       col=c("red", "blue"), lty = 1:2, cex=0.8)



plot(x=1:100,y=normalizedtype1[,"pkthrt"],type = 'l',col = "red",main="PKTHRT")
lines(x=1:100, y=normalizedtype2[,"pkthrt"],type = 'l',col = "blue")
# Add a legend to the plot
legend("topright", legend=c("TYPE 1", "TYPE 2"),
       col=c("red", "blue"), lty = 1:2, cex=0.8)


plot(x=1:100,y=normalizedtype1[,"dfdrrt"],type = 'l',col = "red",main="DFDRRT")
lines(x=1:100, y=normalizedtype2[,"dfdrrt"],type = 'l',col = "blue")
# Add a legend to the plot
legend("topright", legend=c("TYPE 1", "TYPE 2"),
       col=c("red", "blue"), lty = 1:2, cex=0.8)


plot(x=1:100,y=normalizedtype1[,"rrt"],type = 'l',col = "red",main="RRT")
lines(x=1:100, y=normalizedtype2[,"rrt"],type = 'l',col = "blue")
# Add a legend to the plot
legend("topright", legend=c("TYPE 1", "TYPE 2"),
       col=c("red", "blue"), lty = 1:2, cex=0.8)


plot(x=1:100,y=normalizedtype1[,"frt"],type = 'l',col = "red",main="FRT")
lines(x=1:100, y=normalizedtype2[,"frt"],type = 'l',col = "blue")
# Add a legend to the plot
legend("topright", legend=c("TYPE 1", "TYPE 2"),
       col=c("red", "blue"), lty = 1:2, cex=0.8)



cor(normalizedtype1[,"tpthrt"],normalizedtype1[,"pkthrt"],method = c("pearson"))
cor(normalizedtype2[,"tpthrt"],normalizedtype2[,"pkthrt"],method = c("pearson"))
cor(normalizedtype1[,"tpthrt"],normalizedtype2[,"tpthrt"],method = c("pearson"))








