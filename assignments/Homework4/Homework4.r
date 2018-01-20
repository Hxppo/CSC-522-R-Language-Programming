#Question1
library("plyr")
library("dplyr")
library("tidyr")
library("lattice")
library("ggplot2")
library("tibble")
library("gridExtra")
library("maps")
library("rgl")

setwd("/Users/xinhuang/Google Drive/CSC 522 R Language Programming /Homework4")
#1a
myData <- read.csv("HW01pb1data.csv", header = FALSE)
#exam all the columns
class(myData$V1)
class(myData$V2)
class(myData$V3)
class(myData$V4)
class(myData$V5)

#1b 
#print out the levels
levels(myData$V4)

#1c
mat <- matrix(1 : 2, nrow = 2)
layout(mat)
plot(myData[, 1])
plot(myData[, 4])


#Answers for Question2
#2a
myData <- read.csv("HW01pb2data.csv",header=FALSE)
sampleData <- sample(myData[, 1], 10000, replace=TRUE)

#2b
paste("Mean of sample data: ", mean(sampleData))
paste("Max of sample data: ", max(sampleData))
paste("Var of sample data: ", var(sampleData))
paste("Quantile of sample data: ", quantile(sampleData, 0.25))

#2c
paste("Mean of whole data: ", mean(myData[, 1]))
paste("Max of whole data: ", max(myData[, 1]))
paste("Var of whole data: ", var(myData[, 1]))
paste("Quantile of whole data: ", quantile(myData[, 1], 0.25))

#2d
write.csv(sampleData, file = "sampleData.csv", row.names= FALSE, col.names = FALSE)

dev.off()

#Answers for Question3
#3a
ocean <- read.csv("HW01pb3OceanViewdata.csv", header = FALSE)
desert <- read.csv("HW01pb3Desertdata.csv", header = FALSE)

boxplot(ocean, at = 1, xlim = c(0.5, 2.5), 
        ylim = range(c(ocean, desert)), 
        main = "House Box Plots",
        ylab="Values",
        pch = 20,
        cex = 0.7,
        col ="blue") 
abline(h = median(ocean$V1), col = "lightblue", lty = 3)
text(1,  500, 
     paste("Median of Ocean ", median(ocean$V1)), 
     col = "blue", cex = 0.6)


boxplot(desert, at = 2, 
        add = TRUE, pch = 20,
        col ="red", cex = 0.7)
abline(h = median(desert$V1), col="red", lty = 3)
text(2,  1700, 
     paste("Median of Desert ", median(desert$V1)), 
     col = "red", cex = 0.6)

#3b
names(ocean)[1] <- "HousePrice"
names(desert)[1] <- "HousePrice"
breaks <- seq.int(0, 3000, by = 250)
hist(ocean$HousePrice, breaks, main = "Ocenview House Distribution by Price",xlab = "Houce Price")

#3c
par(mfrow=c(2,1))
plot(ecdf(ocean$HousePrice), 
     pch = 20 , cex = 0.5, col = "blue",
     main = "Empirical Cummulative Distribution Function of Ocenview Houses ")
legend("topleft", c("ocean"), col = c("blue"),
       pch = 20 ,inset =.1, title = "House price",
       cex = 0.8)

plot(ecdf(desert$HousePrice), 
     pch = 20 , cex = 0.5, col = "red",
     main = "Empirical Cummulative Distribution Function of Ocenview Houses ")
legend("bottomright", c("ocean"), col = c("red"),
       pch = 20 ,inset =.1, title = "House price",
       cex = 0.8)

#Answers for Question4
#4a
orange <- as.data.frame(Orange)
par(mfrow=c(1,1))
plot(orange$age, orange$circumference,
     main = "Orange Age by Circumference",
     xlim = c(min(orange$age),max(orange$age) + 100),
     ylim = c(min(orange$circumference),max(orange$circumference) + 100),
     xlab = "Orange Age",
     ylab = "Orange Circumference")

#4b
cor <- cor(orange[which(orange$Tree == 1), 2], orange[which(orange$Tree == 1), 3])
paste("Correlation between Age and Circumference is: ", cor)

#4c
names(orange) <- toupper(names(orange))
result <- orange %>% 
    group_by(TREE) %>% 
    summarise(COVARIANCE = cov(AGE, CIRCUMFERENCE), CORRELATION = cor(AGE, CIRCUMFERENCE))

result$TREE <- as.numeric(as.character(result$TREE))
result[order(result$TREE), ]

#Answer for Question5 
#5a
median(desert$HousePrice)
mean(desert$HousePrice)

#5b
hist(desert$HousePrice, breaks = 100)
abline(v = median(desert$HousePrice), col = "red")
abline(v = mean(desert$HousePrice), col = "blue")

#5c
add10desert <- desert + 10
median(add10desert$HousePrice)

#5d
mult2desert <- desert * 2
median(mult2desert$HousePrice)


