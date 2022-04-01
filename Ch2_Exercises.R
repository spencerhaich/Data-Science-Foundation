### Chapter 2.4 Exercises

### College Data
# Setting working directory
setwd("C:/Users/Spencer/Documents/Data Science/Intro to Statistical Learning")

# Reading in college data
college = read.csv("College.csv")

# Setting row names to college names
rownames(college) <- college[,1]
View(college)

# Removing college names from data
college <- college[,-1]
View(college)

# Printing statistics for each variable
summary(college)

# Scatterplots of first 10 numerical columns
pairs(college[,2:11])

# Box plot of out-of-state tuition for public and private colleges
attach(college)
Private <- as.factor(Private)
plot(Private, Outstate, 
     xlab="Private", 
     ylab="Out-of-State Tuition",
     main="Out-of-State Tuition for Private vs Public Colleges")

# Creating Elite variable based on whether or not the proportion of students
# coming from the top 10% of their high school classes exceeds 50%
Elite <- rep("No", nrow(college))
Elite[Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)
college <- data.frame(college, Elite)
summary(Elite)

# Boxplot of out-of-state tuition for Elite and non-elite colleges
plot(Elite, Outstate,
     xlab="Elite",
     ylab="Out-of-State Tuition",
     main="Out-of-State Tution for Elite vs Other Colleges")

# Histograms of number of applications, room and board costs, instructional
# expenditure per student, and graduation rate
par(mfrow = c(2,2))
hist(Apps, 
     xlab="Number of Applications", 
     main="College Applications",
     col="blue")
hist(Room.Board, 
     xlab="Room and Board Cost (in $)", 
     main="College Room and Board Cost",
     col="red")
hist(Expend, 
     xlab="Instructional Expenditure per Student (in $)", 
     main="College Instructional Expenditure",
     col="green")
hist(Grad.Rate, 
     xlab="Percent Graduation Rate",
     main="College Graduation Rate",
     col="yellow")


### Automobile data
# Loading automobile data and removing rows with missing data
auto <- read.table("Auto.data", header=T, na.strings="?", stringsAsFactors=T)
auto <- na.omit(auto)
attach(auto)
View(auto)
summary(auto)

# Range for numeric columns
for (i in 1:ncol(auto)) {
  if (is.numeric(auto[1,i])) {
    r = range(auto[,i])
    print(paste(names(auto)[i],"range is",r[1],"to",r[2],sep=" "))
  }
}

# Mean and standard deviation for numeric columns, with and without rows 10 to 86
for (i in 1:ncol(auto)) {
  if (is.numeric(auto[1,i])) {
    m = format(round(mean(auto[,i]),2),nsmall=2)
    mw = format(round(mean(auto[-(10:86),i]),2),nsmall=2)
    std = format(round(sd(auto[,i]),2),nsmall=2)
    stdw = format(round(sd(auto[-(10:86),i]),2),nsmall=2)
    print(paste(names(auto)[i],"mean is",m,"and",mw,"without rows 10 to 86.",sep=" "))
    print(paste(names(auto)[i],"std is",std,"and",stdw,"without rows 10 to 86.",sep=" "))
  }
}

# Plotting mpg vs cylinders
par(mfrow = c(1,1))
cylinders <- as.factor(cylinders)
plot(cylinders, mpg,
     xlab="Number of Cylinders",
     ylab="Miles per Gallon",
     main="Cylinders vs MPG")

# Plotting acceleration vs horsepower
plot(acceleration, horsepower,
     main="Acceleration vs Horsepower",
     col=1)
abline(lm(horsepower ~ acceleration),
       col=2)
legend("topright",
       c("Data points","Line of best fit"),
       pch=c(1,NA),
       lty=c(0,1),
       col=c(1,2))

# Plotting weight vs year
plot(year, weight,
     main="Acceleration vs Horsepower",
     col=4)
abline(lm(weight ~ year),
       col=3)
legend("topright",
       c("Data points","Line of best fit"),
       pch=c(1,NA),
       lty=c(0,1),
       col=c(4,3))


### Boston data
# Loading data
library(ISLR2)
View(Boston)

# Plotting per capita crime rate by town vs proportion of owner-occupied units built prior to 1940
plot(Boston$age, Boston$crim,
     xlab="Proportion of Owner-Occupied Units Built Prior to 1940",
     ylab="Per Capita Crime Rate",
     main="Crime Rate vs Old Units")

# Boxplots of median value of owner-occupied homes vs whether it is on charles river
charles = Boston$chas
Boston$chas <- as.factor(Boston$chas)
plot(Boston$chas, Boston$medv,
     xlab="On Charles River (=1 if bounds river, 0 otherwise)",
     ylab="Median Value of Owner-Occupied Homes (in $1000s)",
     main="Value of Homes on the River vs Not")

# Number of home on the Charles River
print(paste("There are",sum(charles),"tracts of land that bound the Charles River.",sep=" "))

# Median pupil-teacher ratio
print(paste("The median pupil-teacher ratio is",median(Boston$ptratio),sep=" "))

# Minimum median value of owner-occupied homes
mmv = min(Boston$medv)
print(paste("The minimum of the median value of owner-occupied homes in Boston is $",mmv,",000",sep=""))

Boston$crim[Boston$medv==mmv]
Boston$zn[Boston$medv==mmv]

# Identifying minimum median value of owner-occupied homes on various histograms
# Crime rate
hist(Boston$crim,
     xlab="Per Capita Crime Rate",
     col="lightblue1")
abline(v=Boston$crim[Boston$medv==mmv],
       col="dodgerblue3",
       lty=2,
       lwd=2)
legend("topright",
       c("Crime Rate","Min of Median Value of Homes"),
       fill=c("lightblue1",NA),
       lty=c(0,2),
       lwd=c(0,2),
       border=c(1,NA),
       col=c(NA,"dodgerblue3"))

# Nitrogen oxide concentration
hist(Boston$nox,
     xlab="Nitrogen Oxide Concentration (ppm)",
     col="firebrick1")
abline(v=Boston$nox[Boston$medv==mmv],
       col="darkorchid4",
       lty=2,
       lwd=2)
legend("topright",
       c("Nitrogen Oxide","Min of Median Value of Homes"),
       fill=c("firebrick1",NA),
       lty=c(0,2),
       lwd=c(0,2),
       border=c(1,NA),
       col=c(NA,"darkorchid4"))

# Number of tracts with an average of more than 7 and 8 rooms per dwelling
print(paste(" There are",sum(Boston$rm > 7),"tracks with a greater than 7 average number of rooms per dwelling."))
print(paste(" There are",sum(Boston$rm > 8),"tracks with a greater than 8 average number of rooms per dwelling."))




