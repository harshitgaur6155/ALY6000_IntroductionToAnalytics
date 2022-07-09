#---------------------- GAUR_HARSHIT_M1_PROJECT1 ----------------------#

#STEP 1: Printing my name.
print("HARSHIT GAUR")

#STEP 2: Installing the 'vcd' package.
install.packages("vcd")

#STEP 3: Importing the 'vcd' package
library("vcd")

#STEP 4: Plot age ~ weight scatter plot.
#Note: Defining 'age' vector
age <- c(8,20,17,32,59,41,60,31,15,30)
#Note: Defining 'weight' vector
weight <- c(65,120,167,184,130,147,127,140,105,150)
#Note: Plot a Scatter plot for age ~ weight
plot(age, weight, main = "Age vs Weight", xlab = "Age", ylab = "Weight", col = "BLUE")
abline(lm(weight~age), col="red", lwd=2)

#Note: Finding correlation between age and weight
cor(age, weight)

#STEP 5: Median of the 'weight' vector
paste("Median of weight :", median(weight))

str(age)
#STEP 6: Deleted the 7th element from 'age' vector
age <- age[-7]
#STEP 7: Insert 26 as 7th element in 'age' vector
age <- append(age, 26, 6)
str(age)

#STEP 8: Creating a 'color' vector
color <- c("Red", "Green", "Blue")
str(color)

#STEP 9: Creating a 5x3 matrix ranging from 1 to 15
rNames <- paste("R", seq.int(1:5), sep = "")
cNames <- paste("C", seq.int(1:3), sep = "")
intMatrix <- matrix(1:15, nrow = 5, ncol = 3, byrow = TRUE, dimnames = list(rNames, cNames))
View(intMatrix)

#STEP 10: Creating a 'people' data frame using 'age' and 'weight' vectors
people <- data.frame(age, weight)
View(people)

#STEP 11: Displaying the structure of 'people' data frame
str(people)
#STEP 12: Displaying the summary of 'people' data frame
summary(people)

#STEP 13: Import 'Student.csv' data set
#Note: Change the working directory as per the file's location.
setwd("/Users/HarshitGaur/Documents/Northeastern University/MPS Analytics/ALY 6000/Class 1")
student <- read.csv("Student.csv", header = TRUE)

#STEP 14: Display the variable names of the data set.
colnames(student)

#Note: Find summary of 'Student - Math'
summary(student$Math)

#Note: Find summary of 'Student - Science'
summary(student$Science)

#Note: Find summary of 'Student - Social Science'
summary(student$Social.Studies)
