#---------------------- GAUR_M2_PROJECT2 ----------------------#

#STEP 1: Printing my last-name with a prefix.
print("Plotting Basics: GAUR")

#STEP 2: Installing the packages.
install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("dplyr")
install.packages("plotrix")
install.packages("ggplot2")
install.packages("moments")

#STEP 2: Importing the packages.
library("FSA")
library("FSAdata")
library("magrittr")
library("dplyr")
library("plotrix")
library("ggplot2")
library("moments")

#STEP 3: Import 'BullTroutRML2.csv' data set
data(BullTroutRML2)
View(BullTroutRML2)

#STEP 4: Print first 3 records of 'BullTroutRML2.csv' data set
head(BullTroutRML2, 3)
#STEP 4: Print last 3 records of 'BullTroutRML2.csv' data set
tail(BullTroutRML2, 3)

#STEP 5: Remove all records except those of 'Harrison' lake
#from 'BullTroutRML2.csv' data set
BullTroutRML2 <- filterD(BullTroutRML2, lake == 'Harrison')
View(BullTroutRML2)

#STEP 6: Print first 5 and last 5 records of the filtered 'BullTroutRML2.csv' data set
View(head(BullTroutRML2, 5))
View(tail(BullTroutRML2, 5))

#STEP 7: Display the structure of the filtered 'BullTroutRML2.csv' data set
str(BullTroutRML2)

#STEP 8: Display the summary of the filtered 'BullTroutRML2.csv' data set
summary(BullTroutRML2)

#STEP 9: Plot a Scatter plot for fl ~ age
# ----------- Plot 1: Harrison Lake Trout ----------- #

plot(BullTroutRML2$fl, BullTroutRML2$age, xlim = c(0,500), ylim = c(0,15), main = "Plot 1: Harrison Lake Trout", xlab = "Fork Length (mm)", ylab = "Age (yrs)", pch = 21, bg = "GREEN")


#STEP 10: Plot an histogram graph for age using 'ggplot'
# ----------- Plot 2: Harrison Fish Age Distribution ----------- #

ggplot(BullTroutRML2) + 
  geom_histogram(
    mapping = aes(age),
    color="BLACK",
    fill="CADETBLUE",
    binwidth = 1
  ) +
  labs(
    title = "Plot 2: Harrison Fish Age Distribution",
    x = "Age (yrs)",
    y = "Frequency"
  ) +
  scale_x_continuous(limits = c(-1,15), breaks = c(0:15)) +
  scale_y_continuous(limits = c(-1,15), breaks = c(0:15)) +
  theme(plot.title = element_text(colour = "CADETBLUE"))


#STEP 11: Plot an Overlay Dense Plot (Smooth Scatter) for age using 'ggplot'
# ----------- Plot 3: Harrison Density Shaded by Era ----------- #

smoothScatter(x = BullTroutRML2$fl, y = BullTroutRML2$age, main = "Plot 3: Harrison Density Shaded by Era",
              xlab = "Fork Length (mm)", ylab = "Age (yrs)", xlim = c(0,500), ylim = c(0,15), 
              pch = 20, colramp = colorRampPalette( c('LIGHTYELLOW','GREEN') ), col = 'DARKGREEN')

#STEP 12: Create a new variable 'tmp' the first 3 and last 3 records of the data set.
tmp <- headtail(BullTroutRML2, 3)
View(tmp)

#STEP 13: Display 'era' column from the 'tmp' variable.
View(tmp['era'])

#STEP 14: Create 'pchs' vector with arguments '+' and '*'
pchs <- c('+', 'x')

#STEP 15: Create 'cols' vector with arguments 'red' and 'gray60'
cols <- c('red', 'gray60')

#STEP 16: Convert 'temp$era' values to numeric values
original_tmp_era <- tmp$era
tmp$era <- as.numeric(tmp$era)

#STEP 17: Initialize 'cols' vector with 'temp$era' values
cols <- unique(tmp$era)

#STEP 18: Plot a graph for 'Age (yrs) versus Fork Length (mm)'
# ----------- Plot 4: Symbol & Color by Era ----------- #

plot(x = BullTroutRML2$fl, y = BullTroutRML2$age, main = "Plot 4: Symbol & Color by Era",
     xlab = "Fork Length (mm)", ylab = "Age (yrs)", xlim = c(0,500), ylim = c(0,15), 
     pch = c(pchs), col = c(cols))


#STEP 18: Plot a graph for 'Age (yrs) versus Fork Length (mm)'
# --------------- Plot 5: Regression Overlay --------------- #

plot(x = BullTroutRML2$fl, y = BullTroutRML2$age, main = "Plot 5: Regression Overlay",
     xlab = "Fork Length (mm)", ylab = "Age (yrs)", xlim = c(0,500), ylim = c(0,15), 
     pch = c(pchs), col = c(cols))
#Regression Line
abline(lm(age~fl, data = BullTroutRML2), col="ORANGE", lwd=2)


#STEP 19: Plot a graph for 'Age (yrs) versus Fork Length (mm)'
# ---------------- Plot 6: Legend Overlay ------------------- #

plot(x = BullTroutRML2$fl, y = BullTroutRML2$age, main = "Plot 6: Legend Overlay",
     xlab = "Fork Length (mm)", ylab = "Age (yrs)", xlim = c(0,500), ylim = c(0,15), 
     pch = c(pchs), col = c(cols))
#Plot Regression Line
abline(lm(age~fl, data = BullTroutRML2), col="ORANGE", lwd=2)
#Add Legend
legend(x = "topleft", legend = unique(original_tmp_era), fill = unique(original_tmp_era))


