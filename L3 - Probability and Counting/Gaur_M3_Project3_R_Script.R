#---------------------- GAUR_M3_PROJECT3 ----------------------#

#STEP 1A: Printing my name.
print("HARSHIT GAUR")

#STEP 1B: Importing the packages.
library("FSA")
library("FSAdata")
library("magrittr")
library("dplyr")
library("tidyr")

install.packages("plyr")
install.packages("tidyverse")
library("plyr")
library("tidyverse")

#STEP 2: Import 'inchBio.csv' data set
#Note: Change the working directory as per the file's location.
setwd("/Users/HarshitGaur/Documents/Northeastern University/MPS Analytics/ALY 6000/Class 3/Assignment")
bio <- read.csv("inchBio.csv", header = TRUE)
View(bio)

#STEP 3A: Print the head of 'inchBio.csv' data set
View(head(bio))
#STEP 3B: Print the tail of 'inchBio.csv' data set
View(tail(bio))
#STEP 3C: Print the structure of 'inchBio.csv' data set
str(bio)
#STEP 3D: Print the summary of 'inchBio.csv' data set
summary(bio)

#STEP 4A: List the species of 'inchBio.csv' data set
#speciesList <- bio[,3]
speciesList <- list(bio$species)
speciesList

#STEP 4B: Count the records of species of 'inchBio.csv' data set
#counts <- length(bio$species)
#counts <- count(bio$species)
counts <- table(bio$species)
View(counts)
is.object(counts)

#STEP 5: Display the 8 levels of species of 'inchBio.csv' data set
speciesLevel <- unique(bio$species)
speciesLevel

#STEP 6: Create variable to Display the levels of species & their frequencies
tmp <- count(bio$species)
View(tmp)

#STEP 7: Create variable to Display the subset of first 5 levels of species
tmp2 <- subset(head(bio, 5), select = "species")
View(tmp2)

#STEP 8A: Create a table containing species
w <- table(bio$species)
View(w)
#STEP 8B: Display the class of above table
class(w)

#STEP 9A: Convert the above table to data frame
t <- data.frame(w)
#STEP 9B: Class of the data frame 't'
class(t)
#STEP 9C: Structure of the data frame 't'
str(t)
#STEP 9D: Summary of the data frame 't'
summary(t)

#STEP 10: Display the 'frequency' values from the data frame 't'
t$Freq

#STEP 11: Create a table 'cSpec' from species and confirm with class and View
cSpec <- table(bio$species)
class(cSpec)
View(cSpec)

#STEP 12: Create a table 'cSpecPct' displaying the species and its percentages (not frequencies)
cSpecPct <- prop.table(cSpec) * 100
class(cSpecPct)
View(cSpecPct)

#STEP 13A: Convert the table 'cSpecPct' to data frame
u <- data.frame(cSpecPct)
#STEP 13B: Class of the data frame 'u'
class(u)

#STEP 14: Plot a Barplot of 'cSpec'
# ---------------- Plot 1: Fish Count ------------------- #
par(mar = c(5, 6, 4, 2) + 0.1)
barplot(cSpec, main = "Fish Count", xlab = "COUNTS", las = 1, horiz = TRUE, 
        cex.names = 0.6, xlim = c(0,250), col = 'LIGHTGREEN')
#barplot(cSpec, main = "Fish Count", ylab = "COUNTS", las = 2, horiz = FALSE, cex.names = 0.6, ylim = c(0,250), col = 'LIGHTGREEN')


#STEP 15: Plot a Barplot of 'cSpecPct'
# ---------------- Plot 2: Fish Relative Frequency ------------------- #
par(mar = c(8, 5, 4, 2) + 0.1)
barplot(cSpecPct, main = "Fish Relative Frequency", ylab = "FREQUENCY (Percentage)", las = 2, horiz = FALSE, ylim = c(0,40))
axis(side = 2, at = 5*(0:8), label = 5*(0:8), col.axis = "LIGHTBLUE", cex.axis = 1, las = 2) 
#Both axes as LightBlue
#barplot(cSpecPct, main = "Fish Relative Frequency", ylab = "FREQUENCY", las = 2, horiz = FALSE, cex.names = 1, ylim = c(0,40), col.axis = 'LIGHTBLUE')

#STEP 16: Rearrange the 'u' data frame 
#with decreasing order of frequency
d <- u[order(u$Freq, decreasing = TRUE), ]
View(d)

#STEP 17: Rename the columns of 'd' data frame
colnames(d) <- c("Species", "RelFreq")

#STEP 18: Add 'cumfreq', 'counts', 'cumcounts' to the 'd' data frame
d <- mutate(d, cumfreq = cumsum(d$RelFreq))
d <- merge(d, counts, by.x = "Species", by.y = "Var1", sort = FALSE)
d <- rename(d, replace = c("Freq" = "counts"))
d <- mutate(d, cumcounts = cumsum(d$counts))
View(d)

#STEP 19: Define variables for parameter variables
def_par <- 3.05 * max(d$counts)

#STEP 20: Plot a Barplot of 'pc'
# ---------------- Plot 3: Species Pareto ------------------- #
par(mar = c(7, 6, 3, 3))
pc <- barplot(d$counts,  main = "Species Pareto \n Made by Harshit Gaur", width = 1, space = 0.5, border = NA, axes = F, ylim = c(0, def_par), 
              na.rm = TRUE, ylab = "Cummulative Counts", cex.names = 0.7, names.arg = d$Species, las = 2)

#STEP 21: Add cummulative counts line to the plot
lines(pc, d$cumcounts, type = "b", cex = 0.7, pch = 20, col = "CYAN4")

#STEP 22: Place a grey box around the pareto plot
box(col = "GREY62")

#STEP 23: Add a left-side axis
axis(side = 2, at = c(0, d$cumcounts), col.axis = "GREY62", col = "GREY62", cex.axis = 0.8, las = 1) 

#STEP 24: Add a right-side axis
axis(side = 4, at = c(0, d$cumcounts), labels = paste( c(0, round(d$cumfreq)), "%", sep = "" ), col.axis = "CYAN4", col = "CYAN4", cex.axis = 0.8, las = 1)
                                                   

