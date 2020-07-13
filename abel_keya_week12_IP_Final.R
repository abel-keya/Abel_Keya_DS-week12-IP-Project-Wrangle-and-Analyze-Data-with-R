
# Save the commands used during the session
savehistory(file="mylog.Rhistory")


# Install the following packages:
install.packages("foreign")
library(foreign)
install.packages("car")
install.packages("Hmisc")
install.packages("reshape")

library(dplyr) 

advertising <- read.csv(file = 'advertising.csv')
## 3. Checking the Data

# Previewing the dataset
# ---
# 
head(advertising)

head(advertising, n=10)# First 10 rows of dataset

head(advertising, n= -10) # All rows but the last 10


tail(advertising) # Last 6 rows

tail(advertising, n=10) # Last 10 rows

tail(advertising, n= -10) # All rows but the first 10

advertising[1:10, ] # First 10 rows

advertising[1:10,1:3] # First 10 rows of data of the first 3 variables

summary(advertising) # Provides basic descriptive statistics and frequencies.


edit(advertising) # Open data editor

str(advertising) # Provides the structure of the dataset

names(advertising) # Lists variables in the dataset

#Missing data

rowSums(is.na(advertising)) # Number of missing per row

colSums(is.na(advertising)) # Number of missing per column/variable

# FINDING DUPLICATES

# check dimensions
dim(advertising)

#check unique values
unique_items <- unique(advertising)
unique_items

#check duplicates by rows
duplicated_rows <- df[duplicated(advertising),]
duplicated_rows

rowMeans(is.na(advertising))*length(advertising) # No. of missing per row (another way)
# length = num. of variables/elements in an object


## 4. Tidying the Dataset

# list rows of data that have missing values
advertising[!complete.cases(advertising),]

#removing missing data

# The function na.omit() returns the object with listwise deletion of missing values.
# Creating a new dataset without missing data
advertising <- na.omit(advertising) 

#Renaming variables

# Using library –-reshape--
library(reshape)
advertising <- rename(advertising, c(Daily.Time.Spent.on.Site="time.spent"))
advertising <- rename(advertising, c(Ad.Topic.Line="topic"))
advertising <- rename(advertising, c(Daily.Internet.Usage="Usage"))
advertising <- rename(advertising, c(Clicked.on.Ad ="Clicked"))
advertising <- rename(advertising, c(Timestamp="timestamp"))
advertising <- rename(advertising, c(Area.Income.="income"))
advertising <- rename(advertising, c(Male.="gender"))

#confirming the dataset
head(advertising)

#confirm data types per column
str(advertising)

#converting data types

print(advertising)

#FEATURE ENGINEERING:FORMATTING TIME/DATE COLUMNS
#DATE CONVERSION OF THE TIMESTAMP COLUMN
advertising$timestamp <- as.Date(advertising$timestamp)
# Type cast the column to date

advertising$timestamp<- as.Date(advertising$timestamp)

#confirm the date conversion
str(advertising)

#FEATURE ENGINEERING:FORMATTING TIME/DATE COLUMNS

install.packages("lubridate")
install.packages("tidyr")
library(tidyr)
library(lubridate)

advertising_2 <- separate(advertising, timestamp, c("Year", "Month", "Day"))
head(advertising_2)

head(advertising)

#OUTLIERS

#boxplot =advertising.boxplot(column=['time.spent','Age','Area.Income','Usage','topic','City','Male','Country','timestamp','Clicked','hour','day','year'])

names(advertising)

## 5. Exploratory Analysis
#DESCRIPTIVE STATISTICS FOR THE DATASET
names(advertising)
#installing packages for descriptive stats
if(!require(psych)){install.packages("psych")}
if(!require(FSA)){install.packages("FSA")}
if(!require(plyr)){install.packages("plyr")}
if(!require(boot)){install.packages("boot")}
if(!require(DescTools)){install.packages("DescTools")}
# summary of descriptive statistics
library(psych)

describe(advertising)

#statistiscal measures of despersion by groped by male follwed by city
# Summarize function in the FSA package reports the five-number summary,descriptive statistics for grouped ordinal data.
library(FSA)
Summarize(time.spent ~ Male + City,
          data=advertising)
# package will summarize all variables in a data frame, listing the frequencies for levels of nominal variables; and for interval/ratio data, the minimum, 1st quartile, median, mean, 3rd quartile, and maximum
summary(advertising)
#descriptive statistics for internet usage
describe(advertising$Usage,
         type=3)         ### Type of calculation for skewness and kurtosis
#descriptive statistics for time.spent
describe(advertising$time.spent,
         type=3)
#descriptive statistics for Age
describe(advertising$Age,
         type=3)
#descriptive statistics for income
describe(advertising$Area.Income,
         type=3)

#UNIVARIATE VISUALIZATIONS ANALYSIS

#1.BARPLOTS

#barplot(table(advertising$Time.Spent))

barplot(table(advertising$Male))

#2.BOXPLOTS

boxplot.stats(advertising$time.spent)


plot(x = advertising_2$Day, y = advertising$Age)

#COUNTPLOT

counts <- table(advertising_2$Day)
barplot(counts, main="Frequency of Days",  xlab="Number of Days")

counts <- table(advertising_2$Month)
barplot(counts, main="Frequency of months",  xlab="Number ofmonths")


pairs(~advertising$Area.Income+advertising$Clicked+advertising$time.spent+advertising$Usage, data=advertising, main="scatterplot matrix  for the Advertising")

barplot(table(advertising$Age), horiz=TRUE, main="Barplot")
boxplot(rt(100, 5), main="Boxplot")
stripchart(sample(1:20, 10, replace=TRUE), method="stack", main="Stripchart")
pie(table(sample(1:6, 10, replace=TRUE)), main="Piechart")

screen(1)
barplot(advertising$Age, main="Barplot")
screen(2)
boxplot(sample(1:20, 100, replace=TRUE) ~ gl(4, 25, labels=LETTERS[1:4]),
col=rainbow(4), notch=TRUE, main="Boxplot")
screen(3)
plot(sample(1:20, 40, replace=TRUE), pch=20, xlab=NA, ylab=NA,
main="Scatter plot")
close.screen(all.screens=TRUE)

#BIVARIATE PLOTS

library("ggplot2")
#
geom_line()
ggplot(data =advertising_2,aes(x=time.spent,y=Area.Income))+
geom_line()


install.packages("corrplot")

#Compute correlation matrix


install.packages("Hmisc")


#correlations
res <- cor(advertising[0:4])
round(res, 2)

#Correlation matrix with significance levels (p-value)
# function rcorr() [in Hmisc package] can be used to compute the significance levels for pearson and spearman correlations. It returns both the correlation coefficients and the p-value of the correlation for all possible pairs of columns in the data table.
library("Hmisc")
#rcorr(advertising[0:4], type = c("pearson","spearman"))

res2 <- rcorr(as.matrix(advertising[0:4]))
res2

#Correlation matrix with significance levels (p-value)

# Extract the correlation coefficients
res2$r
# Extract p-values
res2$P

# flattenCorrMatrix
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
ut <- upper.tri(cormat)
data.frame(
row = rownames(cormat)[row(cormat)[ut]],
column = rownames(cormat)[col(cormat)[ut]],
cor  =(cormat)[ut],
p = pmat[ut]
)
}
res2<-rcorr(as.matrix(advertising[,0:4]))
flattenCorrMatrix(res2$r, res2$P)

#The R function symnum() replaces correlation coefficients by symbols according to the level of the correlation. It takes the correlation matrix as an argument 
symnum(res, abbr.colnames = FALSE)

install.packages("corrplot")


library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
tl.col = "black", tl.srt = 45)

# combine correlogram with the significance test.
#In the plot, correlations with p-value > 0.01 are considered as insignificant. 
#Use chart.Correlation(): Draw scatter plots
corrplot(res2$r, type="upper", order="hclust", 
p.mat = res2$P, sig.level = 0.01, insig = "blank")
# Insignificant correlations are leaved blank
corrplot(res2$r, type="upper", order="hclust", 
p.mat = res2$P, sig.level = 0.01, insig = "blank")



#the package PerformanceAnalytics] used to display a chart of a correlation matrix.

install.packages("PerformanceAnalytics")

#The distribution of each variable is shown on the diagonal.
#On the bottom of the diagonal : the bivariate scatter plots with a fitted line are displayed
#On the top of the diagonal : the value of the correlation plus the significance level as stars
#Each significance level is associated to a symbol : p-values(0, 0.001, 0.01, 0.05, 0.1, 1) <=> symbols(“***”, “**”, “*”, “.”, " “)

library("PerformanceAnalytics")
my_data <- advertising[0:4, c(0,1,2,3,4)]
chart.Correlation(my_data, histogram=TRUE, pch=19)

#heatmap()
#x : the correlation matrix to be plotted
#col : color palettes
#symm : logical indicating if x should be treated symmetrically; can only be true when x is a square matrix.

# Get some colors
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE)

#ggcorrplot: Visualization of a correlation matrix using ggplot2

#gcorrplot can be installed from CRAN as follow:
install.packages("ggcorrplot")

library(ggcorrplot)

# Compute a correlation matrix
data(advertising[0:3])
corr <- round(cor(advertising[0:3]),1)
corr

# Compute a matrix of correlation p-values
p.mat <- cor_pmat(advertising[0:3])
head(p.mat[, 1:3])

#Correlation matrix visualization

# Visualize the correlation matrix
# --------------------------------
# method = "square" (default)
ggcorrplot(corr)

# method = "circle"
ggcorrplot(corr, method = "circle")

# Reordering the correlation matrix
# --------------------------------
# using hierarchical clustering
ggcorrplot(corr, hc.order = TRUE, outline.col = "white")

# Types of correlogram layout
# --------------------------------
# Get the lower triangle
ggcorrplot(corr, hc.order = TRUE, type = "lower",
outline.col = "white")

# Get the upeper triangle
ggcorrplot(corr, hc.order = TRUE, type = "upper",
outline.col = "white")

# Change colors and theme
# --------------------------------
# Argument colors
ggcorrplot(corr, hc.order = TRUE, type = "lower",
outline.col = "white",
ggtheme = ggplot2::theme_gray,
colors = c("#6D9EC1", "white", "#E46726"))

# Add correlation coefficients
# --------------------------------
# argument lab = TRUE
ggcorrplot(corr, hc.order = TRUE, type = "lower",
lab = TRUE)

# Add correlation significance level
# --------------------------------
# Argument p.mat
# Barring the no significant coefficient
ggcorrplot(corr, hc.order = TRUE,
type = "lower", p.mat = p.mat)

# Leave blank on no significant coefficient
ggcorrplot(corr, p.mat = p.mat, hc.order = TRUE,
type = "lower", insig = "blank")

#CATEGORICAL VARIABLE ENCODING

#library('fastDummies')
install.packages("dummies")

library(dummies)
#advertising.new1 <- dummy.data.frame(advertising, names = c("city","country") , sep = ".")
advertising.new <- dummy.data.frame(advertising, sep = ".")
names(advertising.new)
head(advertising.new)

#Principal Component Analysis
#find the best low-dimensional representation of the variation in a multivariate data set. 
advertisement<- as.data.frame(scale(advertising.new[1:9]))
advertisement.pca <- prcomp(advertisement)
 
#principal component analysis using the “prcomp()” function in R

# summary of the principal component analysis results using the “summary()” function on the output of “prcomp()”
summary(advertisement.pca)

advertisement.pca$sdev

#The total variance explained by the components 
#is the sum of the variances of the components:

sum(advertisement.pca$sdev)^2

#principal components should be retained,
#summarise the results of a principal components analysis by making a scree plot
#using the “screeplot()” function:
screeplot(advertisement.pca, type="lines")

#where the change in slope in the scree plot occurs at component, 
#which is the “elbow” of the scree plot. 
#based on the basis of the scree plot that the first three components should be retained.


(advertisement.pca$sdev)^2

advertisement.pca$rotation[,1]

sum(advertisement.pca$rotation[,1])^2

#check names
names(advertisement.pca)
#Scatterplots of the Principal Components

plot(advertisement.pca$x[,1],advertisement.pca$x[,2]) # make a scatterplot
text(advertisement.pca$x[,1],advertisement.pca$x[,2], advertising$time.spent,cex=0.7, pos=4, col="red") # add labels

#The scatterplot shows the first principal component on the x-axis, and the second principal component on the y-axis. We can see from the scatterplot that wine samples of cultivar 1 have much lower values of the first principal component than wine samples of cultivar 3. Therefore, the first principal component separates wine samples of cultivars 1 from those of cultivar 3.

#LINEAR DISCRIMINANT ANALYSIS
#find the linear combinations of the original variables in our data set. Linear discriminant analysis is also known as “canonical discriminant analysis”, or simply “discriminant analysis”.

install.packages("MASS")

names(advertising)

#LABEL ENCODING:Using CatEncoders package : Encoders for Categorical Variables

install.packages("CatEncoders")

# Saving names of categorical variables
factors <- names(which(sapply(advertising, is.factor)))
factors

library(CatEncoders)

# Saving names of categorical variables
factors <- names(which(sapply(advertising, is.factor)))
# Label Encoder
#actors<-list(c(advertisement$topic,advertisement$Country,advertisement$City))
for (i in factors){
encode <- LabelEncoder.fit(advertising[, i])
advertising[, i] <- transform(encode, advertising[, i])
}
advertising

#carry out a linear discriminant analysis using the “lda()” function from the R “MASS” package. To use this function, we first need to install the “MASS” R package
# load the MASS package
library("MASS")                                                
advertising.lda <- lda(advertising$Clicked ~ advertising$Age + advertising$Area.Income + advertising$Usage+advertising$Male+advertising$time.spent)

advertising.lda

advertising.lda$scaling[,1]


#To calculate the values of the first discriminant function
#define function “calclda()”


advertising.lda.values <- predict(advertising.lda, advertising[2:4])
advertising.lda.values$x[,1] # values for the first discriminant function, using the unstandardised data

#A Stacked Histogram of the LDA Values displaying the results of a linear discriminant analysis (LDA) is to make a stacked histogram of the values of the discriminant function for the samples from different groups

# Stacked Histogram of the LDA Values
#displaying the results of a linear discriminant analysis (LDA) 
#make a stacked histogram of the values of the discriminant function 
#for the samples from different groups

ldahist(data = advertising.lda.values$x[,1], g=advertising$Clicked)

ldahist(data = advertising.lda.values$x[,1], g=advertising$Clicked)

#Scatterplots of the Discriminant Functions

plot(advertising.lda.values$x[,1]) #advertising.lda.values$x[,2]) # make a scatterplot
text(advertising.lda.values$x[,1],advertising$Clicked,cex=0.7,pos=4,col="red") #advertising.lda.values$x[,2]add labels

#Conclusuion:
#The dataset was appropriate. it contained no missing values and minimal outliers amongst the varaibles
#Both univariate and Bivariate analysis revealed that the dataset is collinear, hence it can be analysed better by use of a classification algorithms.