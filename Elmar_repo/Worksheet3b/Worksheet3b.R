#Worksheet#3b

install.packages("dplyr")
library(dplyr)
library(tidyverse)

#ELMAR AUGUSTINE FERNANDEZ BSIT 2-A

#1. Create a data frame using the table below.
# a.Write the codes.
Respondents <- c(seq(1,20))
Sex <- c(2,2,1,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,1,2)
FathersOccupation <- c(1,3,3,3,1,2,3,1,1,1,3,2,1,3,3,1,3,1,2,1)
Personsathome <- c(5,7,3,8,5,9,6,7,8,4,7,5,4,7,8,8,3,11,7,6) 
Siblingsatschool <- c(6,4,4,1,2,1,5,3,1,2,3,2,5,5,2,1,2,5,3,2)
Typesofhouses <- c(1,2,3,1,1,3,3,1,2,3,2,3,2,2,3,3,3,3,3,2)

dframe <- data.frame(Respondents,Sex,FathersOccupation,Personsathome,Siblingsatschool,Typesofhouses)

#b.Describe the data. Get the structure or the summary of the data

summary(dframe)


#c. Is the mean number of siblings attending is 5?

# Answer: No 

#d. Extract the 1st two rows and then all the columns using the subsetting functions.
#Write the codes and its output.

c1 <- subset(dframe[1:2, 1:6, drop = FALSE])
c1

#e. Extract 3rd and 5th row with 2nd and 4th column. Write the codes and its
#result.
c2 <- subset(dframe[c(3,5),c(2,4)])
c2
#f. Select the variable types of houses then store the vector that results as types_houses.
#Write the codes.
c3 <- dframe[c(6)]

type_houses <- c3


#g. Select only all Males respondent that their father occupation was farmer. Write
#the codes and its output.
c22 <- subset(dframe[c(3,11),c(2,3)])
c22





#h. Select only all females respondent that have greater than or equal to 5 number
#of siblings attending school. Write the codes and its outputs

c5 <- subset(dframe[c(1:20), c(2,5)])
girlS <- c5[dframe$Siblingsatschool >= 5,]
girlS

#2. Write a R program to create an empty data frame. Using the following codes:
df = data.frame(Ints=integer(),
                Doubles=double(), Characters=character(),
                Logicals=logical(),
                Factors=factor(),
                stringsAsFactors=FALSE)
print("Structure of the empty dataframe:")
print(str(df))

#Describe the results.

Using data is a simple way to create an empty DataFrame in the R programming language. frame() method with no parameters This generates a R DataFrame with no rows or columns (0 rows and 0 columns).

#Interpret the graph.

The negative sentiments of tweets per day outnumber the positive, and the neutral sentiment is the lowest.