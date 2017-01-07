## factor analysis

setwd(dir = "~/Desktop/Google Drive/SDSU/DA-Exam-2017/")

# ggplot2 examples
library(readr)
library(dplyr)
options(dplyr.width = Inf)

library(ggplot2) 

df <- read_csv(file = "df.csv")

glimpse(df)


## interesting fact: 1995 Mazda RX-7 doesn't have a conventional piston engine; It has two rotational things that act like a 6-cylinder engine.
sum(df$`Number of cylinders` == ".")
df[57, "Number of cylinders"] <- NA

## Chevy Corvette and Mazda RX-7 have NA for Rear seat room; no rear seats
df$`Rear seat room` <- as.numeric(df$`Rear seat room`)
df[is.na(df$`Rear seat room`),]

# create factors with value labels 
df$`Number of cylinders` <- factor(df$`Number of cylinders`, levels=c("3","4","5","6","8"),
                                   labels=sapply(c("3","4","5","6","8"), function(x) paste(x, "cylinders"))) 

df$`Drive train type` <- factor(df$`Drive train type`, levels=c(0, 1, 2),
                                labels=c("rear wheel drive", "front wheel drive", "all wheel drive"))


df$`Manual transmission available` <- factor(df$`Manual transmission available`, levels=c(0, 1),
                                             labels=c("No", "Yes")) 

df$`Type` <- as.factor(df$Type)

df$`Air Bags standard` <- factor(df$`Air Bags standard`, levels=c(0, 1, 2),
                                 labels=c("None","Driver only","Driver & passenger")) 


df$Domestic <- factor(df$Domestic, levels = c(0, 1), 
                      labels = c("non-US", "US"))


# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors, 
# with varimax rotation 
numericdata <- sapply(df, is.numeric)
numericdata
df[, numericdata]

fit <- factanal(na.omit(df[, numericdata]), 5, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2 
loads12 <- fit$loadings[,1:2] 
loads23 <- fit$loadings[,2:3] 
loads34 <- fit$loadings[,3:4]


plot(loads12, type="p") # set up plot 
text(loads12,labels=colnames(df),cex=.7) # add variable names

plot(loads23, type="p") # set up plot 
text(loads23,labels=colnames(df),cex=.7) # add variable names

plot(loads34, type="p") # set up plot 
text(loads34,labels=colnames(df),cex=.7) # add variable names




