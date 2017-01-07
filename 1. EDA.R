
setwd(dir = "~/Desktop/Google Drive/SDSU/DA-Exam-2017/")

list.files(getwd())
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
df$Manufacturer <- as.factor(df$Manufacturer)

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


# grouped by number of gears (indicated by color)
df %>% group_by(`Number of cylinders`) %>% summarise(count = n())

## there are only two 5 cylinder engines
qplot(`Number of cylinders`, data=df, geom="bar", fill=`Number of cylinders`, alpha=I(.5), 
      main="Distribution of Cylinders", xlab="Number of Cylinders", 
      ylab="Count")




qplot(`City MPG`, data=df, geom="density", fill=`Number of cylinders`, alpha=I(.5), 
      main="Distribution of Gas Milage", xlab="Miles Per Gallon", 
      ylab="Density")



qplot(`Highway MPG`, data=df, geom="density", fill=`Number of cylinders`, alpha=I(.5), 
      main="Distribution of Gas Milage", xlab="Miles Per Gallon", 
      ylab="Density")


# Scatterplot of mpg vs. hp for each combination of gears and cylinders
# in each facet, transmittion type is represented by shape and color

qplot(x = `Horsepower`, y = `City MPG`, data=df, shape=`Manual transmission available`, 
      color = `Manual transmission available`,size=I(3),
      xlab="Horsepower", ylab="Miles per Gallon") 

qplot(x = `Horsepower`, y = `City MPG`, data=df, shape=`Manual transmission available`, 
      color = `Manual transmission available`, size=I(3),
      xlab="Horsepower", ylab="Miles per Gallon", main = "Horse Power v City MPG by Transmission Type") 


qplot(x = `Horsepower`, y = `Highway MPG`, data=df, shape=`Manual transmission available`, 
      color = `Manual transmission available`, size=I(3),
      xlab="Horsepower", ylab="Miles per Gallon", main = "Horse Power v City MPG by Transmission Type") 


qplot(x = `Horsepower`, y = `Highway MPG`, data=df, shape=`Manual transmission available`, 
      color = `Manual transmission available`, size=I(3),
      xlab="Horsepower", ylab="Miles per Gallon", main = "Horse Power v City MPG by Transmission Type") 



# Scatterplot of price vs. hp, am, mpg
qplot(x = `Horsepower`, y = `Minimum Price`, data=df, size=I(3),
      xlab="Horsepower", ylab="Miles per Gallon", main = "Horse Power v City MPG by Transmission Type") 

qplot(x = `Horsepower`, y = `Minimum Price`, data=df, shape=`Manual transmission available`, 
      color = `Manual transmission available`, size=I(3),
      xlab="Horsepower", ylab="Miles per Gallon", main = "Horse Power v City MPG by Transmission Type") 




qplot(x = `Horsepower`, y = `Midrange Price`, data=df, size=I(3),
      xlab="Horsepower", ylab="Miles per Gallon", main = "Horse Power v City MPG by Transmission Type") 

qplot(x = `Horsepower`, y = `Midrange Price`, data=df, shape=`Manual transmission available`, 
      color = `Manual transmission available`, size=I(3),
      xlab="Horsepower", ylab="Miles per Gallon", main = "Horse Power v City MPG by Transmission Type") 




qplot(x = `Horsepower`, y = `Maximum Price`, data=df, size=I(3),
      xlab="Horsepower", ylab="Miles per Gallon", main = "Horse Power v City MPG by Transmission Type") 

qplot(x = `Horsepower`, y = `Maximum Price`, data=df, shape=`Manual transmission available`, 
      color = `Manual transmission available`, size=I(3),
      xlab="Horsepower", ylab="Miles per Gallon", main = "Horse Power v City MPG by Transmission Type") 






qplot(x = `Engine size`, y = `Minimum Price`, data=df, size=I(3),
      xlab="Engine size", ylab="Minimum Price", main = "Engine size v Minimum Price by Transmission Type") 

qplot(x = `Engine size`, y = `Minimum Price`, data=df, shape=`Manual transmission available`, 
      color = `Manual transmission available`, size=I(3),
      xlab="Engine size", ylab="Minimum Price", main = "Engine size v Minimum Price by Transmission Type") 




qplot(x = `Engine size`, y = `Midrange Price`, data=df, size=I(3),
      xlab="Engine size", ylab="Midrange Price", main = "Engine size v Minimum Price by Transmission Type") 

qplot(x = `Engine size`, y = `Midrange Price`, data=df, shape=`Manual transmission available`, 
      color = `Manual transmission available`, size=I(3),
      xlab="Engine size", ylab="Midrange Price", main = "Engine size v Midrange Price by Transmission Type") 




qplot(x = `Engine size`, y = `Maximum Price`, data=df, size=I(3),
      xlab="Engine size", ylab="Max Price", main = "Engine size v Max Price by Transmission Type") 

qplot(x = `Engine size`, y = `Maximum Price`, data=df, shape=`Manual transmission available`, 
      color = `Manual transmission available`, size=I(3),
      xlab="Engine size", ylab="Max Price", main = "Engine size v Max Price by Transmission Type") 




# Separate regressions of mpg on weight for each number of cylinders
qplot(x = `Engine size`, y = `Horsepower`, data=df, size = I(3),
      color = `Manual transmission available`, shape = `Manual transmission available`, 
      main="Regression of Engine size on Horsepower", 
      xlab="Engine size", ylab="Horsepower")



qplot(x = `Engine size`, y = `Horsepower`, data=df, size = I(3),
      color = `Number of cylinders`, shape = `Number of cylinders`,
      main="Regression of Engine size on Horsepower", 
      xlab="Engine size", ylab="Horsepower")



qplot(x = `Engine size`, y = `Horsepower`, data=df, #geom=c("point", "smooth"), 
      #method="lm", formula=y~x, #color=`Number of cylinders`, 
      main="Regression of Engine size on Horsepower", 
      xlab="Engine size", ylab="Horsepower")




# Boxplots of mpg by number of gears 
# observations (points) are overlayed and jittered
qplot(as.factor(gear), mpg, data=mtcars, geom=c("boxplot", "jitter"), 
      fill=gear, main="Mileage by Gear Number",
      xlab="", ylab="Miles per Gallon")


qplot(`Number of cylinders`, `City MPG`, data=df, geom="boxplot", 
      fill=`Number of cylinders`, main="MPG by Number of Cylinders",
      xlab="Cylinders", ylab="Miles per Gallon")


qplot(`Number of cylinders`, `City MPG`, data=df, geom=c("boxplot", "jitter"), 
      fill=`Number of cylinders`, main="MPG by Number of Cylinders",
      xlab="Cylinders", ylab="Miles per Gallon")



qplot(x = `Manufacturer`, y = `Midrange Price`, data=df, geom=c("boxplot", "jitter"), 
      fill=`Manufacturer`, main="Midrange Price by Manufacturer",
      xlab="Manufacturer", ylab="Midrange Price")




qplot(x = `Domestic`, y = `Midrange Price`, data=df, geom="boxplot", 
      fill=`Manufacturer`, main="Midrange Price by Manufacturer",
      xlab="Domestic & Manufacturer", ylab="Midrange Price")





qplot(x = `Domestic`, y = `Midrange Price`, data=df, geom="boxplot", 
      fill=`Domestic`, main="Midrange Price by Manufacturer",
      xlab="Manufacturer", ylab="Midrange Price")
