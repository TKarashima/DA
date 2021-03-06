## principal component analysis
## principal components v factor analysis
# http://support.minitab.com/en-us/minitab/17/topic-library/modeling-statistics/multivariate/principal-components-and-factor-analysis/differences-between-pca-and-factor-analysis/

# for review:
# http://www.ats.ucla.edu/stat/sas/output/principal_components.htm

setwd(dir = "~/Desktop/Google Drive/SDSU/DA-Exam-2017/")

# ggplot2 examples
library(readr)
library(dplyr)
options(dplyr.width = Inf)

library(ggplot2) 

#df <- read_csv(file = "df.csv")
## see data from 1. EDA.R
glimpse(df)


# create factors with value labels 
# Run code from 1. EDA.R

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors, 
# with varimax rotation 
numericdata <- sapply(df, is.numeric)
#numericdata[c(4,5,6)] <- FALSE ## omitting Price as this may be our dependent var
numericdata[c(4,6, 7, 8)] <- FALSE ## omitting Minimum Price and Maximum Price, and city and hw mpg
## city mpg will be the dependent variable for future analysis!

## remove missing data
pcdata <- na.omit(df[, numericdata]) ## drops two observations


library(reshape2)

correlations <- cor(pcdata, method = "spearman")
qplot(x=Var1, y=Var2, data=melt(correlations), fill=value, geom="tile",
      xlab = "", ylab = "", main = "Correlation matrix of numeric data (spearman)") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

## there seems to be some severe collinearity!

# If any of the correlations are too high (say above .9), you may need to remove one of the variables from 
# the analysis, as the two variables seem to be measuring the same thing.  Another alternative would be to 
# combine the variables in some way (perhaps by taking the average).  If the correlations are too low, say 
# below .1, then one or more of the variables might load only onto one principal component (in other words,
# make its own principal component).  This is not helpful, as the whole point of the analysis is to 
# reduce the number of items (variables).





# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 
fit <- princomp(x = pcdata, cor=TRUE)

summary(fit) # print variance accounted for 

loadings(fit) # 13 pc loadings 

plot(fit,type="lines", main = "scree plot") # scree plot 

qplot(x = 1:13, y = fit$sdev^2, geom = c("line", "point"), 
      main = "scree plot", xlab = "components", ylab = "Variance")



qplot(x = 1:13, y = (fit$sdev^2) / sum(fit$sdev^2), geom = c("line", "point"), 
      main = "scree plot (proportion of variance)", xlab = "components", ylab = "Proportion Variance Explained")



head(fit$scores[,1:2]) # the first two components account for 76% of the variance in the data


biplot(fit, choices = c(1,2), pc.biplot = FALSE)
biplot(fit, choices = c(1,2), pc.biplot = TRUE)
# If true, use what Gabriel (1971) refers to as a "principal component biplot", with lambda = 1 
# and observations scaled up by sqrt(n) and variables scaled down by sqrt(n). 
# Then inner products between variables approximate covariances and distances between observations 
# approximate Mahalanobis distance.


biplot(fit, choices = c(2,3))






components <- data.frame(fit$scores[,1:2])
rnames <- apply(df[, c("Manufacturer" , "Model")], 1, function(x) paste(x[1], x[2]))
rownames(components) <- rnames

library(mclust)
mfit <- Mclust(data = components)
  
plot(mfit)  
  
  
  
  
  
  
  
  
  






