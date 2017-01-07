## 3. Linear regression

df2 <- df[, !(colnames(df) %in% c("Model", "Maximum Price", "Minimum Price"))]
df2 <- na.omit(df2)
glimpse(df2)



linfit <- lm(formula = `Midrange Price` ~ ., data = df2)

summary(linfit)

anova(linfit)


stepfit <- step(object = linfit)

summary(stepfit)

anova(stepfit, linfit) ## keeping those additional variables is not impactful
## keep the smaller model





