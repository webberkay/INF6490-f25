
#load data 
data("ToothGrowth")
df <- ToothGrowth 
head(df) 

#Check Structure 
str(df) 

#Histogram 
hist(df$len, 
     main = "Distribution of Tooth Length",
     xlab = "Lenght", 
     col = "lightblue",
     border = "black")

#Scatterplot (length vs. dose)
plot(df$dose, df$len, 
     main = "Tooth Length vs Dose",
     xlab = "Dose",
     ylab = "Tooth Lenght",
     pch = 19, 
     col = "blue")
abline(lm(len ~ dose, data = df), col = "red", lwd = 2)

#Boxplot (length by supplement)
boxplot( len ~ supp, data = df,
         main = "Tooth Length by Supplement Type",
         ylab = "Tooth Length",
         col = c("orange", "lightgreen"))

#Regression Model 
model <- lm(len ~ dose + supp, data = df)
summary(model) 

#Model Evaluation 
par(mfrow = c(2,2))
plot(model)
par(mfrow = c(1,1))

#Predictions
newdata <- data.frame(dose = c(1.5, 1.5), supp = c("VC", "OJ"))

pred <- predict(model, newdata, interval = "confidence")
cbind(newdata, pred)
