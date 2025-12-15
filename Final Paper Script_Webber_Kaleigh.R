#Load Dataset 
data(mtcars)
df <- mtcars

#Inspect Data
str(df)
summary(df)

#Convert Categorical Variables to Factors 
df$cyl <- as.factor(df$cyl)
df$am <- as.factor(df$am)
df$gear <- as.factor(df$gear)
df$carb <- as.factor(df$carb)

#Check for Missing Values 
colSums(is.na(df))

#Detect Outliers Using Boxplots 

#Boxplot for MPG (miles per gallon)
boxplot(df$mpg, main = "Boxplot of MPG", ylab = "MPG")
cat("Outliers for MPG: ", boxplot(df$mpg, plot = FALSE)$out, "\n")

#Boxplot for WT (weight)
boxplot(df$wt, main = "Boxplot for WT", ylab = "WT")
cat("Outliers for WT: ", boxplot(df$wt, plot = FALSE)$out, "\n")

#Boxplot for HP (horsepower)
boxplot(df$hp, main = "Boxplot for HP", ylab = "HP")
cat("Outliers for HP: ", boxplot(df$hp, plot = FALSE)$out, "\n")

#Scale Continuous Features 
scaled_df <- as.data.frame(scale(df[, c("hp","wt","disp")]))
library(psych)

#Continuous Variables 
describe(df[, c("mpg","hp","wt","disp")])

#Categorical Frequency Tables 
table(df$cyl)
prop.table(table(df$am))

#Univeriate Plots 
hist(df$mpg, main = "MPG Distribution", col = "lightblue")
boxplot(df$hp, main = "Horsepower Boxplot")
barplot(table(df$cyl), main = "Cylinders", col = "orange")

#Multivariate Plots 
pairs(df[, c("mpg","hp","wt","disp")])
library(corrplot)
corrplot(cor(df[, c("mpg","hp","wt","disp")]), method = "color")

#Test Correlations 
cor.test(df$mpg, df$wt)
cor.test(df$mpg, df$hp)

#T-Test: Transmission Type vs MPG
t.test(mpg ~ am, data = df)

#Train/Test Split 
set.seed(123)
index <- sample(1:nrow(df), size = 0.7*nrow(df))
train <- df[index, ]
test <- df[-index, ]

#Build Model 
model <- lm(mpg~wt + hp + cyl + am, data = train)
summary(model)

#Predict and Evaluate 
pred <- predict(model, newdata = test)

#Compute RMSE
rmse <- sqrt(mean((pred - test$mpg)^2))
rmse




