# Case Study Solutions : GrinnellHouses.csv file
#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 1:
# Reading the CSV file and dropping the first column
data=read.csv('GrinnellHouses.csv')
# View the data loaded
data
# Dropping the first column which is nothing but the Serial number
data=data[2:16]
# View the dimensions (shape) of the data to be used for the analysis
dim(data)
# There are 929 rows and 15 columns

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 2:

data$profit <- (data$OrigPrice - data$SalePrice)


#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 3:

summary(data)


# Check the datatypes
str(data)
sapply(data, class)

# We observe NA's in Squarefeet and LotSize columns in the given dataset
# List Price has the max value of 695K and the median being 125K
# Sale Price has the max value of 606K and the median being 119K
# Year Built has the latest value in 2013 and as early as 1870
# Year Sold has the latest vale in 2015 and as early as 2005
# Bedrooms have a max value of 8 and Baths have a max value of 6
# Profit has a max value of 173K and Median value of 9K

#-------------------------------------------------------------------------------------------------
# Soln. to Question 4:

# Histogram of the newly created feature

hist(data$profit, breaks = 50, xlab="Profit / Loss", main="Net Profit / Loss Distribution", col="lightblue", xlim = c(-20000, 40000))

#-------------------------------------------------------------------------------------------------

# Soln. to Question 5:

for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}


summary(data)


#-------------------------------------------------------------------------------------------------
# Soln. to Question 6:

# Correlation Plot

data <- subset(data, volunteer = 'no', select = c(Bedrooms, Baths, SquareFeet, LotSize, YearBuilt, YearSold, MonthSold, DaySold, OrigPrice, SalePrice, ListPrice, profit))

library(corrplot)
corrplot(cor(data))

cor(data)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 7:

# No of Bedrooms is correlated with No of Bathrooms and Squarefeet
# ( Bigger the house, more the bedrooms and bathrooms)
# Original Price and Sale Price is highly correlated
# The original and sale prices are correlated with bathrooms, square feet
# From the correlation plot: we know that List Price have a high correlation to some variables such as Bathrooms, Square feet of house, Year built, orig price and Sale price

# 2 of the variables which profit is highly correlated with are : Square Feet and Orig Price

#-------------------------------------------------------------------------------------------------
# Soln. to Question 8:

library(car)
# Scatterplot and box plot of two most correlated features
scatterplot(y = data$profit, x = data$SquareFeet,
            main = 'Profit & Square Feet' ,
            ylab = 'Profit', xlab = 'Square Feet' ,
            regLine=list(method=lm, lty=1, lwd=2, col='red'),
            grid = FALSE)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 9:

scatterplot(y = data$profit, x = data$OrigPrice,
            main = 'Profit & Original Price' ,
            ylab = 'Profit', xlab = 'Original Price' ,
            regLine=list(method=lm, lty=1, lwd=2, col='red'),
            grid = FALSE)


#-------------------------------------------------------------------------------------------------
# Soln. to Question 10:

# Observations :
# It appears the original price of the home and square footage of the home have potential to be used for prediction of profit / loss. 
# The two variable have high correlation to Profit and appear to have a linear relationship.

#-------------------------------------------------------------------------------------------------
# Soln. to Question 11:

library(ggplot2)
ggplot(data, aes(ListPrice)) + 
  geom_histogram(col="pink", aes(fill=..count..)) +
  scale_fill_gradient("Count", low="white", high="coral") + 
  labs(title = "Price histogram", x = "Price", y = "Count")

#-------------------------------------------------------------------------------------------------
# Soln. to Question 12:

boxplot(ListPrice~Bedrooms, data = data, xlab = "Bedrooms", ylab="List Price", main = "List Price vs Bedrooms")

# Observation:
# It can be seen that the distribution is different for bedrooms wrt. List Price
# There are also a lot of outliers observed for 3,4,5 bedrooms

#-------------------------------------------------------------------------------------------------
# Soln. to Question 12:

boxplot(ListPrice~MonthSold, data = data, xlab = "Months", ylab="List Price", main = "List Price vs Months Sold")

# Observation:
# It can be seen that the distribution are almost similar except for few months wrt. List Price
# There are also potential outliers seen ( High seen in months : April(4), August (8) and November (11)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 13:

boxplot(ListPrice~YearSold, data = data, xlab = "Years", ylab="List Price", main = "List Price vs Year Sold")

# Observation:
# It can be seen that the distribution are almost similar except for few months wrt. List Price
# There are also potential outliers seen (High seen in : 2007, 2008 and 2012)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 14:

# Regression Assumptions :

# Linear regression makes several assumptions about the data, such as :
  
# 1) Linearity of the data. The relationship between the predictor (x) and the outcome (y) is assumed to be linear.
# 2) Normality of residuals. The residual errors are assumed to be normally distributed.
# 3) Homogeneity of residuals variance. The residuals are assumed to have a constant variance (homoscedasticity)
# 4) Independence of residuals error terms.

#-------------------------------------------------------------------------------------------------
# Soln. to Question 15:

# Potential problems include:
# a) Non-linearity of the outcome - predictor relationships
# b) Heteroscedasticity: Non-constant variance of error terms.
# c) Presence of influential values in the data that can be:
# d) Outliers: extreme values in the outcome (y) variable
# e) High-leverage points: extreme values in the predictors (x) variable


#-------------------------------------------------------------------------------------------------
# Soln. to Question 16:

model1<- lm (SalePrice ~ ListPrice, data=data)
summary(model1)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 17:

# The adjusted R-squared value and the p-significance shows the variables are highly related

#-------------------------------------------------------------------------------------------------
# Soln. to Question 18:

# The diagnostic plots show residuals in four different ways:
  
# a) Residuals vs Fitted. Used to check the linear relationship assumptions. 
# A horizontal line, without distinct patterns is an indication for a linear relationship, what is good.

# 2) Normal Q-Q. Used to examine whether the residuals are normally distributed. 
# It's good if residuals points follow the straight dashed line.

# 3) Scale-Location (or Spread-Location). Used to check the homogeneity of variance of the residuals (homoscedasticity). 
# Horizontal line with equally spread points is a good indication of homoscedasticity. 

# 4) Residuals vs Leverage. Used to identify influential cases, that is extreme values that might influence the regression results when included or excluded from the analysis. 

#-------------------------------------------------------------------------------------------------
# Soln. to Question 19:

# Diagnostic Plots
par(mfrow = c(2, 2))
plot(model1)


#-------------------------------------------------------------------------------------------------
# Soln. to Question 20:

model2<-lm(profit~Bedrooms+Baths+SquareFeet+LotSize+YearBuilt+YearSold+MonthSold+DaySold+SalePrice+ListPrice, data = data)
summary(model2)

# Observations :
# The significant variables as observed from the above summary are :
# Baths, SquareFeet, LotSize, OrigPrice and SalePrice
# We found that as the amenities increased the price also increased
  
#-------------------------------------------------------------------------------------------------
# Soln. to Question 21:

# Diagnostic Plots
par(mfrow = c(2, 2))
plot(model2)

#-------------------------------------------------------------------------------------------------