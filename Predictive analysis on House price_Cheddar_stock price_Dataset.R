#APPLIED LINEAR STATISTICAL MODELS

#(1) The file stockdata.csv uploaded to the blackboard is a dataset that contains the 
#price of a stock in the last 100 days as the response and the following variables as 
#predictors:
#- vol: Volatility of the stock
#- cap.to.gdp: The ratio of the market cap to GDP
#- q.ratio: The ratio of market cap to net worth
#- gaap: Shiller Cape index
#- avg.allocation: Average investor equity allocation of the stock
#Fit a model to explain price in terms of the predictors. Perform regression diagnostics 
#to answer the following question. Display any plots that are relavant and explain your 
#reasoning. Suggest possible improvements if there are any.

#Loading the required libraries

library(readr)
library(perm)
library(lmtest)
library(car)
library(MASS)
library(EnvStats)
library(ggplot2)

#Importing the dataset.

stock_data <- read_csv("/Users/nikkureddy/Downloads/ISLM_Final/stockdata.csv")

summary(stock_data)
plot(stock_data)

stock_data <- na.omit(stock_data) 
#There are no NAs

hist(stock_data$cap.to.gdp, xlab = "cap.to.gdp", main = "Histogram of cap.to.gdp")
hist(stock_data$q.ratio, xlab = "q.ratio", main = "Histogram of q.ratio")
hist(stock_data$gaap, xlab = "cap.to.gaap", main = "Histogram of gaap")
hist(stock_data$avg.allocation, xlab = "avg.allocation", main = "Hist of avg.allocation")
hist(stock_data$price, xlab = "price", main = "Histogram of price")
#hist(stock_data$vol, xlab = "volatility", main = "Histogram of volatility")

boxplot(stock_data$price, stock_data$vol, stock_data$cap.to.gdp, stock_data$q.ratio,stock_data$gaap, stock_data$avg.allocation)

#looking at the boxplot, we see that the means are different and there are no outliers.

#(a) Fit a model to explain price in terms of the predictors. Which variables are important, can any of the variables 
# be removed ? Please use F-tests to justify.

#Performing regression on price as the response and vol, cap.to.gdp, q.ratio, gaap, avg.allocation as the predictors:

model_1 <- lm(price ~ vol + cap.to.gdp + q.ratio + gaap + avg.allocation, data = stock_data)
summary(model_1)

#From the summary we see that variable vol is not statistically significant and R-square is pretty descent.
#Let us run the regression removing vol and check the performance of the model:

model_2 <- lm(price ~ cap.to.gdp + q.ratio + gaap + avg.allocation, data = stock_data)
summary(model_2)
plot(model_2)

#From the summary, we see that all variables and intercept are statistically significant.
#The R^2 is good which is 83.5%.

#Let us check if the model is good by performing the F-test on the model:

var.test(stock_data$price, stock_data$cap.to.gdp + stock_data$q.ratio + stock_data$gaap 
                    + stock_data$avg.allocation, conf.level=0.99)

#From the F test in comparing the varainces, we see that at 99% confidence interval, the ratio of varaince is very
#low and the F-statistics from the summary of the regression model is 126.4 and also the p-value is almost 0.
#This can be considered as the good regression model for analysis.


#(b) Construct confidence intervals using permutation tests.

#permutation= onesampleper

#oneSamplePermutationTest(model_2$coefficients, alternative = "two.sided", mu = 0, exact = FALSE, 
                         #n.permutations = 5000, seed = NULL)

pt <- t.test(stock_data$price,mu=stock_data$price[1],alternative="two.sided", alpha=.05, n.permutations = 500)
pt

#At 95% confidence interval, taking 500 permutations, we get the t value as 9.1958 with p-value almost equal to 0.
#The upper limit for price being 1.156705 and the lower limit being 1.151785

#(c) Check the constant variance assumption for the errors

#To check for the constant variance assumption of the error, we can perform ANOVA on the model:

bptest(model_2)

#From the above result we see that the p-value is 0.7487.
#Also, from the residuals vs fitted plot, we see that the regression line is not linear.
#This means that there is a presence of constant variance in our model, we cannot reject the null hypothesis
#There is presence of homoscedasticity.

#Another way to check the constant variance is through the ncv test.

ncvTest(model_2)

#(d) Check the independentness of the errors assumption.

#One way is to check the correlation of the residuals:
n <- length(residuals(model_2))
cor(tail(residuals(model_2),n-1), head(residuals(model_2),n-1))

#We can also use D-W autocorrelation test to check the independentness of the error:
durbinWatsonTest(model_2)

#The result is 0.05242293

#(e) Check the normality assumption

#qqPlot(model_2, main="QQ Plot")

# distribution of studentized residuals

sresid <- studres(model_2) 
hist(sresid, freq=FALSE, col = "blue", main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit, col = "red")

#Since the errors in the residuals plot ie from the Normal Q-Q plot (qqnorm), 
#We can see they are very well scattered, the errors do fit to the normality assumptions.

#(f) Is nonlinearity a problem?

# Evaluate Nonlinearity
# component + residual plot 
crPlots(model_2)

#From the Component + Residual Plot we see that non-linearity is a problem for the dataset, since most of the points
#are converging towards the regression line for all the predicting variables. So, it is preferred the data to be
#linear.


#(g) Check for outliers, compute and plot Cook’s distance

cooksd <- cooks.distance(model_2)
head(cooksd)
plot(cooksd, pch="*", cex=1, main="Influential Observations - Cooks distance")  
abline(h = 4*mean(cooksd, na.rm=T), col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="purple")

#From the Cook's distance plot we see that the infuential data points are highlighted in purple above a threshold
#set at 4* mean of the cook's distance computation.

#(h) Check for influential points.

# added variable plots 
avPlots(model_2)
# Cook's D plot
# identifying the D values > 4/(n-k-1) 
cutoff <- 4/((nrow(stock_data)-length(model_2$coefficients)-2)) 
plot(model_2, which=4, cook.levels=cutoff)

# Influence Plot 
influencePlot(model_2,	id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance")

#The plot shows the infuential points where the circumference of the circle is proportional to the Cook's Distance.

#(i) The return at time t is defined as:
#r(t) = p(t + 1)/p(t) − 1
#where p is the price data for day t. Are the returns normally distributed? Please justify your answer using Q-Q plots 
#and normality tests.
head(stock_data)
for (i in stock_data$X1[1:99])  
  stock_data$return[i]=stock_data$price[i=i+1]/(stock_data$price[i]-1)

sort(stock_data$return)
qqPlot(stock_data$return, main="QQ Plot")
#distribution of studentized residuals
#lines(xreturn, yreturn)

x <- as.numeric(stock_data$return[1:100])
h<-hist(x, breaks=10, col="purple", xlab="Stock Return", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(sort(x)),max(sort(x)),length=66) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="red", lwd=2)

#From the plot we can say that the returns are almost normally distributed.


####################################################################################################

#(2) Repeat the same question from (a) to (h) on the cheddar dataset (except part (i)) 
#from the book by fitting a model with taste as the response and the other three 
#variables as predictors. Answer the questions posed in the first question.

#Loading the required libraries

library(readr)
library(faraway)
library(lmtest)
library(MASS)
library(EnvStats)

#Loading the data
data(cheddar)

cheddar_data <- data.frame(cheddar)
summary(cheddar_data)
plot(cheddar_data)

cheddar_data <- na.omit(cheddar_data) #There are no NAs

hist(cheddar_data$taste, xlab = "taste", main = "Histogram of taste")
hist(cheddar_data$Acetic, xlab = "Acetic", main = "Histogram of Acetic")
hist(cheddar_data$H2S, xlab = "H2S", main = "Histogram of H2S")
hist(cheddar_data$Lactic, xlab = "Lactic", main = "Histogram of Lactic")

boxplot(cheddar_data$taste, cheddar_data$Acetic, cheddar_data$H2S, cheddar_data$Lactic)

#Looking at the boxplot, we see that the means are different and there are no outliers.

#(a) Fit a model to explain price in terms of the predictors. Which variables are important, can any of the variables 
#be removed ? Please use F-tests to justify.

#Let us fit a regression model with taste as the response and other variables ie Acetic, H2S and Lactic being the 
#predictors:

m_1 <- lm(taste ~ Acetic + H2S + Lactic, data = cheddar_data)
summary(m_1)

#From the summary, we see that Intercept and Acetic are not statisticall significant.
#Further, let us remove Acetic variable and retain the Intercept and run the regression model to observe the 
#performance of the model.

m_2 <- lm(taste ~ H2S + Lactic, data = cheddar_data)
summary(m_2)

#From the summary, we see there is a marginal improvement in the R^2.
#The variable Lactic is significant but not extremely significant.
#Let us look at the model by removing this variable.

m_3 <- lm(taste ~ H2S, data = cheddar_data)
summary(m_3)

#We see the R^2 has not improved.
#It is better to retain our second model.

m_4 <- lm(taste ~ 0 + H2S, data = cheddar_data)
summary(m_4)

#From the summary, we see that there is a significant increase in R^2 and also we see that H2S is 
#statistically significant.
#Let us check with sqrt(response) and see the performance of the model

m_5 <- lm(sqrt(taste) ~ 0 + H2S, data = cheddar)
summary(m_5)
plot(m_5)

#Here we see that Adjusted R^2 has improved substantially ie 0.9384

#Let us check if the model is good by performing the F-test on the model:

var.test(cheddar_data$taste, cheddar_data$H2S, conf.level=0.99)

#From the F test in comparing the variances, we see that at 99% confidence interval, the ratio of variance is high
#and the F-statistics from the summary of the regression model is 58.413 and also the p-value is almost 0.
#This can be considered as the good regression model for analysis when compared to other models.

#(b) Construct confidence intervals using permutation tests.

pt1 <- t.test(cheddar_data$taste, mu=cheddar_data$taste[1],alternative="two.sided", alpha=.05, n.permutations = 600)
pt1

#At 95% confidence interval, taking 600 permutations, we get the t value as 4.122 with p-value almost equal to 0.
#The upper limit for price being 30.60319 and the lower limit being 18.46347


#(c) Check the constant variance assumption for the errors

#To check for the constant variance assumption of the error, we can perform ANOVA on the model:

#bptest(m_5)--> not applicable because our model do not have intercept value.
#NCV Test
ncvTest(m_5)

#From the above result we see that the p-value is 0.57498.
#Also, from the residuals vs fitted plot, we see that the regression line is not linear.
#This means that there is a presence of constant variance in our model, we cannot reject the null hypothesis
#There is presence of homoscedasticity.

#(d) Check the independentness of the errors assumption.

#One way is to check the correlation of the residuals:
n <- length(residuals(m_5))
cor(tail(residuals(m_5),n-1), head(residuals(m_5),n-1))

#We can also use D-W autocorrelation test to check the independentness of the error:
durbinWatsonTest(m_5)


#(e) Check the normality assumption
#qqPlot(m_5, main="QQ Plot")


# distribution of studentized residuals

sresid <- studres(m_5) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit, col="Red")

#From the errors in the residuals plot ie from the Normal Q-Q plot (qqnorm), 
#We can see they are not very well scattered, the errors do not fit to the normality assumptions.

#(f) Is nonlinearity a problem?

# Evaluate Nonlinearity
# component + residual plot 
crPlots(m_5)

#From the Component + Residual Plot we see that non-linearity is a problem for the dataset, since most of the points
#are not converging towards the regression line. So, it is preferred the data to be linear.


#(g) Check for outliers, compute and plot Cook’s distance

cooksd <- cooks.distance(m_5)
head(cooksd)
plot(cooksd, pch="*", cex=1, main="Influential Observations - Cooks distance")  
abline(h = 4*mean(cooksd, na.rm=T), col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="purple")

#From the Cook's distance plot we see that the infuential data points are highlighted in purple above a threshold
#set at 4* mean of the cook's distance computation.

#(h) Check for influential points.

# Cook's D plot
# identifying the D values > 4/(n-k-1) 
cutoff <- 4/((nrow(cheddar_data)-length(m_5$coefficients)-2)) 
plot(m_5, which=4, cook.levels=cutoff)

# Influence Plot 
influencePlot(m_5,	id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance")

#The plot shows the infuential points where the circumference of the circle is proportional to the Cook's Distance.


####################################################################################################

#(3) The problem is to discover relation between US new house construction starts data (HOUST) and macro economic
#indicators: GDP, CPI and Population (POP). Please download the relevant data from house.rar from blackboard. 

#(a) Data preparation: combine all data into an R dataframe object, and construct dummy or factor variable for 4 
#quarters. First model is HOUST ∼ GDP + CPI + quarter.

#Loading the required libraries

library(readxl)
library(plyr)
library(fpp)
library(TTR)
require(sqldf)
require(zoo)

HOUST <- read_xls("/Users/nikkureddy/Downloads/ISLM_Final/House/HOUST.xls")
CPI <- read_xls("/Users/nikkureddy/Downloads/ISLM_Final/House/CPI.xls")
GDP <- read_xls("/Users/nikkureddy/Downloads/ISLM_Final/House/GDP.xls")
POP <- read_xls("/Users/nikkureddy/Downloads/ISLM_Final/House/POP.xls")

#Merging all the 4 files

house = merge(x = CPI ,y= GDP , by.x = 'DATE' , by.y = 'DATE' ,all='TRUE')
house = merge(x = house , y= HOUST ,  by.x = "DATE" , by.y= "observation_date", all = "TRUE")
house = merge(x = house, y =POP , by.x="DATE" , by.y= "observation_date", all="TRUE")
house = na.omit(house)
head(house)

#Defining the variables

Date <- house$DATE
CPI <-  house$VALUE.x
GDP <- house$VALUE.y
Houst <- house$HOUST
Pop <- house$B230RC0Q173SBEA_CHG
summary(house)

#Formatting the date and factoring the Quarters

df1 = as.yearqtr(house$DATE, format = "%Y-%m-%d")
qtr = as.numeric(format(df1, format="%q"))
house1 = data.frame(Date, CPI, GDP, Houst, Pop, qtr)
house1$qtr= factor(house1$qtr)

plot(house1)
summary(house1)

#From the summary we see that the new variable qtr is created which refers to the quarters taken from the Date variable.

#First model : Linear Regression model with Houst as the response and GDP, CPI and qtr as the predictors, we get:

h1 <- lm(Houst ~ GDP + CPI + qtr, data= house1)
summary(h1)
plot(h1)
plot(density(house1$Houst))

#From the summary we see that, GDP, CPI, qtr4 are not statistically significant wrt the quarter 1 taken as the reference
#level.
#Also the Adjusted R^2 is very less ie 0.1515. F-Statistics being 6.677.

#(b) Do you think the data needs some cleaning? If so, clean the data.

boxplot(house1)
plot(Houst ~ qtr, house1)
sapply(house1,function(h1) sum(is.na(h1)))

#The data does not require any further cleaning as seen from the analysis on the graphs.

#(c) Is there a seasonal effect you observe in data? Show necessary steps and explanation. This is an 
#open-ended question and you are free to use any tool that you find appropriate.

#To check for seasonality, let us convert the dataset to time series, with frequency being 4 as the data is 
#quarterly

house_ts <- ts(house1$Houst,frequency = 4)
head(house_ts)
plot(house_ts)

#From the plot, we see that there is some trend and seasonality in the dataset.
#To clearly understand, let us decompose the dataset and plot the time series graph:
decom <- decompose(house_ts)
plot(decom)

#From the plot, we can clearly observe that there is a seasonality in the dataset.
head(decom$seasonal)

#The above values show the seasonality value of the data points that is contributing to the dataset.

#The seasonal plot is shown below:
seasonplot(house_ts)

#Hence from the observed results above, we confirm that there is a seasonal effect in the data.

#OR

#We can also explain the seasonality effect using the ANOVA function.
#Let us see how the model performs taking only the quarters as the predictor.

h2 = lm(Houst ~ qtr , data= house1) 
anova(h2)

round(coef(h2),1)
#Quarter 1 is taken as the refernce level, Quarter2 ,Quarter3, Quarter4 are greater by 111.2, 92.4 and 32.4
#respectively to the average. 
#Let us look at the design matrix.

head(model.matrix(h2))

#The three test statistics for the group levels correspond to comparisons with the reference
#Quarter 1. We see a pattern in the matrix. 

anova(h1)

#We see that there is a difference in the Quarters and hence there is seasonal effect in the dataset.

#(d) Do a pair-wise comparison for different quarters. Which quarter do you think is the best one to buy a 
# house? Show necessary steps and explanation. Use any statistical test or tool that you think is appropriate, 
# this is an open-ended question and there is no one way of answering the question.

#let us look at the pairwise using t test wrt the variables Houst and quarters:

pairwise.t.test(house1$Houst, house1$qtr, p.adj = "none")

#The pariwise comparison using t test with pooled Standard Deviation is shown above.
#We see that Q2-Q1 has the least value whereas Q3-Q2 is the highest.

#Let us also look at the pairwise using the TukeyHSD:

pwa <- TukeyHSD(aov(h1))
plot(pwa)

#The Tukey multiple comaprison of means with 95%confidence interval is shown above.
#We see that the padj value is minimum for Q2-Q1 and highest for Q3-Q2.

#From both the sets and plot, we can infer that the best quarter to buy the house is when the Houst value is low.
# Lower value of Houst means padj must be high as we are not selling but buying the house.
#In Conclusion Quarter 3 seems to be a good choice to buy the house whereas Quarter 1 would be the least choice 
#as the prices are higher when compared to other quarter relations in the pair-wise comparison analysis.


#(e) Add population to the first model, do the steps (b) and (c) again.

#Adding Pop to the first model, we get:

h3 <- lm(Houst ~ GDP + CPI + qtr + Pop, data= house1)
summary(h3)
plot(h3)

#From the summary we see that, GDP, CPI, qtr4 and Pop are not statistically significant wrt the quarter 1 taken as the
#reference level.
#Also the Adjusted R^2 is very less ie 0.1477. F-Statistics being 5.594.
#Thus, adding a new varaible did not make any improvement in the performance of the model.

#Checking for data cleaning.

boxplot(house1)
plot(Houst ~ qtr, house1)
sapply(house1,function(h1) sum(is.na(h1)))

#The data does not require any further cleaning as seen from the analysis on the graphs.

#To check for seasonality, let us convert the dataset to time series, with frequency being 4 as the data is 
#quarterly

house_ts <- ts(house1$Houst,frequency = 4)
head(house_ts)
plot(house_ts)

#From the plot, we see that there is some trend and seasonality in the dataset.
#To clearly understand, let us decompose the dataset and plot the time series graph:
decom <- decompose(house_ts)
plot(decom)

#From the plot, we can clearly observe that there is a seasonality in the dataset.
head(decom$seasonal)

#The above values show the seasonality value of the data points that is contributing to the dataset.

#The seasonal plot is shown below:
seasonplot(house_ts)

#Hence from the observed results above, we confirm that there is a seasonal effect in the data.

#OR

#We can also explain the seasonality effect using the ANOVA function.
#Let us see how the model performs taking only the quarters as the predictor.

h2 = lm(Houst ~ qtr , data= house1) 
anova(h2)

round(coef(h2),1)
#Quarter 1 is taken as the refernce level, Quarter2 ,Quarter3, Quarter4 are greater by 111.2, 92.4 and 32.4
#respectively to the average. 
#Let us look at the design matrix.

model.matrix(h2)

#The three test statistics for the group levels correspond to comparisons with the reference
#Quarter 1. We see a pattern in the matrix. 

anova(h3)

#We see that there is a difference in the Quarters and hence there is seasonal effect in the dataset.


##########################################################################################################

#(4) Read the train.csv and test.csv files in R which contains training and test data containing information on ten 
#thousand customers. The aim here is to predict which customers will default on their credit card debt. 

#Hints: In class, we provided solutions in R to a similar problem but for a different dataset.

#Loading the required libraries

#Sys.setenv(JAVA_HOME='/Library/Java/JavaVirtualMachines/jdk-11.jdk/Contents/Home')
#library(rJava)
#ibrary(xlsx)
library(readxl)
library(plyr)
library(fpp)
library(TTR)
require(sqldf)
library(pscl)
library(ggplot2)
library(gridExtra)
library(InformationValue)

#Loading and viewing the train and test data csv files

Test1 <- read.csv("/Users/nikkureddy/Downloads/ISLM_Final/test-default.csv")
Train1 <- read.csv("/Users/nikkureddy/Downloads/ISLM_Final/train-default.csv")

summary(Train1)
summary(Test1)

#Let us analyse the Train data with plots. It is enough to analyze the train data for our purpose as the test data is
#used to predict.

p1 <- qplot(x=Train1$balance, y=Train1$income, color=Train1$default, shape=Train1$default, geom='point')+scale_shape(solid=FALSE)
p2 <- qplot(x=Train1$default, y=Train1$balance, fill=Train1$default, geom='boxplot')+guides(fill=FALSE)
p3 <- qplot(x=Train1$default, y=Train1$income, fill=Train1$default, geom='boxplot')+guides(fill=FALSE)
p1
grid.arrange(p2, p3, nrow=1)

boxplot(Train1)

#From the plots it is observed that the relationship between the predictor balance and the default rate is rather 
#pronounced, this will likely be a major predictor in the forthcoming model.

#The attributes student and default are categorical, converting these to Numeric values Yes = 1, No = 0.

Train1$studentD <- 0
Train1$studentD[Train1$student=="Yes"] <- 1

Train1$Default <- 0
Train1$Default[Train1$default=="Yes"] <- 1

#Deleting the categorical variable columns student and default

Train1 <- subset(Train1,select=-c(student))
Train1 <- subset(Train1,select=-c(default))

summary(Train1)



#Performing the same on the test data
#The attribute student and default are categorical, converting these to Numeric values Yes = 1, No = 0.

Test1$studentD <- 0
Test1$studentD[Test1$student=="Yes"] <- 1

Test1$Default <- 0
Test1$Default[Test1$default=="Yes"] <- 1

#Deleting the categorical variable columns student and default

Test1 <- subset(Test1,select=-c(student))
Test1 <- subset(Test1,select=-c(default))

summary(Test1)



#(a) Fit a logistic regression model with the default as the response and other variables balance and income as 
#the predictor. Make sure that variables in your model are significant. Perform regression diagnostics on this model 
#to answer the following questions. Display any plots that are relevant.

#Fitting the logistic regression model with the default as the response and balance and income as the predictors:

mod1 <- glm(Default ~ balance + income, family = binomial(link='logit'), data = Train1)
summary(mod1)

#Here, in mod1 we see that income is not very statistically significant when compared to the intercept and balance.

#Let us fit a model ignoring the income to check if the model improves:

mod2 <- glm(Default ~ balance, family = binomial(link='logit'), data = Train1)
summary(mod2)

#Now we see that the predictors are statistically significant with AIC 912.69 and p value almost equal to 0.

#Plotting the summary of the model

plot(mod2)

sapply(Train1,function(mod2) sum(is.na(mod2)))
#This data does not need further cleaning.


#(b) Why is your model a good/reasonable model? Check the AIC and pseudo-R2 values.

nullh <- glm(Default ~ 1 -X, family = binomial, data = Train1)
summary(nullh)
1-logLik(mod2)/logLik(nullh)

#OR

pR2(mod2)

pR2(mod1)

#The AIC value for this model is 912.69 and the pseudo R^2 value is 0.472 with r2ML of 0.1259.
#Whereas pseudo R^2 for the first model is 0.4767 which is negligibly higher, but the variable income was not
#statistically significant when compared to others. Considering all these factors, the model with default as the 
#response and balance as the predictor is a good fit for this dataset.

#The 95% confidence interval with intercept and balance is as shown below

confint(mod2)

#(c) Give an interpretation of the regression coefficients (in words).

summary(mod2)

#From the summary of the logistic regression model, we see that the intercept is -11.01 and the β1 ie balance value
# is 0.00566, which indicates that the balance is associated with an increase in probability of default. 
#Therefore, for every unit increase in balance, the log odds of default also increases by 0.00566 units.
#And, the default minimum value is -11.01 units.


#(d) Form the confusion matrix over the test data. What percentage of the time, are your predictions correct?

predicted = predict(mod2, Test1, type = "response")
misClassError(Test1, predicted)
confusionMatrix(Test1$Default, predicted)

#Forming the confusion matrix over the test data, the misclassification error is 2582.
#From the confusion matrix , the diagonal elements (3803 and 40) are predicted accurately whereas the off-diagonal
#elements (98 and 12) are predicted wrong.
#The percentage of the time, the predictions coming out correct is (3803 + 40)/Total = 3843/3953 = 0.97
#97%  of the time, the predictions were accurate.

#(e)In your model, what is the estimated probabilty of default for a student with a credit card balance of $2,000 
#and an income of $40,000? What is the probabilty of the default for a non-student with the same credit card 
# balance and income to default?

# balance = $2,000 , income = $40,000. studentD = 0

#In our previous models, we did not consider studentD as the predictor. We are doing this to calculate the probability
#of the student and non-stundet going default with the given values in the question.

#Fitting the logistic regression model with studentD also as the predictor,

mod3 <- glm(Default ~ balance + income + studentD, family = binomial(link='logit'), data = Train1)
summary(mod3)

#The estimated probabilty of default for a student with a credit card balance of $2,000 
#and an income of $40,000 is given by:
pre_1 <- predict(mod3, data.frame(balance = 2000,income=40000,studentD=1), type = "response")
paste("The estimated probabilty of default for a student with a credit card balance of $2,000 and an income of $40,000 is", pre_1*100)

#The estimated probabilty of default for a non-student with a credit card balance of $2,000 
#and an income of $40,000 is given by:
pre_2 <- predict(mod3, data.frame(balance = 2000,income=40000,studentD=0), type = "response")
paste("The estimated probabilty of default for a non-student with a credit card balance of $2,000 and an income of $40,000 is", pre_2*100)


#(f) Are the variables student and balance are correlated? If yes, why do you think this is the case? 
# If no, please explain.

cor(Train1$balance, Train1$studentD)
cor(Test1$balance, Test1$studentD)

#From the correlation results from both the Train and Test data , we see that there is marginal correlation 
#between balance and student.
#Confounding can be the case, with the given credit card balance students are less likely to be under risk when
#compared to non-students.

#(g) Now, let’s add the binary variable student to the model. Fit a logistic regression model of the form ”
#default balance + income + student”, in other words, regress the variable default to all the other predictors 
#with logistic regression.

mod3 <- glm(Default ~ balance + income + studentD, family = binomial(link='logit'), data = Train1)
summary(mod3)
plot(mod3)

#h) (Extra Credit) Does the data say that it is more likely for a student to default compared to a non-student 
# for different values of income level? Please comment.

mod4 <- glm(Default ~ studentD, family = binomial(link='logit'), data = Train1)
summary(mod4)

#From the result we see that the model predicts that students tend to have higher default probabilities than 
#non-students, as the coefficient of student is positive.

############################################################################################################



