data <- read.csv('SeoulBikeData.csv') #load dataset

data$Date <- as.Date(gsub('/', '-', data$Date), format = "%d-%m-%Y") #convert 
#character into datetime format 
data$Month <- format(data$Date, "%m")
#Extract Month from columns (per suggestions by the professor)
#Drop date column as it's no longer useful anymore
data <- data[ , !(names(data) %in% "Date")] 

#stratified sampling to get same number of observations for each month
library(dplyr)
sample <- data %>% 
  group_by(Month) %>% 
  sample_n(size=100)
#randomly sample 1000 data points for the purpose of this report
#response variable is Rented.Bike.Count, predictors are everything else

#Data preprocessing: Convert character columns into factor
df <- data.frame(sample)
row.names(df) <- NULL
df$Seasons <- factor(df$Seasons)
df$Holiday <- factor(df$Holiday)
df$Functioning.Day <- factor(df$Functioning.Day)
df$Month <- as.numeric(df$Month)

pairs(df)
#from the graph we see dew point temperature and temperature have extremely high 
#positive correlation. Like I don't even need to look at R^2, the relationship
#LOOKS like a straight line lmao. As dew point temperature seems to have higher
#collinearity with humidity, drop dew point temperature column first.
df <- df[ , !(names(df) %in% c("Dew.point.temperature"))] 

#Check response variables too:
hist(df$Rented.Bike.Count,
     xlab   = "Bike Rentals",
     col    = "darkorange",
     border = "dodgerblue",
     breaks = 20)
#distribution looks very skewed to the right.


#investigate relationship between Bikes rented and Hour 
library(ggplot2)
ggplot(data=df, aes(x=Hour, y=Rented.Bike.Count)) + geom_point() + 
  geom_smooth(se = FALSE) + geom_vline(aes(xintercept = 16)) + 
  annotate("text", x=16.5, y=3000, label="Hour = 17", angle=270)
#The graph seems to have a bimodal distribution  
df$Rush.Period <- as.factor( ifelse((df$Hour > 16), "rush", "not rush") )
df <- data.frame(df)
#and then start with full model + no interaction
lm_base <- lm(Rented.Bike.Count ~ ., data = df)
summary(lm_base)

#Check if model fits assumptions 
#Residual Plot:
graphics.off()
plot(fitted(lm_base), resid(lm_base), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residual",cex=2,
     main = "Rental Bike Counts: Fitted versus Residuals")
abline(h = 0, col = "darkorange", lwd = 2)

#Normality assumption:
qqnorm(resid(lm_base))
qqline(resid(lm_base), col = "dodgerblue", lwd = 2)
#Linearity, Equal Variance and Normality all clearly violated. First we deal 
#with Linearity
#Maybe adding interactions will improve the situation?
lm_2 <- lm(Rented.Bike.Count ~ .^2, data = df)
summary(lm_2)

plot(fitted(lm_2), resid(lm_2), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residual",cex=2,
     main = "Rental Bike Counts: Fitted versus Residuals")
abline(h = 0, col = "darkorange", lwd = 2)

lm_3 <- lm(Rented.Bike.Count ~ .^3, data = df)
summary(lm_3)

plot(fitted(lm_3), resid(lm_3), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residual",cex=2,
     main = "Rental Bike Counts: Fitted versus Residuals")
abline(h = 0, col = "darkorange", lwd = 2)

qqnorm(resid(lm_3))
qqline(resid(lm_3), col = "dodgerblue", lwd = 2)
#With three-way interactions, Linearity seems to be mostly satisfied. 
#Now we will perform Box-Cox Transformation to satisfy both normality and 
#equal variance:

library(MASS)
par(mfrow=c(1,1))
res <- boxcox(lm(Rented.Bike.Count+1 ~ .^3, data = data))
#TODO: ask TA or prof why this works when I sample over the entire dataset but 
#not over specific dataset

#As lambda is greater than 0, we perform the power transformation:
lambda = res$x[which.max(res$y)]
lm_trans <- lm(((Rented.Bike.Count^(lambda)-1)/(lambda))~ .^3, data = df)

#Check plots for assumptions:
plot(fitted(lm_trans), resid(lm_trans), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residual",cex=2,
     main = "Rental Bike Counts: Fitted versus Residuals")
abline(h = 0, col = "darkorange", lwd = 2)

qqnorm(resid(lm_trans))
qqline(resid(lm_trans), col = "dodgerblue", lwd = 2)
#Shit Box-Cox makes equal variance violation worse and doesn't fix normality
#Back to Variance Stabilizing Transformations
#try g(Y) = sqrt(Y), spread looks linear-ish?:
lm_vst1 <- lm(sqrt(Rented.Bike.Count) ~ .^3, data = df)
plot(fitted(lm_vst1), resid(lm_vst1), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residual",cex=2,
     main = "Rental Bike Counts: Fitted versus Residuals")
abline(h = 0, col = "darkorange", lwd = 2)
#better even though there's still violation
#Did normality improve?
qqnorm(resid(lm_vst1))
qqline(resid(lm_vst1), col = "dodgerblue", lwd = 2)

#Adding predictors: exp(Temperature), Month^2, (Temperature*Month)^2.
#None of them significantly improved model, 
lm_4a <- lm(sqrt(Rented.Bike.Count) ~ .^3 + I(exp(Temperature)), data = df)
summary(lm_4a)
lm_4b <- lm(sqrt(Rented.Bike.Count) ~ .^3 + I((Month)^2), data = df)
summary(lm_4b)
lm_4c <- lm(sqrt(Rented.Bike.Count) ~ .^3 + I((Temperature*Month)^2), data = df)
summary(lm_4c)
plot(fitted(lm_4c), resid(lm_4c), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residual",cex=2,
     main = "Rental Bike Counts: Fitted versus Residuals")
abline(h = 0, col = "darkorange", lwd = 2)

qqnorm(resid(lm_4b))
qqline(resid(lm_4b), col = "dodgerblue", lwd = 2)

#I've tried everything! I'll stick with the Variance Stabilized Model with three
#way interactions. 
#Right now most predictors in the model are not statistically significant so 
#there is likely collinearity going on.
library(car)
vif(lm_vst1)
#Can't calcuate VIF because some variables are perfectly multi-collinear with 
#each other

#TODO: FIgure out how to get rid of perfectly multicollinear variables
