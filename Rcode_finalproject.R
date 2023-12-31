data <- read.csv('SeoulBikeData.csv') #load dataset

data$Date <- as.Date(gsub('/', '-', data$Date), format = "%d-%m-%Y") #convert 
#character into datetime format 
data$Month <- format(data$Date, "%m")
#Extract Month from columns (per suggestions by the professor)
#Drop date column as it's no longer useful anymore
data <- data[ , !(names(data) %in% "Date")] 
data$ID <- seq_along(data[,1])


#stratified sampling to get same number of observations for each month
set.seed(123)
library(dplyr)
df <- data.frame(data %>% group_by(Month) %>% sample_n(size=100))
#randomly sample 1000 data points for the purpose of this report
#response variable is Rented.Bike.Count, predictors are everything else
#Data preprocessing: Convert character columns into factor
row.names(df) <- NULL

#BASE BASE model (original dataset variables)
lm1 <- lm(Rented.Bike.Count ~ ., data = df)
summary(lm1)


#Function for model diagnostics
check_model_assumption_graphs <- function(lm){
  #Check model assumptions:
  #residual plot
  plot(fitted(lm), resid(lm), col = "grey", pch = 20,
       xlab = "Fitted", ylab = "Residuals", main = "Residual plot")
  abline(h = 0, col = "darkorange", lwd = 2)
  #QQ plot
  qqnorm(resid(lm), main = "Normal Q-Q Plot",col = "darkgrey",pch=20)
  qqline(resid(lm), col = "dodgerblue", lwd = 2)
}

check_model_assumption_graphs(lm1)

#All three assumptions violated. WE CAN DO BETTER!

#Improve the model:
sample <- data.frame(df)
library(GGally) #pairwise plot on numeric variables
ggpairs(sample[,0:10])

#from the graph we see dew point temperature and temperature have extremely high 
#positive correlation. Like I don't even need to look at R^2, the relationship
#LOOKS like a straight line lmao. As dew point temperature seems to have higher
#collinearity with humidity, drop dew point temperature column first.
sample <- sample[ , !(names(sample) %in% c("Dew.point.temperature"))] 

#-----------------------------------------------------------------------------

#Check response variables too:
hist(sample$Rented.Bike.Count,
     xlab   = "Bike Rentals",
     col    = "darkorange",
     border = "dodgerblue",
     breaks = 20)
#distribution looks very skewed to the right -> will need to transform Y.

#Check pairwise plots again for special relationships:
ggpairs(sample)

#investigate relationship between Bikes rented and Hour 
ggplot(data=sample, aes(x=Hour, y=Rented.Bike.Count)) + geom_point() + geom_smooth(se = FALSE)
ggplot(data=sample, aes(x=Hour, y=Rented.Bike.Count)) + geom_point() + 
  geom_smooth(se = FALSE) + geom_vline(aes(xintercept = 16)) + 
  annotate("text", x=16.5, y=3000, label="Hour = 17", angle=270)
#The graph seems to have a bimodal distribution  
sample$Rush.Period <- ifelse((sample$Hour > 16), "rush", "not rush")

ggplot(sample, aes(x=Temperature, y=Dew.point.temperature)) + geom_point()
#pretty strongly positive linear relationship
ggplot(sample, aes(x=Rush.Period, y=Rented.Bike.Count)) + geom_boxplot()
#Pretty significant difference

#Relationship between rented bikes and weather info
pairs(sample[,0:9])
#It seems like the relationship between bikes rented and 
#rainfall/snowfall is almost binary
ggplot(sample, aes(x=Rainfall.mm., y=Rented.Bike.Count)) + geom_point()
ggplot(sample, aes(x=Snowfall..cm. , y=Rented.Bike.Count)) + geom_point()
ggplot(sample, aes(x=Temperature , y=Snowfall..cm.)) + geom_point()

sum(sample$Rainfall.mm. == 0)
sum(sample$Snowfall..cm. == 0)
#yup, basically unbalanced binary distribution; transform rainfall/snowfall into
#categorical variables
sample$Rain <- ifelse((sample$Rainfall.mm. > 0), "raining", "no rain")
sample$Snow <- ifelse((sample$Snowfall..cm. > 0), "snowing", "no snow")

ggplot(sample, aes(x=Rain, y=Rented.Bike.Count)) +  geom_boxplot()
ggplot(sample, aes(x=Snow, y=Rented.Bike.Count)) +  geom_boxplot()
#significant difference in rental rates depending on if it's raining/snowing that
#day

#drop ID
sample<- data.frame((sample[ , !(names(sample) %in% c("ID"))]))
#------------------------------------------------------------------------------

#Interactions:
#Temperature/Seasons, Temperature/Solar Radiation, Humidity/Solar Radiation
#Wind speed/Solar Radiation, Humidity/Rain, Hour/Temperature/Solar Radiation,
#Humidity/Wind speed/Rush period

#Add interactions:
lm2 <- lm(Rented.Bike.Count ~ . + (Temperature * Humidity) + (Wind.speed..m.s. * Seasons) + 
(Solar.Radiation*Rush.Period) + 
(Hour * Rush.Period * Seasons) + (Humidity * Wind.speed..m.s. * Visibility..10m.), 
data = sample)
summary(lm2)

check_model_assumption_graphs(lm2)

#------------------------------------------------------------------------------

#Fit is significantly better, but violations still exist. Let's try some new stuff

#Polynomial X terms:
# Temperature?
sample <- as.data.frame(sample)
ggplot(sample, aes(x=Temperature, y=Rented.Bike.Count)) + geom_point()
#Definitely not linear (I'll try second + third term)

lm3 <- lm(Rented.Bike.Count ~ . + (Temperature * Humidity) + (Wind.speed..m.s. * Seasons) + 
            (Solar.Radiation*Rush.Period) + (Hour * Rush.Period * Seasons) + 
            (Humidity * Wind.speed..m.s. * Visibility..10m.) + I(Temperature^3), 
           data = sample)
summary(lm3)
check_model_assumption_graphs(lm3)
#This is like, the best I can do, man

#Y transformations:
#According to stackexchange my residual plot looks like...that is because my residuals are capped
#to a lower boundary. This makes sense as my data has a strong right skew.
#The simplest transformation is taking ln() of Y:

lm4 <- lm(log(Rented.Bike.Count+1) ~ . + (Temperature * Humidity) + (Wind.speed..m.s. * Seasons) + 
            (Solar.Radiation*Rush.Period) + (Hour * Rush.Period * Seasons) + 
            (Humidity * Wind.speed..m.s. * Visibility..10m.) + I(Temperature^3), 
          data = sample) #correct for 0s in response variable
summary(lm4)
check_model_assumption_graphs(lm4)
#There's like this linear cluster of residuals??????

sum(sample$Rented.Bike.Count == 0)
sample <- data.frame(sample %>%  filter(Rented.Bike.Count != 0))

#Bikes are only rented on weekdays in my sample???
#BIKES ARE ONLY RENTED ON WEEKDAYS PERIOD???
sample <- sample[ , !(names(sample) %in% c("Functioning.Day"))]

lm5 <- lm(log(Rented.Bike.Count) ~ . + (Temperature * Humidity) + (Wind.speed..m.s. * Seasons) + 
            (Solar.Radiation*Rush.Period) + (Hour * Rush.Period * Seasons) + 
            (Humidity * Wind.speed..m.s. * Visibility..10m.) + I(Temperature^3), 
          data = sample) 
summary(lm5)
check_model_assumption_graphs(lm5)

#Linearity assumption mostly satisfied, EV and Normality still violated

#BoxCox on top of log transformation?
library(MASS)
par(mfrow=c(1,1))

boxcox_result <- boxcox(log(Rented.Bike.Count) ~ . + (Temperature * Humidity) + (Wind.speed..m.s. * Seasons) + 
(Solar.Radiation*Rush.Period) + (Hour * Rush.Period * Seasons) + 
  (Humidity * Wind.speed..m.s. * Visibility..10m.) + I(Temperature^3), 
       data = sample, lambda = seq(0, 5, by = 0.05))
#As lambda is wayyyyy larger than 0, get optimal lambda to transform Y.
lambda = boxcox_result$x[which.max(boxcox_result$y)]
lm_trans <- lm(((logb(Rented.Bike.Count, base = 5)^(lambda)-1)/(lambda))~ . + 
(Temperature * Humidity) + (Wind.speed..m.s. * Seasons) + (Solar.Radiation*Rush.Period) 
+ (Hour * Rush.Period * Seasons) + (Humidity * Wind.speed..m.s. * Visibility..10m.) 
+ I(Temperature^3),data=sample)
summary(lm_trans) #slight imporvement on goodness of fit
check_model_assumption_graphs(lm_trans) #EV and Normality definitely improved
#BP test just to see how bad the EV violation is
library(lmtest)
bptest(lm_trans) #p < 2.2e^-16, yikes

#What if I did boxcox first then log transform?
lm_trans2 <- lm((logb(Rented.Bike.Count^(lambda-1)/(lambda), base = 5))~ . + 
(Temperature * Humidity) + (Wind.speed..m.s. * Seasons) + (Solar.Radiation*Rush.Period) 
+ (Hour * Rush.Period * Seasons) + (Humidity * Wind.speed..m.s. * Visibility..10m.) 
+ I(Temperature^3),data=sample)
summary(lm_trans2) #goodness of fit worse
check_model_assumption_graphs(lm_trans2) #Normality violation is worse

#VST on top of log transformation?

lm6 <- lm(sqrt(logb(Rented.Bike.Count, base = 5)) ~ .  + (Temperature * Humidity) 
+ (Wind.speed..m.s. * Seasons) + (Solar.Radiation*Rush.Period) 
+ (Hour * Rush.Period * Seasons) + (Humidity * Wind.speed..m.s. * Visibility..10m.) 
+ I(Temperature^3), data = sample) 
summary(lm6) #goodness of fit worse
check_model_assumption_graphs(lm6) 
bptest(lm6) #YOU HAD ONE JOB!

lm7 <- lm(log(logb(Rented.Bike.Count, base = 5)) ~ .  + (Temperature * Humidity) 
+ (Wind.speed..m.s. * Seasons) + (Solar.Radiation*Rush.Period) 
+ (Hour * Rush.Period * Seasons) + (Humidity * Wind.speed..m.s. * Visibility..10m.) 
+ I(Temperature^3), data = sample) 
summary(lm7) #goodness of fit even worse
check_model_assumption_graphs(lm7) 
bptest(lm7) #YOU HAD ONE JOB! ONE!!!!!

#------------------------------------------------------------------------------

#There's bound to be high collinearity between some of the variables (I literally
#created factors based off of existing variables)
library(olsrr) #VIF from faraway package can't handle complicated models, so VIF from olsrr was used instead.
#Both calculate the same thing so no biggie.
vif_values <- ols_vif_tol(lm_trans)
vif_values[,c("Variables","VIF")] #We see Month has near Infinite VIF; remove first and see results

lm_trans3 <- lm(((logb(Rented.Bike.Count, base = 5)^(lambda)-1)/(lambda))~ . + 
(Temperature * Humidity) + (Wind.speed..m.s. * Seasons) + (Solar.Radiation*Rush.Period) 
+ (Hour * Rush.Period * Seasons) + (Humidity * Wind.speed..m.s. * Visibility..10m.) 
+ I(Temperature^3) -Month,data=sample)

vif_values <- ols_vif_tol(lm_trans3)
vif_values[,c("Variables","VIF")]

summary(lm_trans3)

lm_trans4 <- lm(((log(Rented.Bike.Count, base = 5)^(lambda)-1)/(lambda))~ . + 
(Temperature * Humidity) + (Solar.Radiation*Rush.Period) + 
  (Humidity * Wind.speed..m.s. * Visibility..10m.) 
+ I(Temperature^3) -Seasons ,data=sample)

vif_values <- ols_vif_tol(lm_trans4)
vif_values[,c("Variables","VIF")]

lm_trans5 <- lm(((log(Rented.Bike.Count)^(lambda)-1)/(lambda))~ . + 
(Temperature * Humidity) + (Solar.Radiation * Rush.Period) + 
(Humidity * Visibility..10m.) + I(Temperature^3) -Seasons -Wind.speed..m.s. ,data=sample)

vif_values2 <- ols_vif_tol(lm_trans5)
vif_values2[,c("Variables","VIF")]

summary(lm_trans4)
summary(lm_trans5)
#-------------------------------------------------------------------------------

#TODO: redo this section
fit_back_bic = step(lm_trans5, direction = "backward", k=log(nrow(sample)), trace = 0)
summary(fit_back_bic)
fit_back_aic = step(lm_trans5, direction = "backward", trace = 0)
summary(fit_back_aic)

#BIC model has 24 predictors, AIC has 30, original model has 31

#Is there a statistically significant difference between full and reduced models?
anova(fit_back_bic, fit_back_aic)
anova(fit_back_aic, lm_trans5)

#AIC model performs significantly better than BIC model, 
#but full model does not perform significantly better than AIC model
#AIC is final model

#-------------------------------------------------------------------------------
#Prediction

#filter for observations in original data set that was not used for modelling
test_set <- anti_join(data, df, by = "ID")
#Drop Dew Point Temperature and Functioning Day
test_set <- test_set[ , !(names(test_set) %in% c("Dew.point.temperature"))] 
test_set <- test_set[ , !(names(test_set) %in% c("Functioning.Day"))]
#Add Rush Period, Raining, Snowing
test_set$Rush.Period <- ifelse((test_set$Hour > 16), "rush", "not rush")
test_set$Rain <- ifelse((test_set$Rainfall.mm. > 0), "raining", "no rain")
test_set$Snow <- ifelse((test_set$Snowfall..cm. > 0), "snowing", "no snow")

#sample 500 observations 
test <- data.frame(test_set %>% sample_n(size=500))

#drop ID from actual testing set
test<- data.frame((test[ , !(names(test) %in% c("ID"))]))

#Get predicted values
res_y1 <- test$Rented.Bike.Count - predict(fit_back_aic, newdata = subset(test, select = -c(1)))
#RMSE
sqrt(sum(res_y1^2)/nrow(test))

#test with previously created models
#log-boxcox nested model before applying AIC/BIC
res_y2 <- test$Rented.Bike.Count - predict(lm_trans5, newdata = subset(test, select = -c(1)))
sqrt(sum(res_y2^2)/nrow(test))

#before multicollinearity reduction
res_y3 <- test$Rented.Bike.Count - predict(lm_trans, newdata = subset(test, select = -c(1)))
sqrt(sum(res_y3^2)/nrow(test))

#before boxcox
res_y4 <- test$Rented.Bike.Count - predict(lm5, newdata = subset(test, select = -c(1)))
sqrt(sum(res_y4^2)/nrow(test))

#before log transformation
res_y5 <- test$Rented.Bike.Count - predict(lm3, newdata = subset(test, select = -c(1)))
sqrt(sum(res_y5^2)/nrow(test))

#base model
base <- lm(Rented.Bike.Count ~., data = sample)
res_y6 <- test$Rented.Bike.Count - predict(base, newdata = subset(test, select = -c(1)))
sqrt(sum(res_y6^2)/nrow(test))

#Final chosen model performs better on predictions than all Y-transformed models
#But performs worse with models without Y-transformation
#output all significant coefficients of final model
summary(fit_back_aic)$coefficients[, "Pr(>|t|)"][summary(fit_back_aic)$coefficients[, "Pr(>|t|)"] < 0.05]
