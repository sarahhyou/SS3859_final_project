data <- read.csv('SeoulBikeData.csv') #load dataset

data$Date <- as.Date(gsub('/', '-', data$Date), format = "%d-%m-%Y") #convert 
#character into datetime format 
data$Month <- format(data$Date, "%m")
#Extract Month from columns (per suggestions by the professor)
#Drop date column as it's no longer useful anymore
data <- data[ , !(names(data) %in% "Date")] 


#stratified sampling to get same number of observations for each month
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

#------------------------------------------------------------------------------

#Interactions:
#Temperature/Seasons, Temperature/Solar Radiation, Humidity/Solar Radiation
#Wind speed/Solar Radiation, Humidity/Rain, Hour/Temperature/Solar Radiation,
#Humidity/Wind speed/Rush period

#Add interactions:
lm2 <- lm(Rented.Bike.Count ~ . + (Temperature * Seasons) + (Humidity * Rain) + 
(Solar.Radiation*Rush.Period) + 
(Hour * Temperature * Solar.Radiation) + (Humidity * Wind.speed..m.s. * Rush.Period), 
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

lm3 <- lm(Rented.Bike.Count ~ . + (Temperature * Seasons) + (Humidity * Rain) + 
(Solar.Radiation*Rush.Period) + (Hour * Temperature * Solar.Radiation) 
+ (Humidity * Wind.speed..m.s. * Rush.Period) + I(Temperature^3), 
           data = sample)
summary(lm3)
check_model_assumption_graphs(lm3)
#This is like, the best I can do, man

#Y transformations:
#According to stackexchange my residual plot looks like...that is because my residuals are capped
#to a lower boundary. This makes sense as my data has a strong right skew.
#The simplest transformation is taking ln() of Y:

lm4 <- lm(log(Rented.Bike.Count) ~ . + (Temperature * Seasons) 
          + (Humidity * Rain) + (Solar.Radiation*Rush.Period) + 
            (Hour * Temperature * Solar.Radiation) 
          + (Humidity * Wind.speed..m.s. * Rush.Period) + I(Temperature^3), 
          data = sample) #plus 0.1 to adjust 0 values
summary(lm4)
check_model_assumption_graphs(lm4)
#There's like this linear cluster of residuals??????

sum(sample$Rented.Bike.Count == 0)
sample <- data.frame(sample %>%  filter(Rented.Bike.Count != 0))

#Bikes are only rented on weekdays in my sample???
#BIKES ARE ONLY RENTED ON WEEKDAYS PERIOD???
sample <- sample[ , !(names(sample) %in% c("Functioning.Day"))]

lm5 <- lm(logb(Rented.Bike.Count, base = 5) ~ . + (Temperature * Seasons) 
          + (Humidity * Rain) + (Solar.Radiation*Rush.Period) + 
            (Hour * Temperature * Solar.Radiation) 
          + (Humidity * Wind.speed..m.s. * Rush.Period) + 
            I(Temperature^3), 
          data = sample) 
summary(lm5)
check_model_assumption_graphs(lm5)

#Linearity assumption mostly satisfied, EV and Normality still violated

#BoxCox on top of log transformation?
library(MASS)
par(mfrow=c(1,1))

boxcox_result <- boxcox(logb(Rented.Bike.Count, base = 5) ~ . + (Temperature * Seasons) 
       + (Humidity * Rain) + (Solar.Radiation*Rush.Period) + 
         (Hour * Temperature * Solar.Radiation) 
       + (Humidity * Wind.speed..m.s. * Rush.Period) + 
         I(Temperature^3), 
       data = sample, lambda = seq(0, 5, by = 0.05))
#As lambda is wayyyyy larger than 0, get optimal lambda to transform Y.
lambda = boxcox_result$x[which.max(boxcox_result$y)]
lm_trans <- lm(((logb(Rented.Bike.Count, base = 5)^(lambda)-1)/(lambda))~ . + 
                 (Temperature * Seasons) + (Humidity * Rain) + 
                 (Solar.Radiation*Rush.Period) + 
                 (Hour * Temperature * Solar.Radiation) 
               + (Humidity * Wind.speed..m.s. * Rush.Period) + 
                 I(Temperature^3),data=sample)
summary(lm_trans) #slight imporvement on goodness of fit
check_model_assumption_graphs(lm_trans) #EV and Normality definitely improved

#What if I did boxcox first then log transform?
lm_trans2 <- lm((logb(Rented.Bike.Count^(lambda-1)/(lambda), base = 5))~ . + 
                  (Temperature * Seasons) + (Humidity * Rain) + 
                  (Solar.Radiation*Rush.Period) + 
                  (Hour * Temperature * Solar.Radiation) 
                + (Humidity * Wind.speed..m.s. * Rush.Period) + 
                  I(Temperature^3) -Month,data=sample)
summary(lm_trans2) #goodness of fit worse
check_model_assumption_graphs(lm_trans2) #Normality violation is worse

#VST on top of log transformation?

lm6 <- lm(sqrt(logb(Rented.Bike.Count, base = 5)) ~ . + (Temperature * Seasons) 
          + (Humidity * Rain) + (Solar.Radiation*Rush.Period) + 
            (Hour * Temperature * Solar.Radiation) 
          + (Humidity * Wind.speed..m.s. * Rush.Period) + 
            I(Temperature^3), 
          data = sample) #plus 0.1 to adjust 0 values
summary(lm6) #goodness of fit worse
check_model_assumption_graphs(lm6) #EV got WORSE! YOU HAD ONE JOB!

#------------------------------------------------------------------------------

#There's bound to be high collinearity between some of the variables (I literally
#created factors based off of existing variables)
library(olsrr) #VIF from faraway package can't handle complicated models, so VIF from olsrr was used instead.
#Both calculate the same thing so no biggie.
vif_values <- ols_vif_tol(lm_trans)
vif_values$VIF #We see Month has near Infinite VIF; remove first and see results

lm_trans2 <- lm(((logb(Rented.Bike.Count, base = 5)^(lambda)-1)/(lambda))~ . + 
                  (Temperature * Seasons) + (Humidity * Rain) + 
                  (Solar.Radiation*Rush.Period) + 
                + (Humidity * Wind.speed..m.s. * Rush.Period) + 
                  I(Temperature^3) -Month,data=sample)
summary(lm_trans2)
vif_values <- ols_vif_tol(lm_trans2)
vif_values$VIF

#Removing Month reduced multicollinearity in the model and improved goodness of fit.
#Removing the term with second highest multicollinearity, (Hour * Temperature * Solar.Radiation),
#significantly improved VIF but reduced goodness of fit. I'll keep the three way interaction
#And reduce variables based on AIC/BIC.

#-------------------------------------------------------------------------------

lm_trans <- lm(((logb(Rented.Bike.Count, base = 5)^(lambda)-1)/(lambda))~ . + 
                 (Temperature * Seasons) + (Humidity * Rain) + 
                 (Solar.Radiation*Rush.Period) + 
                 (Hour * Temperature * Solar.Radiation) 
               + (Humidity * Wind.speed..m.s. * Rush.Period) + 
                 I(Temperature^3) -Month,data=sample)

fit_back_bic = step(lm_trans, direction = "backward", k=log(n), trace = 0)
fit_back_bic
fit_back_aic = step(lm_trans, direction = "backward", trace = 0)
fit_back_aic

#BIC model has 15 predictors, AIC has 24, original model has 30

#Is there a statistically significant difference between full and reduced models?
anova(fit_back_bic, fit_back_aic)
anova(fit_back_aic, lm_trans)

#AIC model performs better than BIC model, but full model does not perform significantly better than AIC model
#Final model is AIC model

#-------------------------------------------------------------------------------
#Prediction

