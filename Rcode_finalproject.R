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
ggplot(data=sample, aes(x=Hour, y=Rented.Bike.Count)) + geom_point() + 
  geom_smooth(se = FALSE) + geom_vline(aes(xintercept = 16)) + 
  annotate("text", x=16.5, y=3000, label="Hour = 17", angle=270)
#The graph seems to have a bimodal distribution  
sample$Rush.Period <- ifelse((sample$Hour > 16), "rush", "not rush")

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

#TODO: draw graphs for these later

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

lm4 <- lm(log(Rented.Bike.Count+1) ~ . + (Temperature * Seasons) + (Humidity * Rain) + 
            (Solar.Radiation*Rush.Period) + (Hour * Temperature * Solar.Radiation) 
          + (Humidity * Wind.speed..m.s. * Rush.Period) + I(Temperature^3), 
          data = sample) #plus 0.1 to adjust 0 values
summary(lm4)
check_model_assumption_graphs(lm4)
#TODO: Check to see how many instances of the data has rented bike count = 0.
#Maybe remove?
#------------------------------------------------------------------------------

#There's bound to be high collinearity between some of the variables (I literally
#created factors based off of existing variables)

#first convert all variables to numeric for VIF function to work
sample$Month <- as.numeric(sample$Month)
lm2 <- lm(Rented.Bike.Count ~., data = sample)
summary(lm2)

pruning <- data.frame(sample)
                      
i <- sapply(pruning, is.character)
pruning[i] <- lapply(pruning[i], as.factor)

library(faraway)
lm_prune <- lm(Rented.Bike.Count ~ .,data = pruning)
vif(lm_prune)

#Remove Temperature, Hour and Month
lm_prune <- lm(Rented.Bike.Count ~ . -Temperature -Hour -Month,data = pruning)
vif(lm_prune)

#new base model:
lm3 <- lm(Rented.Bike.Count ~. -Temperature -Hour -Month, data = sample)
summary(lm3)