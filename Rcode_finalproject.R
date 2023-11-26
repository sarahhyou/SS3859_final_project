data <- read.csv('SeoulBikeData.csv') #load dataset

data$Date <- as.Date(gsub('/', '-', data$Date), format = "%d-%m-%Y") #convert 
#character into datetime format 
data$Month <- format(data$Date, "%m")
data$Month #Extract Month from columns (per suggestions by the professor)
drops <- c("Date")
#Drop date column as it's no longer useful anymore
data <- data[ , !(names(data) %in% drops)] 


df <- data[sample(nrow(data), size = 1000, replace = FALSE), ] 
#randomly sample 1000 data points for the purpose of this report
#response variable is Rented.Bike.Count, predictors are everything else

#Data preprocessing: Convert character columns into factor
df$Seasons <- factor(df$Seasons)
df$Holiday <- factor(df$Holiday)
df$Functioning.Day <- factor(df$Functioning.Day)
df$Month <- factor(df$Month)

pairs(df)
#from the graph we see dew point temperature and temperature have extremely high 
#positive correlation. Like I don't even need to look at R^2, the relationship
#LOOKS like a straight line lmao. As dew point temperature seems to have higher
#collinearity with humidity, drop dew point temperature column first.
df <- df[ , !(names(df) %in% c("Dew.point.temperature"))] 

#TODO: investigate relationship between Bikes rented and Hour 
#(maybe add predictor rush hour/not rush hour)
#and then start with full model (all variables + interaction)