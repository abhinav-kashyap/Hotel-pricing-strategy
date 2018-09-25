#Read and Merge all the multiple city data frames in single dataframe

setwd("D:/Data Science/Internship Material/Capstone Project/Data/")
# Get the files names
files = list.files(pattern="*.csv")
# First apply read.csv, then rbind
myfiles = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
write.csv(myfiles,file = "D:/Hotel.csv")

Hotel <- myfiles

#Understanding the Dataset
summary(Hotel$RoomRent)
summary(Hotel$Airport)

Pricing <- subset(Hotel,select = c("RoomRent","Population", "IsMetroCity","IsTouristDestination","IsWeekend","IsNewYearEve", "StarRating","Airport","FreeWifi","FreeBreakfast","HotelCapacity","HasSwimmingPool"))

corrgram(Pricing,type = NULL,lower.panel = panel.shade, upper.panel = NULL)

#Bar plot of Room rent with various factors
boxplot(Pricing$RoomRent ~ Pricing$IsMetroCity ,horizontal=TRUE, las=1,xlab="Room Rent")

#Aggregate Mean External Factors
v1 <- aggregate(Pricing$RoomRent,by=list(IsMetroCity = Pricing$IsMetroCity),mean)
v2 <- aggregate(Pricing$RoomRent, by=list(IsTouristDestination=Pricing$IsTouristDestination),mean)
v3 <- aggregate(Pricing$RoomRent, by=list(IsWeekend=Pricing$IsWeekend),mean)
v4 <- aggregate(Pricing$RoomRent, by=list(IsNewYearEve=Pricing$IsNewYearEve),mean)

#Aggregate Mean Internal Factors
v5 <- aggregate(Pricing$RoomRent, by=list(FreeWifi=Pricing$FreeWifi),mean)
v6 <- aggregate(Pricing$RoomRent, by=list(FreeBreakfast=Pricing$FreeBreakfast),mean)
v7 <- aggregate(Pricing$RoomRent, by=list(HasSwimmingPool=Pricing$HasSwimmingPool),mean)
v8 <- aggregate(Pricing$RoomRent, by=list(StarRating=Pricing$StarRating),mean)
v9 <- aggregate(Pricing$RoomRent, by=list(Airport=Pricing$Airport),mean)

par(mfrow=c(2,2))
barplot(v1$x,width = 1,names.arg = v1$IsMetroCity,ylab = "Average Room rent",main = "Average room rent for Metro cities")
barplot(v2$x,width = 1,names.arg = v2$IsTouristDestination, ylab = "Average Room rent",main = "Average room rent for Tourist Destinations")
barplot(v3$x,width = 1,names.arg = v3$IsWeekend, ylab = "Average Room rent",main = "Average room rent on Weekends and Weekdays")
barplot(v4$x,width = 1,names.arg = v4$IsNewYearEve, ylab = "Average Room rent",main = "Average room rent for New Year Eve")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
barplot(v5$x,width = 1,names.arg = v5$FreeWifi,ylab = "Average Room rent",main = "Average room rent for Hotels with Free Wifi")
barplot(v6$x,width = 1,names.arg = v6$FreeBreakfast, ylab = "Average Room rent",main = "Average room rent for Hotels offering free Breakfast")
barplot(v7$x,width = 1,names.arg = v7$HasSwimmingPool, ylab = "Average Room rent",main = "Average room rent for Hotels with Swimming Pool")
barplot(v8$x,width = 1,names.arg = v8$StarRating, ylab = "Average Room rent",main = "Average room rent for Star Ratings of Hotel")
barplot(v9$x,width = 1,names.arg = v9$Airport, ylab = "Average Room rent",main = "Average room rent based on Airport Distance from Hotel")
par(mfrow=c(1,1))

boxplot(Pricing$RoomRent ~ Pricing$IsMetroCity,horizontal=TRUE, las=1,main="Room Rent for Metro and Non-Metro cities")
boxplot(Pricing$RoomRent ~ Pricing$IsTouristDestination,horizontal=TRUE, las=1,main="Room Rent for Tourist and Non Tourist Destinations")
boxplot(Pricing$StarRating ~ Pricing$HasSwimmingPool, horizontal=TRUE,las=1,main="Hotels which have Swimming Pool",xlab="Hotel Star Rating",ylab="Swimming Pool")
boxplot(Hotel$RoomRent ~ Hotel$CityName, las=1,main="Hotel Room rent for different Cities",xlab="Hotel Room Rent",ylim = c(0,10000),ylab="Cities")

aggregate(RoomRent ~ IsTouristDestination + HasSwimmingPool,data=Pricing,mean)
aggregate(RoomRent ~ StarRating + IsTouristDestination, data=Pricing,mean)
aggregate(RoomRent ~ IsTouristDestination, data=Pricing,mean)

#Scatterplot for Room Rent vs Top top 3 variables
plot(x=Pricing$StarRating, y=Pricing$RoomRent,xlab = "Star Rating",ylab = "Room Rent",main = "Scatterplot for Room Rent for Hotels with different Star Ratings")
par(mfrow=c(1,2))
plot(x=Pricing$IsTouristDestination, y=Pricing$RoomRent, xlab = "Tourist Destination", ylab = "Room Rent", main = "Room rent vs Tourist Destinations")
plot(x=Pricing$HasSwimmingPool, y=Pricing$RoomRent, xlab = "Swimming Pool", ylab = "Room Rent", main = "Room rent vs Swimming Pools")
plot(x=Pricing$RoomRent, y=Pricing$HotelCapacity)

#Corrgram of top 3 variables
m1 <- subset(Pricing, select = c("RoomRent","StarRating","IsTouristDestination","HasSwimmingPool"))
corrgram(m1,order = TRUE, type = NULL, lower.panel = panel.shade, upper.panel = NULL)

#Variance-covariance matrix for top 3 variables
m2 <- cov(m1)
m3<- cor(Pricing)


#Hypothesis Testing

H1 <- lm(RoomRent ~ StarRating + IsWeekend + IsNewYearEve,Pricing)
H1
summary(H1)

H2 <- lm(RoomRent ~ StarRating + IsMetroCity + IsTouristDestination,Pricing)
summary(H2)

H3 <- lm(RoomRent ~ StarRating + FreeWifi + FreeBreakfast + HasSwimmingPool + Airport,Pricing)
summary(H3)

H4 <- lm(RoomRent ~ StarRating + IsNewYearEve + IsMetroCity + IsTouristDestination + HasSwimmingPool + Airport,Pricing)
summary(H4)


#Comparing the models using Anova
anova(H1,H2,H3,H4)

#Confirmation of our Model
Full.model <- lm(RoomRent ~ StarRating + IsWeekend + IsNewYearEve + IsMetroCity + IsTouristDestination + FreeWifi + FreeBreakfast + HasSwimmingPool + Airport,Pricing)
Reduced.model <- lm(RoomRent ~ StarRating + IsNewYearEve + IsMetroCity + IsTouristDestination + HasSwimmingPool + Airport,Pricing)
anova(Red.1, Reduced.model)
