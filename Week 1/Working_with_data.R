c(2,3,5,8,13)
Country = c("Brazil","China","India","Switzerland","USA")
LifeExpectancy = c(74,76,65,83,79)
Country[1]
LifeExpectancy[3]
seq(0,100,2)
CountryData = data.frame(Country,LifeExpectancy)
CountryData$Population = c(199000,1390000,1240000, 7997, 318000)
Country = c("Australia","Greece")
LifeExpectancy = c(82,81)
Population = c(23050,11125)
NewCountryData = data.frame(Country,LifeExpectancy,Population)
AllCountryData = rbind(CountryData,NewCountryData)

getwd()
WHO = read.csv("WHO.csv")
str(WHO)
summary(WHO)
WHO_europe = subset(WHO,Region == "Europe")
str(WHO_europe)
write.csv(WHO_europe,"WHO_europe.csv")
ls()
rm(WHO_europe)
ls()
Under15
WHO$Under15
mean(WHO$Under15)
sd(WHO$Under15)
summary(WHO$Under15)
which.min(WHO$Under15)
WHO$Country[86]
which.max(WHO$Under15)
WHO$Country[124]
plot(WHO$GNI,WHO$FertilityRate)
Outliers = subset(WHO,GNI > 10000 & FertilityRate > 2.5)
nrow(Outliers)
Outliers[c("Country","GNI","FertilityRate")]
mean(WHO$Over60)
which.min(WHO$Over60)
WHO$Country[183]
WHO$Country[which.max(WHO$LiteracyRate)]
hist(WHO$CellularSubscribers)
boxplot(WHO$LifeExpectancy~WHO$Region,xlab = '',ylab = 'Life Expentancy',main='Life Expentancy of countries by region')
table(WHO$Region)
tapply(WHO$Over60,WHO$Region,mean)
tapply(WHO$LiteracyRate,WHO$Region, min,na.rm = TRUE )
tapply(WHO$ChildMortality,WHO$Region,mean)
