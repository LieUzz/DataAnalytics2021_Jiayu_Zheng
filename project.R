library(ggplot2)
library("dplyr")
# sea level data monthly
seaLevelMo_raw = read.csv("RPI/course/data analystic/term project/climate/Reconstructed_Global_Mean_Sea_Level_for_1880_to_2009/CSIRO_Recons_gmsl_mo_2011.csv")

summary(seaLevelMo_raw)
seaLevelMo_raw = rename(seaLevelMo_raw, sealevel=GMSL..mm., uncertainty = GMSL.uncertainty..mm.)

par(mfrow = c(1, 2))
plot(seaLevelMo_raw$sealevel)
boxplot(seaLevelMo_raw$sealevel)
hist(seaLevelMo_raw$sealevel)
plot(ecdf(seaLevelMo_raw$sealevel))
qqnorm(seaLevelMo_raw$sealevel)
qqline(seaLevelMo_raw$sealevel)

# sea level data yearly
seaLevelYe_raw = read.csv("Desktop/climate/Reconstructed_Global_Mean_Sea_Level_for_1880_to_2009/CSIRO_Recons_gmsl_yr_2011.csv")
seaLevelYe_raw = rename(seaLevelYe_raw, sealevel=GMSL..mm., uncertainty = GMSL.uncertainty..mm.)
summary(seaLevelYe_raw)

par(mfrow = c(2, 3))
plot(seaLevelYe_raw$sealevel)
boxplot(seaLevelYe_raw$sealevel)
hist(seaLevelYe_raw$sealevel)
plot(ecdf(seaLevelYe_raw$sealevel))
qqnorm(seaLevelYe_raw$sealevel)
qqline(seaLevelYe_raw$sealevel)
par(mfrow = c(1, 2))



# clean data
library(tidyverse)
seaLevelMo_title = c("Year", "Month", "sealevel", "uncertainty")
seaLevelMo_raw$Time = as.character(seaLevelMo_raw$Time)

library(stringr)
x = str_split_fixed(seaLevelMo_raw$Time, "", 2)

x = separate(seaLevelMo_raw, Time, into = c("year", "month"), sep = ".")

re = strsplit(as.character(seaLevelMo_raw$Time),".")


# sea level trend
sealevel_year = seq(1880, 2009)
idx = seq(1, length(seaLevelMo_raw$GMSL..mm.), 12)
plot(sealevel_year, seaLevelMo_raw$GMSL..mm.[idx])
lines(sealevel_year, seaLevelMo_raw$GMSL..mm.[idx])
sea_level = seaLevelMo_raw$GMSL..mm.[idx]
sea_level_data = data.frame(sea_level, sealevel_year)
sea_level_data

# sea level line
boxplot(sea_level_data$sea_level)
sea_line<-ggplot(
    data=sea_level_data,
    mapping=aes(
        x=sealevel_year,
        y=sea_level
    )
) + geom_smooth()
sea_line+geom_point()



# temperature data
temp_raw = read.csv("Desktop/climate/tempchanges/GlobalTemperatures.csv")

# analysis of temp data
## land average
summary(temp_raw)
par(mfrow = c(2, 3))
plot(temp_raw$LandAverageTemperature)
boxplot(temp_raw$LandAverageTemperature)
hist(temp_raw$LandAverageTemperature)
plot(ecdf(temp_raw$LandAverageTemperature))
qqnorm(temp_raw$LandAverageTemperature)
qqline(temp_raw$LandAverageTemperature)
## land+sea average
plot(temp_raw$LandAndOceanAverageTemperature)
boxplot(temp_raw$LandAndOceanAverageTemperature)
hist(temp_raw$LandAndOceanAverageTemperature)
plot(ecdf(temp_raw$LandAndOceanAverageTemperature))
qqnorm(temp_raw$LandAndOceanAverageTemperature)
qqline(temp_raw$LandAndOceanAverageTemperature)





# operate
idx_1<-seq(1,length(temp_raw$LandAverageTemperature),by=12)
idx_6<-seq(6,length(temp_raw$LandAverageTemperature),by=12)
temp = temp_raw$LandAverageTemperature


temp_Jan = temp[idx_1]
temp_Jun = temp[idx_6]
temp_year = seq(1750,2015)
temp_Jan_data = data.frame(temp_Jan, temp_year)
temp_Jun_data = data.frame(temp_Jun, temp_year)
# Feb. Temp change from 1750 -> 2015 
plot(temp_y, temp[idx_1])
lines(temp_y, temp[idx_1])


# Jun. Temp changes from 1750 -> 2015
plot(temp[idx_6])
lines(temp[idx_6])


plot(temp_raw$LandMaxTemperature)
boxplot(temp_raw$LandMaxTemperature)

# curve of temperature in Jan.
temp_Jan_data
boxplot(temp_Jan_data$temp_Jan)
p<-ggplot(
    data=temp_Jan_data,
    mapping=aes(
        x=temp_year,
        y=temp_Jan
    )
) + geom_smooth()
p+geom_point()




# GHG data

GHG.raw = read.csv("RPI/course/data analystic/term project/climate/emissions/caitcountryghgemissions-csv0/new/CAIT Country GHG Emissions.csv", skip = 1)
summary(GHG.raw)



# sealevel and temp model
## linear regression
### data rebuild

# sealevel line
sea_line<-ggplot(
    data=temp.sealevel,
    mapping=aes(
        x=temp_year,
        y=sealevel
    )
) + geom_smooth()
sea_line+geom_point()

# temperature line
p<-ggplot(
    data=temp_Jan_data,
    mapping=aes(
        x=temp_year,
        y=temp_Jan
    )
) + geom_smooth()
p+geom_point()

# clean the data
sealevel_year = seq(1880, 2009)
seaLevelYe_raw$sealevel
plot(sealevel_year, seaLevelYe_raw$sealevel)
sealevel.data = data.frame(sealevel_year, seaLevelYe_raw$sealevel)
plot(sealevel.data)

temp_year = seq(1750,2015)
idx_5<-seq(5,length(temp_raw$LandAverageTemperature),by=12)


temp.land = temp_raw$LandAverageTemperature[idx_5]
temp.landandsea = temp_raw$LandAndOceanAverageTemperature[idx_5]
temp.max = temp_raw$LandMaxTemperature[idx_5]
temp.min = temp_raw$LandMinTemperature[idx_5]
temp.data = data.frame(temp_year, temp.land, temp.landandsea, temp.max, temp.min)
temp.data = filter(temp.data, temp.data$temp_year>1879)
temp.data = filter(temp.data, temp.data$temp_year<2010)

sealevel = sealevel.data$seaLevelYe_raw.sealevel


temp.sealevel = data.frame(temp.data, sealevel)

# model
lmsealevel = lm(temp.sealevel$sealevel~temp.sealevel$temp.land
              +temp.sealevel$temp.landandsea
              +temp.sealevel$temp.max
              +temp.sealevel$temp.min
              , temp.sealevel)
summary(lmsealevel)
confint(lmsealevel)

sealevel = temp.sealevel$sealevel
landT = temp.sealevel$temp.land
conbineT = temp.sealevel$temp.landandsea
minT = temp.sealevel$temp.min
lmsealevel.opitimize = lm(sealevel~landT
                +conbineT
                +minT
                , temp.sealevel)
summary(lmsealevel.opitimize)
confint(lmsealevel.opitimize)



# optimize
samData = temp.sealevel
pSealevel = samData$sealevel
pland = samData$temp.land
plandandsea = samData$temp.landandsea
pmin = samData$temp.min
# pre_data_1 = predict(lmsealevel.opitimize, data.frame(landT= pland, conbineT = plandandsea, minT = pmin), interval="prediction")
pre_data_1 = predict(lmsealevel.opitimize, data.frame(landT= pland, conbineT = plandandsea, minT = pmin), interval="confidence")
pre_data_1

title = c('original', 'predict', 'upper', 'lower')
predict_v = pre_data_1[1:130]
low_v = pre_data_1[131:260]
upper_v = pre_data_1[261:390]
original = temp.sealevel$sealevel
lines_v = data.frame(original,predict_v,low_v,upper_v)

plot(lines_v$original, pch=20,col="red")
lines(lines_v$original, col="red")

points(lines_v$predict_v,pch=18,col="green")
lines(lines_v$predict_v, col="green")
points(lines_v$low_v,pch=18,col="blue")
lines(lines_v$low_v, col="blue")
points(lines_v$upper_v,pch=18,col="yellow")
lines(lines_v$upp, col="yellow")

legend("topleft",                                    
     legend=c("Original","low_predict","predict","upper_predict"),        
     col=c("red","blue","green","yellow"),                
     lty=1,lwd=2)    

par(mfrow=c(2,2))
plot(lmsealevel.opitimize)
par(mfrow=c(1,1))



# Clean emission gas data
gas.raw.data = read.csv("Desktop/climate/emissions/caitcountryghgemissions-csv0/CAIT Country GHG Emissions.csv", skip=2)
summary(gas.raw.data)

temp.country.raw.data = read.csv("Desktop/climate/tempchanges/GlobalLandTemperaturesByCountry.csv")
summary(temp.country.raw.data)
library(tidyverse)
temp.country.raw.data = separate(data = temp.country.raw.data, col = dt, into = c("Year", "Month", "Day"), sep = "-")
temp.country.data = filter(temp.country.raw.data, temp.country.raw.data$Year > 1989)
idx_5<-seq(5,length(temp.country.data$year),by=12)

summary(temp.country.data)
temp.country.may = subset(temp.country.data, temp.country.data$Month == "05")
temp.country.may$Year = as.numeric(temp.country.may$Year)
temp.country.may$Month = as.numeric(temp.country.may$Month)
temp.country.may$Day = as.numeric(temp.country.may$Day)

summary(temp.country.may)
temp.country.may = temp.country.may[,-2:-3]
merge.data = merge(gas.raw.data, temp.country.may)


par(mfrow = c(4, 5))
plot(merge.data$Total.GHG.Emissions.Excluding.Land.Use.Change.and.Forestry..MtCO2e.)
plot(merge.data$Total.GHG.Emissions.Including.Land.Use.Change.and.Forestry..MtCO.e..)
plot(merge.data$Total.CO2..excluding.Land.Use.Change.and.Forestry...MtCO2.)
plot(merge.data$Total.CH4..MtCO2e.)
plot(merge.data$Total.N2O..MtCO2e.)
plot(merge.data$Total.F.Gas..MtCO2e.)
plot(merge.data$Total.CO2..including.Land.Use.Change.and.Forestry...MtCO2.)
plot(merge.data$Total.CH4..including.Land.Use.Change.and.Forestry...MtCO2e.)
plot(merge.data$Total.N2O..including.Land.Use.Change.and.Forestry...MtCO2e.)
plot(merge.data$Energy..MtCO2e.)
plot(merge.data$Industrial.Processes..MtCO2e.)
plot(merge.data$Agriculture..MtCO2e.)
plot(merge.data$Waste..MtCO2e.)
plot(merge.data$Land.Use.Change.and.Forestry..MtCO2.)
plot(merge.data$Bunker.Fuels..MtCO2.)
plot(merge.data$Electricity.Heat..MtCO2.)
plot(merge.data$Manufacturing.Construction..MtCO2.)
plot(merge.data$Transportation..MtCO2.)
plot(merge.data$Other.Fuel.Combustion..MtCO2e.)
plot(merge.data$Fugitive.Emissions..MtCO2e.)
par(mfrow = c(1,1))

data.clean = na.omit(merge.data)

plot(data.clean$AverageTemperature)

data.clean$AverageTemperature = round(data.clean$AverageTemperature)
# model Random forest
library(randomForest)
summary(data.clean)
set.seed(111)
train = sample(nrow(data.clean), 0.7*nrow(data.clean), replace = FALSE)
Trainset = data.clean[train,]
Validset = data.clean[-train,]

model2 <- randomForest(as.factor(AverageTemperature) ~ ., data = Trainset, ntree = 500, mtry = 3, importance = TRUE, na.action = na.pass)
predValid = predict(model2, Validset, type = "class")
predValid
summary(predValid)
Validset$AverageTemperature
table(predValid, Validset$AverageTemperature)
Validpercent = mean(predValid == Validset$AverageTemperature)
Validpercent
plot(model2)
importance(model2)
varImpPlot(model2)
margins.2 = margin(model2,Trainset)
plot(margins.2)
hist(margins.2,main = "Margines of Random Forest for cancer dataset")


percent <- function(x, y){
    correct = 0
    n = length(x)
    for (i in 1:n)
        if (x[i] == y[i])
            correct = correct + 1
    return (correct/n)
}
#knn model
summary(data.clean)
normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
}
data.knn = data.clean
data.knn$AverageTemperature <- cut(data.knn$AverageTemperature, br=c(-15,0,10,20,36), labels = c("cold", "cool", "warm", "hot")) 
data.knn[3:22] <- as.data.frame(lapply(data.knn[3:22], normalize))
ind <- sample(2, nrow(data.knn), replace=TRUE, prob=c(0.7, 0.3))
KNNtrain <- data.knn[ind==1,]
KNNtest <- data.knn[ind==2,]
KNNtest
sqrt(1937)
library("class")
KNNpred <- knn(train = KNNtrain[3:22], test = KNNtest[3:22], cl = KNNtrain$AverageTemperature, k = 45)
table(KNNpred)
KNNtest_result = KNNtest$AverageTemperature

KNNpred
KNNtest_result
percent(KNNtest_result, KNNpred)

# K-means
data.kmeans = data.clean
data.kmeans$AverageTemperature <- cut(data.kmeans$AverageTemperature, br=c(-15,0,10,20,36), labels = c("cold", "cool", "warm", "hot")) 
data.kmeans.train = data.kmeans[3:22]
new_icluster <- kmeans(data.kmeans.train,4,nstart = 20,iter.max =1000)
new_icluster
data.kmeans[,23]
new_icluster$cluster
table(data.kmeans[,23],new_icluster$cluster)
plot(cancer,col=new_icluster$cluster)
new_icluster$tot.withinss
original_res = cancer[,20] + 1
1 - percent(original_res, new_icluster$cluster) 

