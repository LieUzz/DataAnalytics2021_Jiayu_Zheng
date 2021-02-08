days <- c('Mon', 'Tue', 'Wed', 'Thur', 'Fri', 'Sat', 'Sun')
temp <- c(28,30.5,32,31.2,29.3,27.9,26.4)
snowed <- c('T','T','F','F','T','T','F')
help("data.frame")
RPI_Weather_Week <- data.frame(days,temp,snowed)
RPI_Weather_Week
head(RPI_Weather_Week)
str(RPI_Weather_Week)
summary(RPI_Weather_Week)


RPI_Weather_Week[1,]
RPI_Weather_Week[,1]
RPI_Weather_Week[,2]
RPI_Weather_Week[,'snowed']
RPI_Weather_Week[,'temp']
RPI_Weather_Week[,'days']
RPI_Weather_Week[1:5,c("days","temp")]
RPI_Weather_Week$temp
subset(RPI_Weather_Week,subset = snowed==TRUE)
subset(RPI_Weather_Week,subset=temp==TRUE)
sorted.snowed <- order(RPI_Weather_Week['snowed'])
sorted.snowed
RPI_Weather_Week[sorted.snowed,]


dec.snow <- order(RPI_Weather_Week$temp)
dec.snow
empty.DataFrame <- data.frame()
v1 <- 1:10
v1
letters
v2 <- letters[1:10]
df <- data.frame(col.name.1 = v1, col.name.2 = v2)
df



getwd()

data <- read.csv("/Users/zhengjiayu/Desktop/GPW3_GRUMP_SummaryInformation_2010.csv", header = FALSE)
data()
help(data)

View(data)
attach(data)
fix(data)


EPI_data <- read.csv("/Users/zhengjiayu/Desktop/RPI/course/data ana/lab/2010EPI_data.csv", skip=1)
View(EPI_data)
attach(EPI_data) 
fix(EPI_data)
EPI

tf <- is.na(EPI)
E <- EPI[!tf]
E
summary(EPI)
fivenum(EPI,na.rm=TRUE)

stem(EPI)
hist(EPI)
hist(EPI,seq(30, 95, 1), prob=TRUE)
hist(EPI,seq(30, 95, 2), prob=TRUE)
lines(density(EPI,na.rm = TRUE,bw=1))

lines(density(EPI,na.rm = TRUE,bw=2))
rug(EPI)



plot(ecdf(EPI), do.points=FALSE,verticals = TRUE) 

par(pty='s')
qqnorm(EPI);qqline(EPI)

x=seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab = 'QQ plot for idsn')
qqline(x)



DALY
summary(DALY)
fivenum(DALY,na.rm=TRUE)
stem(DALY)
hist(DALY)
hist(DALY,seq(0,100,1),prob=TRUE)
lines(density(DALY,na.rm=TRUE,bw=1))
rug(DALY)
plot(ecdf(DALY),do.points=FALSE,verticals = TRUE)
qqnorm(DALY);qqline(DALY)


AIR_H
summary(AIR_H)
fivenum(AIR_H,na.rm=TRUE)
stem(AIR_H)
hist(AIR_H)
hist(AIR_H,seq(0,100,1),prob=TRUE)
lines(density(AIR_H,na.rm=TRUE,bw=1))
rug(AIR_H)
plot(ecdf(AIR_H),do.points=FALSE,verticals = TRUE)
qqnorm(AIR_H);qqline(AIR_H)

help(distributions)



EPILAND <-EPI[!Landlock]
EPILAND
Eland <- EPILAND[!is.na(EPILAND)]
Eland
summary(Eland)
fivenum(Eland,na.rm=TRUE)
hist(Eland)
hist(Eland,seq(30,95,1),prob=TRUE)
lines(density(Eland,na.rm=TRUE,bw=1))
rug(Eland)
plot(ecdf(Eland),do.points=FALSE,verticals = TRUE)
qqnorm(Eland);qqline(Eland)



NOSURFACEWATER <-EPI[!No_surface_water]
NOSURFACEWATER
No_S_W <- NOSURFACEWATER[!is.na(NOSURFACEWATER)]
No_S_W
summary(No_S_W)
fivenum(No_S_W,na.rm=TRUE)
hist(No_S_W)
hist(No_S_W,seq(30,95,1),prob=TRUE)
lines(density(No_S_W,na.rm=TRUE,bw=1))
rug(No_S_W)
plot(ecdf(No_S_W),do.points=FALSE,verticals = TRUE)
qqnorm(No_S_W);qqline(No_S_W)


DESERT <-EPI[!Desert]
DESERT
Desert_place <- DESERT[!is.na(DESERT)]
Desert_place
summary(Desert_place)
fivenum(Desert_place,na.rm=TRUE)
hist(Desert_place)
hist(Desert_place,seq(30,95,1),prob=TRUE)
lines(density(Desert_place,na.rm=TRUE,bw=1))
rug(Desert_place)
plot(ecdf(Desert_place),do.points=FALSE,verticals = TRUE)
qqnorm(Desert_place);qqline(Desert_place)


High_Population_Density
HIGH_P <-EPI[!High_Population_Density]
HIGH_P
High_P <- HIGH_P[!is.na(HIGH_P)]
High_P
summary(High_P)
fivenum(High_P,na.rm=TRUE)
hist(High_P)
hist(High_P,seq(30,95,1),prob=TRUE)
lines(density(High_P,na.rm=TRUE,bw=1))
rug(High_P)
plot(ecdf(High_P),do.points=FALSE,verticals = TRUE)
qqnorm(High_P);qqline(High_P)

EPI_regions
EPI_South_Asia<-EPI[EPI_regions=="South Asia"]
EPI_South_Asia



GPW_data <- read.csv("/Users/zhengjiayu/Desktop/RPI/course/data ana/lab/GPW3_GRUMP_SummaryInformation_2010.csv")
View(GPW_data)
attach(GPW_data)



Water_data <- read.csv('/Users/zhengjiayu/Desktop/RPI/course/data ana/lab/water-treatment.csv')
View(Water_data)
attach(Water_data)
PH.D
hist(PH.D)
hist(PH.D,seq(7.0,8.4,0.1))
hist(PH.D,seq(7.0,8.4,0.1),prob=TRUE)
lines(density(PH.D,na.rm=TRUE,bw=1))
rug(PH.D)

