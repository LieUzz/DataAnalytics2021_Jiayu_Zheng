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


EPI_data <- read.csv("/Users/zhengjiayu/Desktop/2010EPI_data.csv", skip=1)
View(EPI_data)
attach(EPI_data) 
fix(EPI_data)
EPI
tf <- is.na(EPI)
E <- EPI[!tf]
E
summary(EPI)
fivenum(EPI,na.rm=TRUE)