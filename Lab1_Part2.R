
# ggplot example
plot(mtcars$wt,mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt,mtcars$mpg)
qplot(wt,mpg,data=mtcars)

ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()
plot(pressure$temperature,pressure$pressure,type='l')
points(pressure$temperature,pressure$pressure)

lines(pressure$temperature,pressure$pressure/2, col='red')
points(pressure$temperature,pressure$pressure/2, col='blue')
library(ggplot2)
qplot(pressure$temperature,pressure$pressure,geom = 'line')
qplot(temperature, pressure, data=pressure, geom = 'line')


ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line() + geom_point()

# bar graphs
barplot(BOD$demand,names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl))
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))

qplot(factor(cyl), data = mtcars)
ggplot(mtcars, aes(x=factor(cyl))) + geom_bar()

# Histograms
hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 10)
hist(mtcars$mpg, breaks = 5)
hist(mtcars$mpg, breaks = 12)
qplot(mpg, data = mtcars, binwidth=4)
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth =4)
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth =5)

# Creating Box-plots using ggplot
plot(ToothGrowth$supp, ToothGrowth$len)
boxplot(len~supp,data=ToothGrowth)
boxplot(len~supp+dose,data=ToothGrowth)      
library(ggplot2)       
qplot(ToothGrowth$supp,ToothGrowth$len, geom="boxplot")
qplot(supp,len,data=ToothGrowth, geom="boxplot")
ggplot(ToothGrowth,aes(x=supp,y=len)) + geom_boxplot()

qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len, geom="boxplot")
qplot(interaction(supp,dose), len, data=ToothGrowth,geom="boxplot")
ggplot(ToothGrowth, aes(x=interaction(supp,dose), y=len)) + geom_boxplot()



# additional example
library(gcookbook)
ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat = "identity")
BOD
str(BOD)
ggplot(BOD, aes(x=Time, y=demand)) + geom_bar(stat = "identity")
ggplot(BOD, aes(x=factor(Time), y=demand)) + geom_bar(stat = "identity")
ggplot(pg_mean, aes(x=group, y=weight)) +geom_bar(stat = "identity", fill="lightblue", colour = "red")
ggplot(BOD, aes(x=factor(Time), y=demand)) +geom_bar(stat = "identity", fill="orange", colour = "red")



# 2
library(gcookbook) 
library(ggplot2)
ggplot(cabbage_exp, aes(x=Date, fill=Cultivar)) + geom_bar(position = "dodge")
library(gcookbook)
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat="identity")
ggplot(diamonds, aes(x=cut)) +geom_bar() 
data("diamonds")
diamonds
ggplot(diamonds,aes(x=carat)) + geom_bar()
geom_bar()


# 3
ggplot(diamonds, aes(x=carat)) + geom_histogram()
library(gcookbook) 
ups <- subset(uspopchange, rank(Change)>40)
ups
ggplot(ups, aes(x=Abb, y= Change, fill=Region)) + geom_bar(stat = "identity")
ggplot(ups, aes(x=Abb, y=Change, fill=Region)) +geom_bin2d()
ggplot(ups, aes(x=Abb, y=Change, fill=Region)) + geom_col()
ggplot(ups, aes(x=reorder(Abb,Change), y=Change, fill=Region)) + geom_bar(stat = "identity", colour= "red") +
  scale_fill_manual(values=c("#669933", "#FFCC66")) + xlab("US-States")
ggplot(ups, aes(x=reorder(Abb,Change), y=Change, fill=Region)) + geom_bar(stat = "identity", color = "purple") +
  scale_fill_manual(values=c("#224455","#DDCC33"))

# 5
library(gcookbook)
csub <- subset(climate, source="Berkeley" & Year >= 1900)
csub
csub$pos <- csub$Anomaly10y >=0
csub
ggplot(csub, aes(x=Year, y=Anomaly10y, fill= pos)) + geom_bar(stat = "identity", position = "identity")
ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) + geom_bar(stat="identity", colour="black", size=0.25) +
  scale_fill_manual(values=c("#CCEEFF", "#FFDDDD"), guide=FALSE)
library(gcookbook) 
ggplot(pg_mean, aes(x=group, y=weight)) +geom_bar(stat="identity")
ggplot(pg_mean, aes(x=group, y=weight)) +geom_bar(stat="identity", width = 0.5)
ggplot(pg_mean, aes(x=group, y=weight)) +geom_bar(stat = "identity", width = 0.95)
ggplot(cabbage_exp, aes(x=Date, y= Weight, fill=Cultivar)) + geom_bar(stat = "identity", width = 0.5, position = "dodge")
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat = "identity", width = 0.5, position = position_dodge(0.7))


# 6
library(gcookbook) 
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat = "identity")
cabbage_exp
ggplot(cabbage_exp, aes(x= Date, y= Weight, fill=Cultivar)) + geom_bar(stat = "identity") + guides(fill=guide_legend(reverse = TRUE))
library(gcookbook)
ggplot(cabbage_exp, aes(x=interaction(Date,Cultivar), y=Weight)) +geom_bar(stat = "identity") + geom_text(aes(label=Weight),vjust=1.5,colour="white")
library(ggplot2)
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Weight), vjust=-0.2) +
  ylim(0, max(cabbage_exp$Weight) * 1.05)

ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(y=Weight+0.1, label=Weight))

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=Weight), vjust=1.5, colour="white",
            position=position_dodge(.9), size=3)

# 7
library(gcookbook)
tophit <- tophitters2001[1:25,]
tophit
ggplot(tophit, aes(x=avg, y=name)) + geom_point()
tophit[,c("name","lg","avg")]
ggplot(tophit, aes(x=avg, y= reorder(name,avg))) + geom_point(size=3, colour="red") + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour ="grey60",linetype="dashed"))

ggplot(tophit, aes(x=avg, y=reorder(name,avg))) + geom_point(size=2.5, colour="blue") + 
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey60", linetype = "twodash"))

nameorder <- tophit$name[order(tophit$lg, tophit$avg)]
tophit$name <- factor(tophit$name, levels = nameorder)
ggplot(tophit, aes(x=avg, y=name)) +
  geom_segment(aes(yend=name), xend=0, colour="grey70")+
  geom_point(size=3, aes(colour=lg)) +
  scale_color_brewer(palette="Set1", limits=c("NL","AL")) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = c(1,0.55),
        legend.justification = c(1,0.5))

ggplot(tophit, aes(x=avg, y=name)) +
  geom_segment(aes(yend=name), xend=0, colour="grey40") +
  geom_point(size=3, aes(colour=lg)) +
  scale_color_brewer(palette="Set1", limits=c("NL","AL"), guide=FALSE) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  facet_grid(lg ~ ., scales = "free_y", space="free_y")

