# initialization
### load the packages
library(readxl)
library(ggplot2)
library(plotROC)
data <- read_excel("C:\\Users\\69414\\Desktop\\house-info-fillna.xlsx")
View(data)
### load the data
y <- data$price_new
y <- log(y)
district <- data$district
furnish <- data$furnish
volume <- data$volume
subway <- data$subway
bus <- data$bus
shopping <- data$shopping
bank <- data$bank
food <- data$food
hospital <- data$hospitals
school <- data$schools

# Discriptive statistical analysis
### dstribution
par(mfrow=c(1,1))
hist(y,breaks=100,freq=F,col='Azure1',
     main='Density Function of price',border='Black')
lines(density(y),col='DodgerBlue4',lwd=2)
library(moments)
library(timeDate)
library(timeSeries)
library(fBasics)
library(plotrix)
q1<-mean(y)
q2<-sd(y)
q3<-median(y)
q4<-mean(y,trim=0.1)
q5<-mad(y)
q6<-min(y)
q7<-max(y)
q8<-range(y)
q9<-skewness(y)
q10<-kurtosis(y)
q11<-std.error(y)
q1
q2
q3
q4
q5
q6
q7
q8
q9
q10
q11
### count
furnish <- factor(furnish)
table(furnish)
volume <- factor(volume)
table(volume)
district <- factor(district)
table(district)
data$sub <- (subway > 0)*1
data$bu <- (bus > 0)*1
data$shop <- (shopping > 0)*1
data$ban <- (bank > 0)*1
data$foo <- (food > 0)*1
data$sch <- (school>0)*1
data$hos <- (hospital>0)*1
table(factor(data$shop))
table(factor(data$ban))
table(factor(data$foo))
table(factor(data$sch))
table(factor(data$hos))
### boxplot
library(ggplot2)
par(mfrow=c(1,2))
g1 = 
  ggplot(data=data, aes(x=as.factor(shop), y=price_new, group=as.factor(shop), fill = as.factor(shop))) + 
  geom_boxplot() +
  labs(fill='shopping', xlab='shopping') + ggtitle('Box Plot') +
  theme(plot.title=element_text(vjust=0.5, hjust=0.5, face='bold'))
g1
g2 = 
  ggplot(data=data, aes(x=as.factor(ban), y=price_new, group=as.factor(ban), fill = as.factor(ban))) + 
  geom_boxplot() +
  labs(fill='bank', xlab='bank') + ggtitle('Box Plot') +
  theme(plot.title=element_text(vjust=0.5, hjust=0.5, face='bold'))
g2
### scatter plot
plot(subway, y)
axis(side=1, at=c(0, 1), labels=c(0, 1))
### heat map
data2 <- read_excel("C:\\Users\\69414\\Desktop\\house-info-fillna.xlsx")
View(data2)
mydata <- data2[,c(3:12)]
res <- cor(mydata)
round(res, 2)
col <- colorRampPalette(c('blue', 'white', 'red'))(20)
heatmap(x=res, col=col, symm=TRUE)

# Regression
### single factor analysis
library(car)
data$sub <- (subway>0)*1
lm1 <- lm(y~as.factor(data$sub))
Anova(lm1, type='III')
summary(lm1)
### double factors analysis
lm2 <- lm(y~as.factor(data$sub)+as.factor(volume))
Anova(lm2, type='III')
summary(lm2)
### interaction analysis
lm3 <- lm(y~as.factor(volume)+as.factor(data$sub)+
            as.factor(data$sub)*as.factor(volume))
Anova(lm3, type='III')
summary(lm3)
### full model analysis
lm4 <- lm(y~as.factor(volume)+as.factor(sub)+as.factor(sub)*as.factor(volume)
          +as.factor(district)+as.factor(furnish)+as.factor(bu)+as.factor(shop)+as.factor(ban)
          +as.factor(sch)+as.factor(hos), data=data)
Anova(lm4, type='III')
summary(lm4)
### reduced model
lm5 <- lm(y~as.factor(volume)+as.factor(sub)+as.factor(sub)*as.factor(volume)
          +as.factor(district)+as.factor(furnish)+
            +as.factor(sch)+as.factor(hos), data=data)
Anova(lm5, type='III')
### final model
lm6 <- lm(y~as.factor(volume)+as.factor(sub)+as.factor(sub)*as.factor(volume)
          +as.factor(district)+as.factor(furnish)+as.factor(sch), data=data)
Anova(lm6, type='III')
summary(lm6)