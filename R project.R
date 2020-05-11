data1 <- read.csv("C:/Users/Nikhil Kamath/Desktop/data/SwedishMotorInsurance.csv", header= TRUE,sep=",")
data2 <- data.frame(data1)

summary(data2)

library(plotrix)

cor(data2$Payment,data2$Claims, method = "pearson")
cor(data2$Payment,data2$Insured, method = "pearson")

plot(data2$Payment,data2$Claims, main = "Correlation between Payment and Claims",xlab = "Payment", ylab = "Claims")
plot(data2$Payment,data2$Insured, main = "Correlation between Payment and Insured",xlab = "Payment", ylab = "Insured")

boxplot(data2$Payment ~ data2$Insured, main = "Correlation between Payment and Claims",xlab ="Claims",ylab="Payment")

lnreg<-lm(Payment~Insured+Claims+Make+Bonus+Zone+Kilometres, data=data2)
summary(lnreg)

lnreg1<-lm(Claims~Insured+Make+Bonus+Zone+Kilometres, data=data2)
summary(lnreg1)

lnreg2<-lm(Payment~Insured+Claims, data=data2)
summary(lnreg2)

agg1=apply(data2[,c(5,6,7)],2,function(x) tapply(x,data2$Kilometres,mean))
agg1

agg2=apply(data2[,c(5,6,7)],2,function(x) tapply(x,data2$Zone,mean))
agg2

agg3 = apply(data2[,c(5,6,7)],2,function(y) tapply(y,data2$Bonus,mean))
agg3
