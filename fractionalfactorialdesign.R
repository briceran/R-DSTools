#Example fractional factorial design analysis code

DATA<-read.table('http://www.stat.ucla.edu/~hqxu/stat201A/data/leafspring.dat',stringsAsFactors = TRUE,header = TRUE)
y1<-DATA$Height1
y2<-DATA$Height2
y3<-DATA$Height3
Y<-c(y1,y2,y3)
#plot(DATA)
model<-cbind(DATA$A,DATA$B,DATA$C,DATA$D,DATA$E)
model[model == 1] <- -1
model[model == 2] <- 1
LrgModel<-rbind(model,model,model)
A<- LrgModel[,1]
B<- LrgModel[,2]
C<- LrgModel[,3]
D<- LrgModel[,4]
E<- LrgModel[,5]
###################

alias(lm(Y~(A+B+C+D+E)^5))

g = lm(Y~ A + B + C + D + E)
summary(g) # E will lower the output

2*coef(g)[-1]
par(mfrow=c(1,2))
plot(g2,which = 1:2)

source("halfnormal.R")   # download from Web and save it in working directory
source("http://www.stat.ucla.edu/~hqxu/stat201A/R/halfnormal.R")   # if online


hist(res, breaks="FD", xlab="Residuals",
     main="Histogram of residuals", ylim=c(0,22))


g2 = lm(Y~ (A + B + C + D + E)^3) #B and E look like they have a significant interaction
summary(g2)
anova(g,g2) # so reject main effects model in favor of higher order model

interaction.plot(B, E, Y, main="BE Interaction plot")
# ASK Why does D becomes significant in this model

g3 = lm(Y~ A+B+C+D+E+A:B+A:C+A:D+A:E+B:E+C:E+D:E)
summary(g3)
2*coef(g3)[-1]
par(mfrow=c(1,2))
normalplot(2*coef(g3)[-1], l=T, n=12)
halfnormalplot(2*coef(g3)[-1], l=T, n=12)



par(mfrow=c(1,2))
normalplot(2*coef(g)[-1], l=T, n=5, ylim=c(-20, 25))
halfnormalplot(2*coef(g)[-1], l=T, n=5)




################## PROBLEM 2 ###############
DATA2<-read.table('http://www.stat.ucla.edu/~hqxu/stat201A/data/asat.dat',stringsAsFactors = TRUE,header = TRUE)
DATA2 # 9 factors
alias(lm(Asat~.^9,data = DATA2))
model_me<-lm(Asat~.,data = DATA2)
2*coef(model_me)[-1]  #How do they conclude C, D, E, F significant when p>.1
summary(model_me)
#plot(model_me)
par(mfrow=c(1,2))
normalplot(2*coef(model_me)[-1], l=T, n=9, ylim=c(-20, 25))
halfnormalplot(2*coef(model_me)[-1], l=T, n=9)

par(mfrow=c(1,2))
interaction.plot(DATA2$A, DATA2$B, DATA2$Asat, main="AB Interaction plot")
interaction.plot(DATA2$A, DATA2$C, DATA2$Asat, main="AC Interaction plot")
interaction.plot(DATA2$A, DATA2$D, DATA2$Asat, main="AD Interaction plot")
interaction.plot(DATA2$A, DATA2$E, DATA2$Asat, main="AE Interaction plot")
interaction.plot(DATA2$A, DATA2$F, DATA2$Asat, main="AF Interaction plot")
interaction.plot(DATA2$A, DATA2$G, DATA2$Asat, main="AG Interaction plot")

interaction.plot(DATA2$D, DATA2$F, DATA2$Asat, main="DF Interaction plot")

model2<-lm(Asat~.^2,data = DATA2)

anova(model_me,model2)

summary(model2)

halfnormalplot(2*coef(model2)[-1], l=T, n=44)
normalplot(2*coef(model2)[-1], l=T, n=44)


model3<-lm(Asat~A+B+C+D+E+F+G+H+K+B:C+B:D+B:E+B:F+B:G+B:H,data = DATA2)
2*coef(model3)[-1]
summary(model3)

l3= length(2*coef(model3)[-1])
par(mfrow=c(1,1))
normalplot(2*coef(model3)[-1], l=T, n=l3)
halfnormalplot(2*coef(model3)[-1], l=T, n=l3)
abline(a = 1, b = 2.3, col = "red")
