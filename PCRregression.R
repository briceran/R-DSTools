#Example of Principal component regression on Iris dataset

#input data before centering
X2=as.matrix(iris[1:150, 1:4])
# centering
X2[,1] = X2[,1] - mean(X2[,1]) # subtract the mean
X2[,2] =X2[,2]-mean(X2[,2])
X2[,3] =X2[,3]-mean(X2[,3])
X2[,4] = X2[,4] - mean(X2[,4])

pollen= c(rnorm(50,20,3), rnorm(50,100, 15), rnorm(50, 37, 6))

# Covariance matrix of X
Sx2= var(X2)
#Finding eigenvalues and eigenvectors.
EP2= eigen(Sx2)
V= EP2$vectors # extract the eigenvectors for future use
Lambda2= EP2$values #extract the eigenvalues for future use
# Principal components of the whole data set
PC2=X2%*%V
pc1<-PC2[,1]
pc2<-PC2[,2]
pc3<-PC2[,3]
pc4<-PC2[,4]
#### linear regression of pc1,pc2,pc3,pc4 on pollen
pc.model<-lm(pollen~ pc1+ pc2+ pc3+ pc4+ 0)
summary(pc.model)
########
# Hwk 9 Q2 part (b)
#########
rpollen.pc1<-cor(pollen,pc1) ##correlations
rpollen.pc2<-cor(pollen,pc2)
rpollen.pc3<-cor(pollen,pc3)
rpollen.pc4<-cor(pollen,pc4)
norm.pollen<-(t(pollen)%*%pollen) ##norm y squared
hatgamm.1sq<-((rpollen.pc1)^2)*norm.pollen
hatgamm.2sq<-((rpollen.pc2)^2)*norm.pollen
hatgamm.3sq<-((rpollen.pc3)^2)*norm.pollen
hatgamm.4sq<-((rpollen.pc4)^2)*norm.pollen
