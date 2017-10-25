#Principal Component Analysis applied to IRIS dataset

#input data before centering
X2=as.matrix(iris[1:150, 1:4])
# centering
X2[,1] = X2[,1] - mean(X2[,1]) # subtract the mean
X2[,2] =X2[,2]-mean(X2[,2])
X2[,3] =X2[,3]-mean(X2[,3])
X2[,4] = X2[,4] - mean(X2[,4])
# Covariance matrix of X
Sx2= var(X2)
Sx2

#Finding eigenvalues and eigenvectors.
EP2= eigen(Sx2)
EP2
V2= EP2$vectors # extract the eigenvectors for future use
Lambda2= EP2$values #extract the eigenvalues for future use
# View the V and the Lambda
V2; Lambda2

# Principal components of the first two flowers
PC2=X2[1:2, ]%*%V2 # they should be like the ones you did by hand
# View PC of the first two flowers.
PC2

# Principal components of the whole data set
PC2=X2%*%V2
PC2
# Plot the first two Principal components
plot(PC2[,1],PC2[,2],xlim=c(-5,5),ylim=c(-2,2),main="First two PC of 150 flowers")

# Helping to find the number of PC to keep for at least 0.85
sum(EP2$values)
cumsum(EP2$values)/sum(EP2$values)



#reading data
PS1<-read.csv(file="PS1.csv")
Scores<-as.matrix(PS1)
# centering
Scores[,1] = Scores[,1] - mean(Scores[,1]) # subtract the mean
Scores[,2] = Scores[,2] - mean(Scores[,2])
Scores[,3] = Scores[,3] - mean(Scores[,3])
Scores[,4] = Scores[,4] - mean(Scores[,4])
Scores[,5] = Scores[,5] - mean(Scores[,5])
Scores[,6] = Scores[,6] - mean(Scores[,6])
Scores[,7] = Scores[,7] - mean(Scores[,7])
Scores[,8] = Scores[,8] - mean(Scores[,8])
Scores[,9] = Scores[,9] - mean(Scores[,9])
Scores[,10] = Scores[,10] - mean(Scores[,10])
Scores[,11] = Scores[,11] - mean(Scores[,11])
Scores[,12] = Scores[,12] - mean(Scores[,12])



# Covariance matrix of X
Sx= var(Scores)
Sx
#######

#Finding eigenvalues and eigenvectors.
EP= eigen(Sx)
EP
V= EP$vectors 
Lambda= EP$values
# View the V and the Lambda
V; Lambda

# Principal components of the first two flowers
PC=Scores[1:2, ]%*%V
# View PC of the first two flowers.
PC

# Principal components of the whole data set
PC=Scores%*%V
PC
# Plot the first two Principal components
plot(PC[,1],PC[,2],xlim=c(-8,8),ylim=c(-8,8),main="First two PC of 1999 obs")

# Helping to find the number of PC to keep for at least 0.85
sum(EP$values)
cumsum(EP$values)/sum(EP$values)
