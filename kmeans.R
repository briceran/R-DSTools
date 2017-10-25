#K means clustering on Iris dataset
#### Create the matrix with the data

datanew=matrix(c(iris$Petal.Length,
                 iris$Petal.Width, iris$Sepal.Length,
                 iris$Sepal.Width), ncol=4)
#### See the first rows of the matrix

head(datanew)

#### Find the distance matrix

D1=as.matrix(dist(datanew))

## Extracting distance between obj 35 and 45
D1[35,45]


##   Do k-means clustering, k=4. Get clusters

iris.kmeans=kmeans(datanew,4)
cluster=iris.kmeans$cluster
#### Extract the flower names of data
flowers=c(iris[,5])
#### Figure out if there is any misclassification
table(cluster, flowers)


plot(datanew[,1], datanew[,2],
     col=c("red","green","blue","orange")[unclass(cluster)],pch=c
     (23,21,24,26)[unclass
                (flowers)],main="K-means of iris data containing three
 species",xlab="sepal length", ylab="sepal width")

legend("topleft",c("setosa","versicolor","virginica"),pch=c
       (23,21,24) )

legend("bottomright",c("cluster 1","cluster 2","cluster  3","cluster 4"),pch=c
       ("O","R","G","B"),col=c("orange","red","green","blue"))

pdf(file="kmeans1.pdf")
### It doesn't look like there are 4 species in the data.
### The three-mean clustering works better at identifying the species
dev.off()
