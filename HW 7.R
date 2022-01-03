 require("datasets")
 #loading the iris data set
data("iris") 
#Viewing the structure of the dataset
str(iris)
#View the statistical summery of the dataset
summary(iris)
#Clustering is a type of unsupervised learning so we dont require the class label output.
#You therefore remove the class attribute species and store it in another variable. You then normalize the attributes
#between 0 and 1 using your own function
iris.new <- iris[,c(1,2,3,4)]
iris.class <- iris[,"Species"]
#viewing the new tow rows of dataset
head(iris.new)
#viewing the new top rows in class species
head(iris.class)
#normalizing all the data in the data set to get ready to apply the k means clustering algorithm
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

iris.new$Sepal.Length<- normalize(iris.new$Sepal.Length)
iris.new$Sepal.Width<- normalize(iris.new$Sepal.Width)
iris.new$Petal.Length<- normalize(iris.new$Petal.Length)
iris.new$Petal.Width<- normalize(iris.new$Petal.Width)
head(iris.new)

#apply k means algorithm with number on centroids (k) = 3
result<- kmeans(iris.new,3)
#gives number of records in each cluster
result$size
#gives the value of cluster center datapoint value
result$centers
#gives cluster vector showing the cluster where each record falls
result$cluster

#plot to see how sepal.length and sepal.width data points have been distributed in clusters
par(mfrow=c(2,2), mar=c(5,4,2,2))
plot(iris.new[c(1,2)], col=result$cluster)

#plot to see how sepal.length and sepal.width data points have been distributed originally as per "class" attribute 
#in dataset
plot(iris.new[c(1,2)], col=iris.class)

#plot to see how petal.length and petal.width data points have been distributed in clusters
plot(iris.new[c(3,4)], col=result$cluster)

plot(iris.new[c(3,4)], col=iris.class)


#the result of the table shows that cluster 1 corresponds to virginica, cluster 2 corresponds to versicolor and 
#cluster 3 to setosa
table(result$cluster,iris.class)
