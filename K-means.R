library(MASS)

#the distance function
dist <- function(x, y){
  ret <- apply(y, 1, function(y) sum((x-y)^2))
  return(ret)
}


#k-means-algorithm
k_means <- function(data, k, init_means = data[sample(1: nrow(data), k),]){
  mu <- init_means
  while(TRUE){
    J <- apply(data, 1, function(x){
      min <- which.min(dist(x, mu))
      return(min)
    })
    mu_new <- sapply(1:k, function(j){
      dj <- data[which(J == j),]
      return((1/nrow(dj))*colSums(dj))
    })
    mu_new <- t(mu_new)
    if (all(mu == mu_new)){
      return(list(mu, J))
    }
    mu <- mu_new
  }
}

#data sampled from multivariate normal distribution
data <- mvrnorm(100, mu = c(0,0), Sigma = matrix(c(1,0,0,1), nrow=2))
plot(data, pch = 16)


#sample 5 centroids 
k = 5
clusters <-  k_means(data, k)
mus <- clusters[[1]]
J <- clusters[[2]]
plot(data, col = (1:k)[unclass(J)], pch = 16)
points(mus, add = T, pch = 4)

load_mnist <- function() {
  load_image_file <- function(filename) {
    ret = list()
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    ret$n = readBin(f,'integer',n=1,size=4,endian='big')
    nrow = readBin(f,'integer',n=1,size=4,endian='big')
    ncol = readBin(f,'integer',n=1,size=4,endian='big')
    x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
    ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
    close(f)
    ret
  }
  load_label_file <- function(filename) {
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    n = readBin(f,'integer',n=1,size=4,endian='big')
    y = readBin(f,'integer',n=n,size=1,signed=F)
    close(f)
    y
  }
  train <<- load_image_file('mnist/train-images-idx3-ubyte')
  test <<- load_image_file('mnist/t10k-images-idx3-ubyte')
  
  train$y <<- load_label_file('mnist/train-labels-idx1-ubyte')
  test$y <<- load_label_file('mnist/t10k-labels-idx1-ubyte')  
}


show_digit <- function(arr784, col=gray(12:1/12), ...) {
  image(matrix(arr784, nrow=28)[,28:1], col=col, ...)
}

#load the mnist-data

mnist <- load_mnist()

show_digit(train$x[5,])

train_data <- train$x[1:500,]

show_digit(train_data[4,])

#apply kmeans to mnist
means <- k_means(train_data, 10, init_means = train_data[1:10,])

#plot the centroids
for(i in 1:10){
  show_digit(means[[1]][i,])
}

#plot the first cluster
cl1 <- train_data[means[[2]] == 1,]
for(i in 1:10){
  show_digit(cl1[i,])
}

#pick first instances of each number as new centroids 

init <- train_data[seq(2,20, by = 2),]

newMeans <- k_means(train_data, 10, init_means = init)

for(i in 1:10){
  show_digit(newMeans[[1]][i,])
}
