rm(list = ls())
library(imager)

# setting
M = 15
Y = as.list(numeric(M))
meanface = array(0 , c(300 , 371 , 1 , 1))
par(mfrow = c(3 , 5), mar = c(0, 0 , 0 , 0 ))

# pretreatment
for(i in 1 : M){
  face = load.image(paste("C:/Users/AMD/Desktop/image/" , i , ".jpg" , sep = "" , collapse = ""))
  face = resize(face , 300 , 371)
  face = grayscale(face)
  meanface = meanface + face/M
  Y[[i]] = face
  plot(face , axes = F, xlab = "", ylab = "")
}

# meanface
plot(meanface, axes = F , xlab = "", ylab = "")
y = matrix(NA , 300*371 , M)
for(i in 1 : M){
  y[,i] = c(Y[[ i ]])
}

# deviation matrix
A = y - rowMeans(y)
L = t(A)%*%A/M
eigL = eigen(L)
eigenface = A%*%eigL$ve
U = matrix(NA , 300*371 , M)
for(i in 1 : M){
  U[,i] = eigenface[,i]/sqrt(sum(eigenface[,i]^2))
}

# eigenfaces
par(mfrow = c(3 , 5),mar = c(0 , 0 , 0 , 0))
for(i in 1 : M){ 
  plot(as.cimg(array(U[,i],c(300 , 371 , 1 , 1))), axes = F, xlab = "", ylab = "")
}
# face reconstruction
recface = rep(0 , 300*371) + c(meanface)
par(mfrow = c(3 , 5),mar = c(0 , 0 , 0 , 0))
for(i in 1 : M) {
  recface = recface + sum(U[,i]*c(Y[[1]]))*U[,i]
  plot(as.cimg(array(recface,c(300 , 371 , 1 , 1))), axes=F, xlab="", ylab="")
}

#Reconstruct a few face images that are not in the database
facenew = load.image(paste("C:/Users/AMD/Desktop/image/",16,".jpg",sep="",collapse=""))
facenew = resize(facenew,300,371)
facenew = grayscale(facenew)
recface=rep(0,300*371)+c(meanface)
for(i in 1:M) {
  recface=recface+sum(U[,i]* c(facenew))*U[,i]
  plot(as.cimg(array(recface,c(300,371,1,1)) ), axes=F, xlab="", ylab="")
}

#Reconstruct a few images which are not faces
facenew = load.image(paste("C:/Users/AMD/Desktop/image/" , 17 , ".jpg" , sep = "",collapse = ""))
facenew = resize(facenew , 300 , 371)
facenew = grayscale(facenew)
recface = rep(0,300*371) + c(meanface)
for(i in 1 : M) {
  recface = recface + sum(U[,i]* c(facenew))*U[,i]
  plot(as.cimg(array(recface,c(300 , 371 , 1 , 1)) ), axes = F, xlab = "", ylab = "")
}
