#library(png)
library(pixmap)
library(scales)
library(Matrix)

add <- function(x,y) # x has smaller dimensions
{
  dr <- nrow(x)
  dc <- ncol(x)
  
  y[1:dr,1:dc] <- y[1:dr,1:dc] + x
  return(y)
}

plot.image <- function(M)
{
  #plot(1:2, type='n')
  #v <-c(ncol(M),nrow(M))
  #m <- max(v)*1.1
  #rasterImage(M, 1,1,1+v[1]/m,1+v[2]/m)
  dev.new()
  image(M, col=heat.colors(255))
}

get.image <- function(filename)
{
  #x <- readPNG(filename)[,,1]
  image <- read.pnm(filename)
  
  red.matrix <- matrix(image@red, nrow = image@size[1], ncol = image@size[2])
  green.matrix <- matrix(image@green, nrow = image@size[1], ncol = image@size[2])
  blue.matrix <- matrix(image@blue, nrow = image@size[1], ncol = image@size[2])
  
  return(list(red.matrix, green.matrix, blue.matrix, image))
}

compress <- function(img, k)
{
  SVD <- svd(img)
  u <- SVD$u
  v <- SVD$v
  d <- SVD$d #Diagonal(x=SVD$d)
  
  d <- sum(d)
  
  x <- as.matrix(d*u %*% t(v))
  x[x<0] <- 0
  x[x>1] <- 1
  
  return(x)
}

compress2 <- function(img, k)
{
  SVD <- svd(img)
  u <- SVD$u
  v <- SVD$v
  d <- SVD$d
  
  return(u[,1:k] %*% diag(d[1:k]) %*% t(v[,1:k]))
}