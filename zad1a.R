library(Matrix)
library(lattice)
library(rgl)

draw_sphere <- function(precision=0.1) 
{
  s<-seq(0, 2*pi, precision)
  s<- append(s,2*pi)
  t<-seq(0, pi, length.out=length(s))
  
  st <- expand.grid(s=s, t=t)
  
  st <- transform(st, x=cos(s)*sin(t),
                      y=sin(s)*sin(t),
                      z=cos(t))

  rr <- c(-1,1)
  
  cloud(z~x+y, st, xlim=rr, ylim=rr, zlim=rr, screen=list(y=20), pretty=TRUE);
  
  #surface3d(s$x,s$y,s$z,col="gray")
}

draw_ellipsoid <- function(A, precision=0.1)
{
  s<-seq(0, 2*pi, precision)
  s<- append(s,2*pi)
  t<-seq(0, pi, length.out=length(s))
  
  st <- expand.grid(s=s, t=t)
  
  st <- transform(st, x=A[1,1]*cos(s)*sin(t)+A[1,2]*sin(s)*sin(t)+A[1,3]*cos(t),
                  y=A[2,1]*cos(s)*sin(t)+A[2,2]*sin(s)*sin(t)+A[2,3]*cos(t),
                  z=A[3,1]*cos(s)*sin(t)+A[3,2]*sin(s)*sin(t)+A[3,3]*cos(t))
  
  st2 <- transform(st, x=cos(s)*sin(t),
                  y=sin(s)*sin(t),
                  z=cos(t))
  
  
  max <- max(unlist(A)) 
  rr<-c(-max,max)

  cloud(z~x+y, st, xlim=rr, ylim=rr, zlim=rr, screen=list(y=20), pretty=TRUE);
  cloud(z~x+y, st2, xlim=rr, ylim=rr, zlim=rr, screen=list(y=20), pretty=TRUE,
  panel.3d.cloud = panel.3dscatter.old);
  
}

draw_sphere2 <- function(precision=0.1) 
{
  t<-seq(0, 2*pi, precision)
  t <- append(t, 2*pi)
  u<-seq(0, pi, length.out=length(t))
  
  xm<-outer(t,u,function(t, u)cos(t)*sin(u))
  ym<-outer(t,u,function(t, u)sin(t)*sin(u))
  zm<-outer(t,u,function(t, u) cos(u))
  
  rr<-c(-1,1)
  wireframe(zm~xm+ym, xlim=rr, ylim=rr, zlim=rr, screen=list(y=30),
            col='red')
  
}

draw_ellipsoid2 <- function(A, precision=0.1)
{
  t<-seq(0, 2*pi, precision)
  t<-append(t, 2*pi)
  u<-seq(0, pi, length.out=length(t))
  
  max <- max(unlist(A))
  
  xm<-outer(t,u,function(t, u)A[1,1]*cos(t)*sin(u)+A[1,2]*sin(t)*sin(u)+A[1,3]*cos(u))
  ym<-outer(t,u,function(t, u)A[2,1]*cos(t)*sin(u)+A[2,2]*sin(t)*sin(u)+A[2,3]*cos(u))
  zm<-outer(t,u,function(t, u) A[3,1]*cos(t)*sin(u)+A[3,2]*sin(t)*sin(u)+A[3,3]*cos(u))
  
  #xm1<-outer(t,u,function(t, u)cos(t)*sin(u))
  #ym1<-outer(t,u,function(t, u)sin(t)*sin(u))
  #zm1<-outer(t,u,function(t, u) cos(u))
  
  rr<-c(-max,max)
  wireframe(zm~xm+ym, xlim=rr, ylim=rr, zlim=rr, screen=list(y=30), col='red')
  #wireframe(zm1~xm1+ym1, xlim=rr, ylim=rr, zlim=rr, screen=list(y=30), col='blue')   
  
}

draw_sphere3 <- function(precision=0.1) 
{
  s<-seq(0, 2*pi, precision)
  s<- append(s,2*pi)
  t<-seq(0, pi, length.out=length(s))
  
  st <- expand.grid(s=s, t=t)
  
  st <- transform(st, x=cos(s)*sin(t),
                  y=sin(s)*sin(t),
                  z=cos(t))
  
  rr <- c(-1,1)
  
  open3d()
  plot3d(st$x, st$y, st$z, col='blue')
}

draw_ellipsoid3 <- function(A, precision=0.1)
{
  s<-seq(0, 2*pi, precision)
  s<- append(s,2*pi)
  t<-seq(0, pi, length.out=length(s))
  
  st <- expand.grid(s=s, t=t)
  
  st <- transform(st, x=A[1,1]*cos(s)*sin(t)+A[1,2]*sin(s)*sin(t)+A[1,3]*cos(t),
                  y=A[2,1]*cos(s)*sin(t)+A[2,2]*sin(s)*sin(t)+A[2,3]*cos(t),
                  z=A[3,1]*cos(s)*sin(t)+A[3,2]*sin(s)*sin(t)+A[3,3]*cos(t))
  
  st2 <- transform(st, x=cos(s)*sin(t),
                   y=sin(s)*sin(t),
                   z=cos(t))
  
  
  max <- max(unlist(A)) 
  rr<-c(-max,max)
  
  open3d()  
 # axes3d()
  
  plot3d(st$x, st$y, st$z, col='blue', add=TRUE)
  plot3d(st2$x, st2$y, st2$z, col='red', add=TRUE)
  
  #decorate3d(axes=TRUE)
  
  # lines for X,Y,Z axes
  lines3d(x=c(0,0),y=c(0,0), z=c(-max,max), col='black', add=TRUE)
  lines3d(x=c(0,0),y=c(-max,max), z=c(0,0), col='black', add=TRUE)
  lines3d(x=c(-max,max),y=c(0,0), z=c(0,0), col='black', add=TRUE)
  
  # lines for ellipsoid (symmetry axes)
  u <- max*svd(A)$u

  lines3d(x=c(-u[1,1],u[1,1]),y=c(-u[2,1],u[2,1]), z=c(-u[3,1],u[3,1]), col='yellow', add=TRUE)
  lines3d(x=c(-u[1,2],u[1,2]),y=c(-u[2,2],u[2,2]), z=c(-u[3,2],u[3,2]), col='yellow', add=TRUE)
  lines3d(x=c(-u[1,3],u[1,3]),y=c(-u[2,3],u[2,3]), z=c(-u[3,3],u[3,3]), col='yellow', add=TRUE)
  
  rgl.bringtotop()
}

plot_SUEVt <- function(A, precision=0.1)
{
  s<-seq(0, 2*pi, precision)
  s<- append(s,2*pi)
  t<-seq(0, pi, length.out=length(s))
  
  st <- expand.grid(s=s, t=t)
  
  v <- t(svd(A)$v) # Already transposed
  u <- svd(A)$u
  d <- Diagonal(x=svd(A)$d)
  
  a <- u %*% d %*% v
  
  ev <- d %*% v
  
  stv <- transform(st, x=v[1,1]*cos(s)*sin(t)+v[1,2]*sin(s)*sin(t)+v[1,3]*cos(t),
                  y=v[2,1]*cos(s)*sin(t)+v[2,2]*sin(s)*sin(t)+v[2,3]*cos(t),
                  z=v[3,1]*cos(s)*sin(t)+v[3,2]*sin(s)*sin(t)+v[3,3]*cos(t))
  
  stev <- transform(st, x=ev[1,1]*cos(s)*sin(t)+ev[1,2]*sin(s)*sin(t)+ev[1,3]*cos(t),
                   y=ev[2,1]*cos(s)*sin(t)+ev[2,2]*sin(s)*sin(t)+ev[2,3]*cos(t),
                   z=ev[3,1]*cos(s)*sin(t)+ev[3,2]*sin(s)*sin(t)+ev[3,3]*cos(t))                  
  
  stA <- transform(st, x=a[1,1]*cos(s)*sin(t)+a[1,2]*sin(s)*sin(t)+a[1,3]*cos(t),
                  y=a[2,1]*cos(s)*sin(t)+a[2,2]*sin(s)*sin(t)+a[2,3]*cos(t),
                  z=a[3,1]*cos(s)*sin(t)+a[3,2]*sin(s)*sin(t)+a[3,3]*cos(t))                
  
  rr <- c(-1,1)
  
  open3d()
  plot3d(stv$x, stv$y, stv$z, col='blue', title="S*V'")
  
  open3d()
  plot3d(stev$x, stev$y, stev$z, col='blue', title="S*E*V'")
  
  open3d()
  plot3d(stA$x, stA$y, stA$z, col='blue', title="S*E*V'")
  
}
