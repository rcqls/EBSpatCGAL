require(EBSpatCGAL)

# init del2
del2m <- Delaunay()

insert(del2m,x=runif(100,-350,350),y=runif(100,-350,350),m=runif(100))


# init gibbs 
gdm <- SimGibbs(del2m ~ 2 + Del2(th[1]*(l<=20)+th[2]*(20<l & l<=80)+(l>80)*v[[1]]$m,th=c(2,4))|m ~ unif(0,1)) 

run(gdm)

# init scene => TODO: atomatically created later
(scm <- Scene()) %<<% window2d(c(-350,350),c(-350,350),xlab="",ylab="",main="what a beautiful plot!") 
scm %<<% points(del2m,col="blue")  %<<% lines(del2m,type="vor") 
scm %<<% lines(del2m)

plot(scm)

# delete some part
delete(del2m,inside=c(-300,-300,300,300))

plot(scm)

run(gdm)

plot(scm)