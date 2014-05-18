require(EBSpatCGAL)

reg2 <- Regular()

insert(reg2,x=runif(10),y=runif(10),w=runif(10))

(sc <- Scene()) %<<% window2d(xlab="",ylab="",main="what a beautiful plot!") 
sc %<<% points(reg2,col="blue")  %<<% lines(reg2,type="vor") 
sc %<<% lines(reg2)

plot(sc)