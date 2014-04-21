require(EBSpatCGAL)

del3 <- Delaunay(3)

insert(del3,matrix(runif(300,-350,350),ncol=3))

# init gibbs 
gd3 <- SimGibbs(del3 ~ 10 + Del2(th[1]*(l<=20)+th[2]*(20<l & l<=80),th=c(2,4)),domain=c(-350,-350,-350,350,350,350)) 

run(gd3)

(sc3 <- Scene()) %<<% window3d(c(-350,350),c(-350,350),c(-350,350)) 
sc3 %<<% points(del3,col="red",radius=3)  %<<% lines(del3)

plot(sc3)
