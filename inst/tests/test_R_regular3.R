require(EBSpatCGAL)
require(rgl)

reg3 <- Regular(3)

insert(reg3,x=runif(100),y=runif(100),z=runif(100),w=runif(100))

(sc3 <- Scene()) %<<% window3d() 
sc3 %<<% lines(reg3) 
sc3 %<<% points(reg3,col="blue")  
#sc3  %<<% lines(reg3,type="vor") 


plot(sc3)