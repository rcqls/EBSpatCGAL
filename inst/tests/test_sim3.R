require(EBSpatCGAL)

new(Del2TermType3D) -> del3Term


del3 <- Delaunay(3)

insert(del3,matrix(runif(300,-350,350),ncol=3))

#### OLD CALL
# gd<-EBGibbs(~ 2 +Del2(th[1]*(l<=20)+th[2]*(20<l & l<=80),th=c(2,4)))

del3Term$set_graph(del3$rcpp())
#del2Term2$set_graph(del2$graph)

del3Term$infos <- c("l")
#del2Term2$infos <- c("x","l","l2")

del3Term$params <- list(th=c(2,4)) #,theta2=1)
#del2Term2$params <- list(theta=2)


del3Term$exprs<-list(substitute(th[1]*(l<=20)+th[2]*(20<l & l<=80))) #,a1=substitute(theta*c(l,sqrt(l2))))
del3Term$exprs.size <- c(1)
#del2Term2$exprs<-list(a3=substitute(theta*l))
#del2Term2$exprs.size <- c(1)

#new(Interaction,list(del2Term,del2Term2)) -> inter

#new(Interaction,list(del2Term)) -> inter


sim3 <- new(SimGibbsDel3D,list(del3Term),del3$rcpp(),c(-350,-350,-350),c(350,350,350))

sim3$single <- 2

sim3$nb_runs <- 10000

(sc3 <- Scene()) %<<% window3d(c(-350,350),c(-350,350),c(-350,350)) 
sc3 %<<% points(del3,col="red",radius=3)  %<<% lines(del3)

sim3$run( )

plot(sc3)