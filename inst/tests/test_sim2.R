require(EBSpatCGAL)

new(Del2TermType2D) -> del2Term

new(Del2TermType2D) -> del2Term2


del2 <- Delaunay()

insert(del2,matrix(runif(200),ncol=2))

#### OLD CALL
# gd<-EBGibbs(~ 2 +Del2(th[1]*(l<=20)+th[2]*(20<l & l<=80),th=c(2,4)))

del2Term$set_graph(del2$graph)
#del2Term2$set_graph(del2$graph)

del2Term$infos <- c("l")
#del2Term2$infos <- c("x","l","l2")

del2Term$params <- list(th=c(2,4)) #,theta2=1)
#del2Term2$params <- list(theta=2)


del2Term$exprs<-list(substitute(th[1]*(l<=20)+th[2]*(20<l & l<=80))) #,a1=substitute(theta*c(l,sqrt(l2))))
del2Term$exprs.size <- c(1)
#del2Term2$exprs<-list(a3=substitute(theta*l))
#del2Term2$exprs.size <- c(1)

#new(Interaction,list(del2Term,del2Term2)) -> inter

new(Interaction,list(del2Term)) -> inter


sim2 <- new(SimGibbsDel2D,list(del2Term),del2$graph,c(0,0),c(1,1))

sim2$single <- 2

sim2$nb_runs <- 10

(sc <- Scene()) %<<% window2d(xlab="",ylab="",main="what a beautiful plot!") 
sc %<<% points(del2,col="blue")  %<<% lines(del2,type="vor") 
sc %<<% lines(del2)


