require(EBSpatCGAL)

new(Del2TermType2D) -> del2Term

new(Del2TermType2D) -> del2Term2


del2 <- Delaunay()

insert(del2,matrix(runif(200,-350,350),ncol=2))


del2Term$set_struct(del2$rcpp())
del2Term2$set_struct(del2$rcpp())

del2Term$infos <- c("l")
del2Term2$infos <- c("x","l","l2")

del2Term$params <- list(th=c(2,4)) #,theta2=1)
del2Term2$params <- list(theta=2)


del2Term$cexprs<-list(a1=substitute(l<=20),a2=substitute(20<l & l<=80)) #,a1=substitute(theta*c(l,sqrt(l2))))
del2Term$cexprs.size <- c(1,1)
del2Term2$cexprs<-list(a3=substitute(l))
del2Term2$cexprs.size <- c(1)

del2Term$exprs<-list(f1=substitute(th[1]*a1+th[2]*a2)) #,a1=substitute(theta*c(l,sqrt(l2))))
del2Term$exprs.size <- c(1)
del2Term2$exprs<-list(f2=substitute(theta*a3))
del2Term2$exprs.size <- c(1)

#new(Interaction,list(del2Term,del2Term2)) -> inter

new(Interaction,list(del2Term,del2Term2)) -> inter

dom2 <- new(Domain,c(-350,-350),c(350,350)) 


cache2 <- new(GNZCacheCpp,inter,dom2)

cache2$set_mode(2)

cache2$make_lists()

cache2$get_lists() -> cachelists

(function(cl,nms) {
	res <- list()
	for(term in 1:length(cl[[1]])) {
		res[[term]] <-list(before=list(),after=list())
		for(nm in nms[[term]]) {
			res[[term]]$before[[nm]] <- c()
			for(pt in 1:length(cl)) {
				if(length(cl[[pt]][[term]]$before)>0) for(j in 1:length(cl[[pt]][[term]]$before)) res[[term]]$before[[nm]] <- c(res[[term]]$before[[nm]],cl[[pt]][[term]]$before[[j]][[nm]])
				if(length(cl[[pt]][[term]]$after)>0) for(j in 1:length(cl[[pt]][[term]]$after)) res[[term]]$after[[nm]] <- c(res[[term]]$after[[nm]],cl[[pt]][[term]]$after[[j]][[nm]])
			}
		}
		res[[term]]$before <- as.data.frame(res[[term]]$before)
		res[[term]]$after <- as.data.frame(res[[term]]$after)
	}
	res
})(cachelists,list(names(del2Term$cexprs),names(del2Term2$cexprs))) -> cachelists2