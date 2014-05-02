require(EBSpatCGAL)

del2 <- Delaunay()

insert(del2,matrix(runif(200,-350,350),ncol=2))

#lc <- ListsCache(list(del2~Del2(Th[1]*(l<=20)+Th[2]*(20<l & l<=80)) , substitute(del2(l<=20)), substitute(del2(20<l & l<=80))))
lc <- ListsCache(del2~Del2(Th[1]*(l<=20)+Th[2]*(20<l & l<=80))+Del1(Th2*(a<20)),expression(1)[[1]],del2(l<=20), del2(20<l & l<=80),
			runs=10000L,
			domain=c(-300,-300,300,300)
		)


#print(
#get.ListsCache(lc)
#) # result in lc$cexprs.cachelist

run(lc,Th=c(0,0),Th2=3,single=-log(1/3600))