require(EBSpatCGAL)

# init del2
del2 <- Delaunay()

#insert(del2,matrix(runif(200,-350,350),ncol=2))


# init gibbs 
gd <- SimGibbs(del2 ~ 2 + Del2(th[1]*(l<=20)+th[2]*(20<l & l<=80),th=c(2,4))) 

run(gd)

require(EBSpat)
pts <- t(vertices(del2))
EBVor(ncol(pts)+1) -> del2bis

insert(del2bis,pts)

pld <- EBPseudoExpo(del2bis~Del2(l<=20,20<l & l<=80),domainSize=500,weight=FALSE,mode="systematic")
run(pld,c(0,0,0))

print(pld$right)

pseudo <- Pseudo(del2~Del2(Th[1]*(l<=20)+Th[2]*(20<l & l<=80)), 
	runs=10000L,
	domain=Domain(c(-250,-250),c(250,250)),
	expo=TRUE
)

run(pseudo,Single=2,Th=c(2,4))


