require(EBSpatCGAL)


del3 <- Delaunay(3)

insert(del3,matrix(runif(300,-350,350),ncol=3))

# init gibbs 
gd3 <- SimGibbs(del3 ~ 14 + Del2(th[1]*(l<=20)+th[2]*(20<l & l<=80),th=c(-2,35)),domain=Domain(c(-350,-350,-350),c(350,350,350))) 

run(gd3)

tkinv3 <- TKInverse(del3~Del2(Th[1]*(l<=20)+Th[2]*(20<l & l<=80)), 
	runs=10000L,
	domain=Domain(c(-250,-250,-250),c(250,250,250))
)

run(tkinv3,Single=0,Th=c(0,0))