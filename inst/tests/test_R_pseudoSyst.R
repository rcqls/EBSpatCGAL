require(EBSpatCGAL)

#del2 <- Delaunay()

#insert(del2,matrix(runif(200,-350,350),ncol=2))

# lc <- GNZCache(del2~Del2(Th[1]*(l<=20)+Th[2]*(20<l & l<=80))+Del1(Th2*(a<20)),
# 			runs=10000L,
# 			domain=c(-300,-300,300,300)
# 		)
# formula(lc,expression(first=exp(-(.V)),second=-(.V)))
# run(lc,Th=c(0,0),Th2=3,single=-log(1/3600))


pseudoSyst <- Pseudo(del2~Del2(Th[1]*(l<=20)+Th[2]*(20<l & l<=80)), 
	domain=Domain(c(-250,-250),c(250,250)),
	grid=10000,
	expo=TRUE
)

run(pseudoSyst,Single=2,Th=c(2,4))