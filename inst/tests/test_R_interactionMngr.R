require(EBSpatCGAL)

interMngr <- InteractionMngr(del2 ~ -2+Del2(Th*(l<30),Th=2) + Del1(Th2*a,Th2=3))


interMngr2 <- InteractionMngr(~2+Del2(Th*(l<30),Th=2)| m ~ unif(0,1))

interMngr3 <- InteractionMngr(~2+Del2(Th*(l<30)*(v[[1]]$m),Th=2)| m : m2 : m3 ~ unif(0,1) : pois(3) : norm(0,2) )

print(interMngr3$mark.fun)

print(interMngr3$mark.expr)

print(interMngr3$mark.name)

#interMngr4 <- InteractionMngr(del2 ~ -2+Del2(Th*(l<30),Th=2) + All2(Th2*(l<40),Th2=3,range=100))