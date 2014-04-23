require(EBSpatCGAL)

interMngr <- InteractionMngr(~2+Del2(Th*(l<30),Th=2))


interMngr2 <- InteractionMngr(~2+Del2(Th*(l<30),Th=2)| m ~ unif(0,1))

interMngr3 <- InteractionMngr(~2+Del2(Th*(l<30)*(v$m),Th=2)| m : m2 : m3 ~ unif(0,1) : pois(3) : norm(0,2) )

print(interMngr3$mark.fun)

print(interMngr3$mark.expr)

print(interMngr3$mark.name)