require(EBSpatCGAL)

tmp <- Delaunay()
insert(tmp,x=6:8,y=6:8,z=3:1,m=1:3,m2=3:5) # z is here a mark

print(vertices(tmp,"all"))

tmp2 <- Delaunay()
insert(tmp2,x=6:8,y=6:8) # no mark even an empty R list is attached to each point

print(vertices(tmp2,"all"))