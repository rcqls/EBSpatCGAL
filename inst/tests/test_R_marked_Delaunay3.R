require(EBSpatCGAL)

tmp <- Delaunay(3)
insert(tmp,x=6:8,y=6:8,z=3:1,m=1:3,m2=3:5)

tmp2 <- Delaunay(3)
insert(tmp2,x=6:8,y=6:8,z=3:1) # no mark even an empty R list is attached to each point