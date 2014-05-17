require(EBSpatCGAL)
formMngr <- ComponentFunctionalFormulaManager()

formula(formMngr,gd~Del2(Th[1]*(l<=20)+Th[2]*(20<l & l<=80))+Del1(Th2*(a<=20)),local=TRUE)
formula(formMngr,1,local=TRUE)
formula(formMngr,del2(l<=20),local=TRUE) 
formula(formMngr,del2(20<l & l<=80),local=TRUE)
formula(formMngr,all2(range=80| l<=20),local=TRUE)
formula(formMngr,all2(range=100| l<=80),local=TRUE)

print(formula(formMngr))