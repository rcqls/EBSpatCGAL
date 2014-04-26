##update toDebug to your needs

#source("../../R/formMngr.R")

#cleanDebug <- function() {
#	rm(.TermTypes,TermTypes,ComponentFunctionalFormulaManager,formula.ComponentFunctionalFormulaManager,autoCaracFormula,cleanDebug,envir=globalenv())
#}

formMngr <- ComponentFunctionalFormulaManager()

formula(formMngr,Del2(c(1,l<30)),local=TRUE)

formula(formMngr,d2(l<30))
print(formula(formMngr))


formMngr2 <- ComponentFunctionalFormulaManager()

formula(formMngr2,d2(l<30))
formula(formMngr2,Del2(c(C=1,D=l<30)),local=TRUE)
print(formula(formMngr2))

## idea: formula represented as a tree simplified!
## find all the common carac inside the formulas!!!


formMngr3 <- ComponentFunctionalFormulaManager()

formula(formMngr3,1,local=TRUE)
formula(formMngr3,Del2(l<30),local=TRUE)


formula(formMngr3,1)
formula(formMngr3,d2(l<30))

print(formula(formMngr3))


