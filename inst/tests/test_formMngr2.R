## try to create a parser for formula 

if(FALSE) print(tmp <- autoCompFuncFormula(c(exp(-all2(c(l2<40,l2>50)))^2+Del1(a>10),d2(2|20<l & l<50)+A2(30|l>30,2),all2(l>50)^2)))

## no need to parse! tmp2$func is then NULL
if(FALSE) print(tmp2 <- autoCompFuncFormula(1+exp(-.V)))

if(FALSE) print(tmp <- autoCaracFormula(~exp(l<40)^Th[1]+Th[2]*(l2>2000)+Th3*(2000 < l2)))

if(FALSE) print(tmp <- formula(EBFunc(~Del2(l)),EBFunc(~Del2(l2))))

if(FALSE) { 
	CompFuncFormulaManager() -> cpfMngr
	formula(cpfMngr,gd~Del2(Th*(l2<40)))
	formula(cpfMngr,c(exp(-all2(c(l2<40,l2>50)))^2+Del1(a>10),d2(2|20<l & l<50)+A2(30|l>30,2),all2(l>50)^2))
	print(formula(cpfMngr)) #to see the formula
	formula(cpfMngr,c(exp(-all2(c(l2<400,l2>500)))^2+Del1(a>100),d2(2|200<l & l<500)+A2(300|l>300,2),all2(l>500)^2))
	print(formula(cpfMngr)) #to see the formulas
}

if (FALSE) {
	CompFuncFormulaManager() -> cpfMngr
	formula(cpfMngr,gd~del2(Th[1]*(l<20)+Th[2]*(l>=20 & l<80)))
	print(formula(cpfMngr)) #to see the formula
	formula(cpfMngr, c(d2(l<20),d2(l>=20 & l<80)))
	print(formula(cpfMngr)) #to see the formulas
}

if (TRUE) {
	CompFuncFormulaManager() -> cpfMngr
	formula(cpfMngr,gd~del2(2*(l<20)+3*(l>=20 & l<80))) #when Th replaced by value this does not work!!!!
	print(formula(cpfMngr)) #to see the formula
	formula(cpfMngr, c(d2(l<20),d2(l>=20 & l<80)))
	print(formula(cpfMngr)) #to see the formulas
}
