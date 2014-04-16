# Option I:
# gd <- SimGibbs(del2dom ~ 2 + Del2(th[1]*(l<=20)+th[2]*(20<l & l<=80),th=c(2,4)))
# => params(gd,th=c(2,4)) and gd$struct <- del2
# run(gd) 					# as many times as desired
#
# Option II:
# gd <- SimGibbs(~ 2 + Del2(th[1]*(l<=20)+th[2]*(20<l & l<=80),th=c(2,4)))
# => struct to be specified later! 
# gd$nb_runs <- 1000 # optional since default value is 10000
# As an example, you want to change "th" parameter too
# run(gd,del2dom,th=c(3,4)) 	# then run(gd) as many times as desired
# or
# params(gd,th=c(3,4))
# run(gd,del2dom) 				# then run(gd) as many times as desired
# or 
# params(gd,th=c(3,4))
# gd$dom <- del2dom 
# run(gd) 					# as many times as desired

SimGibbs <-function(form,runs=10000) {
	self <- newEnv(SimGibbs,interMngr=InteractionMngr(form),runs=runs)

	# Maybe no need to generate at the beginning but only when needed
	new(SimGibbsDel2D,list(del2Term),del2$rcpp(),c(-350,-350),c(350,350))

#sim2$single <- 2

#sim2$nb_runs <- 10000
} 