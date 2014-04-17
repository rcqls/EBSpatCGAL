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


## TODO: domain would be included later in Domain object embedding Struct 
## playing the role of response in formula
SimGibbs <-function(form,runs=10000,domain=c(-350,-350,350,350)) {
	self <- newEnv(SimGibbs,interMngr=InteractionMngr(form),runs=runs,domain=domain)
	if(!is.null(self$response)) {
		self$struct <- try(eval.parent(self$response))
		if(inherits(self$struct,"try-error")) warning("No proper response in SimGibbs!")
		self$dim <- self$struct$dim
		dim(self$interMngr) <- self$dim
	} else self$dim <- 2

	self$single <- self$interMngr$single

	init.SimGibbs(self)

	self
}

init.SimGibbs <- function(self) {
	# Maybe no need to generate at the beginning but only when needed
	PersistentRcppObject(self,{
		class <- eval(parse(text=paste("SimGibbsDel",self$dim,"D",sep="")))
		new(class,terms(self$interMngr),self$struct$rcpp(),self$domain[1:self$dim],self$domain[self$dim+(1:self$dim)])
	})
}