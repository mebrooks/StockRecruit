##' Fit a stock recruitment curve
##' @param S vector of spawning stock biomasses
##' @param R  vector of recruitment values
##' @param shape can be "contHockey", "Ricker", or "BevertonHolt"
##' @param g is the smoothing parameter gamma in the bent hyperbola hockey-stick stock-recruitment model
##' @param start (optional) list of parameters
##' @param weights (optional) weights of each data point to reflect uncertainty
##' @details
##' \itemize{
##' \item "contHockey" shape has pars=c(beta, delta, log_sd) following the bent hyperbola hockey-stick stock-recruitment model (Mesnil &Rochet 2010)
##' \item "Ricker" shape has pars=c(log_a, log_b, log_sd) where expected recruitment is a*S*exp(-b*S)
##' \item "BevertonHolt" shape has pars=c(log_a, log_b, log_sd) where expected recruitment is a*S/(1+b*S)
##' }
##' @useDynLib contHockey
##' @useDynLib Ricker
##' @useDynLib BevertonHolt
##' @export
fitSRCurve = function(S, R, shape="contHockey", g=0.1, start=NULL, weights=NULL) {
	if(length(S)!=length(R)) stop("Lengths of S and R must match")

	if(is.null(start)) start=initializePars(dat=data.frame(S=S, R=R), shape)
	if(is.null(weights)) weights=rep(1, length(S))

	dat=list(S=S, R=R, weights= weights, g=g)

	mod=MakeADFun(dat, parameters=start, DLL=shape, silent = TRUE)
	fit=nlminb(mod$par, mod$fn, mod$gr)
	return(mod)
}

##' @details contHockey breakpoint is the SSB corresponding to the geometric mean recruitment
initializePars=function(dat, shape) {
	if(any(is.na(dat))) {dat=na.omit(dat); warning("NaN is being removed from data.")}
	pars=switch(shape,
		"contHockey" =  list(log_beta=log(median(dat$R/dat$S)), log_delta=log(dat$S[which.min(dat$R-exp(mean(log(dat$R))))]), log_sd=0),
		"Ricker" = c(log_a=0, log_b=0, log_sd=0),
		"BevertonHolt" = c(log_a=0, log_b=0, log_sd=0)
	)

	return(pars)
}
