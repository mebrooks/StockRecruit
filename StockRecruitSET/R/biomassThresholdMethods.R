##' Calculate Blim in ways described by ICES Advice Technical Guidelines Table 12.4.3.1.3
##' @param S vector of spawning stock biomasses
##' @param R  vector of recruitment values
##' @param quant quantile above which recruitment is considered to be "large" in a "spasmodic stock" (language from ICES Advice Technical Guidelines)
##' @param type way of calculating Blim.
##'\itemize{
##' \item 1 is the minimum S that gives large recruitment. Must specify \code{quant} to define "large".
##'  \item 2 is the estimated S for a breakpoint in a hockey stick model.
##'  \item 2.1 is estimated S for inflection point in a bent hyperbola hockey-stick stock-recruitment model (Mesnil & Rochet 2010).
##'  \item 5 is the minimum observed S (Blim=Bloss).
##'  }
##' @param g is the assumed smoothing parameter in the bent hyperbola hockey-stick model.
##' @param by the precision needed for a grid search for breakpoint in a hockey-stick model. If missing, 100 points from min to max SSB are tried.
##' @param AIC should the AIC be returned instead of the estimate? Only availabel with type 2 and 2.1.
##' @importFrom bbmle mle2
##' @importFrom bbmle coef
##' @importFrom bbmle logLik
##' @export
calcBlim = function(S, R, quant=0.75, type=2.1, g=.1, by=NULL, AIC=FALSE)
{
	#remove missing combinations
	dat=data.frame(S, R)
	if(any(is.na(dat))) {dat=na.omit(dat); warning("NaN is being removed from data.")}
	S=dat$S; R=dat$R

	if(length(S)!=length(R)) stop("Lengths of S and R must match")

	if(type==1)	{ return(min(S[which(R>=quantile(R, quant))])) }

	if(type==2) {
		if(is.null(by)) by=(max(S)-min(S))/99
		Blim=seq(min(S), max(S), by=by)
		LLB=Blim
		LLB[]=NA

		for(i in 1:length(Blim)) {
			x=Blim[i]
			fit = mle2(log(R)~ dnorm(log(ifelse(S<x, exp(log_alpha)*S, exp(log_alpha)*x)), sd=exp(log_sd)),
								 start=list(log_alpha=log(median(R/S)), log_sd=0), data=data.frame(S, R, x), method = "Nelder-Mead")
			LLB[i]=logLik(fit)
		}
		if(!AIC) return(Blim[which.min(-LLB)])
		if(AIC) return(2*2 + 2*min(-LLB))
	}

	if(type==2.1) {
		dat=data.frame(S=S, R=R)
		mod=fitSRCurve(S, R, shape="contHockey", g=g)
		if(!AIC) return(unname(exp(mod$env$last.par[c('log_delta')])))
		if(AIC) return(2*2 + 2*mod$fn())
	}

	if(type==5) { return(min(S)) }

}

##' Calculate biomass reference points (BRPs) in ways based on maximum recruitment (Myers et al. 1994)
##' @param S vector of spawning stock biomasses
##' @param R  vector of recruitment values
##' @param perc percent of maximum recruitment (based on S-R curve fit)
##' @param shape "Ricker" or "BevertonHolt" shape of S-R curve to fit
##' @param by the precision needed for a grid search along the S-R curve.
##' @references
##'\itemize{
##' \item R. A. Myers, A. A. Rosenberg, P. M. Mace, N. Barrowman, V. R. Restrepo, In search of thresholds for recruitment overfishing, ICES Journal of Marine Science, Volume 51, Issue 2, 1994, Pages 191-205, https://doi.org/10.1006/jmsc.1994.1020
##' }
##' @export
calcBRP=function(S, R, perc=50, shape="Ricker", by=NULL, maxS=NULL) {
	mod=fitSRCurve(S, R, shape)

	a=exp(mod$env$last.par['log_a'])
	b=exp(mod$env$last.par['log_b'])

	if(is.null(maxS)) maxS=max(S)
	if(is.null(by)) by=(maxS-0)/99

	Srange=seq(0, maxS, by=by)
	Rfit=simR(S=Srange,
						shape=shape,
						pars=c(a, b))

	maxR=unname(switch(shape,
							"Ricker"=a/(b*exp(1)),
							"BevertonHolt"=a/b,
							"contHockey"= 2*a*b,
							"hockey"= b))
	Rtarget=maxR*perc/100
	Starget=Srange[which.min(abs(Rtarget-Rfit))]
	return(c(S=Starget, R=Rtarget))

}

##' Given parameters a and b to define a curve, and a target value of S, find the percentage of maxR corresponding to that S value.
##' @param shape can be "hockey", "Ricker", or "Beverton-Holt"
##' @param by the precision needed for a grid search for target. If missing, 100 points from 0 to maxS are tried.
##' @details
##' \itemize{
##' \item "hockey" shape must have pars=c(a,b) where (a,b) is the inflection point
##' \item "contHockey" shape must have pars=c(beta, S*) following the notation of Mesnil & Rochet 2010 (eqn 4)
##' \item "Ricker shape" must have pars=c(a,b) where expected recruitment is a*S*exp(-b*S)
##' \item "BevertonHolt" shape must have pars=c(a,b) where expected recruitment is a*S/(1+b*S)
##' \item tail is a named list containing a probability ("prob") of observing a bonanza year and a multiplier ("mult") to indicate how a bonanza compares to the expected mean. e.g. tail=list(prob=0.2, mult=1.5)
##' }
##' @export
calcPerc=	function(a, b, Starget, shape="Ricker", by=NULL, maxS=10000) {

	if(is.null(by)) by=(maxS-0)/99

	Srange=seq(0, maxS, by=by)
	Rfit=simR(S=Srange,
					shape=shape,
					pars=c(a, b))

	maxR=unname(switch(shape,
									 "Ricker"=a/(b*exp(1)),
									 "BevertonHolt"=a/b,
									 "contHockey"= 2*a*b,
									 "hockey"= b))


	Rtarget=Rfit[which.min(abs(Starget-Srange))]

	perc=Rtarget/maxR*100
	return(perc)

}


##' Bootstrap the breakpoint of a hockey-stick stock recruitment curve
##' @param S vector of spawning stock biomasses
##' @param R  vector of recruitment values
##' @param FUN function to be applied to summarize Blim across nsim simulations. The default is the CV.
##' @param ... arguments to pass to calcBlim function
##' @export
bootBlim=function(S, R, nsim=100, FUN = function(x){sqrt(var(x))/mean(x)}, type = 2, ...){
	if(length(S)!=length(R)) stop("Lengths of S and R must match")
	take=replicate(nsim, sample(length(S), size=length(S), replace=TRUE))
	Blims=apply(take, 2, function(x){ calcBlim(S[x], R[x], type=type, ...)})
	FUN(Blims)
}
