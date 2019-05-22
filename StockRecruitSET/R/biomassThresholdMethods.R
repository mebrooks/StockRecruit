##' Calculate Blim in ways described by ICES Advice Technical Guidelines Table 12.4.3.1.3
##' @param S vector of spawning stock biomasses
##' @param R  vector of recruitment values
##' @param quant quantile above which recruitment is considered to be "large" in a "spasmodic stock" (language from ICES Advice Technical Guidelines)
##' @param type way of calculating Blim.
##' 1 is the minimum S that gives good recruitment.
##' 2 is the estimated S for a breakpoint in a hockey stick model.
##' 2.1 is estimated S for inflection point in a hyperbolic hockey-stick model.
##' 5 is the minimum observed S (Blim=Bloss).
##' @param g is the assumed smoothing parameter in the hyperbolic hockey-stick model.
##' @param by the precision needed for a grid search for breakpoint in a hockey-stick model.
##' @importFrom bbmle mle2
##' @importFrom bbmle coef
##' @importFrom bbmle logLik
##' @export
calcBlim = function(S, R, quant=0.75, type=2.1, g=1, by=1)
{
	if(length(S)!=length(R)) stop("Lengths of S and R must match")

	if(type==1)	{ return(min(S[which(R>=quantile(R, quant))])) }

	if(type==2) {
		Blim=seq(min(S), max(S), by=by)
		LLB=Blim
		LLB[]=NA

		for(i in 1:length(Blim)) {
			x=Blim[i]
			fit = mle2(log(R)~ dnorm(log(ifelse(S<x, alpha*S, alpha*x)), sd=exp(log_sd)),
								 start=list(alpha=10, log_sd=10), data=data.frame(S, R, x), method = "Nelder-Mead")
			LLB[i]=logLik(fit)
		}
		return(Blim[which.min(-LLB)])
	}

	if(type==2.1) {
		dat=data.frame(S=S, R=R)
		mod=fitSRCurve(S, R, shape="contHockey", g=g)
		k=mod$env$last.par[c('delta')]
		return(unname(k))
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
calcBRP=function(S, R, perc=50, shape="Ricker", by=1, maxS=500) {
	mod=fitSRCurve(S, R, shape)

	a=mod$env$last.par['a']
	b=mod$env$last.par['b']

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

