params <-
list(EVAL = TRUE)

## ----load_lib,message=FALSE---------------------------------------------------
library(StockRecruitSET)
library(TMB)
knitr::opts_chunk$set(warning = FALSE)
set.seed(1111)

## ----chsdat-------------------------------------------------------------------
S = simS(n=100, pars=c(60,200))
g=1 #assume this for 
R = simR(S=S, shape="contHockey", pars=c(.5, 100, g), varlog=.1)
plot(S,R)

## ----chsfit, message=FALSE----------------------------------------------------
mod=fitSRCurve(S, R, shape="contHockey", g=g)
sdr=sdreport(mod)
summary(sdr)

## ----chblim-------------------------------------------------------------------
calcBlim(S, R) #hockey-stick breakpoint

#If the stock was spasmodic, we might use the
# average  of lowest 3 SSBs that give large (above median) recruitment
calcBlim(S, R, type=1, quant=0.5, nmin=3)  

## ----chsplot------------------------------------------------------------------
Srange=seq(min(S), max(S), length=100)
Rfit=simR(S=Srange,
		shape="contHockey", 
		pars=c(exp(mod$env$last.par[c('log_beta','log_delta')]), g)
)
plot(S, R)
lines(Srange, Rfit, col=2, lwd=3)
abline(v = calcBlim(S, R), lty=2)

## ----rdat---------------------------------------------------------------------
S = simS(n=100, pars=c(1,200))
R = simR(S=S, shape="Ricker", pars=c(exp(1), 1/100), varlog=0.1)
plot(S,R)

## ----rfit, message=FALSE------------------------------------------------------
mod=fitSRCurve(S, R, shape="Ricker")
sdr=sdreport(mod)
summary(sdr)

## ----rplot--------------------------------------------------------------------
Srange=seq(min(S), max(S), length=100)
Rfit=simR(S=Srange,
		shape="Ricker", 
		pars=c(exp(mod$env$last.par[c('log_a','log_b')]))
)
plot(S, R)
lines(Srange, Rfit, col=2, lwd=3)

## ----bhdat--------------------------------------------------------------------
S = simS(n=100, pars=c(1,200))
R = simR(S=S, shape="BevertonHolt", pars=c(1.5456836,  0.0108868), varlog=0.1)
plot(S,R)

## ----bhfit, message=FALSE-----------------------------------------------------
mod=fitSRCurve(S, R, shape="BevertonHolt")
sdr=sdreport(mod)
summary(sdr)

## ----bhplot-------------------------------------------------------------------
Srange=seq(min(S), max(S), length=100)
Rfit=simR(S=Srange,
		shape="BevertonHolt", 
		pars=c(exp(mod$env$last.par[c('log_a','log_b')]))
)
plot(S, R)
lines(Srange, Rfit, col=2, lwd=3)

