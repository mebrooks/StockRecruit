params <-
list(EVAL = TRUE)

## ----load_lib,echo=FALSE-------------------------------------------------
library(StockRecruitSET)
library(TMB)
knitr::opts_chunk$set(eval = if (isTRUE(exists("params"))) params$EVAL else FALSE)

## ----chsdat--------------------------------------------------------------
S = simS(n=100, pars=c(60,200))
g=1 #assume this for 
R = simR(S=S, shape="contHockey", pars=c(1, 100, g), varlog=.1)
plot(S,R)

## ----chsfit, message=FALSE-----------------------------------------------
mod=fitSRCurve(S, R, shape="contHockey", g=g)
sdr=sdreport(mod)
summary(sdr)

## ----chsplot-------------------------------------------------------------
Srange=seq(min(S), max(S), length=100)
Rfit=simR(S=Srange,
		shape="contHockey", 
		pars=c(mod$env$last.par[c('beta','delta')], g)
)
plot(S, R)
lines(Srange, Rfit, col=2, lwd=3)

## ----rdat----------------------------------------------------------------
S = simS(n=100, pars=c(1,200))
R = simR(S=S, shape="Ricker", pars=c(exp(1), 1/100), varlog=0.1)
plot(S,R)

## ----rfit, message=FALSE-------------------------------------------------
mod=fitSRCurve(S, R, shape="Ricker")
sdr=sdreport(mod)
summary(sdr)

## ----rplot---------------------------------------------------------------
Srange=seq(min(S), max(S), length=100)
Rfit=simR(S=Srange,
		shape="Ricker", 
		pars=c(mod$env$last.par[c('a','b')])
)
plot(S, R)
lines(Srange, Rfit, col=2, lwd=3)

## ----bhdat---------------------------------------------------------------
S = simS(n=100, pars=c(1,200))
R = simR(S=S, shape="BevertonHolt", pars=c(1.5456836,  0.0108868), varlog=0.1)
plot(S,R)

## ----bhfit, message=FALSE------------------------------------------------
mod=fitSRCurve(S, R, shape="BevertonHolt")
sdr=sdreport(mod)
summary(sdr)

## ----bhplot--------------------------------------------------------------
Srange=seq(min(S), max(S), length=100)
Rfit=simR(S=Srange,
		shape="BevertonHolt", 
		pars=c(mod$env$last.par[c('a','b')])
)
plot(S, R)
lines(Srange, Rfit, col=2, lwd=3)

