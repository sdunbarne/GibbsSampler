N <- 10000            # Number of iterations
Nb <- 2000; N1 <- N+1  # Burn-in

psi <- numeric(N) 
psi[1] <- .5          # Initial value

alpha.0  <- 1.0
beta.0  <- 1.0
eta <- 0.99
theta <- 0.97
r <- 233
n  <- 1000

for(i in 2:N) {      # Gibbs Sampler Loop
tau<-psi[i-1]*eta+(1-psi[i-1])* (1-theta)        
X <- rbinom(1, r, psi[i-1]* eta/tau)
Y <- rbinom(1, n-r, psi[i-1]*(1-eta)/(1-tau))
psi[i] <- rbeta(1, alpha.0+X+Y, beta.0+n-X-Y)
}

gspsi  <- mean(psi[Nb:N])

par(mfrow=c(2,2))
hist(psi)
plot(1:N,cumsum(psi)/(1:N),type="l",ylab= "psi", ylim=c(0.20,0.24))
plot(psi,type='p',pch='.',ylim=c(0.15,0.30))
acf(psi)


## NAME: spamfilter.R
## USAGE: within R, at interactive prompt
##        source("spamfilter.R")
## REQUIRED ARGUMENTS: none
## OPTIONS: none
## DESCRIPTION: Use Bayesian analysis with Gibbs sampler to estimate
##              prevalence of spam emails from sample
## DIAGNOSTICS: none
## CONFIGURATION AND ENVIRONMENT: Base R
## DEPENDENCIES: none
## INCOMPATIBILITIES: none known
## PROVENANCE: based on
## https://rstudio-pubs-static.s3.amazonaws.com/279858_010f9da7c8d744988019397e3fe51cb2.html
## BUGS AND LIMITATIONS: none known 
## FEATURES AND POTENTIAL IMPROVEMENTS: better reporting

## AUTHOR:  Steve Dunbar
## VERSION: Version 1.0 Fri Feb  5 08:17:12 AM CST 2021
## KEYWORDS: Gibbs sampler, conjugate, Bayesian analysis

