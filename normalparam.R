# summary statistics of sample
n    <- 30
ybar <- 15
s2   <- 3

# sample from the joint posterior (mu, tau | data)
mu      <- rep(NA, 11000)
tau <- rep(NA, 11000)
T       <- 1000    # burnin
tau[1] <-  1
## tau[1]  <- 1  # initialisation
for(i in 2:11000) {   
    mu[i]  <- rnorm(n = 1, mean = ybar, sd = sqrt(1 / (n * tau[i - 1])))
##    sigmasq[i] <- sigmasq[i-1] * rchisq(n = 1, n-1) / (n - 1)
    tau[i] <- rgamma(n = 1, shape = n / 2, scale = 2 / ((n - 1) * s2 + n * (mu[i] - ybar)^2))
}
mu  <- mu[-(1:T)]   # remove burnin
tau <- tau[-(1:T)] # remove burnin

par(mfrow=c(1,2))
hist(mu)
hist(tau)
par(mfrow=c(1,1))


## NAME:  gibbsSampler.R
##
## USAGE: within R, at interactive prompt
##        source("gibbsSampler .R")
## REQUIRED ARGUMENTS: none
## OPTIONS: none
## DESCRIPTION: Suppose Y∼N(mean=\mu,Var=1/\tau)
## Based on a sample, obtain the posterior distributions of \mu
## and \tau using the Gibbs sampler.
## DIAGNOSTICS: None
## CONFIGURATION AND ENVIRONMENT: Base R
## DEPENDENCIES: Base R
## INCOMPATIBILITIES: none known
## PROVENANCE: lightly adapted from https://stats.stackexchange.com/questions/266665/gibbs-sampler-examples-in-r
## BUGS AND LIMITATIONS: Just a very minimal example
## FEATURES AND POTENTIAL IMPROVEMENTS:
## AUTHOR:  adapted by Steve Dunbar
## VERSION: Version 1.0 as of Thu 26 Mar 2020 07:45:57 AM CDT

## KEYWORDS:

