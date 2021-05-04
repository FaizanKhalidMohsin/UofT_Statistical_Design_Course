###
### Simulation involves writing code to generate data under an
### assumed model and perform a particular analysis of it.
###
### The basic approach is to write a function that generates the data
### and performs the analysis for a single simulation. This function
### can then be run many times to generate many simulated results.

###
### Example 1: Comparison of two means
###

###
### First, determine sample size for a difference of 0.5
### (assuming means of 1 and 1.5 - relevant for non-normal cases)
### and sd = 2, with 80% power and 2-sided alpha = 0.05
###

power.t.test(delta=0.5,sd=2,power=0.8)

###
### extract n per group for later
###

print(n1 <- ceiling(power.t.test(delta=0.5,sd=2,power=0.8)[["n"]]))

###
### Simulate the t-test
###

### Make the required function first

sim.t.test.norm <- function(n1,m1,m2,s1,n2=n1,s2=s1) {
    g1 <- rnorm(n1,m1,s1)
    g2 <- rnorm(n2,m2,s2)
    t.test(g1,g2)[["p.value"]]
}

set.seed(2133)

sim.t.test.norm(n1,1,1.5,2) # test it

### Run 10,000 simulations

sim1 <- replicate(10000,sim.t.test.norm(n1,1,1.5,2))
mean(sim1<0.05) # compute power


sim1 <- replicate(10000,sim.t.test.norm(n1,1,1.5,2))
mean(sim1<0.05) # compute power
#sim.t.test.norm(n1,1,1.5,2)

### Simulate under the null

mean(replicate(10000,sim.t.test.norm(n1,1,1,2))<0.05)

### change the MCID

mean(replicate(10000,sim.t.test.norm(n1,1,2,2))<0.05) # delta = 1

mean(replicate(10000,sim.t.test.norm(n1,1,1.25,2))<0.05) # delta = 0.25

### change the sd

mean(replicate(10000,sim.t.test.norm(n1,1,1.5,2.5))<0.05) # sd = 2.5

### Change the distribution - need new function

sim.t.test.exp <- function(n1,m1,m2,n2=n1) {
    g1 <- rexp(n1,1/m1)
    g2 <- rexp(n2,1/m2)
    t.test(g1,g2)[["p.value"]]
}

mean(replicate(10000,sim.t.test.exp(n1,1,1.5))<0.05) # original H1

mean(replicate(10000,sim.t.test.exp(n1,1,1))<0.05) # null

###
### Example 2: Comparison of 2 proportions
###

### Need sample size to detect difference between 0.2 and 0.4

power.prop.test(p1=0.2,p2=0.4,power=0.8)

print(n2 <- ceiling(power.prop.test(p1=0.2,p2=0.4,power=0.8)[["n"]]))

### make the required function

sim.chisq.test <- function(p1,p2,n1,n2=n1,correct=FALSE) {
    x1 <- rbinom(1,n1,p1)
    x2 <- rbinom(1,n2,p2)
    tbl <- cbind(c(x1,n1-x1),c(x2,n2-x2))
    chisq.test(tbl,correct=correct)[["p.value"]] #continuity correction off.
}

set.seed(3651)

sim.chisq.test(0.2,0.4,n2) # test it

### Run 10,000 sims

mean(replicate(10000, sim.chisq.test(0.2,0.4,n2))<0.05)

mean(replicate(10000, sim.chisq.test(0.4,0.4,n2))<0.05) # under null

mean(replicate(10000, sim.chisq.test(0.2,0.4,n2,correct=TRUE))<0.05) # use CC

mean(replicate(10000, sim.chisq.test(0.4,0.4,n2,correct=TRUE))<0.05) # under null, CC

###
### Example 3: Linear Regression
###
### For sample size calculations you would likely need the slope to detect,
### the residual standard error and the standard deviation of X.
###
### Additionally, to perform a simulation you will need to specify the
### means of X and Y (or do you?).
###
### This gives enough to specify parameters for the simulation. The usual
### RHS is beta0 + beta1 * x where beta0 is the y-intercept. We can determine
### this from the supplied parameters by noting that if you centre X you
### get a RHS of alpha + beta1 * (x - xbar) where alpha will equal ybar.
### This means that beta0 = ybar - beta1 * xbar.
###
### Alternatively, you could simulate the model with X centred since it
### does not affect inference on beta1 and for multiple regression models,
### working with centred X variables is probably easier.
###
### For this simulation let's go with the following settings:
###
### n = 200
### beta1 = 10
### sigma = 100 (residual standard error or sqrt(MSE))
### sigmaX = 2 (standard deviation of X)
### xbar = 20
### ybar = 700
###
### These are the parameters for the simulation function.
###

sim.regr <- function(n, beta1, sigma, sigmaX, xbar, ybar) {
    beta0 <- ybar - beta1 * xbar
    ei <- rnorm(n,0,sigma) # simulate error
    x <- rnorm(n,xbar,sigmaX) # simulate x, could choose other distr
    y <- as.vector(model.matrix(~x) %*% rbind(beta0,beta1)) + ei
    fit <- lm(y~x)
    pval <- summary(fit)[["coefficients"]][2,4]
    pval
}
#Q what is model.matrix(~X)
set.seed(164)
sim.regr(200, 10, 100, 2, 20, 700) # test it

### Run 10,000 simulations

mean(replicate(10000, sim.regr(200, 10, 100, 2, 20, 700))<0.05)

### What happens if you increase the variability of X?

mean(replicate(10000, sim.regr(200, 10, 100, 4, 20, 700))<0.05)

### Do the means of X and Y matter?

mean(replicate(10000, sim.regr(200, 10, 100, 2, 0, 0))<0.05)

###
### Example 4: Logistic Regression
###
### Use a single binary covariate X.
###
### The linear part is beta0 + beta1*x
###
### In logistic regression beta1 = log(OR_X) and
###   beta0 = log odds of outcome when X=0.
###
### In the function we will specify a total sample size (n),
### the probability that X = 1 (px), the OR (theta) and
### Pr(Y=1|X=0) (py)
###

sim.logistic <- function(n,px,theta,py) {
    b0 <- log(py/(1-py))
    b1 <- log(theta)
    x <- rbinom(n,1,px)
    XB <- as.vector(model.matrix(~x) %*% rbind(b0,b1))
    pY <- 1/(1 + exp(-XB))
    y <- sapply(pY, function(x) rbinom(1,1,x))
    fit <- glm(y~x,family=binomial)
    pval <- summary(fit)[["coefficients"]][2,4]
    pval
}

### n = 310
### px = 0.5
### theta = 2
### py = 0.25

set.seed(1237)
sim.logistic(310,0.5,2,0.25) # test it

### Run 10,000 simulations

mean(replicate(10000, sim.logistic(310,0.5,2,0.25))<0.05)

### Suppose X only occurs in 10% (px = 0.1)

mean(replicate(10000, sim.logistic(310,0.1,2,0.25))<0.05)

# Q

# Rules of stratification:1. if very large n does not matter. 
#                         2. Varaible should be very strongly related to the outcome.
#                         3. Do not go overboard with startification.

###
### Example 5: Survival Analysis
###
### Sample size estimation and simulation for survival data are a bit
### more complicated. We generally make a simplifying assumption that
### survival time follows an exponential distribution with the rate
### parameter equal to the hazard. This gives rise to the survivor function
###
### P(T >= t) = S(t) = exp(-rho * t)
###
### Since the treatment effect is typically expressed as a hazard ratio,
### it follows that we will want to know the "baseline" hazard. The
### hazard is often not reported but can be estimated from any given time
### on a Kaplan-Meier curve by solving the above equation for rho.
###
### For example, if 1 yr cumulative mortality is 15%, we have
### S(1) = 0.85 ==> rho = -log(S(t))/t = 0.1625
###
### If we wish to detect a HR = 0.75, that means rho in the experimental
### group is 0.75 * 0.1625 = 0.1219.
###
### The other information that is needed is related to speed of accrual
### and follow-up. One way to describe this is in terms of length of
### recruitment time and minimum duration of follow (i.e. how long after
### the last patient is entered do you plan to stop follow-up).
###
### For the sake of the example, let's suppose recruitment lasts 2 years
### and minimum follow-up is 3 years (implies max follow-up is 5 years).
###
### I use the package gsDesign for sample size calculations. We will also
### need the package survival for the simulations.
###

library(gsDesign)
library(survival)

print(rho0 <- -log(0.85)/1)
print(hr <- 0.75)

### sample size - default gives total sample size

nSurv(lambdaC=rho0, hr=hr, R=2, T=5, minfup=3, alpha=0.05, beta=0.2, sided=2)
# IS there one function for time dependent covariates. 
# Larger hazard, need fewer people, as will have more events in shorter time,
# Need fewer people to have more events. 

# Q is it possible to get p-value ss because sample size is so large.


print(n <- nSurv(lambdaC=rho0, hr=hr, R=2, T=5, minfup=3, alpha=0.05, beta=0.2, sided=2)[["n"]])
print(n5 <- ceiling(n/2)) # per group

sim.exp.surv <- function(n0,rho0,hr,from,to,n1=n0) {
    rho1 <- hr * rho0
    stime0 <- rexp(n0,rho0)
    cens0 <- runif(n0,from,to)
    status0 <- ifelse(stime0<cens0,1,0)
    stime0 <- ifelse(stime0<cens0,stime0,cens0)
    stime1 <- rexp(n1,rho1)
    cens1 <- runif(n1,from,to)
    status1 <- ifelse(stime1<cens1,1,0)
    stime1 <- ifelse(stime1<cens1,stime1,cens1)
    stime <- c(stime0,stime1)
    status <- c(status0,status1)
    group <- c(rep(0,n0),rep(1,n1))
    Chi <- survdiff(Surv(stime,status)~group)[["chisq"]]
    pchisq(Chi,1,lower.tail=FALSE)
}

set.seed(9567)
sim.exp.surv(n5,rho0,hr,3,5) # test it

### Run 10,000 simulations

mean(replicate(10000, sim.exp.surv(n5,rho0,hr,3,5))<0.05)

###
### Example 6: Piecewise constant hazard and time-varying HR
###

###
### Start with sample size estimation
###
### Specify intervals and piecewise constant (daily) hazards
###
### Since zero and infinity correspond to the start of the first interval
### and end of third interval, we only need to specify the two intermediate
### values, 10 and 120 here.
###

print(lam0 <- c(-log(1-0.004)/10,-log(1-0.06)/110,-log(1-0.041)/365))
ints1 <- c(10,120)

###
### HR (after lag) = 0.75
###

HR <- c(1,0.75,0.75)
print(lam1 <- HR*lam0)

###
### Variable f-up. Assume 15 months recruitment and 36 month minimum
### follow-up.
###

###
### The function S() evaluates the survival function assuming
### Piecewise constant hazards in 3 intervals.
###

S <- function(x,lam,ints) {
  H <- function(x,lam,ints) {
    ifelse(x >= ints[2],
           lam[1]*ints[1] + lam[2]*(ints[2]-ints[1]) + lam[3]*(x-ints[2]),
           ifelse(x < ints[1], lam[1]*x,
                           lam[1]*ints[1] + lam[2]*(x-ints[1])))
  }
  exp(-H(x,lam,ints))
}

### Estimate "average" hazards and HR (note these are yearly)

print(h0 <- -log(S(4*365.25,lam0,ints1))/4)
print(h1 <- -log(S(4*365.25,lam1,ints1))/4)
print(hr1 <- h1/h0)

### Estimate the sample size

print(sscalc <- nSurv(lambdaC=h0,hr=hr1,sided=2,alpha=0.05,beta=0.1,R=15/12,minfup=36/12,T=(15+36)/12))
print(n6 <- ceiling(sscalc$n/2))

###
### The following function is used to solve for t in S() in order
### to simulate survival times from the distribution.
###

Sroot <- function(x,u,lam,ints) S(x,lam,ints) - u

###
### The basic approach will be to simulate large populations of survival
### times and then sample from these populations to to estimate power for
### various sample sizes.
###
###
### Simulate 1 million survival times for each of the two groups
###

set.seed(3891)
u <- runif(1000000)
Group0t <- sapply(u, function(x) uniroot(Sroot, u=x, lam=lam0, ints=ints1, interval = c(0,1e+7))$root)
u <- runif(1000000)
Group1t <- sapply(u, function(x) uniroot(Sroot, u=x, lam=lam1, ints=ints1, interval = c(0,1e+7))$root)

###
### Function to compute power for a given study with sample size x per group.
###

Power <- function(x,from,to,B=1000,alpha=0.05,lost=0) {
  sim <- function() { # This function Generates one sample (one iteration)
    stime0 <- sample(Group0t,size=x)
    cens <- runif(x,from,to)
    if (lost>0) {
      ctime <- rexp(x,lost)
      cens <- pmin(cens,ctime)
    }
    stat0 <- ifelse(stime0<cens,1,0)
    stime0 <- ifelse(stime0<cens,stime0,cens)
    stime1 <- sample(Group1t,size=x)
    cens <- runif(x,from,to)
    if (lost>0) {
      ctime <- rexp(x,lost)
      cens <- pmin(cens,ctime)
    }
    stat1 <- ifelse(stime1<cens,1,0)
    stime1 <- ifelse(stime1<cens,stime1,cens)
    stime <- c(stime0,stime1)
    status <- c(stat0,stat1)
    rx <- c(rep(0,x),rep(1,x))
    Chi <- survdiff(Surv(stime,status)~rx)$chisq
    pchisq(Chi,1,lower.tail=FALSE)
  }
  mean(replicate(B,sim())<alpha)
}

set.seed(3673)
Power(n6,floor(36*30.4375),ceiling(51*30.4375))

###
### 20% cumulative loss at 4 years computed as daily hazard
###

print(lhaz <- -log(1-0.2)/(4*365.25))

set.seed(6531)
Power(n6,floor(36*30.4375),ceiling(51*30.4375),lost=lhaz)
4