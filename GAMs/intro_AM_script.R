# load required packages  
require(mgcv)
require(visreg)
require(gratia) # a useful but slow package for visualising GAMs
require(plyr)
require(dplyr)
require(ggplot2)

# let's make some dummy data
set.seed(1)
n <- 400

t <- sort(runif(n, 0,n))

# autocorrelated stationary errors, see
# https://stats.stackexchange.com/questions/29239/creating-auto-correlated-random-values-in-r
ctrl <- cos(t/5) + exp(t/200) + 5 +
  stats::filter(rnorm(n,0,.1), filter=rep(1,3), circular=TRUE)
trt1 <- cos(t/5) + exp(t/200) + 20 +
  stats::filter(rnorm(n,0,.1), filter=rep(1,3), circular=TRUE)
trt2 <- cos(t/5) + exp(t/400) + 5 +
  stats::filter(rnorm(n,0,.1), filter=rep(1,3), circular=TRUE)


dat <- data.frame(t = rep(t,3),
                  Treat = rep(c('ctrl','trt1','trt2'), each=n),
                  resp = c(ctrl,trt1,trt2) )

dat %>%
  ggplot( aes(x=t, y=resp, Group=Treat, col=Treat) ) +
  geom_line() +
  geom_point() +
  geom_smooth(method="gam") +
  theme_classic()

# smooth model of just the control - REML
mod1_gcv <- gam( resp ~ s(t), 
                 data=filter(dat, Treat=='ctrl'), 
                 method='REML' )
summary(mod1_gcv)

visreg(mod1_gcv)
gam.check(mod1_gcv)

# smooth model of just the control - cross validation (default)
mod2_gcv <- gam( resp ~ s(t, bs='ad'), 
                 data=filter(dat, Treat=='ctrl'), 
                 method='GCV.Cp' )
summary(mod2_gcv)
par(mfrow=c(3,2))
plot(mod2_gcv,shade=TRUE, rug=TRUE, all.terms=TRUE)
gam.check(mod2_gcv)

# specifying by-interactions without a centering term (group intercepts)
mod3_bad <- gam( resp ~ 
                   s(t, by=Treat, bs='ad'), 
                 data=dat )
mod3 <- gam( resp ~ 
               Treat +
               s(t, by=Treat, bs='ad'), 
             data=dat )

par(mfrow=c(2,2))
plot(mod3_bad, rug=FALSE, shade=TRUE, ask=FALSE, all.terms=TRUE, seWithMean=TRUE)
par(mfrow=c(2,2))
plot(mod3, rug=FALSE, shade=TRUE, ask=FALSE, all.terms=TRUE, seWithMean=TRUE)

# using by-interaction with an ordered factor
dat_ord <- dat %>%
  mutate(Treat=factor(Treat, ordered=TRUE, levels=c('ctrl','trt1','trt2')))

mod4 <- gam( resp ~ 
               Treat +
               s(t, bs='ad') + 
               s(t, by=Treat, bs='ad', m=1),
             method='REML',
             data=dat_ord )

summary(mod4)
par(mfrow=c(2,2))
plot(mod4, rug=FALSE, shade=TRUE, ask=FALSE, all.terms=TRUE, seWithMean=TRUE)

# RANDOM EFFECTS specficiations
set.seed(2)
n <- 200

t <- sort(runif(n, 0,n))
nind <- 20

# the autocorrelated time series error in function format for convenience
tserr <- function(n)
  stats::filter(rnorm(n,0,0.01), filter=rep(1,3), circular=TRUE)

# generate similar series to above, but with random variation
func <- function(t, int=rnorm(1,5,2), coef1=rnorm(1,5,2), coef2=rnorm(1,400,10))
{
  o <- coef1*cos(t/25) + exp(t/coef2) + int + unlist(tserr(n))
  attributes(o) <- NULL
  o
}

# create data frame of observations with random INTERCEPTS
dat_re_int <- 
  ldply( 1:nind, function(i) 
    data.frame(Ind=as.character(rep(i,n)), t=t, resp=func(t, coef1=2,coef2=400)) )

dat_re_int %>%
  ggplot( aes(x=t, y=resp, Group=Ind, col=Ind) ) +
  geom_line() +
  theme_classic() +
  theme(legend.position='none')

mod5 <- gam( resp ~ 
               s(t) + 
               s(Ind,bs='re'),
             method='REML',
             data=dat_re_int )

summary(mod5)
par(mfrow=c(1,2))
plot(mod5, rug=FALSE, shade=TRUE, ask=FALSE, all.terms=TRUE, seWithMean=TRUE)

# Random slopes
dat_re_slp <- 
  ldply( 1:nind, function(i) 
    data.frame(Ind=as.character(rep(i,n)), t=t, resp=func(t)) )

dat_re_slp %>%
  ggplot( aes(x=t, y=resp, Group=Ind, col=Ind) ) +
  geom_line() +
  theme_classic() +
  theme(legend.position='none')

mod6 <- gam( resp ~ 
               s(t) + 
               s(Ind,bs='re') +
               s(t,by=Ind,bs='re'),
             method='ML',
             data=dat_re_slp )

summary(mod6)
par(mfrow=c(1,2))
plot(mod6, rug=FALSE, shade=TRUE, ask=FALSE, all.terms=TRUE, seWithMean=TRUE)

# alternative with factor-smooths
# reduce number of individuals to speed convergence
nind <- 6
dat_re_slp <- 
  ldply( 1:nind, function(i) 
    data.frame(Ind=as.character(rep(i,n)), t=t, resp=func(t)) )

mod7 <- gam( resp ~ 
               s(t) + 
               s(t,Ind,bs='fs'),
             method='REML',
             data=dat_re_slp )

summary(mod7)
par(mfrow=c(1,2))
plot(mod7, rug=FALSE, shade=TRUE, ask=FALSE, all.terms=TRUE, seWithMean=TRUE)
