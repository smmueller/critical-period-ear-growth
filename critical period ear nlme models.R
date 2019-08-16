# Non-Linear Mixed Effects Model Analysis for 
# Mueller et al. 2019 
# The role of the exponential and linear phases of maize (Zea mays L.) ear growth for determination of kernel number and kernel weight. 

library(nlme)

# Read in the data
# all fixed and random effects need to be specified as factors
df <-read.csv('Critical Period Ear Sampling Data.csv',
              colClass=c(rep('factor', 6), rep('numeric', 4), 'factor'))

# Define expolinear model 
# Goudriaan and Monteith, 1990 
# A mathematical function for crop growth based on light interception and leaf area expansion
xp_lin<-function(cmx,rmx,tb,tx){
  wt <- (cmx/rmx) * log( 1 + exp( rmx * (tx - tb) ) )
  return(wt)
}

# the following example tests the variable ear dry weight
# Pinero and Bates 2000
# Mixed-effects models in S and S-PLUS (statistics and computing)

# first, perform nonlinear least squares (nls) to use as starting parameter estimates in non-linear mixed effect model (nlme)
m.nls0 <- nls(data=df,EarDw ~ xp_lin(cmx,rmx,tb,GDD),start=c(cmx =0.2, rmx = 0.03, tb=250))
summary(m.nls0)

# second, fit nlme
# Testing with no hybrid*N treatment interaction
no_interaction <- nlme(EarDw ~ xp_lin(cmx,rmx,tb,GDD),
            data = df,
            fixed = list(cmx ~ Year + Nlevel + YOR,
                         rmx ~ Year + Nlevel + YOR,
                         tb  ~ Year + Nlevel + YOR ),
            random = list(yr_block=pdDiag(cmx + rmx+ tb ~ 1)),
            start = c(coef(m.nls0)[1],rep(0,(1+ 4 + 6 )),
                      coef(m.nls0)[2],rep(0,(1+ 4 + 6 )),
                      coef(m.nls0)[3],rep(0,(1+ 4 + 6 ))),
            na.action = na.omit, weights=varPower(),
            verbose = F)  
summary(no_interaction)
anova(no_interaction)

# Testing with no hybrid*N treatment interaction
interaction <- nlme(EarDw ~ xp_lin(cmx,rmx,tb,GDD),
            data = df,
            fixed = list(cmx ~ Year + Nlevel + YOR + Nlevel:YOR,
                         rmx ~ Year + Nlevel + YOR,
                         tb ~ Year + Nlevel + YOR + Nlevel:YOR),
            random = list(yr_block=pdDiag(cmx + rmx+ tb ~ 1)),
            start = c(coef(m.nls0)[1],rep(0,(1+ 4 + 6 + 4*6)),
                      coef(m.nls0)[2],rep(0,(1+ 4 + 6 )),
                      coef(m.nls0)[3],rep(0,(1+ 4 + 6 + 4*6))),
            na.action = na.omit,
            verbose = F)         
summary(interaction)
anova(interaction)
