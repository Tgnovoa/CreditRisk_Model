## Construction of the expected default spread as a function of bond price

#### libraries ----
library(tidyverse)
library(magrittr)
library(stringr)
library(xts)

#### input variables : ----
# Y = yield spreads
# V = the value of the assets
# B = the value of the debt
# F = the promised debt payment
# r = treasury yield rates (and curve)
# sigma_e = equity volatility (option-implied volatility /or/ historical returns data)

#### estimate AAA-rated bond spreads ----
# Construct time matched avg. AAA-rated bond spreads and subtract them from the bond spreads mentioned before

#### variable transformation ----
## w ~ the fims levarage
#     w = B/V 
## s ~ the promised yield spread
#     s = 1/T ln(F/B) - r
## d1 = (-ln(w) - (s - (sigma**2) / 2)*T)/sigma * sqrt(T)
## d2 = d1 - sigma*sqrt(T)

#### calibrate merton model ----
## solve each one with 5 different values for T to obtain sigma values
# pnorm(-d1)/w + exp(s*T)*pnorm(d2) = 1
# sigma_e = sigma*pnorm(d1)/(1-w)
## use the average of the results (T,sigma) as a seed and solve simultaneously : 
# pnorm(-d1)/w + exp(s*T)*pnorm(d2) = 1
# sigma_e = sigma*pnorm(d1)/(1-w)

#### obtain expected default spread and default risk premium -----
## rPi ~ risk premia on assets 
#     rPi = Mu - r
## rPi_e ~ risk premia on equity
#     rPi_e = rPi*sigma_e/sigma
## delta ~ expected default spread
#     delta = -1/T*ln((exp(rPi-s)*T)*pnorm(-d1-rPi*sqrt(T)/sigma)/w + pnorm(d2 + rPi*sqrt(T/sigma)))
## gamma ~ default risk premium
#     gamma = s - delta

