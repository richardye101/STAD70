###
### Q4
### 
library(zoo)
library(tseries)
tickers = c("DVEM", "EXT", "HYEM", "LTPZ", "SCHP", 
            "EDV",  "SPMB", "TLT", "GRI", "GOVT")
S=list()
for(i in 1:length(tickers)){
  S[[i]] = get.hist.quote(tickers[i], start='2018-01-01', end='2019-12-31', quote='AdjClose', drop = TRUE)  
}

# (a) 
r = lapply(S, FUN=function(x){diff(log(x))}) 
R = lapply(r, FUN=function(x){exp(x)-1}) 

plot(S[[1]])
plot(R[[1]])

# (b) 
Rmat = simplify2array(R)
fmod = factanal( Rmat, factors = 2,  lower = 0.005)
b = fmod$loadings
v = fmod$uniquenesses

# (c)
MU = sapply(R, mean)
SD = sapply(R, sd)
VC = ( b%*%t(b) + diag(v) ) * (SD %*% t(SD)) 

library(mvtnorm)
Rsim = mvtnorm::rmvnorm(250, MU, VC)

R_eqwt = rowMeans( exp( apply(Rsim,2,cumsum) ) - 1 )
plot(R_eqwt, type = "l");

###
### Q5
###
S0 = K = 100; T1 = 1; T2 = 2; r = .05; v = .2

# (a)
library(fExoticOptions)
SimpleChooserOption(S = S0, X = K, time1 = 1, Time2 = 2, 
                    r = r, b = r, sigma = v)@price

# (b)
set.seed(123); 
n = 10000; dT = T2-T1
Z1 = rnorm(10000); Z2 = rnorm(10000)
S1 = S0*exp( (r-v^2/2)*T1 + v*sqrt(T1)*Z1 )
S2 = S1*exp( (r-v^2/2)*dT + v*sqrt(dT)*Z2 )
payoff.b = exp(-r*T2) * ifelse( S1 > K * exp(-r*dT), 
                              pmax( S2 - K, 0),
                              pmax( K - S2, 0) )
mean(payoff.b) 
sd(payoff.b)/sqrt(N)

# (c)
library(fOptions)
payoff.c = exp(-r*T1) * ifelse( S1 > K * exp(-r*dT), 
                                GBSOption( "c", S1, K, dT, r, r, v )@price,
                                GBSOption( "p", S1, K, dT, r, r, v )@price )
mean(payoff.c) 
sd(payoff.c)/sqrt(N)

# (d)
m = 20; Dt = T2/m
Z = matrix( rnorm( n * m ), n, m)
S = matrix( S0, n, m + 1 )
for(i in 1:m){
  S[,i+1] = S[,i]  + r * S[,i] * Dt + v * log(S[,i]) * sqrt(Dt) * Z[,i] 
}
payoff.d = exp(-r*T2) * ifelse( S[, m/2 + 1] > K * exp(-r*dT),
                                pmax( S[, m + 1] - K, 0),
                                pmax( K - S[, m + 1], 0) )
mean(payoff.d)
sd(payoff.d) / sqrt(n)
