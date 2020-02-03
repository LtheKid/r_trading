# install.packages("CombinePortfolio")
# Import dependencies
library(CombinePortfolio)

# Lester's work + Akbar's help
# stopped here
# 3 portfolios - tangency, gmv, and naive, adapted with csv input
df<- read.csv("prices.csv", row.names="Date", header=TRUE)
df[] <- lapply(df, function(x) {
    as.numeric(as.character(x))
})
sapply(df, class) #check dtype
df = as.ts(df)
ret<- diff(log(df)) ## sample asset returns
crule<- combination.rule(ret,detailed.output=TRUE)
crule$w["1'",] ## Adjusted Kan-Zhou(2007) 2-fund rule
crule$w["1''2",] ## Adjusted Kan-Zhou(2007) 3-fund rule
crule$w["124",] ## Combination rule: Tangency+GMV+naive 4-fund rule, plug-in estimator
crule$delta["124",] ## Combination weights
output <- crule$V[,c(1,2,4)] ## Combination targets: Tangency, GMV and naive (1/N)

# Look at the time series of price
plot.ts(df)






# TEMPLATES

ret<- diff(log(EuStockMarkets)) ## sample asset returns
crule<- combination.rule(ret,detailed.output=TRUE)
crule$w["1'",] ## Adjusted Kan-Zhou(2007) 2-fund rule
crule$w["1''2",] ## Adjusted Kan-Zhou(2007) 3-fund rule
crule$w["124",] ## Combination rule: Tangency+GMV+naive 4-fund rule, plug-in estimator
crule$delta["124",] ## Combination weights
crule$V[,c(1,2,4)] ## Combination targets: Tangency, GMV and naive (1/N)

diff(log(df))



##setting
ret<- diff(log(EuStockMarkets))
T<- dim(ret)[1]
N<- dim(ret)[2]
gamma<- 1
## Example Tu-Zhou(2011) on Markowitz portfolio
a1<- T/(T-N-2)
rule<- c(1,4) ## as. TZ on Tangency and naive restriction index

HC<- array( c(c(gamma*a1,N ) ) , dim=c(length(rule), 1) )## C^{-1} H conditions...
h0<- c(1)
## plug-in estimator, theta^2-adjusted, psi^2-adjusted:
rcrule<-combination.rule.restriction(ret,rule=rule,HC=HC,h0=h0,gamma=gamma,detailed.output=TRUE)
rcrule
## compare with TZ:
we<- rep.int(1/N, N)
TT<- T
mu<- apply(ret, 2, mean)## exess return
Sigma<- cov(ret) * (TT-1)/TT
Sigma.inv<- solve(Sigma)
sharpe.squared<- as.numeric( tcrossprod(crossprod(mu, Sigma.inv),mu) )
Sigma.inv.unb<- Sigma.inv * (TT-N-2)/TT
w.Markowitz<- 1/gamma * crossprod(Sigma.inv.unb, mu) ##
weSigmawe<- as.numeric( tcrossprod(crossprod(we, Sigma),we) )
wemu<- crossprod(we,mu)
pi1<- as.numeric( weSigmawe - 2/gamma * wemu + 1/gamma^2 *sharpe.squared )
bb<- (TT-2)*(TT-N-2)/( (TT-N-1)*(TT-N-4) ) ##c1 in tu-zhou
pi2<- (bb-1) * sharpe.squared /gamma^2 + bb/gamma^2 * N/TT
pi3<- 0
delta.TZ.Markowitz<- (pi1 - pi3)/(pi1 + pi2 - 2*pi3)
w.TZ.Markowitz<- (1- delta.TZ.Markowitz)* we + delta.TZ.Markowitz * w.Markowitz
w.TZ.Markowitz
rcrule$w["r:14",]
## adjusted Tu-Zhou on Markowitz
ibeta<- function(x,a,b) pbeta(x,a,b) * beta(a,b) ## incomplete beta
sharpe.squared.adj<- ((TT-N-2)*sharpe.squared - N)/TT + 2*(sharpe.squared^(N/2)*
(1+ sharpe.squared)^(-(TT-2)/2))/TT/ibeta(sharpe.squared/(1+sharpe.squared),N/2,(TT-N)/2)
pi1.adj<- as.numeric( weSigmawe - 2/gamma * wemu + 1/gamma^2 *sharpe.squared.adj )
pi2.adj<- (bb-1) * sharpe.squared.adj /gamma^2 + bb/gamma^2 * N/TT
delta.TZ.Markowitz.adj<- (pi1.adj - pi3)/(pi1.adj + pi2.adj - 2*pi3)
w.TZ.Markowitz.adj<- (1- delta.TZ.Markowitz.adj)* we + delta.TZ.Markowitz.adj * w.Markowitz
w.TZ.Markowitz.adj
rcrule$w["r:1'4",]
## Example Tu-Zhou(2011) on Kan-Zhou(2007) 3-fund
cd<- combination.rule(ret, detailed.output=TRUE)[[2]]["1''2",1:2] ## KZ3fund combination weights
rule<- c(1,2,4) ## as. TZ on KZ3fund restriction index
HC<- array( c(c(gamma,0, N*cd[1] ), c(0, gamma, N*cd[2] )) , dim=c(length(rule), 2) )
h0<- c(cd[1]/N, cd[2]/N)
combination.rule.restriction(ret, rule=rule, HC=HC, h0=h0)




# 3 portfolios - tangency, gmv, and naive
ret<- diff(log(EuStockMarkets)) ## sample asset returns
crule<- combination.rule(ret,detailed.output=TRUE)
crule$w["1'",] ## Adjusted Kan-Zhou(2007) 2-fund rule
crule$w["1''2",] ## Adjusted Kan-Zhou(2007) 3-fund rule
crule$w["124",] ## Combination rule: Tangency+GMV+naive 4-fund rule, plug-in estimator
crule$delta["124",] ## Combination weights
crule$V[,c(1,2,4)] ## Combination targets: Tangency, GMV and naive (1/N)


print()


ret<- diff(log(EuStockMarkets)) ## sample asset returns
crule<- combination.rule(ret,detailed.output=TRUE)
crule$w["1'",] ## Adjusted Kan-Zhou(2007) 2-fund rule
crule$w["1''2",] ## Adjusted Kan-Zhou(2007) 3-fund rule
crule$w["124",] ## Combination rule: Tangency+GMV+naive 4-fund rule, plug-in estimator
crule$delta["124",] ## Combination weights
crule$V[,c(1,2,4)] ## Combination targets: Tangency, GMV and naive (1/N)

diff(log(df))

# Lester's work + Akbar's help
# stopped here
# 3 portfolios - tangency, gmv, and naive, adapted with csv input
df<- read.csv("C:\\Users\\P1329091\\Downloads\\wuhanport\\prices.csv", row.names="Date", header=TRUE)
df[] <- lapply(df, function(x) {
    as.numeric(as.character(x))
})
sapply(df, class) #check dtype
df = as.ts(df)
ret<- diff(log(df)) ## sample asset returns
crule<- combination.rule(ret,detailed.output=TRUE)
crule$w["1'",] ## Adjusted Kan-Zhou(2007) 2-fund rule
crule$w["1''2",] ## Adjusted Kan-Zhou(2007) 3-fund rule
crule$w["124",] ## Combination rule: Tangency+GMV+naive 4-fund rule, plug-in estimator
crule$delta["124",] ## Combination weights
crule$V[,c(1,2,4)] ## Combination targets: Tangency, GMV and naive (1/N)









