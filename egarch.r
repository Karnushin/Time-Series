library(TSA)
library(rugarch)
library(fBasics)

data = read.csv("PTC_data.csv", header=TRUE, sep=";")

priceclose = data$X.CLOSE.

to_income_func = function(vec) {
  temp = vector()
  for (i in 2:length(vec)) {
      temp[i-1] = vec[i]/vec[i-1] - 1
  }
  
  return(temp)
}

income = to_income_func(priceclose)

spec = ugarchspec(variance.model = list(model ="eGARCH",garchOrder = c(1,1)),
                  mean.model = list(armaOrder = c(1, 1)),distribution.model = "ged")

model_est = ugarchfit(spec=spec, data=income)

model_est

eps = model_est@fit$residuals

plot(eps, type='l', lwd=3, col="blue", main="Residuals")

acf(eps, col='blue', lwd=3)

k1 = kernel('daniell', 5)
k2 = kernel('daniell', c(5, 5, 5))
sp1 = spec(eps, kernel=k1, log='no', sub='', col='blue', lwd=3, plot=FALSE)
sp2 = spec(eps, kernel=k2, log='no', sub='', col='red', lwd=3, plot=FALSE)

plot(sp1$freq, sp1$spec, type='l', xlab='Frequency', ylab='Smoothed specral density', col='blue', lwd=3, main='Smoothed Periodogram')
lines(sp2$freq, sp2$spec, col='red', lwd=3)
legend('topright', c('Daniell window', 'Daniell^3 window'), bty='n',lwd=2, col=c('blue', 'red'))
#per = periodogram(eps, plot=FALSE)
#lines(per$freq, per$spec)
