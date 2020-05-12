library(TSA)

#Считываем данные
PATH = '/Users/Barnett/Desktop/Interventions'
data = read.csv(file.path(PATH, 'gazp_2014.csv'), header = TRUE, sep = ',')

#Возьмем цены закрытия
#priceclose = ts(data$X.CLOSE., start=c(2014, 1), frequency = length(data$X.DATE.))
priceclose = data$X.CLOSE.

#Посмотрим на данные
plot(priceclose, type='l', lwd=3, col="blue", main="Price close")

#Перейдем к логарифмическим доходностям
income = log(exp(diff(log(priceclose))))

#Предполагается, что интервенция произошла в момент времени Т = 3 марта 2014
#Intervention.Time = 2014 + (which(data$X.DATE. == 20140303)-1) / length(data$X.DATE.)
Intervention.Time = 0 + (which(data$X.DATE. == 20140303)-1)

#Построим график доходностей и отметим момент интервенции
plot(income, type='b', lwd=2, col="blue", main="Income")
abline(v = Intervention.Time,col = "red",lwd = 2)

#Подберем ARIMA модель

#дифференцируем
dincome = diff(income, 1)
plot(dincome, type='b', lwd=2, col="blue", main="Income")
#q=1
acf(dincome,lag.max=20,lwd=3,col = "blue",main = "ACF")
#p=2
pacf(dincome,lag.max=20,lwd=3,col = "blue",main = "PACF")

#Учет интервенции 3 марта 2014 в одной точке (интервенция типа импульс)
I1 = 1*((seq(income)==(which(data$X.DATE. == 20140303)-1)))

#Обучение ARIMA c интервенциями 
#arimax позволяет учитывать интервенции
model1=arimax(income,
              order=c(2,1,1),
              xtransf=data.frame(I1,I1),
              transfer=list(c(0,0), c(1,0)),
              method='ML')
#AIC критерий
AIC(model1)

plot(income, type = "b", pch = 20, ylab='Income', 
     main = 'Income and Fitted results. Intervantion deleted', col= "blue",lwd = 2)
lines(fitted(model1), col= "red",lwd =2)
abline(v = Intervention.Time,col = "red",lwd = 1)
legend('topleft', c('Income', 'ARIMAX results'), bty='n', lwd=2, col=c('blue', 'red'))

#Остатки после удаления модели
plot(model1$residuals, type = "b", pch = 20,main = "Residuals", col = "blue",lwd = 2)
abline(v = Intervention.Time, col = "red",lwd = 1)

#Посмотрим на коэффициенты
model1$coef

#Эффект интервенции
u_0 = model1$coef[4]
r_1 = model1$coef[5]
w_0 = model1$coef[6]
inter1 = I1 * u_0 + filter(I1, filter=r_1, method='recursive', side=1) * w_0
#inter1 = ts(inter1, start = c(2014,1),frequency = length(inter1))
plot(inter1, ylab='Intervention Effects', type='h', col = "blue", lwd = 2)

#Удаление модели интервенции с моментом интервенции (удаление модели из данных)
income.interven.deleted = income - inter1
plot(income.interven.deleted, main = paste("Income Intervention ", Intervention.Time, "deleted")
     , col = "blue", type = "b", lwd =2)
abline(v= Intervention.Time,col = "red",lwd = 1)

#Подберем ARIMA модель после удаления интервенции

#дифференцируем
plot(income.interven.deleted, type='b', lwd=2, col="blue", main="Income, intervention is deleted")
dincome_intdel = diff(income.interven.deleted, 1)
#q=6
acf(dincome_intdel,lag.max=20,lwd=3,col = "blue",main = "ACF")
#p=5
pacf(dincome_intdel,lag.max=20,lwd=3,col = "blue",main = "PACF")

model_intdel = arima(income.interven.deleted, order = c(5, 1, 6), method = "ML", optim.control = list(maxit = 1000))
AIC(model_intdel)

#Построение графиков, когда интервенция уже удалена
plot(income.interven.deleted, type='l', lwd=2, col="blue", main="Income")
lines(fitted(model_intdel), col='red', lwd=2, type='l')
legend('topleft', c('Income no interventions', 'ARIMA results'), bty='n', lwd=2, col=c('blue', 'red'))

#Исследование остатков после удаления модели ARIMA
plot(model_intdel$residuals, type='l', lwd=2, col="blue", main="Residuals")
qqnorm(model_intdel$residuals, main = "Residuals of model after deleted intervetion")
qqline(model_intdel$residuals)
#Формально проверим гипотезу о нормальности
shapiro.test(model_intdel$residuals)
#Остатки нормально распределены (на уровне значимости 5%)

#Проверим гипотезу об автокоррелированности
Box.test(model_intdel$residuals, lag = 20, type='Ljung-Box', fitdf = 5+1+6)
#Автокорреляции нет на уровне значимости 5%


