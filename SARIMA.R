library(tseries)
#Считываем данные
PATH = ""
data = read.csv(file.path(PATH, "1_1.csv"), header=TRUE, sep=";")

#Перейдем к объектам типа time series
importFact = ts(data$fact, start = c(2010, 1), frequency = 12)
#Посмотрим на данные. Видно, что ряд нестационарен, также нестабильна диссперсия
plot(importFact, type ="l",col = "blue",lwd = 1.5, xlab = 'Time', ylab = 'Value')
#Проверим гипотезу нестационарности формально
adf.test(importFact, alternative = c("stationary"), k=14)
#Получаем что p > 0.05 => гипотеза не отвергается

#Посмотрим так же на STL-декомпозицию
additive_model <- decompose(importFact, type = "additive")
plot(additive_model)
#Похоже на то, что есть тренд, что так же свидетельствует против стационарности ряда

#На мой взгляд, на графике исходных данных трудно уловить явную сезонность
#поэтому воспользуемся дифференцированием для приведения ряда к стационарному виду
X = diff(importFact, lag = 1)
adf.test(X, alternative = c("stationary"), k=14)
#гипотеза о нестационарности отвергается на уровне значимости 0.05
additive_model <- decompose(X, type = "additive")
plot(additive_model)
#Посмотрим на сами данные
plot(X, type ="l",col = "blue",lwd = 1.5, xlab = 'Time', ylab = 'Value')

#Сейчас ряд действительно похож на стационарный

#Подбор модели через построение ACF и PACF
acf(X, lwd = 3, col = "blue", lag.max = 25)
pacf(X, lwd = 3, col = "blue", lag.max = 25)

model1 = arima(importFact, order = c(3, 1, 3), seasonal = c(1, 0, 2), method = "ML", optim.control = list(maxit = 1000))
model1$aic
model2 = arima(importFact, order = c(3, 1, 3), seasonal = c(1, 0, 1), method = "ML", optim.control = list(maxit = 1000))
model2$aic
#Модель тем лучше, чем меньше у нее значение AIC => значит model2 лучше

#Посмотрим на остатки после удаления модели
acf(model1$residuals, lwd=3, col="blue", lag.max = 25, main="Residuals")
acf(model2$residuals, lwd=3, col="blue", lag.max = 25, main="Residuals")
#Исходя из визуального анализа ACF следует, что остатки неавтокоррелированы при данном числе лагов
model1$sigma2
model2$sigma2
#Проверим формально гипотезу некоррелированности
Box.test(model1$residuals, lag = 14, type = "Ljung-Box", fitdf = 10)
Box.test(model2$residuals, lag = 14, type = "Ljung-Box", fitdf = 9)
#Исходя из полученных результатов, сделаем вывод, что модель SARIMA(3,1,3)x(1,0,1) лучше описывает данные,
#ее остатки неавтокоррелированы(подтверждается критерием Льюнга-Бокса и коррелограммой)

#Model2
coeftest(model2)
print()
#Сделаем доп анализ остатков
#Посмотрим нормально ли распределение остатков
qqnorm(model1$residuals, main = "Model_1")
qqline(model1$residuals)
qqnorm(model2$residuals, main = "Model_2")
qqline(model2$residuals)
#Визуально видны тяжелые хвосты, проверим формально

#Проверим формально распределены ли остатки нормально
shapiro.test(model1$residuals)
shapiro.test(model2$residuals)

#Остатки ни у одной из моделей не имеют нормального распределения


