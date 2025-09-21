# Малов Илья,КМБО-01-20,вариант 16


library("lmtest")
library("GGally") 
library("car")  # без этого не работает функция vif()

# При чтении избавляемся от записей с недостающими данными.
data = na.omit(swiss)

# Выводим данные
data

# Examination ~ Fertility, Catholic, Agriculture

# 1. Проверим отсутствие зависимости между регрессорами перед построением модели
linfunc_1 = lm(Fertility~Catholic, data)
summary(linfunc_1) # R^2 < 22% - зависимости нет

linfunc_1 = lm(Fertility~Agriculture, data)
summary(linfunc_1) # R^2 < 13% - зависимости нет

linfunc_1 = lm(Catholic~Agriculture, data)
summary(linfunc_1) # R^2 < 17% - зависимости нет

# Можно использовать регрессоры вместе



# 2. Построим линейную модель и оценим её
model = lm(Examination ~ Fertility + Catholic + Agriculture, data)
summary(model) 
# R^2 ~ 0.69, p-значение у Catholic ненадёжно (одна звездочка) - модель достаточно хороша(остальные p-значения имеют по 3 звезды)

#Уберём из модели регрессор Catholic, как наименее значимый, и проверим, как изменится R^2 
model = lm(Examination ~ Fertility + Agriculture, data)
summary(model) 
# R^2 ~ 0.66 - R^2 практически не изменился(у всех параметров по 3 звезды)

# Попробуем убрать ещё один регрессор
model = lm(Examination ~ Fertility, data)
summary(model) #R^2 ~ 0.42 - изменился сильно, регрессор Agriculture лучше не убирать

# Попробуем убрать другой регрессор
model = lm(Examination ~ Agriculture, data)
summary(model) #R^2 ~ 0.47 - изменился сильно, регрессор Fertility лучше не убирать


# В дальнейшем будем работать с моделью:
model = lm(Examination ~ Fertility + Agriculture + Catholic , data) #R^2 ~ 0.69


# 3. Попробуем ввести в модель логарифмы регрессоров, предварительно проверяя, что нет линейной зависимости

model = lm(Examination ~ I(log(Fertility)) + I(log(Agriculture)) + I(log(Catholic)) , data)
vif(model) # линейной зависимости нет.
summary(model) #R^2 ~0.67

model = lm(I(log(Examination)) ~ I(log(Fertility)) + I(log(Agriculture)) + I(log(Catholic)) , data)     
vif(model) # линейной зависимости нет.
summary(model) # R^2 ~ 0.54, p-статистика неплоха,при I(log(Examination))- R-заметно снижается

model = lm(Examination ~ Fertility + Agriculture + I(log(Catholic)) , data)
vif(model) # линейной зависимости нет.
summary(model) #R^2 ~0.68

model = lm(Examination ~ I(log(Fertility)) + I(log(Agriculture)) + Catholic , data)     
vif(model) # линейной зависимости нет.
summary(model) # R^2 ~ 0.69

model = lm(Examination ~ Fertility + I(log(Agriculture)) + Catholic , data)     
vif(model) # линейной зависимости нет.
summary(model) # R^2 ~ 0.70, p-статистика достаточно хороша

model = lm(Examination ~ Fertility + I(log(Agriculture)) + I(log(Catholic)) , data)     
vif(model) # линейной зависимости нет.
summary(model) # R^2 ~ 0.68

model = lm(Examination ~ I(log(Fertility)) + Agriculture + I(log(Catholic)) , data)
vif(model) # линейной зависимости нет.
summary(model) #R^2 ~0.68

model = lm(Examination ~ I(log(Fertility)) + Agriculture + Catholic , data)
vif(model) # линейной зависимости нет.
summary(model) #R^2 ~0.69, p-статистика плоха для Catholic 

# Наилучшей из них будет следующая модель: 
model = lm(Examination ~ Fertility + I(log(Agriculture)) + Catholic , data)  # R^2 ~ 0.70


# 4. Попробуем ввести в модель всевозможные произведения пар регрессоров, предварительно проверяя, что нет линейной зависимости

model = lm(Examination ~ Fertility + I(log(Agriculture)) + Catholic + I(Fertility^2) + I(Agriculture^2) + I(Fertility*Agriculture) + I(Fertility*Catholic) + I(Catholic*Agriculture) + I(Catholic^2), data) 
vif(model) # есть линейная зависимость, уберём регрессоры с максимальным VIF

model = lm(Examination ~ Fertility + I(log(Agriculture)) + Catholic + I(Agriculture^2) + I(Fertility*Agriculture) + I(Fertility*Catholic) + I(Catholic*Agriculture) + I(Catholic^2), data) 
vif(model) # есть линейная зависимость, уберём регрессоры с максимальным VIF

model = lm(Examination ~ Fertility + I(log(Agriculture)) + Catholic + I(Agriculture^2) + I(Fertility*Agriculture) + I(Catholic*Agriculture) + I(Catholic^2), data) 
vif(model) # есть линейная зависимость, уберём регрессоры с максимальным VIF

model = lm(Examination ~ Fertility + I(log(Agriculture)) + Catholic + I(Agriculture^2) + I(Fertility*Agriculture) + I(Catholic*Agriculture), data) 
vif(model) # линейной зависимости нет

model = lm(Examination ~ Fertility + I(log(Agriculture)) + Catholic + I(Agriculture^2) + I(Catholic*Agriculture), data) 
vif(model)

model = lm(Examination ~ Fertility + I(log(Agriculture)) + Catholic + I(Agriculture^2), data) 
vif(model)

model = lm(Examination ~ Fertility + Catholic + I(Agriculture^2), data) 
vif(model)
summary(model) # R^2 ~ 0.65, p-статистика крайне плоха для Catholic

model = lm(Examination ~ Fertility + I(Agriculture^2), data) 
vif(model)
summary(model) # R^2~ 0.63

#Наилучшая модель:
model = lm(Examination ~ Fertility + I(log(Agriculture)) + Catholic , data)  # R^2 ~ 0.70
